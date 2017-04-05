#'========================================================================================================================================
#' Project:  FOODSECURE WP7
#' Subject:  Script to create tables
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("WDI", "countrycode", "stargazer")

### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### DATAPATH
dataPath <- "C:\\Users\\vandijkm\\Dropbox\\FOODSECURE Scenarios\\Results"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### LOAD DATA CUBE
TOTAL <- read_csv(file.path(dataPath, "TOTAL_2017-02-16.csv"))

### SELECT RELEVANT INDICATORS
FNS_db <- TOTAL %>%
  filter(model %in% c("MAGNET", "GLOBIOM"), 
         #(variable == "GDPC" & sector == "TOT") |
         (variable == "XFPI" & sector == "AGR" & unit == "Paasche index (2010=100)") |
           (variable == "XFPI" & sector == "AGR" & unit == "Paasche index (2007=100)") |
           (variable == "PROT" & unit ==  "g prt/cap/d" & sector == "LSP") |
           (variable == "IMDR" & unit ==  "%" & sector == "CER") |           
           (variable == "CALO" & unit ==  "g prt/cap/d" & sector == "LSP") |
           (variable == "CALO" & unit ==  "kcal/cap/d" & sector == "TOT") |
           (variable == "CALO" & unit ==  "%" & sector == "CER")) %>%
  mutate(variable = ifelse((model == "GLOBIOM" & variable == "CALO" & unit == "g prt/cap/d"), "PROT", variable),
         FNS = paste(variable, sector, sep = "_")) %>%
  ungroup()

FNS_db <- bind_rows(
  FNS_db %>%
    group_by(scenario, FNS, year, region) %>%
    summarize(index = mean(index)) %>%
    mutate(model = "Average"),
  FNS_db %>%
    group_by(scenario, FNS, year, region, model) %>%
    summarize(index = mean(index))
)

### ANOVA
aov_db <- filter(FNS_db, model != "Average") %>%
  spread(FNS, index) %>%
  filter(year != 2010) %>%
  select(-IMDR_CER)

CALO_CER_aov <- aov(CALO_CER ~ factor(year) + factor(scenario) + factor(region) + factor(model), data = aov_db)
CALO_TOT_aov <- aov(CALO_TOT ~ factor(year) + factor(scenario) + factor(region) + factor(model), data = aov_db)
PROT_LSP_aov <- aov(PROT_LSP ~ factor(year) + factor(scenario) + factor(region) + factor(model), data = aov_db)
XFPI_AGR_aov <- aov(XFPI_AGR ~ factor(year) + factor(scenario) + factor(region) + factor(model), data = aov_db)

aov_prep <- data.frame(class = c("year", "scenario", "region","model", "residuals"),
                      CALO_CER = summary(CALO_CER_aov)[[1]]$'Sum Sq',
                      CALO_TOT = summary(CALO_TOT_aov)[[1]]$'Sum Sq',
                      PROT_LSP = summary(PROT_LSP_aov)[[1]]$'Sum Sq',
                      XFPI_AGR = summary(XFPI_AGR_aov)[[1]]$'Sum Sq') %>%
  gather(variable, value, -class)

Tab_aov <- aov_prep %>%
  group_by(variable) %>%
  mutate(var_sh = value/sum(value)*100) %>%
  select(-value) %>%
  spread(variable, var_sh)
                     

### CORRELATIONs
cor_db <- filter(FNS_db, model != "Average") %>%
  spread(model, index) %>%
  filter(year != 2010) 

lapply
cor(cor_db[c("MAGNET", "GLOBIOM")], method = "pearson")
cor(cor_db[c("MAGNET", "GLOBIOM")], method = "spearman")
cor(cor_db[c("MAGNET", "GLOBIOM")], method = "kendall")


# Write data
write_csv(file.path(root, "Produced_data/"))

# Create table