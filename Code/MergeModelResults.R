#'========================================================================================================================================
#' Project:  FOODSECURE WP7
#' Subject:  Merge model results for comparison
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
BasePackages<- c("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer")
lapply(BasePackages, library, character.only = TRUE)
#SpatialPackages<-c("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
#lapply(SpatialPackages, library, character.only = TRUE)
#AdditionalPackages <- c("WDI")
#lapply(AdditionalPackages, library, character.only = TRUE)

### SET WORKING DIRECTORY
#wdPath<-"C:\\Users\\vandijkm\\Dropbox\\FOODSECURE Scenarios"
wdPath<-"D:\\Dropbox\\FOODSECURE Scenarios"
setwd(wdPath)

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


## READ DATA
# GLOBIOM
# Process
GLOBIOM <- read.csv("./Results/GLOBIOM_FoodSecure_15dec16.csv") %>% 
  rename(variable = Var, sector = Item, scenario = Scen, year = Year, value = Val, region = Reg, unit = Unit) %>% 
  mutate(model = "GLOBIOM", 
         variable =toupper(variable), 
         sector=toupper(sector)) 
xtabs(~GLOBIOM$sector + GLOBIOM$variable)

# Check if there are variables with missing information for 2010
# There are a few combination in GLOBIOM that lack 2010 data
check <- GLOBIOM %>%
  arrange(model, scenario, region, sector, variable, unit, year) %>%
  group_by(model, scenario, region, sector, unit, variable) %>%
  filter(!any(year==2010))
#write.csv(check, file = "./Results/GLOBIOMmiss.csv", row.names = F)

GLOBIOM <- GLOBIOM %>%
  arrange(model, scenario, region, sector, variable, unit, year) %>%
  group_by(model, scenario, region, sector, variable, unit) %>%
  filter(any(year==2010)) # to remove values with missing 2010 
xtabs(~sector + variable, data = GLOBIOM)
xtabs(~unit + variable, data = GLOBIOM)

# Process
IMAGE <- read_excel("./Results/IMAGE_19-12-2016.xlsx") %>% 
    rename(variable = Variable, scenario = Scenario, model = Model, unit = Unit, region = Region, year = Year, value = Value, sector = Item) %>%
    mutate(year = as.numeric(year),
           unit = ifelse(unit == "B USD 2005", "B USD 2005 PPP", unit),
           unit = ifelse(unit == "million", "Mpers", unit),
           scenario = ifelse(scenario ==  "FFNF", "FFANF", scenario)) %>%
    na.omit # removes rows that are all NA
  
xtabs(~sector + variable, data = IMAGE)
xtabs(~scenario + variable, data = IMAGE)

# Check
unique(IMAGE$variable)
unique(IMAGE$sector)
xtabs(~IMAGE$variable + IMAGE$sector)
# check <- filter(IMAGE, variable == "GDPT") %>%
#   spread(scenario, value)

# MAGNET
MAGNET <- read.csv("./Results/MAGNET_t_st_2017-02-15.csv") %>%
            rename(sector = FSsector, region = FSregion) %>%
            select(-Modelrun) %>%
            filter(unit != "mil USD") %>%
            mutate(unit = ifelse(variable %in% c("NQSECT", "NQT"), tolower(unit), unit)) # lowercase units

# Remove lower level regional aggregations
MAGNET <- filter(MAGNET, !(region %in% c("CHN", "GHA", "IDN", "IND", "NAF", "SAF", "UGA", "EAF", "WAF", "KEN")))

# Remove _M sectors that do not include primary processing
Xsector <- unique(MAGNET$sector[grep("_M", MAGNET$sector)])
MAGNET <- filter(MAGNET, !(sector %in% Xsector))

# Remove current values and some nutrient variables
MAGNET <- filter(MAGNET, !(unit %in% c("M USD", "prot", "quant", "cal", "carb", "fat")))

# Remove variables that are not needed for the analysis
MAGNET <- filter(MAGNET, !(variable %in% c("PCONS")))

# check
xtabs(~MAGNET$variable + MAGNET$unit) 
xtabs(~MAGNET$variable + MAGNET$region) 
xtabs(~MAGNET$variable + MAGNET$sector) 


# Bind in one file
SIMULATION <- bind_rows(MAGNET, IMAGE, GLOBIOM) %>% 
              filter(year>=2010)

TOTAL <- SIMULATION



# Index (2010=100)
TOTAL2 <- TOTAL %>%
  arrange(model, scenario, region, sector, variable, year, unit) %>%
  group_by(model, scenario, region, sector, variable, unit) %>%
  mutate(index = value/value[year==2010]*100) %>%
  arrange(model, scenario, variable, region, sector, year)

xtabs(~variable + model, data = TOTAL2)
xtabs(~sector + model, data = TOTAL2)
xtabs(~unit + model, data = TOTAL2)
xtabs(~region + model, data = TOTAL2)
xtabs(~scenario + model, data = TOTAL2)

# Save files
write.csv(TOTAL2, paste0("Results/TOTAL_", Sys.Date(), ".csv"), row.names = F)
