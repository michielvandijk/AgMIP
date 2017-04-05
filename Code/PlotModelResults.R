#'========================================================================================================================================
#' Project:  FOODSECURE WP7
#' Subject:  Plot and compare model results
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

### FUNCTIONS
# Line plot to compare models
lineplot_f <- function(df, yas){
  unit_c <- paste(unique(df$unit), collapse = " and ")
  title <- unique(with(df, paste(variable, sector, sep="_")))
  title <- paste(title, unit_c, sep = "_")
  point <- filter(df, year == 2050)
  
  p = ggplot() +
    geom_line(data = df, aes(x = year, y = value, linetype = model, colour = scenario), size = 0.5) +
    geom_point(data = point, aes(x = year, y = value, shape = model, colour = scenario)) +
    scale_colour_manual(values = c("green","cyan","red","purple"), name="Scenario")+ 
    scale_linetype_manual(values=c("solid","longdash", "dotted"), name = "Model") +
    scale_shape_manual(values=c(16,17,18), name = "Model") +
    ylab(yas) + xlab("") +
    facet_wrap(~region, scale = "free")
  
  p = p +ggtitle(title) 
  
  p = p + guides(fill = guide_legend(keywidth = 1, keyheight = 1, override.aes = 
                                       list(alpha = 0.1, size = 0.5, colour = c("green","cyan","red","purple"))))
  
  p = p + theme_classic() +
    theme(panel.border = element_blank(),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
    scale_x_continuous(limits = c(2010,2050.1), breaks = seq(2010,2050,by=10),expand=c(0,0)) + # note that: Limits sets the limits for the axes (added 0.1) but always some space is added. Latter is controlled by expand
    scale_y_continuous(labels = comma) +
    theme(legend.background=element_blank()) +
    theme(legend.key=element_rect(size=0.5, color="white"), # Increase space between legend boxes - doubt if it works
          strip.background = element_rect(colour="white", fill="white")) # Remove box and background of facet
  
  
  p
}



# CALO EMIS EXPO FEED FOOD FRTN GDPpc GDPval IMDR IMPO NETT  NQT OTHU XCPI XPRI XPRP 

# Compare all
# # Line plot - index
# TOTAL_lineplot_i <- TOTAL2 %>%
#   group_by(variable, sector) %>%
#   select(-value) %>%
#   rename(value = index) %>%
#   do(plots = lineplot_f(., "Index")) 
# 
# pdf(file = "./Results/Graphs/TOTAL.pdf", width = 7, height = 7)
# TOTAL_lineplot_i$plots
# dev.off()

# Load data
TOTAL2 <- read.csv("Results/TOTAL_2017-02-16.csv")


# Comparison GDP, POP and YEXO
GDP_POP_YEXO <- TOTAL2 %>%
  filter(variable %in% c("POPT", "GDPT", "YEXO"), unit != "dm t/ha") 
# need to filter out dm t/ha (or fm t/ha - index is the same) to remove duplicate with fm t/ha
  
xtabs(~ model + variable, data = GDP_POP_YEXO)
xtabs(~ model + scenario, data = GDP_POP_YEXO)
xtabs(~ model + unit, data = GDP_POP_YEXO)
xtabs(~ variable + unit, data = GDP_POP_YEXO)

# Line plot - index
GDP_POP_YEXO_lineplot_i <- GDP_POP_YEXO %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Graphs/GDP_POP_YEXO_i.pdf", width = 7, height = 7)
GDP_POP_YEXO_lineplot_i$plots
dev.off()

# Comparison of consumption
# Filter out 1000 t dm otherwise there are two units per variable
CONS <- TOTAL2 %>% 
        filter(variable == "CONS", unit != "1000 t dm")
xtabs(~unit + sector, data = CONS)

# Line plot - index
CONS_lineplot_i <- CONS %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Graphs/CONS_lineplot_i.pdf", width = 7, height = 7)
CONS_lineplot_i$plots
dev.off()

# Comparison AREA and LAND
LAND_AREA <- TOTAL2 %>%
  filter(variable %in% c("AREA", "LAND")) 
xtabs(~unit + model + sector, data = LAND_AREA)


inf.nan.na.clean_f<-function(x){
  x[do.call(cbind, lapply(x, is.nan))]<-NA
  x[do.call(cbind, lapply(x, is.infinite))]<-NA
  x<-x[complete.cases(x),]
  return(x)
}

# IMAGE has zero values for LAND/AREA for a number of sector-region combinations
LAND_AREA <- inf.nan.na.clean_f(LAND_AREA)
xtabs(~model + sector, data = LAND_AREA)

# Line plot - index
LAND_AREA_lineplot_i <- LAND_AREA %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Graphs/LAND_AREA_line_i.pdf", width = 7, height = 7)
LAND_AREA_lineplot_i$plots
dev.off()

# Line plot - value
LAND_AREA_lineplot_ha <- LAND_AREA %>%
  group_by(variable, sector) %>%
  do(plots = lineplot_f(., "1000 ha")) 

pdf(file = "./Graphs/LAND_AREA_line_ha.pdf", width = 7, height = 7)
LAND_AREA_lineplot_ha$plots
dev.off()


### Comparison calorie based indicators
# Create df
CALO <- filter(TOTAL2, variable %in% c("CALO"))
xtabs(~model + unit, data = CALO)

# compare index
CALO_lineplot_i <- CALO %>%
  group_by(variable, sector, unit) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Graphs/CALO_line_i.pdf", width = 7, height = 7)
CALO_lineplot_i$plots
dev.off()

### Comparison of XPRP
# Create df
XPRP <- filter(TOTAL2, variable %in% c("XPRP"))
xtabs(~model + unit, data = XPRP)

# compare index
XPRP_lineplot_i <- XPRP %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Graphs/XPRP_line_i.pdf", width = 7, height = 7)
XPRP_lineplot_i$plots
dev.off()

### Comparison of XFPI
XFPI <- filter(TOTAL2, variable %in% c("XFPI"), unit != "Laspeyres index (2010=100)")
xtabs(~model + unit, data = XFPI)

# compare index
XFPI_lineplot_i <- XFPI %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Graphs/XFPI_line_i.pdf", width = 7, height = 7)
XFPI_lineplot_i$plots
dev.off()

### Comparison of XPRM
XPRM <- filter(TOTAL2, variable %in% c("XPRM"))
xtabs(~model + unit, data = XPRM)

# compare index
XPRM_lineplot_i <- XPRM %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Graphs/XPRM_line_i.pdf", width = 7, height = 7)
XPRM_lineplot_i$plots
dev.off()

### Comparison of other food security indicators
FS <- filter(TOTAL2, variable %in% c("PROT", "SHRFC", "GDPC", "SHRM", "IMDR", "WVSP")) 
xtabs(~model + unit + variable, data = FS)

# compare index
FS_lineplot_i <- FS %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Graphs/FS_line_i.pdf", width = 7, height = 7)
FS_lineplot_i$plots
dev.off()


# Check
check <- filter(TOTAL2, variable %in% c("IMDR")) 
xtabs(~model + unit + variable, data = FS)


# Comparison of other food security indicators
FS <- filter(TOTAL2, variable %in% c*"GDPC", "CALO",  "PROT", "SHRF", )
# 
# LAND_AREA_lineplot_WLD <- LAND_AREA_db %>%
#   filter(FSregion == "WLD") %>%
#   group_by(variable, sector) %>%
#   select(-value) %>%
#   rename(value = index) %>%
#   do(plots = lineplot_f(., "Index")) 
# 
# pdf(file = "./Results/Graphs/LAND_AREA_line_WLD.pdf", width = 7, height = 7)
# IM_GL_lineplot_WLD$plots
# dev.off()

# 
# # BW plot
# LAND_AREA_bwplot <- LAND_AREA_db %>%
#   group_by(variable, sector) %>%
#   select(-value) %>%
#   rename(value = index) %>%
#   do(plots = bwplot_f(., "Index")) 
# 
# pdf(file = "./Results/Graphs/LAND_AREA_bw.pdf", width = 7, height = 7)
# IM_GL_bwplot$plots
# dev.off()

# Compare base data models (by = 2010)
LAND_AREA_baseplot <- LAND_AREA %>%
    filter(scenario == "ECO") %>%
    group_by(variable, sector, scenario) %>%
  do(plots = baseplot_f(., 2010)) 

pdf(file = "./Graphs/LAND_AREA_base.pdf", width = 7, height = 7)
LAND_AREA_baseplot$plots
dev.off()

## Comparison YILD, PROD AND AREA
YILD_PROD_AREA <- filter(TOTAL2, variable %in% c("AREA", "PROD", "YILD"), !(unit %in% c("1000 t", "1000 t dm", "dm t/ha", "M USD 2007/cap"))) %>% 
  filter(model != "IMAGE")
xtabs(~model + unit, data = YILD_PROD_AREA)

#check <- filter(YILD_PROD_AREA, sector == "PRIMFOOD", variable == "PROD")
#xtabs(~model + unit, data = check)

# compare index
YILD_PROD_AREA_lineplot_i <- YILD_PROD_AREA %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Graphs/YILD_PROD_AREA_line_i.pdf", width = 7, height = 7)
YILD_PROD_AREA_lineplot_i$plots
dev.off()







# WDI data
# WDI <- WDI(country="all", indicator=c("SP.POP.TOTL"), 
#            start=1960, end=2015) %>%
#   mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c")) %>%
#   filter(!is.na(iso3c)) %>%
#   select(POP = SP.POP.TOTL, iso3c)

#saveRDS(WDI, file = paste("./Data/Add_data", "WDI_", Sys.Date(), ".rds", sep=""))
WDI <- readRDS(file = "./Data/Add_data/WDI_2016-05-25.rds")

# Region concordance
MAGNET2FS_REG <- read.csv(".\\Mappings\\MAGNET2FS_REG.csv") %>%
  dplyr::select(Region = FS_region2, FS_region_name_short) %>%
  unique()

# Country concordance
FS2ISO_REG <- read.csv(".\\Mappings\\FStoISO_MAGNETCountryAggregation.csv") %>%
  select(FS_region_name_short, iso3c = ISO) %>%
  left_join(., MAGNET2FS_REG)

## CALORIE CONSUMPTION
# Load historical data
histcal_r <- read.csv("./Data/Add_data/calcpcpd.csv") %>%
  mutate(iso3c = countrycode(AreaCode, "fao", "iso3c")) %>%
  filter(!is.na(iso3c)) %>% #remove reg aggregates
  select(iso3c, year = Year, value = Value) %>%
  left_join(., FS2ISO_REG) %>% 
  left_join(., WDI) %>%
  group_by(year, Region) %>%
  summarize(value = sum(value*POP, na.rm = T)/sum(POP, na.rm = T))

histcal_w <- read.csv("./Data/Add_data/calcpcpd.csv") %>%
  mutate(iso3c = countrycode(AreaCode, "fao", "iso3c")) %>%
  filter(!is.na(iso3c)) %>% #remove reg aggregates
  select(iso3c, year = Year, value = Value) %>%
  left_join(., FS2ISO_REG) %>% 
  left_join(., WDI) %>%
  group_by(year) %>%
  summarize(value = sum(value*POP, na.rm = T)/sum(POP, na.rm = T)) %>%
  mutate(Region = "WLD")

scen <- expand.grid(Scenario = unique(TOTAL$scenario), Region = unique(TOTAL$region))  
histcal <- rbind(histcal_r, histcal_w) %>%
  left_join(scen, .) %>%
  filter(year <=2010) %>%
  rename(scenario = Scenario, region = Region) %>%
  filter(year >=1990)

histcal_base <- filter(histcal, year == 2010) %>%
  rename(Base2010 = value) %>%
  select(-year) 

# Rebase simulations 2010 to historical data (2010=100)
CALO_i <- CALO %>%
  group_by(scenario, FSregion) %>%
  left_join(., histcal_base) %>%
  mutate(value = Base2010*index/100)

# kcd plot
pdf(file = "./Results/Graphs/CAL_line_kcd.pdf", width = 12, height = 12)
#bwplot2_f(CALO_i, histcal, "kcal/cap/d")
lineplot_f(CALO, "index")
dev.off()

# Plot
pdf(file = "./Results/Graphs/CAL_line_i.pdf", width = 12, height = 12)
#lineplot2_f(CALO_i, histcal, "kcal/cap/d")
lineplot_f(CALO_i, "kcal/cap/d")
dev.off()

# Prices

PRICES <- TOTAL2 %>%
  filter(variable %in% c("XPRI") & sector %in% c("AGR", "CRP"))

PRICES_i <- PRICES %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Results/Graphs/PRICES_line_i.pdf", width = 7, height = 7)
PRICES_i$plots
dev.off()









