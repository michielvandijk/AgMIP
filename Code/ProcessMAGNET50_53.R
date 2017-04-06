#######################################################
##### PROCESS MAGNET VARIABLES             ############
#######################################################

### PACKAGES
BasePackages <- c("readr", "lazyeval", "foreign", "stringr", "car", "zoo", "tidyr", "RColorBrewer", "plyr", "dplyr", "ggplot2", "haven")
lapply(BasePackages, library, character.only = TRUE)
AdditionalPackages <- c("gdxrrw")
lapply(AdditionalPackages, library, character.only = TRUE)

### load required GAMS libraries (folder user specific)
GAMSPath <- "C:\\24.4"
#GAMSPath <- "C:\\Program Files\\GAMS\\win64\\24.6"
igdx(GAMSPath)
# Make sure GDX2HAR.exe and gdxiomh.dll are located in one folder.

### Set working folder
wdPath <- "D:\\R\\AgMIP"
setwd(wdPath)  

dataPath <- "D:\\Diti\\MAGNET_PBL_SSP_PPP_NUTcor2noCCcor"
dataResultPath <- "D:\\Diti\\MAGNET_PBL_SSP_PPP_NUTcor2noCCcor\\4_MAGNET\\Results"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### Define scenarios, periods, path, project, sourcefile and 
scenarios <- read_csv("Mappings/scenMAGNET2agCLIM50_53_AT.csv")
scenarios <- scenarios$scenMAGNET
periods<-c("2007-2010", "2010-2020", "2020-2030", "2030-2050")

### Source script that creates file names
source("Code\\Load_Magnet.r")

### FUNCTIONS
# Simple aggregation over sectors using mapping
subtot_f <-function(df, grp, tvar, map){
  FUN <- match.fun("sum")
  df_subtot <- df %>%
    left_join(.,map) %>%
    na.omit %>% # Remove unmapped sectors
    group_by_(.dots=grp) %>%
    summarise_(value = interp(~FUN(v), v=as.name(tvar))) %>%
    ungroup()
}

# Weighted aggregation
wsubtot_f <-function(df, grp, tvar, weight, map){
  FUN <- match.fun("sum")
  df %>%
    left_join(.,map) %>%
    na.omit %>% # Remove unmapped sectors
    group_by_(.dots=grp) %>%
    #summarize(value = sum(x*y)/sum(y))
    summarise_(value = interp(~FUN(v*w)/FUN(w), v=as.name(tvar), w=as.name(weight))) %>%
    ungroup
}


### CREATE MAPPINGS
# Load concordance table MAGNET2FS
regMAGNET2agCLIM50 <- read_csv("Mappings/regMAGNET2agCLIM50.csv") %>%
  unique()

secMAGNET2agCLIM50 <- read_csv("Mappings/secMAGNET2agCLIM50.csv") %>%
  dplyr::select(-Note) %>%
  unique()

# Create regional and sectoral mappings
map_reg <- regMAGNET2agCLIM50 %>%
  select(REG, region) %>%
  na.omit %>%
  unique

map_con <- regMAGNET2agCLIM50 %>%
  select(REG, region = con) %>%
  na.omit %>%
  unique

map_wld <- regMAGNET2agCLIM50 %>%
  select(REG, region = wld) %>%
  na.omit %>%
  unique

map_sec <- secMAGNET2agCLIM50 %>%
  select(TRAD_COMM, sector = sec) %>%
  na.omit %>%
  unique

map_crp <- secMAGNET2agCLIM50 %>%
  select(TRAD_COMM, sector = crp) %>%
  na.omit %>%
  unique

map_lsp <- secMAGNET2agCLIM50 %>%
  select(TRAD_COMM, sector = lsp) %>%
  na.omit %>%
  unique

map_agr <- secMAGNET2agCLIM50 %>%
  select(TRAD_COMM, sector = agr) %>%
  na.omit %>%
  unique

map_tot <- secMAGNET2agCLIM50 %>%
  select(TRAD_COMM, sector = tot) %>%
  na.omit %>%
  unique
 
####################################################
#### Variables with sector and region dimension ####
####################################################

MAGNET1_raw <- list()

#### AREA: harvested and grazed area
# MAGNET: land demand per sector (km2) = AREA
# Note: several countries have multiple land types, here only GHA. As "ENDWL_COMM" is not in the grouping variable. These are summed.
MAGNET1_raw[["AREA"]] <- current.f("AREA", "BaseData_b.gdx", "LDEM", lookup_upd, "LDEM", c("PROD_SECT", "REG"), c("PROD_SECT", "REG")) %>%
  rename(TRAD_COMM = PROD_SECT) %>%
  mutate(value = value/10, # MAGNET AREA is in km2
         unit = "1000 ha")

#### PROD: total production
MAGNET1_raw[["PROD"]] <- constant.f("PROD", "VALOUTPUT", c("TRAD_COMM","REG", "GDPSOURCE"), c("TRAD_COMM", "REG"), "qo", c("NSAV_COMM", "REG")) %>%
  mutate(unit = "M USD 2007")

#### PRODval: Production value, market prices = VALOUTPUT(SSEC,SREG,SUM) AND VALOUTPUT(SSEC,SREG,SUM)  (NOT certified)
MAGNET1_raw[["PRODval"]] <- current.f("PROD", "BaseData_b_view.gdx", "valoutput", lookup_upd_view, "valoutput", c("TRAD_COMM", "REG", "OUTVALUE"), c("TRAD_COMM","REG")) %>%
  mutate(unit = "M USD")

#### CONS: total domestic consumption
# NB not CONS as defined in MAGNET but domestic use!

PROD <- constant.f("PROD", "VALOUTPUT", c("TRAD_COMM","REG", "GDPSOURCE"), c("TRAD_COMM", "REG"), "qo", c("NSAV_COMM", "REG")) %>%
  rename(PROD = value) %>%
  ungroup() %>%
  select(-variable)

# NB: in case of EXPO REGSOURCE is the exporter and REDDEST the importer.
### EXPO: export volume at market prices
EXPO <- constant2.f("EXPO", "BaseData_b.gdx", "VXMD", c("TRAD_COMM", "REGSOURCE", "REGDEST"), c("TRAD_COMM", "REGSOURCE", "REGDEST"), "qxs", c("TRAD_COMM", "REGSOURCE", "REGDEST")) %>%
  group_by(scenario, year, variable, REGSOURCE, TRAD_COMM) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  rename(REG = REGSOURCE, EXPO = value) %>%
  ungroup() %>%
  select(-variable) 

# NB: in case of IMPO, REGDEST is the importer and REGSOURCE the exporter.
### IMPO: import volume at market prices
IMPO <- constant2.f("IMPO", "BaseData_b.gdx", "VIMS", c("TRAD_COMM", "REGSOURCE", "REGDEST"), c("TRAD_COMM", "REGDEST", "REGSOURCE"), "qxs", c("TRAD_COMM", "REGSOURCE", "REGDEST")) %>%
  group_by(scenario, year, variable, REGDEST, TRAD_COMM) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  rename(REG = REGDEST, IMPO = value) %>% 
  ungroup() %>%
  select(-variable) 

# Domestic use
MAGNET1_raw[["CONS"]] <- left_join(PROD, IMPO) %>%
  left_join(., EXPO) %>%
  mutate(EXPO = ifelse(is.na(EXPO), 0, EXPO)) %>%
  mutate(IMPO = ifelse(is.na(IMPO), 0, IMPO)) %>%
  mutate(value = PROD+IMPO-EXPO) %>%
  select(-PROD, -IMPO, -EXPO) %>%
  mutate(variable = "CONS",
         unit = "M USD 2007")
rm(PROD, EXPO, IMPO)

# Nutrients per sector
# MAGNET1_raw[["NQSECT"]] <- current.f("NQSECT", "fsbasecalories_2007-2010_update_view.gdx",  "NQSECT", lookup_upd_view, "NQSECT", c("NUTRIENTS", "PRIM_AGRI", "REG"), c("NUTRIENTS", "PRIM_AGRI","REG"))  %>%
#   rename(TRAD_COMM = PRIM_AGRI, unit = NUTRIENTS)

# NB: in case of EXPO REGSOURCE is the exporter and REDDEST the importer.
### EXPO: export volume at market prices in volume
MAGNET1_raw[["EXPO"]] <- constant2.f("EXPO", "BaseData_b.gdx", "VXMD", c("TRAD_COMM", "REGSOURCE", "REGDEST"), c("TRAD_COMM", "REGSOURCE", "REGDEST"), "qxs", c("TRAD_COMM", "REGSOURCE", "REGDEST")) %>%
    group_by(scenario, year, variable, REGSOURCE, TRAD_COMM) %>%
    summarize(value = sum(value, na.rm=T)) %>%
    rename(REG = REGSOURCE) %>%
    ungroup() %>%
    mutate(variable = "EXPO",
         unit = "M USD 2007")

# NB: in case of IMPO, REGDEST is the importer and REGSOURCE the exporter.
### IMPO: import volume at market prices in volume
MAGNET1_raw[["IMPO"]] <- constant2.f("IMPO", "BaseData_b.gdx", "VIMS", c("TRAD_COMM", "REGSOURCE", "REGDEST"), c("TRAD_COMM", "REGDEST", "REGSOURCE"), "qxs", c("TRAD_COMM", "REGSOURCE", "REGDEST")) %>%
  group_by(scenario, year, variable, REGDEST, TRAD_COMM) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  rename(REG = REGDEST) %>% 
  ungroup() %>%
  mutate(variable = "IMPO",
       unit = "M USD 2007")

# NB: in case of EXPO REGSOURCE is the exporter and REDDEST the importer.
### EXPO: export value at market prices
MAGNET1_raw[["EXPOval"]] <- current.f("EXPO", "BaseData_b.gdx", "VXMD", lookup_upd, "VXMD", c("TRAD_COMM", "REGSOURCE", "REGDEST"), c("TRAD_COMM", "REGDEST", "REGSOURCE")) %>%
  group_by(scenario, year, variable, REGSOURCE, TRAD_COMM) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  rename(REG = REGSOURCE) %>%
  ungroup() %>%
  mutate(variable = "EXPO",
         unit = "M USD")

# NB: in case of IMPO, REGDEST is the importer and REGSOURCE the exporter.
### IMPO: import value at market prices
MAGNET1_raw[["IMPOval"]] <- current.f("IMPO", "BaseData_b.gdx", "VIMS", lookup_upd, "VIMS", c("TRAD_COMM", "REGSOURCE", "REGDEST"), c("TRAD_COMM", "REGDEST", "REGSOURCE")) %>%
  group_by(scenario, year, variable, REGDEST, TRAD_COMM) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  rename(REG = REGDEST) %>% 
  ungroup() %>%
  mutate(variable = "IMPO",
         unit = "M USD")


### PCONS: Total private domestic consumption volume
# Private domestic consumption volume
# Private consumption of domestic products volume
pridomconsvol <- constant2.f("pridomconsvol", "BaseData_b.gdx", "VDPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"), "qpd", c("TRAD_COMM", "REG"))
# Private consumption of imported products volume
priimpconsvol <- constant2.f("priimpconsvol", "BaseData_b.gdx", "VIPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"), "qpm", c("TRAD_COMM", "REG"))

MAGNET1_raw[["PCONS"]] <- rbind(pridomconsvol, priimpconsvol) %>%
  group_by(REG, TRAD_COMM, scenario, year) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = "PCONS",
         unit = "M USD 2007")
rm(pridomconsvol, priimpconsvol)

### PCONS: Total private domestic consumption value
# Private domestic consumption value
# Private consumption of domestic products value
pridomconsval <- current.f("pridomconsval", "BaseData_b.gdx", "VDPM", lookup_upd, "VDPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"))
# Private consumption of imported products value
priimpconsval <- current.f("priimpconsval", "BaseData_b.gdx", "VIPM", lookup_upd, "VIPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"))

# Total private domestic consumption val
MAGNET1_raw[["PCONSval"]] <- rbind(pridomconsval, priimpconsval) %>%
  group_by(REG, TRAD_COMM, scenario, year) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = "PCONS",
         unit = "M USD")
rm(pridomconsval, priimpconsval)


# Private consumption of imported products volume
MAGNET1_raw[["VIMP"]] <- constant2.f("priimpconsvol", "BaseData_b.gdx", "VIPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"), "qpm", c("TRAD_COMM", "REG")) %>%
  mutate(variable = "VIPM",
         unit = "M USD 2007")

# Private consumption of imported products value
MAGNET1_raw[["VIMPval"]] <- current.f("priimpconsval", "BaseData_b.gdx", "VIPM", lookup_upd, "VIPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG")) %>%
  mutate(variable = "VIPM",
         unit = "M USD")


### FOOD, FEED and OTHU
source("Code\\FOODFEED.r")
MAGNET1_raw[["FEED"]] <- FEED; rm(FEED)
MAGNET1_raw[["FOOD"]] <- FOOD; rm(FOOD)
MAGNET1_raw[["OTHU"]] <- OTHU; rm(OTHU)

############################################
### Variables with only region dimension ###
############################################
MAGNET2_raw <- list()

# GDP volume
MAGNET2_raw[["GDPT"]] <-  constant2.f("GDPT","BaseData_b_view.gdx", "GDPSRC", c("REG", "GDPSOURCE"), "REG", "qgdp", "REG") %>%
         mutate(unit = "M USD 2007")
       
# POP total population
MAGNET2_raw[["POPT"]] <- constant2.f("POPT", "BaseData_b.gdx", "POP", c("REG"), c("REG"), "pop", c("REG")) %>%
  mutate(unit = "Mpers")

# GDP value = GDPSRC(SREG,SUM) AND GDPSRC(SREG,SUM)  (NOT certified)
MAGNET2_raw[["GDPval"]] <- current.f("GDPT", "BaseData_b_view.gdx", "GDPSRC", lookup_upd_view, "GDPSRC", c("REG", "GDPSOURCE"), c("REG")) %>%
  mutate(unit = "M USD")

# MAGNET2_raw[["NQT"]] <- current.f("NQT", "fsbasecalories_2007-2010_update_view.gdx",  "NQT", lookup_upd_view, "NQT", c("NUTRIENTS", "REG"), c("NUTRIENTS", "REG")) %>%
#   rename(unit = NUTRIENTS)



#####################
### COMBINE DATA ###
####################

MAGNET1 <- bind_rows(MAGNET1_raw) %>%
              ungroup() %>%
              mutate(REG = toupper(REG)) # REG in capitals for mapping

MAGNET2 <- bind_rows(MAGNET2_raw) %>%
  mutate(REG = toupper(REG)) # REG in capitals for mapping
         
  

#########################################
### MAKE REGION AND SECTOR AGGREGATES ###
#########################################


# Sectoral mappings
MAGNET1 <- bind_rows(
  subtot_f(MAGNET1, c("scenario", "year", "sector", "REG", "variable", "unit"), "value", map_sec),
  subtot_f(MAGNET1, c("scenario", "year", "sector", "REG", "variable", "unit"), "value", map_agr),
  subtot_f(MAGNET1, c("scenario", "year", "sector", "REG", "variable", "unit"), "value", map_crp),
  subtot_f(MAGNET1, c("scenario", "year", "sector", "REG", "variable", "unit"), "value", map_tot)
)

# Regional mappings
MAGNET1 <-bind_rows(
  subtot_f(MAGNET1, c("scenario", "year", "sector", "region", "variable", "unit"), "value", map_reg),
  subtot_f(MAGNET1, c("scenario", "year", "sector", "region", "variable", "unit"), "value", map_wld),
  subtot_f(MAGNET1, c("scenario", "year", "sector", "region", "variable", "unit"), "value", map_con)
)

MAGNET2 <-bind_rows(
  subtot_f(MAGNET2, c("scenario", "year", "region", "variable", "unit"), "value", map_reg),
  subtot_f(MAGNET2, c("scenario", "year", "region", "variable", "unit"), "value", map_wld),
  subtot_f(MAGNET2, c("scenario", "year", "region", "variable", "unit"), "value", map_con)
) %>%
  mutate(sector = "TOT") # Add TOT for all national level indicators



#################################
### MERGE ALL AGGREGATED DATA ###
#################################

MAGNET1_2 <- rbind(MAGNET1, MAGNET2)


############################
### ADDITIONAL VARIABLES ###
############################

MAGNET3_raw <- list()

# 
# ### YILD: Endogenous yield
# # Need to replace LSP woth LPS defined over RMEAT and DAIRY only.
# PRODlsp <- constant.f("PROD", "VALOUTPUT", c("TRAD_COMM","REG", "GDPSOURCE"), c("TRAD_COMM", "REG"), "qo", c("NSAV_COMM", "REG")) %>%
#   mutate(unit = "mil 2007 USD", 
#          REG = toupper(REG)) %>% 
#   filter(TRAD_COMM %in% c("cattle", "milk"))
# 
# PRODlsp <- subtot_f(PRODlsp, c("scenario", "year", "sector", "REG", "variable", "unit"), "value", map_lsp)
# 
# # Regional mappings
# PRODlsp <-bind_rows(
#   subtot_f(PRODlsp, c("scenario", "year", "sector", "region", "variable", "unit"), "value", map_reg),
#   subtot_f(PRODlsp, c("scenario", "year", "sector", "region", "variable", "unit"), "value", map_wld),
#   subtot_f(PRODlsp, c("scenario", "year", "sector", "region", "variable", "unit"), "value", map_con)
# )

MAGNET3_raw[["YILD"]] <- bind_rows(
  filter(MAGNET1_2, variable %in% c("AREA") & 
           sector %in% c("AGR", "CGR", "CRP", "LSP", "DRY", "OSD", "PFB", "RIC", "RUM", "SGC", "VFN", 
                         "WHT", "TOT")),
  filter(MAGNET1_2, variable %in% c("PROD") & unit %in% c("mil 2007 USD") &
           sector %in% c("AGR", "CGR", "CRP", "DRY", "OSD", "PFB", "RIC", "RUM", "SGC", "VFN", 
                         "WHT", "TOT")),
  PRODlsp) %>% # Only sectors with land
  select(-unit) %>%
  group_by(scenario, region, sector, year) %>%
  dplyr::summarize(value = value[variable == "PROD"]/value[variable == "AREA"]) %>%
  mutate(variable = "YILD",
         unit = "1000 USD/ha")
rm(PRODlsp)

### YEXO: Exogenous yield
# Cumulative yield growth is extracted with aland2.f (2007 = 1)
# Note that in aland2.f year is made numeric, which creates some problems as it is character in all other data frames. This is corrected again.
aland <- aland2.f("aland", "aland", c("PROD_SECT", "MREG")) %>%
  rename(REG = MREG, 
         aland = value,
         TRAD_COMM = PROD_SECT) %>%
  select(-variable)  %>%
  mutate(year = as.character(year))


AREA <-current.f("AREA", "BaseData_b.gdx", "LDEM", lookup_upd, "LDEM", c("PROD_SECT", "REG"), c("PROD_SECT", "REG")) %>%
  rename(TRAD_COMM = PROD_SECT,
         AREA = value) %>% 
  select(-variable) %>%
  filter(year == 2007) %>%
  select(-year)

YEXO_raw <- left_join(aland, AREA) %>%
  mutate(aland_w = aland*AREA) %>%
  select(-aland) %>%
  gather(variable, value, -scenario: -year) %>%
  mutate(unit = "none",
         REG = toupper(REG))

# Sectoral mappings
YEXO_raw <- bind_rows(
  subtot_f(YEXO_raw, c("scenario", "year", "sector", "REG", "variable", "unit"), "value", map_sec),
  subtot_f(YEXO_raw, c("scenario", "year", "sector", "REG", "variable", "unit"), "value", map_agr),
  subtot_f(YEXO_raw, c("scenario", "year", "sector", "REG", "variable", "unit"), "value", map_crp),
  subtot_f(YEXO_raw, c("scenario", "year", "sector", "REG", "variable", "unit"), "value", map_tot)
)

# Regional mappings
YEXO_raw <-bind_rows(
  subtot_f(YEXO_raw, c("scenario", "year", "sector", "region", "variable", "unit"), "value", map_reg),
  subtot_f(YEXO_raw, c("scenario", "year", "sector", "region", "variable", "unit"), "value", map_wld),
  subtot_f(YEXO_raw, c("scenario", "year", "sector", "region", "variable", "unit"), "value", map_con)
)

MAGNET3_raw[["YEXO"]] <- YEXO_raw %>% 
  group_by(scenario, year, sector, region, unit) %>%
  summarize(value = value[variable == "aland_w"]/value[variable == "AREA"]) %>%
  mutate(variable = "YEXO",
         unit = "1000 USD/ha")
rm(YEXO_raw, AREA, aland)


### XPRX: Real export price 
GDPdef <- MAGNET1_2 %>%
  filter(variable %in% c("GDPT")) %>%
  mutate(variable = ifelse(unit == "M USD", "GDPval", variable)) %>%
  select(-unit, -sector) %>%
  spread(variable, value) 

# Paasche price index
MAGNET3_raw[["XPRX"]] <- MAGNET1_2 %>%
  filter(variable %in% c("EXPO")) %>%
  mutate(variable = ifelse(unit == "M USD", "EXPOval", variable)) %>%
  select(-unit) %>%
  spread(variable, value) %>%
  left_join(., GDPdef) %>%
  mutate(value = EXPOval/EXPO/GDPval*GDPT,
         variable = "XPRX",
         unit = "Paasche index") %>%
  select(-EXPO, -EXPOval, -GDPT, -GDPval)
rm(GDPdef)


### XPRP Real producer price 
# Deflator
GDPdef <- MAGNET1_2 %>%
  filter(variable %in% c("GDPT")) %>%
  mutate(variable = ifelse(unit == "M USD", "GDPval", variable)) %>%
  select(-unit, -sector) %>%
  spread(variable, value)  

# Paasche price index
MAGNET3_raw[["XPRP"]] <- MAGNET1_2 %>%
  filter(variable %in% c("PROD")) %>%
  mutate(variable = ifelse(unit == "M USD", "PRODval", variable)) %>%
  select(-unit) %>%
  spread(variable, value) %>%
  left_join(., GDPdef) %>%
  mutate(value = PRODval/PROD/GDPval*GDPT,
         variable = "XPRP",
         unit = "Paasche index (2007=100)") %>%
  select(-GDPT, -GDPval, -PROD, -PRODval)
rm(GDPdef)        


### NETT: Net trade
MAGNET3_raw[["NETT"]] <- MAGNET1_2 %>%
  filter(variable %in% c("EXPO", "IMPO") & unit == "M USD 2007") %>%
  spread(variable, value) %>%
  mutate(value = EXPO - IMPO,
         variable = "NETT",
         unit = "M USD 2007") %>%
  select(-EXPO, -IMPO)

### CAPITAL AND LABOUR PRICES
VFMval <-current.f("VFM", "baseData_b.gdx", "VFM", lookup_upd, "VFM", c("ENDW_COMM","PROD_SECT", "REG"), c("ENDW_COMM","PROD_SECT", "REG")) %>%
  rename(TRAD_COMM = PROD_SECT) %>%
  mutate(unit = "M USD")

VFMvol <- constant.3f("VFM", "VFM", c("ENDW_COMM","PROD_SECT", "REG"), c("ENDW_COMM","PROD_SECT", "REG"), "qf", c("ENDW_COMM","PROD_SECT", "REG")) %>%
  rename(TRAD_COMM = PROD_SECT) %>%
  mutate(unit = "M USD 2007")

VFM <- bind_rows(VFMvol, VFMval) %>%
  mutate(REG = toupper(REG)) # REG in capitals for mapping

VFM <- bind_rows(
  subtot_f(VFM, c("ENDW_COMM", "scenario", "year", "sector", "REG", "variable", "unit"), "value", map_sec),
  subtot_f(VFM, c("ENDW_COMM", "scenario", "year", "sector", "REG", "variable", "unit"), "value", map_agr),
  subtot_f(VFM, c("ENDW_COMM", "scenario", "year", "sector", "REG", "variable", "unit"), "value", map_crp),
  subtot_f(VFM, c("ENDW_COMM", "scenario", "year", "sector", "REG", "variable", "unit"), "value", map_tot)
)

# Regional mappings
VFM <-bind_rows(
  subtot_f(VFM, c("ENDW_COMM", "scenario", "year", "sector", "region", "variable", "unit"), "value", map_reg),
  subtot_f(VFM, c("ENDW_COMM", "scenario", "year", "sector", "region", "variable", "unit"), "value", map_wld),
  subtot_f(VFM, c("ENDW_COMM", "scenario", "year", "sector", "region", "variable", "unit"), "value", map_con)
)


GDPdef <- MAGNET1_2 %>%
  filter(variable %in% c("GDPT")) %>%
  mutate(variable = ifelse(unit == "M USD", "GDPval", variable)) %>%
  select(-unit, -sector) %>%
  spread(variable, value) 


### XPRP: CAPITAL
MAGNET3_raw[["XPRP_CAP"]] <- VFM %>%
  filter(ENDW_COMM %in% c("Capital"), sector == "TOT") %>%
  mutate(variable = ifelse(unit == "M USD", "VFMval", variable)) %>%
  select(-unit) %>%
  spread(variable, value) %>%
  left_join(., GDPdef) %>%
  mutate(value = VFMval/VFM/GDPval*GDPT,
         variable = "XPRP",
         unit = "Paasche index",
         sector = "CAP") %>%
  select(-VFM, -VFMval, -GDPT, -GDPval, -ENDW_COMM)


### XPRP: LABOUR
MAGNET3_raw[["XPRP_LAB"]] <- VFM %>%
  filter(ENDW_COMM %in% c("UnSkLab", "SkLab"), sector == "TOT") %>%
  group_by(year, scenario, sector, region, variable, unit) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(variable = ifelse(unit == "M USD", "VFMval", variable)) %>%
  select(-unit) %>%
  spread(variable, value) %>%
  left_join(., GDPdef) %>%
  mutate(value = VFMval/VFM/GDPval*GDPT,
         variable = "XPRP",
         unit = "Paasche index",
         sector = "LAB") %>%
  select(-VFM, -VFMval, -GDPT, -GDPval)

rm(VFM, GDPdef, VFMval, VFMvol)

#################
### MERGE ALL ###
#################

MAGNET_tot <- bind_rows(MAGNET1_2, MAGNET3_raw) %>%
  mutate(model = "MAGNET",
         year = as.numeric(year))



###################
### CORRECTIONS ###
###################

# Area in Canada for rice is zero, resulting in infinite values for YILD
# Set to zero
MAGNET_tot$value[is.infinite(MAGNET_tot$value)] <- 0

# agCLIM50 uses - LYLD for	Livestock yield (endogenous) and LYXO for	Exogenous livestock yield trend 
# We change the names
MAGNET_tot$variable[MAGNET_tot$variable == "YILD" & MAGNET_tot$sector %in% c("LSP", "DRY", "OAP", "RUM")] <- "LYLD"
MAGNET_tot$variable[MAGNET_tot$variable == "YEXO" & MAGNET_tot$sector %in% c("LSP", "DRY", "OAP", "RUM")] <- "LYXO"

# Change sector into item
MAGNET_tot <- rename(MAGNET_tot, item = sector)

# Rename scenarios in line with agCLIM50
scenMAGNET2agCLIM50 <- read_csv("Mappings/scenMAGNET2agCLIM50_53_AT.csv") %>%
  rename(scenario = scenMAGNET)

MAGNET_tot <- left_join(MAGNET_tot, scenMAGNET2agCLIM50) %>%
  select(-scenario) %>%
  rename(scenario = scenagCLIM50)

# Remove values in current values
xtabs(~variable + scenario, data = MAGNET_tot)
MAGNET_tot <- filter(MAGNET_tot, unit != "mil USD")

############
### SAVE ###
############

write_csv(MAGNET_tot, paste("Cache/agmip_MAGNET2_", Sys.Date(), ".csv", sep=""))
xtabs(~item+variable, data = MAGNET_tot)
xtabs(~scenario+variable, data = MAGNET_tot)