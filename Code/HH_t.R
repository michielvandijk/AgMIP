### PACKAGES
BasePackages <- c("lazyeval", "foreign", "stringr", "car", "zoo", "tidyr", "RColorBrewer", "plyr", "dplyr", "ggplot2", "haven")
lapply(BasePackages, library, character.only = TRUE)
AdditionalPackages <- c("gdxrrw", "micEcon")
lapply(AdditionalPackages, library, character.only = TRUE)

### load required GAMS libraries (folder user specific)
GAMSPath <- "C:\\24.4"
#GAMSPath <- "C:\\Program Files\\GAMS\\win64\\24.6"
igdx(GAMSPath)
# Make sure GDX2HAR.exe and gdxiomh.dll are located in one folder.

### Set working folder
wdPath <- "D:\\R\\FSWP7"
setwd(wdPath)  

dataPath <- "D:\\Shutes\\FOODSECURE/R"
dataResultPath <- "D:\\Shutes\\FOODSECURE/4_MAGNET/Results"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### Define scenarios, periods, path, project, sourcefile and 
scenarios<-c("FFANF_qpc_t_st", "ONEPW_qpc_t_st", "TLTL_qpc_t_st", "ECO_qpc_t_st")
periods<-c("2007-2010", "2010-2020", "2020-2030", "2030-2040", "2040-2050")


### Source script that creates file names
source(".\\Code\\Load_Magnet.r")

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
MAGNET2FS_REG <- read.csv("Concordance\\MAGNET2FS_REG.csv") %>%
  rename(REG = FS_MAGNET) %>%
  unique()

MAGNET2FS_SEC <- read.csv("Concordance\\MAGNET2FS_SEC.csv", na.strings = "") %>%
  dplyr::rename(sector = FS_sector_code, TRAD_COMM = FS_MAGNET_sector_name) %>%
  dplyr::filter(!is.na(sector)) %>% # remove unmapped feed sector in MAGNET
  unique()

# Create regional and sectoral mappings
# Regional mappings
map_fsreg <- MAGNET2FS_REG %>%
  select(REG, FSregion = FS_region2) %>%
  na.omit %>%
  unique

map_wld <- MAGNET2FS_REG %>%
  select(REG, FSregion = World) %>%
  na.omit %>%
  unique

map_af <- MAGNET2FS_REG %>%
  select(REG, FSregion = FS_region3) %>%
  mutate(FSregion = zap_empty(FSregion)) %>%
  na.omit %>%
  unique

map_hh <- MAGNET2FS_REG %>%
  select(REG, FSregion = FS_hhregion) %>%
  mutate(FSregion = zap_empty(FSregion)) %>%
  na.omit %>%
  unique

# Sectoral mappings
map_cer <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = cer) %>%
  na.omit %>%
  unique

map_food <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = food) %>%
  na.omit %>%
  unique

map_crp <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = crp) %>%
  na.omit %>%
  unique

map_lsp <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = lsp) %>%
  na.omit %>%
  unique

map_lspfsh <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = lspfsh) %>%
  na.omit %>%
  unique

map_agr <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = agr) %>%
  na.omit %>%
  unique

map_sec <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = sec) %>%
  na.omit %>%
  unique

map_primfood <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = primfood) %>%
  na.omit %>%
  unique

map_anml <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = anml) %>%
  na.omit %>%
  unique

map_tot <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = tot) %>%
  na.omit %>%
  unique

map_sec_M <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = sec_M) %>%
  na.omit %>%
  unique

map_tot_M <- MAGNET2FS_SEC %>%
  transmute(TRAD_COMM, FSsector = tot_M) %>%
  na.omit %>%
  unique


####################################################
#### Variables with sector and region dimension ####
####################################################

MAGNETHH1_raw <- list()

# Nutrients per sector
MAGNETHH1_raw[["NQSECTH"]]  <- current.f("NQSECTH", "basedata_b_view",  "NQSECTH", lookup_upd_view, "NQSECTH", c("NUTRIENTS", "PRIM_AGRI", "HHLD", "HH_REG"), c("NUTRIENTS", "PRIM_AGRI", "HHLD", "HH_REG"))  %>%
  rename(TRAD_COMM = PRIM_AGRI, unit = NUTRIENTS, REG = HH_REG) %>%
  mutate(REG = toupper(REG))

### CONS: Total consumption at AGENTS' prices
# Note that in basedata_b hh are defined over HHS, NOT HHLD but the code is flexible and overwrites the name!!

# Private consumption of domestic products volume at hh level
VDPMH <- constant2.f("VDPMH", "BaseData_b.gdx", "VDPMH", c("TRAD_COMM", "HHLD", "HH_REG"), c("TRAD_COMM", "HHLD", "HH_REG"), "qpdh", c("TRAD_COMM", "HHLD", "HH_REG"))
# Private consumption of imported products volume at hh level
VIPMH <- constant2.f("VIPMH", "BaseData_b.gdx", "VIPMH", c("TRAD_COMM", "HHLD", "HH_REG"), c("TRAD_COMM", "HHLD", "HH_REG"), "qpmh", c("TRAD_COMM", "HHLD", "HH_REG"))

MAGNETHH1_raw[["CONSH"]] <- rbind(VDPMH, VIPMH) %>%
  group_by(HH_REG, HHLD, TRAD_COMM, scenario, year) %>%
  summarize(value = sum(value)) %>%
  rename(REG = HH_REG) %>%
  ungroup() %>%
  mutate(variable = "CONSH",
         unit = "M USD 2007",
         REG = toupper(REG))

############################################
### Variables with only region dimension ###
############################################
MAGNETHH2_raw <- list()

# POPT per HH group
MAGNETHH2_raw[["POPTH"]] <- constant2.f("POPTH", "BaseData_b.gdx", "POPH", c("HHLD", "HH_REG"), c("HHLD", "HH_REG"), "poph", c("HHLD", "HH_REG")) %>%
  rename(REG = HH_REG) %>%
  mutate(REG = toupper(REG), 
         unit = "Mpers",
         FSsector = "TOT") 
 
#####################
### COMBINE DATA ###
####################

MAGNETHH1 <- bind_rows(MAGNETHH1_raw) %>%
  ungroup() %>%
  mutate(REG = toupper(REG)) # REG in capitals for mapping

MAGNETHH2 <- bind_rows(MAGNETHH2_raw) %>%
  ungroup() %>%
  mutate(REG = toupper(REG)) # REG in capitals for mapping

#########################################
### MAKE REGION AND SECTOR AGGREGATES ###
#########################################

# Sectoral mappings
MAGNETHH1 <- bind_rows(
  subtot_f(MAGNETHH1, c("scenario", "year", "FSsector", "REG", "variable", "HHLD", "unit"), "value", map_cer),
  subtot_f(MAGNETHH1, c("scenario", "year", "FSsector", "REG", "variable", "HHLD", "unit"), "value", map_sec),
  subtot_f(MAGNETHH1, c("scenario", "year", "FSsector", "REG", "variable", "HHLD", "unit"), "value", map_lspfsh),
  subtot_f(MAGNETHH1, c("scenario", "year", "FSsector", "REG", "variable", "HHLD", "unit"), "value", map_sec_M),
  subtot_f(MAGNETHH1, c("scenario", "year", "FSsector", "REG", "variable", "HHLD", "unit"), "value", map_tot_M),
  subtot_f(MAGNETHH1, c("scenario", "year", "FSsector", "REG", "variable", "HHLD", "unit"), "value", map_agr),
  subtot_f(MAGNETHH1, c("scenario", "year", "FSsector", "REG", "variable", "HHLD", "unit"), "value", map_crp),
  subtot_f(MAGNETHH1, c("scenario", "year", "FSsector", "REG", "variable", "HHLD", "unit"), "value", map_primfood),
  subtot_f(MAGNETHH1, c("scenario", "year", "FSsector", "REG", "variable", "HHLD", "unit"), "value", map_anml),
  subtot_f(MAGNETHH1, c("scenario", "year", "FSsector", "REG", "variable", "HHLD", "unit"), "value", map_food),
  subtot_f(MAGNETHH1, c("scenario", "year", "FSsector", "REG", "variable", "HHLD", "unit"), "value", map_tot)
)

# Regional mappings
MAGNETHH1 <-bind_rows(
   subtot_f(MAGNETHH1, c("scenario", "year", "FSsector", "FSregion", "variable", "HHLD", "unit"), "value", map_hh)
)

# Regional mappings
MAGNETHH2 <-bind_rows(
  subtot_f(MAGNETHH2, c("scenario", "year", "FSsector", "FSregion", "variable", "HHLD", "unit"), "value", map_hh)
)

#################################
### MERGE ALL AGGREGATED DATA ###
#################################

MAGNETHH1_2 <- rbind(MAGNETHH1, MAGNETHH2)



### HOUSEHOLD LEVEL VARIABLES
####################
### AVAILABILITY ###
####################

AV <- list()

# # AV2
# # Net Share of energy supply (calories) derived from cereals
# # Nutrients
AV[["AV2h"]] <- MAGNETHH1_2 %>%
  filter(variable %in% c("NQSECTH") & unit %in% c("CAL") &  FSsector %in% c("TOT","CER")) %>%
  group_by(scenario, FSregion, year, HHLD) %>%
  summarize(value = (value[FSsector == "CER"]/value[FSsector == "TOT"]*100)) %>%
  mutate(FSsector = "CER",
         unit = "%",
         variable = "CALO")

# AV3ah
# Average supply of protein derived from animal sources
AV[["AV3ah"]] <- MAGNETHH1_2 %>% 
  filter(variable %in% c("POPTH") | (variable %in% c("NQSECTH") & unit %in% c("PROT") & FSsector %in% c("LSP"))) %>%
  group_by(scenario, FSregion, HHLD, year) %>%
  summarize(value = (value[variable == "NQSECTH"]/value[variable == "POPTH"]/365)) %>%
  mutate(FSsector = "LSP",
         unit = "g prt/cap/day",
         variable = "PROT") %>%
  filter(!is.infinite(value))


AV[["AV3bh"]] <- MAGNETHH1_2 %>% 
  filter(variable %in% c("POPTH") | (variable %in% c("NQSECTH") & unit %in% c("PROT") & FSsector %in% c("LSPFSH"))) %>%
  group_by(scenario, FSregion, HHLD, year) %>%
  summarize(value = (value[variable == "NQSECTH"]/value[variable == "POPTH"]/365)) %>%
  mutate(FSsector = "LSPFSH",
         unit = "g prt/cap/day",
         variable = "PROT") %>%
  filter(!is.infinite(value))



####################
### ACCESIBILITY ###
####################
AC <- list()

# # AC1h	Average share of food expenditures in total household expenditures
VPA <- bind_rows(
  current.f("VDPAH", "BaseData_b.gdx", "VDPAH", lookup_upd, "VDPAH", c("TRAD_COMM", "HHLD", "HH_REG"), c("TRAD_COMM", "HHLD", "HH_REG")) %>%
    mutate(unit = "mil. USD",
           REG = toupper(HH_REG)),
  current.f("VIPAH", "BaseData_b.gdx", "VIPAH", lookup_upd, "VIPAH", c("TRAD_COMM", "HHLD", "HH_REG"), c("TRAD_COMM","HHLD", "HH_REG")) %>%
    mutate(unit = "mil. USD",
           REG = toupper(HH_REG))) %>%
  group_by(scenario, REG, HHLD, year, TRAD_COMM) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = "VPA",
         unit = "M USD")

VPA <- bind_rows(
  subtot_f(VPA, c("scenario", "year", "FSsector", "REG", "variable", "HHLD", "unit"), "value", map_food),
  subtot_f(VPA, c("scenario", "year", "FSsector", "REG", "variable", "HHLD", "unit"), "value", map_tot)
)

VPA <-bind_rows(
  subtot_f(VPA, c("scenario", "year", "FSsector", "FSregion", "variable", "HHLD", "unit"), "value", map_hh)
)

AC[["SHRFC"]] <- VPA %>%
  group_by(scenario, FSregion, HHLD, year) %>%
  summarize(value = value[FSsector == "FOOD"]/value[FSsector == "TOT"]*100) %>%
  mutate(FSsector = "TOT",
         variable = "SHRFCH",
         unit = "%")


# AC5: HINC
# Private consumption of domestic products volume at hh level
AC[["HINC"]] <- MAGNETHH1_2 %>%
  filter(variable %in% c("POPTH", "CONSH") & FSsector %in% c("TOT")) %>%
  group_by(scenario, FSregion, FSsector, HHLD, year) %>%
  summarize(value = (value[variable == "CONSH"]/value[variable == "POPTH"])) %>%
  mutate(unit = "2007 USD/cap",
         variable = "HINC") %>%
  filter(!is.infinite(value))
  
# AC6: Household food consumption per capita
AC[["CONS"]] <- MAGNETHH1_2 %>%
  filter((variable %in% c("POPTH") & FSsector %in% c("TOT")) | (variable %in% c("CONSH") & FSsector %in% c("FOOD"))) %>%
  group_by(scenario, FSregion, HHLD, year) %>%
  summarize(value = (value[variable == "CONSH"]/value[variable == "POPTH"])) %>%
  mutate(unit = "2007 USD/cap",
         variable = "CONSH", 
         FSsector = "FOOD") %>%
  filter(!is.infinite(value))

# AC5: SHICC
# Private consumption of domestic products volume at hh level
AC[["SHIC"]] <- AC[["HINC"]] %>%
  group_by(scenario, FSregion, FSsector, year) %>%
  mutate(value = value/sum(value, na.rm=T)*100) %>%
  mutate(unit = "%",
         variable = "SHIC")


### UTILIZATION
U <- list()

# U1
# Net Share of energy supply (calories) derived from vegetables and fruits
U[["U1h"]] <- MAGNETHH1_2 %>%
  filter((variable %in% c("NQSECTH") & unit %in% c("CAL")) &  (FSsector %in% c("TOT","VFN"))) %>%
  group_by(scenario, FSregion, year, HHLD) %>%
  summarize(value = (value[FSsector == "VFN"]/value[FSsector == "TOT"]*100)) %>%
  mutate(FSsector = "VFN",
         unit = "%",
         variable = "CALO")


#####################################
### MERGE ALL AND WRITE DATA FILE ###
#####################################

MAGNETHH_tot <- bind_rows(MAGNETHH1_2, AC, AV, U) %>%
  mutate(model = "MAGNET",
         year = as.numeric(year),
         scenario = revalue(scenario, c("ECO_qpc_t_st" = "ECO", "FFANF_qpc_t_st"  = "FFANF", "ONEPW_qpc_t_st" = "ONEPW",  "TLTL_qpc_t_st" = "TLTL")),
         modelrun = "qpc_t_st")

FSMIPPath <- "Cache"
write.csv(MAGNETHH_tot, file.path(FSMIPPath, paste("MAGNETHH_t_st_", Sys.Date(), ".csv", sep="")), row.names = F)
xtabs(~FSsector+variable, data = MAGNETHH_tot)


