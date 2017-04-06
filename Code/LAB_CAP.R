
XPRP (Real producer price) for FRT (Fertiliser) and OIL (Fossil fuel) is really easy. You have already formula for XPRP use for agricultural products. So, it should be extended for FRT = CHEM in AgriCLIM50 (we do not have FRT sector in AgriCLIM50 and we use CHEM as proxy) and OIL = C_OIL in AgriCLIM50.

XPRP (Real input price) is more difficult story. Below are formulas from GA use for labour and capital prices calculation. You used them for other variables you should understand them. You know this, but to be 101% sure, to calculate prices for aggregates of regions, you aggregate volumes and values first and then, at the end, you calculate wages/prices. 

Wage labor market (real):
  PostCalc: Wage labor market (real) = Total employment market value DIVIDE BY Total employment volume DIVIDE BY GDP, Value MULTIPLY WITH GDP, volume  (NOT certified)
SumDim: Total employment maket value = Sector employment market value(Sum over DSEC)
PreCalc: Sector employment maket value = Sector skilled employment market value PLUS Sector un-skilled employment market value
Read: Sector skilled employment market value = VFM(SkLab,DSEC,SREG) AND VFM(SkLab,DSEC,SREG)  (NOT certified)
Read: Sector un-skilled employment market value = VFM(UnSkLab,DSEC,SREG) AND VFM(UnSkLab,DSEC,SREG)  (NOT certified)
SumDim: Total employment volume = Sector employment, volume(Sum over DSEC)
PreCalc: Sector employment, volume = Sector skilled employment, volume PLUS Sector un-skilled employment, volume
Read: Sector skilled employment, volume = VFM(SkLab,DSEC,SREG) AND qf(SkLab,DSEC,SREG)  (NOT certified)
Read: Sector un-skilled employment, volume = VFM(UnSkLab,DSEC,SREG) AND qf(UnSkLab,DSEC,SREG)  (NOT certified)
Read: GDP, Value = GDPSRC(SREG,SUM) AND GDPSRC(SREG,SUM)  (NOT certified)
Read: GDP, volume = GDPSRC(SREG,SUM) AND qgdp(SREG)  (NOT certified) 





# Functions VFM, where sets in base year and growth variable are the different
constant.3f <- function(varname, var, set.names, group.var, var.growth, set.names.growth){
  baseValue <- var.extract2.f("BaseData_b.gdx", dataResultPath, var, set.names) %>%
    group_by_(.dots = group.var) %>%
    summarize(value = sum(value, na.rm=T))
  
  ENDW_COMM_unique <- unique(baseValue$ENDW_COMM)
  
  scenValueGrowth <- adply(lookup_sol[,c("gdxResultFiles", "year", "scenario")], 1, var.extract.f, dataResultPath, var.growth, set.names.growth) %>%
    dplyr::select(-gdxResultFiles) %>%
    arrange_(.dots = c("scenario", set.names.growth, "year")) %>%
    group_by_(.dots = c("scenario", set.names.growth)) %>%
    dplyr::mutate(cindex=cumprod((value/100)+1))  %>%
    filter(ENDW_COMM %in% ENDW_COMM_unique) %>%
    dplyr::select(-value) %>%
    mutate(variable = varname)
  
  scenValueGrowth <- left_join(scenValueGrowth, baseValue) %>%
    mutate(value=cindex*value) %>%
    dplyr::select(-cindex)
  
  scen <- unique(scenValueGrowth$scenario)
  
  base.scenario.f <- function(scen, base, varname) {
    base$scenario <- scen
    base$year <- "2007"
    base$variable <- varname
    return(base)
  } 
  
  baseValue2 <- ldply(scen, function(x,y,z) base.scenario.f(x, baseValue, varname))
  
  conValue <-   baseValue2 %>%
    bind_rows(., scenValueGrowth) %>%
    arrange_(.dots = c(group.var, "scenario", "year")) %>%
    ungroup()
  
  return(conValue)
}

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
XPRP_CAP <- VFM %>%
  filter(ENDW_COMM %in% c("Capital"), sector == "TOT") %>%
  mutate(variable = ifelse(unit == "M USD", "VFMval", variable)) %>%
  select(-unit) %>%
  spread(variable, value) %>%
  left_join(., GDPdef) %>%
  mutate(value = VFMval/VFM/GDPval*GDPT,
         variable = "XPRP",
         unit = "Paasche index",
         sector = "CAP") %>%
  select(-VFM, -VFMval, -GDPT, -GDPval, ENDW_COMM)


### XPRP: LABOUR
XPRP_LAB <- VFM %>%
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

                      
Capial price market (real) = Endowment price market (real) for SSEC = Capital
PostCalc: Endowment price market (real) = Total endowmwnt maket value DIVIDE BY Total endowmwnt volume DIVIDE BY GDP, Value MULTIPLY WITH GDP, volume  (NOT certified)
SumDim: Total endowmwnt maket value = Sectoral endowment values at market prices(Sum over DSEC)
Read: Sectoral endowment values at market prices = VFM(SSEC,DSEC,SREG) AND VFM(SSEC,DSEC,SREG)  (NOT certified)
SumDim: Total endowmwnt volume = Sectoral endowment volumes at market prices(Sum over DSEC)
Read: Sectoral endowment volumes at market prices = VFM(SSEC,DSEC,SREG) AND qf(SSEC,DSEC,SREG)  (NOT certified)
Read: GDP, Value = GDPSRC(SREG,SUM) AND GDPSRC(SREG,SUM)  (NOT certified)
Read: GDP, volume = GDPSRC(SREG,SUM) AND qgdp(SREG)  (NOT certified)

Here are results from GA for USA, Canada and North America for SSP2a

Endowment price market (real) (Capital) (SSP2a_FLC3)						
2007	2010	2020	2030	2050
NoAm	1.000	0.995	1.012	1.026	1.046
USA		1.000	0.995	1.016	1.032	1.053
Canada	1.000	0.995	0.994	0.989	1.007

Wage labor market (real) (SSP2a_FLC3)						
2007	2010	2020	2030	2050
NoAm	1.000	0.965	1.151	1.309	1.530
USA		1.000	0.964	1.154	1.315	1.529
Canada	1.000	0.975	1.108	1.212	1.499
Best,
AT
