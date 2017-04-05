###############################
##### FOOD AND FEED ###########
###############################

### NB: MOST HORRIBLE CODING EVER!
### THIS TYPE OF CODING IS BETTER DONE IN GEMPACK AS I AM USING A FLAT TABLE STRUCTURE TO PROCESS MAGNET RESULTS
### ALTERNATIVE IS USING THE TENSOR PACKAGE THAT USES MULIDIMENSIONAL ARRAYS TO DO THE CALCULATION.

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
     
# Calculate domestic use per product = CONS FOR agMIP.
#PROD+IMPO-EXPO
CONS <- left_join(PROD, IMPO) %>%
          left_join(., EXPO) %>%
          mutate(EXPO = ifelse(is.na(EXPO), 0, EXPO)) %>%
          mutate(IMPO = ifelse(is.na(IMPO), 0, IMPO)) %>%
          mutate(CONS = PROD+IMPO-EXPO) %>%
          select(-PROD, -IMPO, -EXPO)


#### CONS: total domestic consumption
# Private consumption of domestic products volume
pridomconsvol <- constant2.f("pridomconsvol", "BaseData_b.gdx", "VDPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"), "qpd", c("TRAD_COMM", "REG"))
# Private consumption of imported products volume
priimpconsvol <- constant2.f("priimpconsvol", "BaseData_b.gdx", "VIPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"), "qpm", c("TRAD_COMM", "REG"))
# Total domestic consumption volume = A
A <- rbind(pridomconsvol, priimpconsvol) %>%
  group_by(REG, TRAD_COMM, scenario, year) %>%
  summarize(value = sum(value)) %>%
  rename(A = value) 
rm(pridomconsvol, priimpconsvol)


# Input use volume from domestic source
VDFM <- constant2.f("VDFM", "BaseData_b.gdx", "VDFM", c("TRAD_COMM", "PROD_SECT", "REG"), c("TRAD_COMM", "PROD_SECT", "REG"), "qfd", c("TRAD_COMM", "PROD_SECT", "REG")) %>%
        rename(VDFM = value) %>%
        select(-variable) 
        
VIFM <- constant2.f("VIFM", "BaseData_b.gdx", "VIFM", c("TRAD_COMM", "PROD_SECT", "REG"), c("TRAD_COMM", "PROD_SECT", "REG"), "qfm", c("TRAD_COMM", "PROD_SECT", "REG")) %>%
        rename(VIFM = value) %>%
        select(-variable) 

VFM <- left_join(VDFM, VIFM) %>%
        mutate(VDFM = ifelse(is.na(VDFM), 0, VDFM)) %>%
        mutate(VIFM = ifelse(is.na(VIFM), 0, VIFM)) %>%
        mutate(VFM = VDFM + VIFM) %>%
        select(-VDFM, -VIFM) 
 

# CALCULATE FOOD CONS using formula of Andrzej.

# Domestic consumption
A <- A %>%
      rename(i = TRAD_COMM)

# Input use volume
C <- VFM %>%
    rename(i = TRAD_COMM, j = PROD_SECT)

# Domestic absorbtion
X <- CONS %>%
  rename(i= TRAD_COMM)

# share of i consumed by hh in tot consumption of i. 
F1 <- left_join(A, X) %>%
  mutate(F1 = A/CONS) %>%
  select(-A, -CONS) 

# Share of intermediate input i used in j in tot consumption of j
X2 <- X %>%
      rename(j=i)

F2 <- left_join(C, X2) %>%
  mutate(F2 = VFM/CONS) %>%
  select(-VFM, -CONS) 
rm(X2)

# F2cvol
Xcvol <- X %>%
  rename(cvol = i, CONScvol = CONS) %>%
  filter(cvol %in% "cvol")

Xoilcake <- X %>%
  rename(oilcake= i, CONSoilcake = CONS) %>%
  filter(oilcake %in% "oilcake")

XF2cvol <- left_join(Xcvol, Xoilcake) %>%
  mutate(CONS = CONScvol+CONSoilcake) %>%
  select(-CONScvol, -CONSoilcake, -oilcake)
  
Ccvol <- C %>%
          rename(cvol=j) %>%
          filter(cvol == "cvol")

F2cvol = left_join(Ccvol, XF2cvol) %>%
          mutate(F2 = VFM/CONS) %>%
          select(-VFM, -CONS)
rm(Xoilcake, XF2cvol, Xcvol, Ccvol)

# F2biog
Xbiog <- X %>%
  rename(biog =i, CONSbiog = CONS) %>%
  filter(biog %in% "biog")

Xddgs <- X %>%
  rename(ddgs =i, CONSddgs = CONS) %>%
  filter(ddgs %in% "ddgs")

XF2biog <- left_join(Xbiog, Xddgs) %>%
  mutate(CONS = CONSbiog+CONSddgs) %>%
  select(-CONSbiog, -CONSddgs, -ddgs)

Cbiog <- C %>%
  rename(biog=j) %>%
  filter(biog == "biog")

F2biog = left_join(Cbiog, XF2biog) %>%
  mutate(F2 = VFM/CONS) %>%
  select(-VFM, -CONS)
rm(Xbiog, Xddgs, XF2biog, Cbiog)

  
# Mappings
pf <- c("cmt", "omt", "vol", "mil", "pcr", "sugar", "ofd", "pfd")
ser <- c("svcs")
pa <- c("pdr", "wht",  "gro",  "hort","osd", "c_b",  "oagr", "crops", "fsh", "ctl", "oap", "rmk")
ac <- c("pdr", "wht",  "gro",  "hort","osd", "c_b",  "oagr", "crops")
lvs <- c("ctl",  "oap", "rmk", "fsh") # Add WOL IF AVAILABLE

# FOOD
# P1
FOOD1 <- A %>%
      filter(i %in% pa) %>%
      rename(value = A) %>%
      mutate(component = "FOOD1")
    
# P2
F1_2 <- rename(F1, j = i) %>%
        filter (j %in% pf)

C2 <- C %>%
      filter (j %in% pf)

FOOD2 <- left_join(C2, F1_2) %>%
      mutate(value = VFM*F1) %>%
      group_by(scenario, year, REG, i) %>%
      summarize(value = sum(value, na.rm=T)) %>%
      filter(i %in% pa) %>%
      mutate(component = "FOOD2")
rm(F1_2, C2)

# P3
C3 <- C %>% 
      rename(d = i, k = j) 

F2_3 <- F2 %>% 
        rename(d = j) %>%
        filter(d %in% pf, i %in% pa) 

F1_3 <- F1 %>%
        rename(k = i) %>%
        filter(k %in% pf)

FOOD3 <- left_join(C3, F2_3) %>%
      left_join(., F1_3) %>%
      mutate(value = sum(VFM*F2*F1)) %>%
      group_by(scenario, year, REG, i) %>%
      summarize(value = sum(VFM*F2*F1, na.rm=T)) %>%
      filter(i %in% pa) %>%
      mutate(component = "FOOD3")
rm(C3, F2_3, F1_3)

# P4
FOOD4 <- C %>%
      rename(serv = j) %>%
      filter(serv %in% ser) %>%
      filter(i %in% pa) %>%
      rename(value = VFM) %>%
      select(-serv) %>%
      mutate(component = "FOOD4")

# P5
C5 <- C %>%
      rename(d = i, serv = j) %>%
      filter(serv %in% ser & d %in% pf) 

F2_5 <- F2 %>%
        rename(d = j) %>%
        filter(d %in% pf, i %in% pa)
        
FOOD5 <- left_join(C5, F2_5) %>%
      group_by(scenario, year, REG, i, serv) %>%
      summarize(value = sum(VFM*F2, na.rm=T)) %>%
      filter(i %in% pa) %>% 
      select(-serv) %>%
      mutate(component = "FOOD5")
rm(C5, F2_5)

# Aggregate P1-5
FOOD <- bind_rows(FOOD1, FOOD2, FOOD3, FOOD4, FOOD5) %>%
        group_by(scenario, year, REG, i) %>%
        summarize(value = sum(value, na.rm=T)) %>%
        rename(TRAD_COMM = i) %>%
        mutate(variable = "FOOD",
               unit = "M USD 2007")

# FEED
# P1
FEED1 <- C %>%
      filter(i %in% ac & j %in% lvs) %>%
      rename(value = VFM) %>%
      mutate(component = "FEED1")
    
# P2
C2 <- C %>%
      rename(k = i) %>%
      filter(k %in% c(pf, "feed") & j %in% lvs)

F2_2 <- F2 %>%
        rename(k = j) %>%
        filter(k %in% c(pf, "feed"), i %in% ac)

FEED2 <- left_join(C2, F2_2) %>%
      group_by(scenario, year, REG, i, j) %>%
      summarize(value = sum(VFM*F2)) %>%
      filter(i %in% ac & j %in% lvs) %>%
      mutate(component = "FEED2")
rm(F2_2, C2)

# P3
C3 <- C %>%
  rename(k = i) %>%
  filter(k %in% c("cvol", "oilcake") & j %in% lvs) %>%
  group_by(scenario, year, REG, j) %>%
  summarize(VFM = sum(VFM, na.rm=T)) %>%
  mutate(cvol = "cvol") 
  
FEED3 <- left_join(C3, F2cvol) %>% 
      mutate(value = F2*VFM,
             component = "FEED3") %>%
      filter(i %in% ac) %>%
      select(-cvol, -F2, -VFM)
rm(C3)

# P4
C4 <- C %>%
      rename(feed = i) %>%
      filter(feed %in% "feed", j %in% lvs)

F2_4 <- F2 %>%
  rename(k = i, F2a = F2, feed = j) %>%
  filter(k %in% c("cvol", "oilcake"), feed %in% "feed") %>%
  group_by(scenario, year, REG, feed) %>%
  summarize(F2a = sum(F2a, na.rm=T)) %>%
  mutate(cvol = "cvol")

FEED4 <- left_join(C4, F2_4) %>%
      left_join(., F2cvol) %>%
      mutate(value = VFM*F2*F2a,
             component = "FEED4") %>%
      filter(i %in% ac) %>%
      select(-VFM,-F2, -F2a, -cvol, - feed)
rm(C4, F2_4)

# P5
C5 <- C %>%
  rename(ddgs = i) %>%
  filter(ddgs %in% "ddgs", j %in% lvs)

Xbiog <- X %>%
  rename(biog =i, CONSbiog = CONS) %>%
  filter(biog %in% "biog")

Xddgs <- X %>%
  rename(ddgs =i, CONSddgs = CONS) %>%
  filter(ddgs %in% "ddgs")

Pa <- left_join(Xbiog, Xddgs) %>%
      mutate(CONSsh = CONSddgs/(CONSddgs+CONSbiog)) %>%
      select(-ddgs, -CONSddgs, -CONSbiog)

FEED5 <- left_join(C5, F2biog) %>%
      left_join(., Pa) %>%
      mutate(value = VFM*F2*CONSsh,
             component = "FEED5") %>%
      filter(i %in% ac) %>%
      select(-VFM, -F2, -CONSsh, -biog, -ddgs)
rm(C5, Xbiog, Xddgs, Pa)  
  

# Aggregate P1-P5
FEEDsec <- bind_rows(FEED1, FEED2, FEED3, FEED4, FEED5) %>%
  group_by(scenario, year, REG, i, j) %>%
  summarize(value = sum(value)) %>%
  rename(TRAD_COMM = i) %>%
  mutate(variable = "FEEDsec",
         unit = "M USD")

FEED <- FEEDsec %>%
  group_by(scenario, year, REG, TRAD_COMM) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = "FEED",
         unit = "M USD 2007")

# OTHU
# CONS = FOOD + FEED + OTHU
# Aggregate FEED
FEEDou <- FEED %>%
          rename(FEED = value) %>%
          select(-variable, -unit)

# CONS over pa
CONSou <- filter(CONS, TRAD_COMM %in% pa) 

# FOOD
FOODou <- rename(FOOD, FOOD = value) %>%
  select(-variable, -unit)

# OTHU
OTHU <- left_join(CONSou, FEEDou) %>%
        left_join(., FOODou) %>%
        mutate(FEED = ifelse(is.na(FEED), 0, FEED),
               value = CONS-FOOD-FEED) %>%
        select(-CONS, -FOOD, -FEED) %>%
        mutate(variable = "OTHU",
         unit = "M USD 2007")


# Clean up
rm(FEEDou, CONSou, FOODou)
rm(A, C, CONS, EXPO, F1, F2, F2biog, F2cvol, IMPO, PROD, VDFM, VFM, VIFM, X)
rm(lvs, pa, pf, ser, ac)
rm(FOOD1, FOOD2, FOOD3, FOOD4, FOOD5)
rm(FEED1, FEED2, FEED3, FEED4, FEED5)
   
