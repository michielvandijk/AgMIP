# ADD option to set unit

# Function to create list of shock files
locationShock.f<-function(path,Scenarios, Periods){
  file1<-paste("Shocks", Periods, sep="")
  file2<-paste(Scenarios, "har", sep=".")
  filecomp<-paste(file1, file2, sep="_")
  location<-paste(path, filecomp, sep="/")
  return(location)
}

unfactorize <- function(df){
  for(i in which(sapply(df, class) == "factor")) df[[i]] = as.character(df[[i]])
  return(df)
}


growth.f<-function (d,var) {
  #d<-as.data.frame(d)
  d$Growth <- c(NA, exp(diff(log(d[[var]]))))
  return(d)
}

# Cumulative growth
cumgrowth.f<-function(d,var){
  perc<-(d[[var]]/100)+1
  cindex<-prod(perc)
  cindex<-(cindex-1)*100
  cindex<-data.frame(index=cindex)
  return(cindex)
}


# Annual growth
angrowth.f<-function(d,var, period){
  perc<-(d[[var]]/100)+1
  cindex<-prod(perc)
  agrowth<-((cindex^(1/period))-1)*100
  agrowth<-data.frame(growth=agrowth)
  return(agrowth)
}
# 
# prodcon <- constant.f("prodcon", "VALOUTPUT", c("TRAD_COMM","REG", "GDPSOURCE"), c("TRAD_COMM", "REG"), "qo", c("NSAV_COMM", "REG"))
# baseValue <- var.extract2.f("BaseData_b_view.gdx", dataResultPath, var, set.names) 
# 
# input <- "BaseData_b_view.gdx"
# path <- dataResultPath
# var <- "VALOUTPUT"
# set.names <- c("TRAD_COMM","REG", "GDPSOURCE")

# Functions to extract variables from GDX files
var.extract2.f<-function(input, path, var, set.names){
  file <- file.path(path, input)
  #TMP<-rgdx.param(file, var, compress=FALSE, names=set.names, squeeze = FALSE) # does not work because it does not include 0 values that are not stored in GDX
  TMP <- rgdx(file,list(name = var, compress = T, form = 'full'))
  TMP <-TMP$val
  ncol <- ncol(TMP)
  if (ncol < 2){
    TMP <- data.frame(V1 = row.names(TMP), V2 = TMP[,1])
    row.names(TMP) <- NULL
  } else {
    TMP <- as.data.frame(as.table(TMP))
  }
  names(TMP) <- c(set.names, "value")
  TMP <-unfactorize(TMP)
  return(TMP)
}

var.extract.f<-function(input, path, var, set.names){
  file <- file.path(path, input["gdxResultFiles"])
  #TMP<-rgdx.param(file, var, compress=FALSE, names=set.names, squeeze = FALSE)
  TMP <- rgdx(file,list(name = var, compress = T, form = 'full'))
  TMP <-TMP$val
  ncol <- ncol(TMP)
  if (ncol < 2){
    TMP <- data.frame(V1 = row.names(TMP), V2 = TMP[,1])
    row.names(TMP) <- NULL
  } else {
    TMP <- as.data.frame(as.table(TMP))
  }
  names(TMP) <- c(set.names, "value")
  TMP <-unfactorize(TMP)
  return(TMP)
}

# varname <- "prodcur"
# basefile <- "BaseData_b_view.gdx"
# scenariofile <- lookup_upd_view
# varbase = "valoutput"
# varsen = "valoutput"
# set.names= c("TRAD_COMM", "REG", "OUTVALUE")
# group.var= c("TRAD_COMM","REG")
# var <- "valoutput"
# rm(varname, basefile, scenariofile, varbase, varsen, set.names, group.var, var)

# function to create current series
current.f <- function(varname, basefile, varbase, scenariofile, varsen, set.names, group.var){
  baseValue <- var.extract2.f(basefile, dataResultPath, varbase, set.names) %>%
    group_by_(.dots = group.var) %>%
    summarize(value = sum(value, na.rm=T))
  
  group.var2 <- c(group.var, "scenario", "year")
  
  scenfile <- dplyr::select_(scenariofile, .dots = c("gdxResultFiles", "year", "scenario"))
  
  scenValue <- adply(scenfile, 1, var.extract.f, dataResultPath, varsen, set.names) %>%
    dplyr::select(-gdxResultFiles) %>%
    group_by_(.dots = group.var2) %>%
    summarize(value = sum(value, na.rm=T)) %>%
    mutate(variable = varname)
  
  scen <- unique(scenValue$scenario)
  
  base.scenario.f <- function(scen, base, varname) {
    base$scenario <- scen
    base$year <- "2007"
    base$variable <- varname
    return(base)
  } 
  
  baseValue2 <- ldply(scen, function(x,y,z) base.scenario.f(x, baseValue, varname))
  
  currValue <-   baseValue2 %>%
    bind_rows(., scenValue) %>%
    arrange_(.dots = group.var2) %>%
    ungroup()
  
  return(currValue)
}
# 
# varname <- "prodcon"
# basefile <- "BaseData_b_view.gdx"
# scenariofile <- lookup_upd_view
# varbase = "valoutput"
# varsen = "valoutput"
# set.names= c("TRAD_COMM","REG", "GDPSOURCE")
# group.var= c("TRAD_COMM", "REG")
# set.names.growth <- c("NSAV_COMM", "REG")
# var.growth <- "qo"
# rm(varname, basefile, scenariofile, varbase, varsen, set.names, group.var, var)

# Functions for prodvol IMPO where sets in base year and growth variable are not the same
constant.f <- function(varname, var, set.names, group.var, var.growth, set.names.growth){
  baseValue <- var.extract2.f("BaseData_b_view.gdx", dataResultPath, var, set.names) %>%
    group_by_(.dots = group.var) %>%
    summarize(value = sum(value, na.rm=T))
  
  TRAD_COMM <- unique(baseValue$TRAD_COMM)
  
  scenValueGrowth <- adply(lookup_sol[,c("gdxResultFiles", "year", "scenario")], 1, var.extract.f, dataResultPath, var.growth, set.names.growth) %>%
    dplyr::select(-gdxResultFiles) %>%
    arrange_(.dots = c("scenario", set.names.growth, "year")) %>%
    group_by_(.dots = c("scenario", set.names.growth)) %>%
    dplyr::mutate(cindex=cumprod((value/100)+1))  %>%
    filter(NSAV_COMM %in% TRAD_COMM) %>%
    dplyr::select(-value) %>%
    rename(TRAD_COMM = NSAV_COMM) %>%
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


# varname = "prodcon"
# var = "VALOUTPUT"
# set.names =  c("TRAD_COMM","REG", "GDPSOURCE")
# group.var = c("TRAD_COMM","REG")
# basefile <- "BaseData_b_view.gdx"
# var.growth <- "qo"
# set.names.growth <-  c("NSAV_COMM", "REG")

# Functions for GDP cons, expo and (perhaps) IMPO where sets in base year and growth variable are the same
constant2.f <- function(varname, basefile, var, set.names, group.var, var.growth, set.names.growth){
  
  baseValue <- var.extract2.f(basefile, dataResultPath, var, set.names) %>%
    group_by_(.dots = group.var) %>%
    summarize(value = sum(value, na.rm=T))
  
  scenValueGrowth <- adply(lookup_sol[,c("gdxResultFiles", "year", "scenario")], 1, var.extract.f, dataResultPath, var.growth, set.names.growth) %>%
    dplyr::select(-gdxResultFiles) %>%
    arrange_(.dots = c("scenario", set.names.growth, "year")) %>%
    group_by_(.dots = c("scenario", set.names.growth)) %>%
    dplyr::mutate(cindex=cumprod((value/100)+1))  %>%
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
    arrange_(.dots = c(set.names.growth, "scenario", "year")) %>%
    ungroup()
  
  return(conValue)
}


# Function for exogenous yield
aland.f <- function(varname, var.growth, set.names.growth){
  
  aland <- adply(lookup_sol[,c("gdxResultFiles", "year", "scenario")], 1, var.extract.f, dataResultPath, var.growth, set.names.growth) %>%
    dplyr::select(-gdxResultFiles) %>%
    arrange_(.dots = c("scenario", set.names.growth, "year")) %>%
    mutate(variable = "YEXO")
  return(aland)
}

# aland <- aland2.f("aland", "aland", c("PROD_SECT", "MREG"))
# varname <- "aland"
# var.growth <- "aland"
# set.names.growth <- c("PROD_SECT", "MREG")

# Function for cumulative aland index
aland2.f <- function(varname, var.growth, set.names.growth){
  
  scenValueGrowth <- adply(lookup_sol[,c("gdxResultFiles", "year", "scenario")], 1, var.extract.f, dataResultPath, var.growth, set.names.growth) %>%
    dplyr::select(-gdxResultFiles) %>%
    arrange_(.dots = c("scenario", set.names.growth, "year")) %>%
    group_by_(.dots = c("scenario", set.names.growth)) %>%
    dplyr::mutate(value=cumprod((value/100)+1),
                  variable = varname,
                  year = as.numeric(year))
  
  scen <- unique(scenValueGrowth$scenario)
  comm <- unique(scenValueGrowth$PROD_SECT)
  reg <- unique(scenValueGrowth$MREG)
  baseValue <- expand.grid(scenario = scen, PROD_SECT = comm, MREG = reg, stringsAsFactors = FALSE) %>%
    mutate(year= 2007, variable = varname, value = 1)
  
  index <- baseValue %>%
    bind_rows(., scenValueGrowth) %>%
    arrange_(.dots = c(set.names.growth, "scenario", "year")) %>%
    ungroup()
  
  return(index)
}
