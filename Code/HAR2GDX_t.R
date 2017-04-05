# Code to prepare indicators for FSMIP based on MAGNET OUTPUT and import into R

# PACKAGES
BasePackages <- c("foreign", "stringr", "car", "zoo", "tidyr", "RColorBrewer", "plyr", "dplyr", "ggplot2", "haven")
lapply(BasePackages, library, character.only = TRUE)
AdditionalPackages <- c("gdxrrw")
lapply(AdditionalPackages, library, character.only = TRUE)

# load required GAMS libraries (folder user specific)
GAMSPath <- "C:\\24.4"
#GAMSPath <- "C:\\Program Files\\GAMS\\win64\\24.6"
igdx(GAMSPath)
# Make sure GDX2HAR.exe and gdxiomh.dll are located in one folder.

# Set working folder
wdPath <- "D:\\Shutes\\FOODSECURE"
setwd(wdPath)  

# Functions
# Function to convert all har files to gdx (in the same folder)
har2gdx.f<-function(har.file, gdx.file=""){
  system(paste("HAR2GDX.exe", har.file, gdx.file, sep=" "))
}

# R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

# Upload data for all scenarios and combine into one file
# Upload update and view files which contain most variables
# Perhaps also view data should be uploaded.

# Create new folder for relevant files
dataBasePath <- "./4_MAGNET/Basedata"
dataUpdatesPath <- "./4_MAGNET/Updates"
dataSolPath <- "./4_MAGNET/Solutions"
#dataShocksPath <- "./4_MAGNET/Shocks"
dataResultPath <- "./4_MAGNET/Results"
if (!file.exists(dataResultPath)) dir.create(dataResultPath) 

# Define scenarios, periods and project
scenarios<-c("FFANF_qpc_t_st", "ONEPW_qpc_t_st", "TLTL_qpc_t_st", "ECO_qpc_t_st")
periods<-c("2007-2010", "2010-2020", "2020-2030", "2030-2040", "2040-2050")

# Create lookup table for update files
sourcefile<-c("update")
lookup_upd <- expand.grid(scenarios, periods, sourcefile, stringsAsFactors = FALSE)

# split periods in start and finish
TMP <- data.frame(do.call(rbind, str_split(lookup_upd$Var2, "-", 2)))
lookup_upd <- cbind(lookup_upd, TMP) ; rm(TMP)
names(lookup_upd) <- c("scenario", "period", "sourcefile", "start", "year")

# Create list of all relevant har and gdx files by period and scenario 
lookup_upd$harSourceFiles <- paste(with(lookup_upd, paste(scenario, period, sourcefile, sep="_")), ".har", sep="")
lookup_upd$gdxResultFiles <- paste(with(lookup_upd, paste(scenario, period, sourcefile, sep="_")), ".gdx", sep="")

# Create lookup table for update_view files
sourcefile<-c("update_view")
lookup_upd_view <- expand.grid(scenarios, periods, sourcefile, stringsAsFactors = FALSE)

# split periods in start and finish
TMP <- data.frame(do.call(rbind, str_split(lookup_upd_view$Var2, "-", 2)))
lookup_upd_view <- cbind(lookup_upd_view, TMP); rm(TMP)
names(lookup_upd_view) <- c("scenario", "period", "sourcefile", "start", "year")

# Create list of all relevant har and gdx files by period and scenario 
lookup_upd_view$harSourceFiles <- paste(with(lookup_upd_view, paste(scenario, period, sourcefile, sep="_")), ".har", sep="")
lookup_upd_view$gdxResultFiles <- paste(with(lookup_upd_view, paste(scenario, period, sourcefile, sep="_")), ".gdx", sep="")

# Create lookup table for solution files files
sourcefile<-c("Solution")
lookup_sol <- expand.grid(scenarios, periods, sourcefile, stringsAsFactors = FALSE)

# split periods in start and finish
TMP <- data.frame(do.call(rbind, str_split(lookup_sol$Var2, "-", 2)))
lookup_sol <- cbind(lookup_sol, TMP); rm(TMP)
names(lookup_sol) <- c("scenario", "period", "sourcefile", "start", "year")

# Create list of all relevant har and gdx files by period and scenario 
lookup_sol$harSourceFiles <- paste(with(lookup_sol, paste(scenario, period, sourcefile, sep="_")), ".sol", sep="")
lookup_sol$gdxResultFiles <- paste(with(lookup_sol, paste(scenario, period, sourcefile, sep="_")), ".gdx", sep="")

# Create list of base files
BaseDataFiles <- data.frame(harSourceFiles = c("BaseData_b.har", "BaseData_b_view.har", "fsbasecalories_2007-2010_update_view.har"),
                            gdxResultFiles = c("BaseData_b.gdx", "BaseData_b_view.gdx", "fsbasecalories_2007-2010_update_view.gdx"))


# Create lookup table for slc files
sourcefile<-c("solution")
destinationfile <- c("solution_slc")
lookup_slc <- expand.grid(scenarios, periods, sourcefile, stringsAsFactors = FALSE)

# split periods in start and finish
TMP <- data.frame(do.call(rbind, str_split(lookup_slc$Var2, "-", 2)))
lookup_slc <- cbind(lookup_slc, TMP) ; rm(TMP)
names(lookup_slc) <- c("scenario", "period", "sourcefile", "start", "year")

# Create list of all relevant har and gdx files by period and scenario 
lookup_slc$harSourceFiles <- paste(with(lookup_slc, paste(scenario, period, sourcefile, sep="_")), ".slc", sep="")
lookup_slc$gdxResultFiles <- paste(with(lookup_slc, paste(scenario, period, destinationfile, sep="_")), ".gdx", sep="")

# convert base files to gdx
############### NB there are warnings in the conversion! CHECK
# NB: in many cases only one scenartio is used to update MAGNET from the GTAP base year to the latest historical year (e.g. from 2007 to 2010)
# In this cases there is a warning that some files could not be converted from har to gdx (because they do not exist)
# This information is needed for all scenarios to update constant volumes and therefore the results for this period need to be copied to other scenarios.
# The script CopyBaseShock.r does this

# Add base period GDX files
apply(BaseDataFiles, 1, function(x) har2gdx.f(file.path(dataBasePath, x[1]), file.path(dataResultPath, x[2])))

# Convert update files to GDX
apply(lookup_upd, 1, function(x) har2gdx.f(file.path(dataUpdatesPath, x["harSourceFiles"]), file.path(dataResultPath, x["gdxResultFiles"])))

# Convert update files to GDX
apply(lookup_upd_view, 1, function(x) har2gdx.f(file.path(dataUpdatesPath, x["harSourceFiles"]), file.path(dataResultPath, x["gdxResultFiles"])))

# One warning: Check
# Convert sol files to GDX # warning about sets! CHECK
apply(lookup_sol, 1, function(x) har2gdx.f(file.path(dataSolPath, x["harSourceFiles"]), file.path(dataResultPath, x["gdxResultFiles"])))

# Convert slc files to GDX
#apply(lookup_slc, 1, function(x) har2gdx.f(file.path(dataSolPath, x["harSourceFiles"]), file.path(dataResultPath, x["gdxResultFiles"])))


