# Code to prepare indicators for FSMIP based on MAGNET OUTPUT and import into R
# Can be improved by:
#             - Extracting values for level variables of POP and GDP to compute GDP per capita
#             - Adding standard functions to plot drivers
#             - Adding code to convert update files to GDX and extract information
#             - Add functions to save in csv format
#             - Add functions to compute growth rates over periods and total period
#             - Include code as standard in MAGNET
#             - Compute data for total world
#             - Adding user defined region concordance to make user defined aggregations

# PACKAGES
BasePackages <-c("readr", "readxl", "stringr", "car", "zoo", "tidyr", "RColorBrewer", "plyr", "dplyr", "ggplot2", "haven")
lapply(BasePackages,library,character.only=TRUE)
AdditionalPackages <-c("gdxrrw")
lapply(AdditionalPackages,library, character.only=TRUE)

# load required GAMS libraries (folder user specific)
GAMSPath <- "C:\\24.4"
#GAMSPath <- "D:\\GAMS64\\GAMS24.2"
#GAMSPath <- "C:\\Program Files\\GAMS\\win64\\24.6"
igdx(GAMSPath)
# Make sure GDX2HAR.exe and gdxiomh.dll are located in one folder.

# Set working folder
wdPath <- "D:\\Diti\\MAGNET_PBL_SSP_PPP_NUTcor2noCCcor"
setwd(wdPath)  

# FUNCTIONS
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
scenarios<-c("SSP1a_FLC3_M", "SSP2a_FLC3", "SSP3a_FLC3_M", 
             "SSP1a_FLC3_M_clim6", "SSP2a_FLC3_clim6", "SSP3a_FLC3_M_clim6", 
             "SSP1a_FLC3_M_clim26_nEnP", "SSP2a_FLC3_clim26_nEnP", "SSP3a_FLC3_M_clim26_nEnP",
             "SSP1a_FLC3_M_clim26_nEnP_noCC", "SSP2a_FLC3_clim26_nEnP_noCC", "SSP3a_FLC3_M_clim26_nEnP_noCC")
periods<-c("2007-2010", "2010-2020", "2020-2030", "2030-2050")

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

######## CONVERT FILES

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


