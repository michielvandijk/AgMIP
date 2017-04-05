# COPY AND RENAME BASESHOCK FILES

# NB: in many cases only one scenario is used to update MAGNET from the GTAP base year to the latest historical year (e.g. from 2007 to 2010)
# In this cases there is a warning that some files could not be converted from har to gdx (because they do not exist)
# This information is needed for all scenarios to update constant volumes and therefore the results for this period need to be copied to other scenarios.
# Below some raw code to do this (unfinished)# 

dataUpdatesPath <- "./4_MAGNET/Updates"
dataSolPath <- "./4_MAGNET/Solutions"
#dataShocksPath <- "./4_MAGNET/Shocks"
dataResultPath <- "./4_MAGNET/Results"  

# har file to be copied and rename
baseShock <- "FFANF_qpc_ti4_st"
basePeriod <- "2007-2010"

# Target scenarios
scenarios<-c("ECO_qpc_ti4_st")

# Create baseshock files: update, update_view, solution
fileType <- c("update", "update_view", "Solution")
lookup_base <- expand.grid(baseShock, scenarios, basePeriod, fileType, stringsAsFactors = FALSE)

# split periods in start and finish
names(lookup_base) <- c("baseShock", "scenario", "period", "fileType")

# Create list of base source files and result files
lookup_base <- lookup_base %>% 
                mutate(ext = ifelse(fileType %in% c("update", "update_view"), ".har", ".sol"),
                       baseSourceFile = paste(paste(baseShock, period, fileType, sep="_"), ext, sep=""),
                       baseResultFile = paste(paste(scenario, period, fileType, sep="_"), ext, sep=""))

dataSolPath <- "D:/R/test"

# Copy sol files 
solCopy <- filter(lookup_base, fileType == "Solution")
file.copy(file.path(dataSolPath, solCopy$baseSourceFile), file.path(dataSolPath, solCopy$baseResultFile), overwrite =T)

# Copy update and update view files 
updCopy <- filter(lookup_base, fileType %in% c("update", "update_view"))
file.copy(file.path(dataUpdatesPath, updCopy$baseSourceFile), file.path(dataUpdatesPath, updCopy$baseResultFile), overwrite =T)
