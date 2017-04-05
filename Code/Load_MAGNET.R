#######################################################
##### LOAD AND CREATE VARIABLES             ###########
#######################################################

source(".\\Code\\Load_Magnet_f.r")

# create lookup table for update files
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
