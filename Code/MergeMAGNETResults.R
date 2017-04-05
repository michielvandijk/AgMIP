# PROJECT: FOODSECURE WP7
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# Merge results from different MAGNET runs
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 

# PACKAGES"
BasePackages <- c("foreign", "stringr", "car", "zoo", "tidyr", "RColorBrewer", "plyr", "dplyr", "ggplot2", "readxl", "readr", "scales")
lapply(BasePackages, library, character.only = TRUE)
#SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
#lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <-  c("WDI", "countrycode")
lapply(AdditionalPackages, library, character.only = TRUE)

# SET PATHS
wdPath<-"D:\\R\\FSWP7"
#wdPath<-"D:\\2 Content\\Dropbox\\FOODSECURE Scenarios"
setwd(wdPath)
dataPath <- "D:\\Shutes\\FOODSECURE\\R\\ProcessedModelResults"
graphPath <- "D:\\Shutes\\FOODSECURE\\R\\Graphs"
  
# R SETTINGS
options(scipen=99) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=2)

# FUNCTIONS
# Plus function that ensures NA + NA is NA not 0 as in sum
plus <- function(x) {
  if(all(is.na(x))){
    c(x[0],NA)} else {
      sum(x,na.rm = TRUE)}
}

# ma function to calculate moving average
ma <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)}

# Read MAGNET data per model and merge
MAGNETruns <- list()
MAGNETruns[["M_qpc_t_st"]] <- read_csv("Cache/MAGNET_qpc_t_st.csv")
MAGNETruns[["M_qpc_ti3_st"]] <- read_csv("Cache/MAGNET_qpc_ti3_st.csv")
MAGNETruns[["M_qpc_ti4_st"]] <- read_csv("Cache/MAGNET_qpc_ti4_st.csv")
MAGNETruns <- bind_rows(MAGNETruns) %>%
  na.omit
xtabs(~Modelrun + variable, data = MAGNETruns)


# Line plot to compare models
lineplot_f <- function(df){
  
  title = unique(with(df, paste(variable, FSsector, sep="_")))
  point <- filter(df, year == 2050)
  
  p = ggplot() +
    geom_line(data = df, aes(x = year, y = value, linetype = Modelrun, colour = scenario), size = 0.5) +
    geom_point(data = point, aes(x = year, y = value, shape = Modelrun, colour = scenario)) +
    scale_colour_manual(values = c("green","cyan","red","purple"), name="Scenario")+ 
    scale_linetype_manual(values=c("solid","longdash", "dotted"), name = "Modelrun") +
    scale_shape_manual(values=c(16,17,18), name = "Modelrun") +
    ylab(unique(df$unit)) + xlab("") +
    facet_wrap(~FSregion, scale = "free")
  
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


# Main regions
mainRegions <- filter(MAGNETruns, FSregion %in% c("EASIA", "EU", "LAC", "MENA", "ROW", "SASIA", "SSA", "WLD"))

plot_i <- mainRegions %>%
  group_by(variable, FSsector, unit) %>%
  do(plots = lineplot_f(.)) 

pdf(file = file.path(graphPath, "mrPlots.pdf"), width = 7, height = 7)
plot_i$plots
dev.off()
rm(plot_i)

# Africa regions
afRegions <- filter(MAGNETruns, FSregion %in% c("EAF", "NAF", "SAF", "WAF"))

plot_i <- afRegions %>%
  group_by(variable, FSsector, unit) %>%
  do(plots = lineplot_f(.)) 

pdf(file = file.path(graphPath, "afPlots.pdf"), width = 7, height = 7)
plot_i$plots
dev.off()
rm(plot_i)

# Household regions
hhRegions <- filter(MAGNETruns, FSregion %in% c("CHN", "GHA", "IDN", "IND", "KEN", "UGA")) %>%
  filter(!is.infinite(value)) # in YILD PFB there are a few infinite values that give problems when making a figure.

plot_i <- hhRegions %>%
  group_by(variable, FSsector, unit) %>%
  do(plots = lineplot_f(.)) 

pdf(file = file.path(graphPath, "hhPlots.pdf"), width = 7, height = 7)
plot_i$plots
dev.off()
rm(plot_i)


