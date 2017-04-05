#########################################################################
###### Functions to make plots  for FS model comparison #################
#########################################################################

library(plyr)
library(scales)
# 
# df <- filter(IM_GL_db, sector == "GRSLND" & variable == "LAND") %>%
#   select(-value) %>%
#   rename(value = index)
# yas <- "index"              
#             


# Line plot to compare models
lineplot_f <- function(df, yas){
  
  title = unique(with(df, paste(variable, sector, sep="_")))
  point <- filter(df, year == 2050)
  
  p = ggplot() +
    geom_line(data = df, aes(x = year, y = value, linetype = model, colour = scenario), size = 0.5) +
    geom_point(data = point, aes(x = year, y = value, shape = model, colour = scenario)) +
    scale_colour_manual(values = c("green","cyan","red","purple"), name="Scenario")+ 
    scale_linetype_manual(values=c("solid","longdash", "dotted"), name = "Model") +
    scale_shape_manual(values=c(16,17,18), name = "Model") +
    ylab(yas) + xlab("") +
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

# Line plot to compare models including historic information
lineplot2_f <- function(df, hist, yas){
  
  ymin <- min(hist$year)
  title = unique(with(df, paste(variable, sector, sep="_")))
  point <- filter(df, year == 2050)
  
  p = ggplot() +
    geom_line(data = df, aes(x = year, y = value, linetype = model, colour = scenario), size = 0.5) +
    geom_point(data = point, aes(x = year, y = value, shape = model, colour = scenario)) +
    scale_colour_manual(values = c("green","cyan","red","purple"), name="Scenario", guide = F)+ # guide = F surpresses linetype and shape in fill legend
    scale_linetype_manual(values=c("solid","longdash", "dotted"), name = "Model") +
    scale_shape_manual(values=c(16,17,18), name = "Model") +
    ylab(yas) + xlab("") +
    facet_wrap(~FSregion, scale = "free")
  
  p = p +ggtitle(title) 
  
  p = p + guides(fill = guide_legend(keywidth = 1, keyheight = 1, override.aes = 
                                       list(alpha = 0.1, size = 0.5, colour = c("green","cyan","red","purple"))))
  
  p = p + theme_classic() +
    theme(panel.border = element_blank(),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
    scale_x_continuous(limits = c(ymin,2050.1), breaks = seq(ymin,2050,by=10),expand=c(0,0)) + # note that: Limits sets the limits for the axes (added 0.1) but always some space is added. Latter is controlled by expand
    scale_y_continuous(labels = comma) +
    theme(legend.background=element_blank()) +
    theme(legend.key=element_rect(size=0.5, color="white"), # Increase space between legend boxes - doubt if it works
          strip.background = element_rect(colour="white", fill="white")) # Remove box and background of facet
  
  p  = p + geom_line(data = hist, aes(x = year, y = value, group = scenario), size = 0.5, colour = "black")
  
  p
}


# Plot to compare base values across models
baseplot_f <- function(df, by){
  df <- filter(df, year == by)
  p = ggplot(data = df, aes(model, value, fill = model)) +
    geom_bar(stat = "identity") +
    ggtitle(paste(df$variable, df$sector, df$scenario, by, sep ="_")) +
    ylab(unique(df$unit)) + 
    facet_wrap(~FSregion, scale = "free") +
    guides(fill=FALSE)
  p
}

# http://www.r-bloggers.com/shading-between-two-lines-ggplot/
# Plot to show bandwith (area) of scenario results NO historical data
bwplot_f <- function(df, yas){
  
  myear <-function(x) {
    x <-x[order(x$year), ]
    y <-x[-c(1, 2, nrow(x) -1, nrow(x)), ]
    x <-rbind(x, y)
    x <-x[order(x$year), ]
    x$group <-rep(letters[1:(nrow(x)/4)], each = 4)
    return(x)
  }
  
  mgroup <-function(x) {
    x <-x[order(x$value), ]
    left <-x[x$year ==min(x$year), ]
    right <-x[x$year ==max(x$year), ]
    if (all(left$model ==right$model)) {
      left <-left[order(left$value, decreasing = T), ]
      right <-right[order(right$value, decreasing = F), ]
      return(rbind(left, right))
    } else {
      return(x[order(x$year), ])
    }
  }
  
  df2 <- ddply(df,.(scenario, FSregion), myear)
  df3 <- ddply(df2, .(scenario, FSregion, group), mgroup)
  point <- filter(df, year == 2050)
 
  # Title
  title = unique(with(df, paste(variable, sector, sep="_")))
  
  # Plot the figure
  p = ggplot() +
    geom_line(data = df, aes(x = year, y = value, linetype = model, colour = scenario), size = 0.5) +
    geom_polygon(data = filter(df3, scenario %in% c("FFANF")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("ECO")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("ONEPW")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("TLTL")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_point(data = point, aes(x = year, y = value, shape = model, colour = scenario)) +
    scale_colour_manual(values = c("green","cyan","red","purple"), name="Scenario", guide = F)+ # guide = F surpresses linetype and shape in fill legend
    scale_fill_manual(values=c("green","cyan","red","purple"), name = "Scenario") +
    scale_linetype_manual(values=c("solid","longdash", "dotted"), name = "Model") +
    scale_shape_manual(values=c(16,17,18), name = "Model") +
    ylab(yas) + xlab("") +
    facet_wrap(~FSregion, scale = "free")
  
  p = p +ggtitle(title) 
  # p = p + guides(fill = guide_legend(override.aes = list(alpha = 0.1)))
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
  

# Plot to show bandwith (area) of scenario results WITH historical data
bwplot2_f <- function(df, hist, yas){
  
  myear <-function(x) {
    x <-x[order(x$year), ]
    y <-x[-c(1, 2, nrow(x) -1, nrow(x)), ]
    x <-rbind(x, y)
    x <-x[order(x$year), ]
    x$group <-rep(letters[1:(nrow(x)/4)], each = 4)
    return(x)
  }
  
  mgroup <-function(x) {
    x <-x[order(x$value), ]
    left <-x[x$year ==min(x$year), ]
    right <-x[x$year ==max(x$year), ]
    if (all(left$model ==right$model)) {
      left <-left[order(left$value, decreasing = T), ]
      right <-right[order(right$value, decreasing = F), ]
      return(rbind(left, right))
    } else {
      return(x[order(x$year), ])
    }
  }
  
  df2 <- ddply(df,.(scenario, FSregion), myear)
  df3 <- ddply(df2, .(scenario, FSregion, group), mgroup)
  title = unique(with(df, paste(variable, sector, sep="_")))
  point <- filter(df, year == 2050)
  ymin <- min(hist$year)
  
  # Plot the figure
  p = ggplot() +
    geom_line(data = df, aes(x = year, y = value, linetype = model, colour = scenario), size = 0.5) +
    geom_polygon(data = filter(df3, scenario %in% c("FFANF")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("ECO")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("ONEPW")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("TLTL")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_point(data = point, aes(x = year, y = value, shape = model, colour = scenario)) +
    scale_colour_manual(values = c("green","cyan","red","purple"), name="Scenario", guide = F)+ # guide = F surpresses linetype and shape in fill legend
    scale_fill_manual(values=c("green","cyan","red","purple"), name = "Scenario") +
    scale_linetype_manual(values=c("solid","longdash", "dotted"), name = "Model") +
    scale_shape_manual(values=c(16,17,18), name = "Model") +
    ylab(yas) + xlab("") +
    facet_wrap(~FSregion, scale = "free")
  
  p = p +ggtitle(title)
  
  p  = p + geom_line(data = hist, aes(x = year, y = value, group = scenario), size = 0.5, colour = "black")
  
  # p = p + guides(fill = guide_legend(override.aes = list(alpha = 0.1)))
  p = p + guides(fill = guide_legend(keywidth = 1, keyheight = 1, override.aes = 
                                       list(alpha = 0.1, size = 0.5, colour = c("green","cyan","red","purple"))))
  
  p = p + theme_classic() +
    theme(panel.border = element_blank(),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
    scale_x_continuous(limits = c(ymin,2050.1), breaks = seq(ymin,2050,by=10),expand=c(0,0)) + # note that: Limits sets the limits for the axes (added 0.1) but always some space is added. Latter is controlled by expand
    scale_y_continuous(labels = comma) +
    theme(legend.background=element_blank()) +
    theme(legend.key=element_rect(size=0.5, color="white"), # Increase space between legend boxes - doubt if it works
          strip.background = element_rect(colour="white", fill="white")) # Remove box and background of facet
  
  
  p
}

  
  