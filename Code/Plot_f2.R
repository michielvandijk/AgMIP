# Function to create bandwidht plot excluding historical data

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
  
  df1 <- filter(df, model == "Average")
  df2 <- filter(df, model != "Average")
  df2 <- ddply(df2,.(scenario, region), myear)
  df3 <- ddply(df2, .(scenario, region, group), mgroup)
  #title = unique(with(df, FNS))
  
  point <- filter(df2, year == 2050) 
  scenarios <- unique(point$scenario)
  for (i in c(1:4))
  {
    k = i*2 + 2050
    
    point$year[point$scenario == scenarios[i]] <- k
  } 
  
  #ymin <- min(hist$year)
  
  # Plot the figure
  p = ggplot() +
    #geom_line(data = df2, aes(x = year, y = value, linetype = model, colour = scenario), size = 0.5) +
    geom_line(data = df1, aes(x = year, y = value, colour = scenario, linetype = model), size = 1.5) +
    geom_polygon(data = filter(df3, scenario %in% c("FFANF")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("ECO")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("TLTL")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("ONEPW")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_point(data = point, aes(x = year, y = value, shape = model, colour = scenario)) +
    scale_colour_manual(values = c("green","cyan","red","purple"), name = "", guide = F)+ # guide = F surpresses linetype and shape in fill legend
    scale_fill_manual(values=c("green","cyan","red","purple"), name = "") +
    scale_linetype_manual(values=c("solid","longdash", "dotted"), name = "") +
    scale_shape_manual(values=c(4, 15), name = "") +
    ylab(yas) + xlab("") +
    facet_wrap(~region, scale = "free_x")
  
  #p = p +ggtitle(title)
  
  p = p + geom_line(data = point, aes(x = year, y = value, colour = scenario))
  
  #p  = p + geom_line(data = hist, aes(x = year, y = value, group = scenario), size = 1.5, colour = "grey50")
  
  # p = p + guides(fill = guide_legend(override.aes = list(alpha = 0.1)))
  p = p + guides(fill = guide_legend(keywidth = 1, keyheight = 1, override.aes = 
                                       list(alpha = 0.1, size = 0.5, colour = c("green","cyan","red","purple"))))
  
  p = p + theme_classic() +
    theme(panel.border = element_blank(),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
    scale_x_continuous(limits = c(2010,2060), breaks = seq(2010,2050,by=10),expand=c(0,0)) + # note that: Limits sets the limits for the axes (added 0.1) but always some space is added. Latter is controlled by expand
    scale_y_continuous(labels = comma) +
    theme(legend.background=element_blank()) +
    theme(legend.key=element_rect(size=0.5, color="white"), # Increase space between legend boxes - doubt if it works
          strip.background = element_rect(colour="white", fill="white"), # Remove box and background of facet
          legend.position="bottom") +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = 1)
  p 
}



# Function to create bandwidht plot including historical data
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
  
  df1 <- filter(df, model == "Average")
  df2 <- filter(df, model != "Average")
  df2 <- ddply(df2,.(scenario, region), myear)
  df3 <- ddply(df2, .(scenario, region, group), mgroup)
  title = unique(with(df, FNS))
  
  point <- filter(df2, year == 2050) 
  scenarios <- unique(point$scenario)
  for (i in c(1:4))
  {
    k = i*1 + 2050
    
    point$year[point$scenario == scenarios[i]] <- k
  } 
  
  ymin <- (round(min(hist$year)/10)*10)
  
  # Plot the figure
  p = ggplot() +
    #geom_line(data = df2, aes(x = year, y = value, linetype = model, colour = scenario), size = 0.5) +
    geom_line(data = df1, aes(x = year, y = value, colour = scenario, linetype = model), size = 1.5) +
    geom_polygon(data = filter(df3, scenario %in% c("FFANF")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("ECO")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("TLTL")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("ONEPW")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_point(data = point, aes(x = year, y = value, shape = model, colour = scenario)) +
    scale_colour_manual(values = c("green","cyan","red","purple"), name="", guide = F)+ # guide = F surpresses linetype and shape in fill legend
    scale_fill_manual(values=c("green","cyan","red","purple"), name = "") +
    scale_linetype_manual(values=c("solid","longdash", "dotted"), name = "") +
    scale_shape_manual(values=c(4, 15), name = "") +
    ylab(yas) + xlab("") +
    facet_wrap(~region, scales = "free_x")
    #facet_wrap(~region)
  
  #p = p +ggtitle(title)
  
  p = p + geom_line(data = point, aes(x = year, y = value, colour = scenario))
  
  p  = p + geom_line(data = hist, aes(x = year, y = value, group = scenario), size = 1.5, colour = "grey50")
  
  # p = p + guides(fill = guide_legend(override.aes = list(alpha = 0.1)))
  p = p + guides(fill = guide_legend(keywidth = 1, keyheight = 1, override.aes = 
                                       list(alpha = 0.1, size = 0.5, colour = c("green","cyan","red","purple"))))
  
  p = p + theme_classic() +
    theme(panel.border = element_blank(),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
    scale_x_continuous(limits = c(ymin,2060), breaks = seq(ymin,2050,by=10),expand=c(0,0)) + # note that: Limits sets the limits for the axes (added 0.1) but always some space is added. Latter is controlled by expand
    scale_y_continuous(labels = comma) +
    theme(legend.background=element_blank()) +
    theme(legend.key=element_rect(size=0.5, color="white"), # Increase space between legend boxes - doubt if it works
          strip.background = element_rect(colour="white", fill="white"), # Remove box and background of facet
          legend.position="bottom") +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
  
  p
   
}


# Function to create bandwidth plot including historical data for single region
bwplot3_f <- function(df, yas, title){
  
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
  
  df1 <- filter(df, model == "Average")
  df2 <- filter(df, model != "Average")
  df2 <- ddply(df2,.(scenario, region), myear)
  df3 <- ddply(df2, .(scenario, region, group), mgroup)
  
  point <- filter(df2, year == 2050) 
  scenarios <- unique(point$scenario)
  for (i in c(1:4))
  {
    k = i*1 + 2050
    
    point$year[point$scenario == scenarios[i]] <- k
  } 
  
  #ymin <- (round(min(hist$year)/10)*10)
  
  # Plot the figure
  p = ggplot() +
    #geom_line(data = df2, aes(x = year, y = value, linetype = model, colour = scenario), size = 0.5) +
    geom_line(data = df1, aes(x = year, y = value, colour = scenario, linetype = model), size = 1.5) +
    geom_polygon(data = filter(df3, scenario %in% c("FFANF")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("ECO")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("TLTL")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("ONEPW")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_point(data = point, aes(x = year, y = value, shape = model, colour = scenario)) +
    scale_colour_manual(values = c("green","cyan","red","purple"), name="", guide = F)+ # guide = F surpresses linetype and shape in fill legend
    scale_fill_manual(values=c("green","cyan","red","purple"), name = "") +
    scale_linetype_manual(values=c("solid","longdash", "dotted"), name = "") +
    scale_shape_manual(values=c(4, 15), name = "") +
    ylab(yas) + xlab("") 
  
  p = p + geom_line(data = point, aes(x = year, y = value, colour = scenario))
  
  #p  = p + geom_line(data = hist, aes(x = year, y = value, group = scenario), size = 1.5, colour = "grey50")
  
  p = p +ggtitle(title)  
  # p = p + guides(fill = guide_legend(override.aes = list(alpha = 0.1)))
  p = p + guides(fill = guide_legend(keywidth = 1, keyheight = 1, override.aes = 
                                       list(alpha = 0.1, size = 0.5, colour = c("green","cyan","red","purple"))))
  
  p = p + theme_classic() +
    theme(panel.border = element_blank(),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
    scale_x_continuous(limits = c(2010, 2058), breaks = seq(2010, 2050,by=10),expand=c(0,0)) + # note that: Limits sets the limits for the axes (added 0.1) but always some space is added. Latter is controlled by expand
    scale_y_continuous(labels = comma) +
    theme(legend.background=element_blank()) +
    theme(legend.key=element_rect(size=0.5, color="white"), # Increase space between legend boxes - doubt if it works
          strip.background = element_rect(colour="white", fill="white"), # Remove box and background of facet
          legend.position="bottom") 
  #guides(fill=guide_legend(nrow=2))
  p 
}

# Function to create bandwidth plot including historical data for single region
bwplot4_f <- function(df, hist, yas, title){
  
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
  
  df1 <- filter(df, model == "Average")
  df2 <- filter(df, model != "Average")
  df2 <- ddply(df2,.(scenario, region), myear)
  df3 <- ddply(df2, .(scenario, region, group), mgroup)
  
  point <- filter(df2, year == 2050) 
  scenarios <- unique(point$scenario)
  for (i in c(1:4))
  {
    k = i*1 + 2050
    
    point$year[point$scenario == scenarios[i]] <- k
  } 
  
  ymin <- (round(min(hist$year)/10)*10)
  
  # Plot the figure
  p = ggplot() +
    #geom_line(data = df2, aes(x = year, y = value, linetype = model, colour = scenario), size = 0.5) +
    geom_line(data = df1, aes(x = year, y = value, colour = scenario, linetype = model), size = 1.5) +
    geom_polygon(data = filter(df3, scenario %in% c("FFANF")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("ECO")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("TLTL")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("ONEPW")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_point(data = point, aes(x = year, y = value, shape = model, colour = scenario)) +
    scale_colour_manual(values = c("green","cyan","red","purple"), name="", guide = F)+ # guide = F surpresses linetype and shape in fill legend
    scale_fill_manual(values=c("green","cyan","red","purple"), name = "") +
    scale_linetype_manual(values=c("solid","longdash", "dotted"), name = "") +
    scale_shape_manual(values=c(4, 15), name = "") +
    ylab(yas) + xlab("") 
  
  p = p + geom_line(data = point, aes(x = year, y = value, colour = scenario))
  
  p  = p + geom_line(data = hist, aes(x = year, y = value, group = scenario), size = 1.5, colour = "grey50")

  p = p +ggtitle(title)  
  # p = p + guides(fill = guide_legend(override.aes = list(alpha = 0.1)))
  p = p + guides(fill = guide_legend(keywidth = 1, keyheight = 1, override.aes = 
                                       list(alpha = 0.1, size = 0.5, colour = c("green","cyan","red","purple"))))
  
  p = p + theme_classic() +
    theme(panel.border = element_blank(),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
    scale_x_continuous(limits = c(ymin,2058), breaks = seq(ymin,2050,by=10),expand=c(0,0)) + # note that: Limits sets the limits for the axes (added 0.1) but always some space is added. Latter is controlled by expand
    scale_y_continuous(labels = comma) +
    theme(legend.background=element_blank()) +
    theme(legend.key=element_rect(size=0.5, color="white"), # Increase space between legend boxes - doubt if it works
          strip.background = element_rect(colour="white", fill="white"), # Remove box and background of facet
          legend.position="bottom") 
  #guides(fill=guide_legend(nrow=2))
  p 
}


# Function to create bandwidht plot including historical data but without line
bwplot5_f <- function(df, hist, yas){
  
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
  
  df1 <- filter(df, model == "Average")
  df2 <- filter(df, model != "Average")
  df2 <- ddply(df2,.(scenario, region), myear)
  df3 <- ddply(df2, .(scenario, region, group), mgroup)
  title = unique(with(df, FNS))
  
  point <- filter(df2, year == 2050) 
  scenarios <- unique(point$scenario)
  for (i in c(1:4))
  {
    k = i*1 + 2050
    
    point$year[point$scenario == scenarios[i]] <- k
  } 
  
  ymin <- (round(min(hist$year)/10)*10)
  
  # Plot the figure
  p = ggplot() +
    #geom_line(data = df2, aes(x = year, y = value, linetype = model, colour = scenario), size = 0.5) +
    geom_line(data = df1, aes(x = year, y = value, colour = scenario, linetype = model), size = 1.5) +
    geom_polygon(data = filter(df3, scenario %in% c("FFANF")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("ECO")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("TLTL")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("ONEPW")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
   #geom_point(data = point, aes(x = year, y = value, shape = model, colour = scenario)) +
    scale_colour_manual(values = c("green","cyan","red","purple"), name="", guide = F)+ # guide = F surpresses linetype and shape in fill legend
    scale_fill_manual(values=c("green","cyan","red","purple"), name = "") +
    scale_linetype_manual(values=c("solid","longdash", "dotted"), name = "") +
    scale_shape_manual(values=c(4, 15), name = "") +
    ylab(yas) + xlab("") +
    facet_wrap(~region, scales = "free_x")
  #facet_wrap(~region)
  
  #p = p +ggtitle(title)
  
  #p = p + geom_line(data = point, aes(x = year, y = value, colour = scenario))
  
  p  = p + geom_line(data = hist, aes(x = year, y = value, group = scenario), size = 1.5, colour = "grey50")
  
  # p = p + guides(fill = guide_legend(override.aes = list(alpha = 0.1)))
  p = p + guides(fill = guide_legend(keywidth = 1, keyheight = 1, override.aes = 
                                       list(alpha = 0.1, size = 0.5, colour = c("green","cyan","red","purple"))))
  
  p = p + theme_classic() +
    theme(panel.border = element_blank(),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
    scale_x_continuous(limits = c(ymin,2060), breaks = seq(ymin,2050,by=10),expand=c(0,0)) + # note that: Limits sets the limits for the axes (added 0.1) but always some space is added. Latter is controlled by expand
    scale_y_continuous(labels = comma) +
    theme(legend.background=element_blank()) +
    theme(legend.key=element_rect(size=0.5, color="white"), # Increase space between legend boxes - doubt if it works
          strip.background = element_rect(colour="white", fill="white"), # Remove box and background of facet
          legend.position="bottom") +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
  
  p
  
}

# Function to create bandwidth plot including historical data for single region but without bars
bwplot6_f <- function(df, hist, yas, title){
  
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
  
  df1 <- filter(df, model == "Average")
  df2 <- filter(df, model != "Average")
  df2 <- ddply(df2,.(scenario, region), myear)
  df3 <- ddply(df2, .(scenario, region, group), mgroup)
  
  point <- filter(df2, year == 2050) 
  scenarios <- unique(point$scenario)
  for (i in c(1:4))
  {
    k = i*1 + 2050
    
    point$year[point$scenario == scenarios[i]] <- k
  } 
  
  ymin <- (round(min(hist$year)/10)*10)
  
  # Plot the figure
  p = ggplot() +
    #geom_line(data = df2, aes(x = year, y = value, linetype = model, colour = scenario), size = 0.5) +
    geom_line(data = df1, aes(x = year, y = value, colour = scenario, linetype = model), size = 1.5) +
    geom_polygon(data = filter(df3, scenario %in% c("FFANF")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("ECO")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("TLTL")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    geom_polygon(data = filter(df3, scenario %in% c("ONEPW")), 
                 aes(x = year, y = value, group = group, fill = scenario), alpha = 0.2) +
    #geom_point(data = point, aes(x = year, y = value, shape = model, colour = scenario)) +
    scale_colour_manual(values = c("green","cyan","red","purple"), name="", guide = F)+ # guide = F surpresses linetype and shape in fill legend
    scale_fill_manual(values=c("green","cyan","red","purple"), name = "") +
    scale_linetype_manual(values=c("solid","longdash", "dotted"), name = "") +
    scale_shape_manual(values=c(4, 15), name = "") +
    ylab(yas) + xlab("") 
  
  #p = p + geom_line(data = point, aes(x = year, y = value, colour = scenario))
  
  p  = p + geom_line(data = hist, aes(x = year, y = value, group = scenario), size = 1.5, colour = "grey50")
  
  p = p +ggtitle(title)  
  # p = p + guides(fill = guide_legend(override.aes = list(alpha = 0.1)))
  p = p + guides(fill = guide_legend(keywidth = 1, keyheight = 1, override.aes = 
                                       list(alpha = 0.1, size = 0.5, colour = c("green","cyan","red","purple"))))
  
  p = p + theme_classic() +
    theme(panel.border = element_blank(),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
    scale_x_continuous(limits = c(ymin,2058), breaks = seq(ymin,2050,by=10),expand=c(0,0)) + # note that: Limits sets the limits for the axes (added 0.1) but always some space is added. Latter is controlled by expand
    scale_y_continuous(labels = comma) +
    theme(legend.background=element_blank()) +
    theme(legend.key=element_rect(size=0.5, color="white"), # Increase space between legend boxes - doubt if it works
          strip.background = element_rect(colour="white", fill="white"), # Remove box and background of facet
          legend.position="bottom") 
  #guides(fill=guide_legend(nrow=2))
  p 
}
