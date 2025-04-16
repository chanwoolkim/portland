# preliminary.R

# Linux box cannot install these packages
if (Sys.info()[4]!="jdube01"){
  library(acs)
  library(Hmisc)
  library(tidycensus)
  library(tidygeocoder)
  library(tidyverse)
}

library(dplyr)
library(data.table)
library(fixest)
library(ggplot2)
library(ggrepel)
library(glue)
library(httr)
library(jsonlite)
library(lubridate)
require(multiwayvcov)
library(RColorBrewer)
library(readr)
library(readxl)
library(scales)
library(stringr)
library(textab)
library(tidyr)
library(zip)
library(zoo)

colours_set <- brewer.pal("Set2", n=8)

fte_theme <- function() {
  # Generate the colours for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background="white"
  color.grid.major=palette[4]
  color.grid.minor=palette[3]
  color.axis.text=palette[7]
  color.axis.title=palette[7]
  color.title=palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=8, base_family="serif") +
    
    # Set the entire chart region to a light gray colour
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major, size=.25)) +
    theme(panel.grid.minor=element_line(color=color.grid.minor, size=.25)) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend
    theme(legend.position="bottom") +
    theme(legend.background=element_rect(fill=color.background)) +
    theme(legend.title=element_text(size=9, color=color.axis.title, family="serif")) +
    theme(legend.text=element_text(size=8, color=color.axis.title, family="serif")) +
    theme(legend.box.background=element_rect(colour=color.grid.major)) +
    theme(legend.title.align=0.5) +
    
    # Set title and axis labels, and format these and tick marks
    theme(axis.text=element_text(size=rel(1), color=color.axis.text)) +
    theme(axis.title.x=element_text(color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin=unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

pie_theme <- function() {
  # Generate the colours for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background="white"
  color.grid.major=palette[3]
  color.axis.text=palette[7]
  color.axis.title=palette[7]
  color.title=palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=8, base_family="serif") +
    
    # Set the entire chart region to a light gray colour
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_blank()) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend
    theme(legend.position="bottom") +
    theme(legend.background=element_rect(fill=color.background)) +
    theme(legend.title=element_text(size=9, color=color.axis.title, family="serif")) +
    theme(legend.text=element_text(size=8, color=color.axis.title, family="serif")) +
    theme(legend.box.background=element_rect(colour=color.grid.major)) +
    theme(legend.title.align=0.5) +
    
    # Set title and axis labels, and format these and tick marks
    theme(axis.text=element_blank()) +
    theme(axis.title.x=element_blank()) +
    theme(axis.title.y=element_blank()) +
    
    # Plot margins
    theme(plot.margin=unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

map_theme <- function() {
  # Generate the colours for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background="white"
  color.grid.major="white"
  color.axis.text=palette[7]
  color.axis.title=palette[7]
  color.title=palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=8, base_family="serif") +
    
    # Set the entire chart region to a light gray colour
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_blank()) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend
    theme(legend.position="bottom") +
    theme(legend.background=element_rect(fill=color.background)) +
    theme(legend.title=element_text(size=9, color=color.axis.title, family="serif")) +
    theme(legend.text=element_text(size=8, color=color.axis.title, family="serif")) +
    theme(legend.box.background=element_rect(colour=color.grid.major)) +
    theme(legend.title.align=0.5) +
    
    # Set title and axis labels, and format these and tick marks
    theme(axis.text=element_blank()) +
    theme(axis.title.x=element_blank()) +
    theme(axis.title.y=element_blank()) +
    
    # Plot margins
    theme(plot.margin=unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

# Function to go around 0 digit issue in textable (use .00000 as default)
fix_0 <- function(tab) {
  nrow <- length(tab$row_list)
  for (row in 1:nrow) {
    if (length(tab$row_list[row][[1]])>1) {
      for (col in 1:length(tab$row_list[row][[1]])) {
        tab$row_list[row][[1]][col] <-
          str_replace(tab$row_list[row][[1]][col], ".00000", "")
      }
    }
  }
  return(tab)
}

# Binscatter function
binscatter <- function(y, x, df, z, group, n.cut = 20, cluster, qt_use_all = TRUE, qt_samp = 10000) {
  require(multiwayvcov)
  if(missing(group) == TRUE) {
    df$group <- 1
  } else {
    df$group <- df[,group]
  }
  if (missing(z) == FALSE) {
    x_res_string <- paste0("df$",x," ~ ",paste0("df$",z, collapse= " + "))
    print(x_res_string)
    y_res_string <- paste0("df$",y," ~ ",paste0("df$",z, collapse= " + "))
    print(y_res_string)
    df$xRes <- residuals(lm(as.formula(x_res_string), na.action="na.exclude")) + mean(df[,x], na.rm = TRUE)
    df$yRes <- resuduals(lm(as.formula(y_res_string), na.action="na.exclude")) + mean(df[,y], na.rm = TRUE)
    reg_string <- paste0("df$",y," ~ df$",x," + ",paste0("df$",z, collapse= " + "))
  } else {
    df$xRes <- df[,x]
    df$yRes <- df[,y]
    reg_string <- paste0("df$",y," ~ df$",x)
  }
  
  print(paste0("Binscatter with ",reg_string))
  lin_model <- lm(as.formula(reg_string))
  if (missing(cluster) == FALSE) {
    lin_model.vcov_cluster <- cluster.vcov(lin_model, as.factor(df[,cluster]))
    cluster_se <- sqrt(lin_model.vcov_cluster[2,2])
  } else {
    cluster_se <- NULL
  }
  
  #being new code for RD
  #qts <- unique(quantile( df$xRes, seq(from=0,to=1,length.out=n.cut+1), na.rm = TRUE))
  qts <- c(unique(quantile( df[df$xRes<0,"xRes"] , seq(from=0,to=1,length.out=n.cut+1), na.rm = TRUE)),
           unique(quantile( df[df$xRes>=0,"xRes"], seq(from=0,to=1,length.out=n.cut+1), na.rm = TRUE))[-1])
  #print(qts)
  # end new code for RD
  df_bin <- df %>% mutate(cut = as.numeric(cut(xRes, qts ,include.lowest = TRUE))) %>%
    group_by(cut,group) %>% summarise( x = mean(xRes, na.rm =  TRUE), y = mean(yRes, na.rm = TRUE))
  return_list <- list("df_bin" = df_bin, "in_model" = lin_model, "cluster_se" = cluster_se)
  return(return_list)
}

loc_lin <- function(df, y, bw, range, cutoff = 0, gran = 40, kern = "triangular") {
  require(rdd) #this is needed for the kernelwts only
  d.l<-data.frame(X=df[df$x<cutoff,"x"],Y=df[df$x<cutoff,y])
  lval<-seq(range[1],cutoff,length.out=(gran%/%2 + 1))
  width <- abs(range[1])/(gran%/%2)
  line_minus <- data.frame(x = lval, y = NA, lwr = NA, upr = NA)
  for(i in 1:(gran%/%2 + 1)) {
    sub<-d.l$X>=(lval[i]-bw) & d.l$X<=(lval[i]+bw)
    w<-kernelwts(X=d.l$X[sub],center=lval[i],bw=bw,kernel=kern)
    ly<-d.l$Y[sub]
    lx<-d.l$X[sub]
    if(length(lx)<=2)
      pred<-rep(NA,3)
    else
      pred<-predict(lm(ly~lx,weights=w),interval="confidence",newdata=data.frame(lx=lval[i]))
    pred
    line_minus[i,c("y","lwr","upr")] = pred
  }
  
  d.r<-data.frame(X=df[df$x>cutoff,"x"],Y=df[df$x>cutoff,y])
  rval<-seq(cutoff,range[2],length.out=(gran%/%2 + 1))
  width <- abs(range[2])/(gran%/%2)
  line_plus <- data.frame(x = rval, y = NA, lwr = NA, upr = NA)
  for(i in 1:(gran%/%2  + 1)) {
    sub<-d.r$X>=(rval[i]-bw) & d.r$X<=(rval[i]+bw)
    w<-kernelwts(X=d.r$X[sub],center=rval[i],bw=bw,kernel=kern)
    ry<-d.r$Y[sub]
    rx<-d.r$X[sub]
    if(length(rx)<=2)
      pred<-rep(NA,3)
    else
      pred<-predict(lm(ry~rx,weights=w),interval="confidence",newdata=data.frame(rx=rval[i]))
    line_plus[i,c("y","lwr","upr")] = pred
  }
  return_list <- list("line_minus" = line_minus, "line_plus" = line_plus)
  return(return_list)
}

gg_bin <- function(df, dvs, file_suffix, coef, se, 
                   xlab, ylab, bw = bw_preferred, 
                   x = "x", scales = NA, y_size=NA, 
                   caption = NA, loc = "ur", coef_size = 4.23, samp_name = samp_file_name,
                   out_path_f = out_path, n.cut = n_cut, ll = TRUE, title=NA,
                   legend_title = "Outcome", legend_labels = dvs, legend_pos = c(1,0),
                   y_range = NA, y_div=NA, grid=TRUE,  save_temp=FALSE) {
  tmp <- data.frame(cut = double(), group = character(), x = double(), y = double())
  for (dv_each in dvs) {
    tmp <- rbind.data.frame(tmp,binscatter(dv_each, x, df %>% filter(abs(x) < bw_window), n.cut = n.cut)$df_bin %>% 
                              mutate(group = dv_each))
  }
  #tmp %>% str
  #tmp %>% print
  #tmp %>% group_by(group) %>% summarise(n()) %>% print
  if (length(dvs) == 1) {
    gg <- ggplot(tmp, aes(x=x, y=y, group=group)) + geom_point(color="navy") +
      labs(x=xlab, y=ylab) + fte_theme() 
    if (ll == TRUE) {
      line <- loc_lin(df, y = dvs, bw = bw, cutoff = cutoff, range = c(-bw_window,bw_window))
      line_alt <- rbind(line$line_minus, line$line_plus) %>% mutate(left_side = row_number() <= gran%/%2 + 1)
      gg <- gg +  geom_line(data = line_alt,aes(x=x, y=y, group = left_side, colour="red"))
    }
  }
  else if (length(dvs) >= 2) {
    gg <- ggplot(tmp, aes(x=x, y=y, group=group,shape=group,colour=group)) + geom_point() + 
      labs(x=xlab, y=ylab) +fte_theme()
    gg <- gg + scale_colour_manual(legend_title,values=c("navy",cbPalette[2:3]), labels = legend_labels)  + 
      scale_shape_discrete(legend_title, labels = legend_labels) +
      theme(legend.position=legend_pos, legend.justification=legend_pos)
  }
  if (!is.na(y_range[1])) {
    gg <- gg + coord_cartesian(ylim = c(y_range[1],y_range[2]))
  }
  if (!is.na(caption)) {
    gg <- gg + coef_label(caption_text, loc, coef_size)
  }
  if (!is.na(title)) {
    gg <- gg +  ggtitle(title)
  }
  
  if (!is.na(scales)) {
    if (scales == "percent") {
      if (length(y_div)>1) {
        gg <- gg + scale_y_continuous(breaks=y_div, labels = scales::percent)
      } else {
        gg <- gg + scale_y_continuous(labels = scales::percent)
      }
    }
    if (scales == "dollar") {
      gg <- gg + scale_y_continuous(labels = scales::dollar)
    }
  }
  
  if (grid==FALSE) {
    gg <- gg + theme(
      #axis.line = element_line(colour = "gray", linetype = "solid", size=0.5),
      panel.grid.major= element_blank(), 
      panel.grid.minor = element_blank())+ geom_vline(xintercept=0, color="gray", size=0.5)
    
  }
  
  if (save_temp==TRUE) {
    saveRDS(tmp, paste0(out_path_f,"rd_data_temp.RDS"))
    saveRDS(line_alt, paste0(out_path_f,"rd_data_line.RDS"))                 
  }
  
  ggsave(gg,file=paste0(out_path_f,"rd_",samp_name,file_suffix), width = wd, height = ht)
  #return(gg)
}
