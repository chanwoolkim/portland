# preliminary.R

# Linux box cannot install these packages
if (Sys.info()[4]!="jdube01"){
  library(acs)
  library(Hmisc)
  library(httr)
  library(tidycensus)
  library(tidygeocoder)
  library(tidyverse)
  library(textab)
}

library(dplyr)
library(data.table)
library(fixest)
library(ggplot2)
library(ggrepel)
library(glue)
library(jsonlite)
library(lubridate)
library(margins)
library(RColorBrewer)
library(scales)
library(stringr)
library(tidyr)

colours_set <- brewer.pal("Set2", n=8)

fte_theme <- function() {
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
      theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
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
