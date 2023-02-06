# Master file

# Preliminary ####
rm(list=ls())
start_time <- Sys.time()

library(acs)
library(data.table)
library(ggplot2)
library(glue)
library(Hmisc)
library(httr)
library(jsonlite)
library(lubridate)
library(RColorBrewer)
library(scales)
library(stringr)
library(tidycensus)
library(tidygeocoder)
library(tidyverse)
library(textables)

wd <- "/Users/ckim/Library/CloudStorage/Dropbox/Research/Research Assistance/Portland"
code_dir <- paste0(wd, "/portland_servus")
data_dir <- paste0(wd, "/SERVUS")
working_data_dir <- paste0(wd, "/working_data")
output_dir <- paste0(code_dir, "/output")

colours_set <- brewer.pal("Set2", n=8)

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
        theme(legend.position=c(0.9, 0.785)) +
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
    if (length(tab$row_list[row][[1]])==tab$ncol) {
      for (col in 1:tab$ncol) {
        tab$row_list[row][[1]][col] <-
          str_replace(tab$row_list[row][[1]][col], ".00000", "")
      }
    }
  }
  return(tab)
}


# Run files ####
# Data preparation
source(paste0(code_dir, "/geocoding_api.R"))
source(paste0(code_dir, "/tidy_census_api.R"))
source(paste0(code_dir, "/setup_portland.R"))
source(paste0(code_dir, "/delinquency_measure.R"))

# Analysis
source(paste0(code_dir, "/descriptive_statistics.R"))

end_time <- Sys.time()
end_time-start_time
