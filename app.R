library(shiny)
library(shinydashboard)
library(tidyverse)
library(gganimate)
library(scales)
library(viridis)
library(plotly)
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(ggalt)

if (interactive()) {
  root_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  root_dir <- getwd()
}

setwd(root_dir)

source("R/data_prep.R")
source("R/utils.R")
source("R/visualizations.R")
source("R/ui.R")
source("R/server.R")

data <- process_suicide_data("data/data.csv")

shinyApp(ui = ui, server = server)