library(tidyverse)
library(gganimate)
library(scales)
library(viridis)
library(plotly)

if (interactive()) {
  root_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  root_dir <- getwd()
}
setwd(root_dir)

source("R/data_prep.R")
source("R/utils.R")
source("R/visualizations.R")

data <- process_suicide_data("data/data.csv")

dir.create("output", showWarnings = FALSE)

generate_all_plots <- function(data) {
  p1 <- create_time_series(data)
  ggsave("output/time_series.png", p1, width = 12, height = 6, dpi = 300)
  
  p2 <- create_age_boxplot(data)
  ggsave("output/age_groups.png", p2, width = 12, height = 6, dpi = 300)
  
  p3 <- create_gdp_scatter(data)
  ggsave("output/gdp_scatter.png", p3, width = 12, height = 6, dpi = 300)
  
  p4 <- create_top_countries(data)
  ggsave("output/top_countries.png", p4, width = 12, height = 6, dpi = 300)
  
  p5 <- create_generation_trends(data)
  ggsave("output/generation_trends.png", p5, width = 12, height = 8, dpi = 300)
  
  p6 <- create_animated_gdp_plot(data)
  anim_save("output/gdp_animation.gif", p6, width = 800, height = 600)
  
  print("Generating interactive plots...")
  
  p1_interactive <- ggplotly(p1)
  p2_interactive <- ggplotly(p2)
  p3_interactive <- ggplotly(p3)
  p4_interactive <- ggplotly(p4)
  p5_interactive <- ggplotly(p5)
  

  htmlwidgets::saveWidget(p1_interactive, "output/time_series_interactive.html")
  htmlwidgets::saveWidget(p2_interactive, "output/age_groups_interactive.html")
  htmlwidgets::saveWidget(p3_interactive, "output/gdp_scatter_interactive.html")
  htmlwidgets::saveWidget(p4_interactive, "output/top_countries_interactive.html")
  htmlwidgets::saveWidget(p5_interactive, "output/generation_trends_interactive.html")
  
  return(list(
    time_series = p1,
    age_groups = p2,
    gdp_scatter = p3,
    top_countries = p4,
    generation_trends = p5,
    animated_gdp = p6
  ))
}

plots <- generate_all_plots(data)

plots$time_series
plots$age_groups
plots$gdp_scatter
plots$top_countries
plots$generation_trends