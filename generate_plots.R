library(tidyverse)
library(gganimate)
library(scales)
library(viridis)
library(plotly)
library(gridExtra)
library(maps)
library(sf)

if (interactive()) {
  root_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  root_dir <- getwd()
}
setwd(root_dir)

source("R/data_prep.R")
source("R/utils.R")
source("R/visualizations.R")

dir.create("output", showWarnings = FALSE)
dir.create("output/static", showWarnings = FALSE)
dir.create("output/animations", showWarnings = FALSE)

data <- process_suicide_data("data/data.csv")

save_plot <- function(plot, name, type = "static") {
  if (type == "static") {
    ggsave(
      filename = paste0("output/static/", name, ".png"),
      plot = plot,
      width = 12,
      height = 8,
      dpi = 300
    )
    print(plot)
  } else if (type == "interactive") {
    p_interactive <- ggplotly(plot)
    htmlwidgets::saveWidget(
      p_interactive,
      file = paste0("output/interactive/", name, ".html"),
      selfcontained = TRUE
    )
    print(p_interactive)
  } else if (type == "animation") {
    anim_save(
      paste0("output/animations/", name, ".gif"),
      animation = plot
    )
    print(plot)
  }
}

cat("\nGenerating overview plots...\n")

p1 <- create_time_series(data)
print(p1)
save_plot(p1, "time_series", "static")

p2 <- create_gender_overview(data, "line")
save_plot(p2, "gender_line", "static")

p3 <- create_gender_overview(data, "pie")
save_plot(p3, "gender_pie", "static")

p4 <- create_age_overview(data, "bar")
save_plot(p4, "age_bar", "static")

p5 <- create_age_time_series(data)
save_plot(p5, "age_time_series", "static")

cat("\nGenerating geographic plots...\n")

p6 <- create_continent_bar(data, "bar")
save_plot(p6, "continent_bar", "static")

p7 <- create_continent_gender_bar(data)
save_plot(p7, "continent_gender", "static")

p8 <- create_continent_age_bar(data)
save_plot(p8, "continent_age", "static")

p9 <- create_continent_time_series(data)
save_plot(p9, "continent_time", "static")

p10 <- create_continent_choropleth(data)
save_plot(p10, "continent_map", "static")
print(p10)

cat("\nGenerating country plots...\n")

p11 <- create_country_bar(data)
save_plot(p11, "country_bar", "static")

p12 <- create_country_gender_proportion(data)
save_plot(p12, "country_gender", "static")

p13 <- create_country_age_proportion(data)
save_plot(p13, "country_age", "static")

p14 <- create_country_choropleth(data)
print(p14)
save_plot(p14, "country_map", "static")

p15 <- create_country_trends(data)
save_plot(p15, "country_trends", "static")

cat("\nGenerating demographic plots...\n")

p16 <- create_gender_time_series(data)
save_plot(p16, "gender_time", "static")

p17 <- create_gender_disparity_evolution(data)
save_plot(p17, "gender_disparity_evolution", "static")

p18 <- create_gender_disparity_plot(data)
save_plot(p18, "gender_disparity", "static")

cat("\nGenerating animations...\n")

p19 <- create_animated_violin_plot(data)
save_plot(p19, "age_violin_animated", "animation")

p20 <- create_animated_gdp_plot(data)
save_plot(p20, "gdp_evolution_animated", "animation")

cat("\nGenerating economic plots...\n")

p21 <- create_gdp_suicide_scatter(data)
save_plot(p21, "gdp_scatter", "static")

cat("\nPlot generation complete!\n")
cat("Static plots saved in: output/static/\n")
cat("Interactive plots saved in: output/interactive/\n")
cat("Animations saved in: output/animations/\n")