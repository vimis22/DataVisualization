# Required libraries
library(tidyverse)
library(gganimate)
library(scales)
library(viridis)
library(plotly)
library(readxl)
library(dplyr)
library(ggplot)
#install.packages("readxl")

setwd("C:/Users/vivek/Downloads/School_Projects/DataVisualization")
# Read and process the data
process_suicide_data <- function(file_path) {
  df <- read.csv(file_path) %>%
    # Convert year to proper date format
    mutate(
      year = as.numeric(year),
      # Calculate suicide rate per 100k
      suicide_rate = suicides_no / population * 100000,
      # Create age_group factor with proper ordering
      age_group = factor(age,
        levels = c("5-14 years", "15-24 years", "25-34 years",
                  "35-54 years", "55-74 years", "75+ years")
      )
    ) %>%
    # Remove any rows with missing values
    na.omit()
  
  return(df)
}

# Required visualizations based on project requirements:

# 1. Time series visualization of suicide rates
create_time_series <- function(data) {
  ggplot(data %>% 
           group_by(year, sex) %>%
           summarise(avg_rate = mean(suicide_rate)), 
         aes(x = year, y = avg_rate, color = sex)) +
    geom_line(size = 1) +
    geom_point() +
    theme_minimal() +
    labs(
      title = "Global Suicide Rates Over Time by Gender",
      x = "Year",
      y = "Suicide Rate (per 100k population)",
      color = "Gender"
    ) +
    scale_y_continuous(labels = comma)
}

# 2. Age group comparison (boxplot)
create_age_boxplot <- function(data) {
  ggplot(data, aes(x = age_group, y = suicide_rate, fill = sex)) +
    geom_boxplot(alpha = 0.7) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = "Suicide Rates Distribution by Age Group and Gender",
      x = "Age Group",
      y = "Suicide Rate (per 100k population)",
      fill = "Gender"
    ) +
    scale_y_continuous(labels = comma)
}

# 3. Animated choropleth map over time
create_animated_map <- function(data) {
  # Calculate average rates by country and year
  country_rates <- data %>%
    group_by(country, year) %>%
    summarise(avg_rate = mean(suicide_rate)) %>%
    ungroup()
  
  p <- ggplot(country_rates, aes(x = year, y = reorder(country, avg_rate))) +
    geom_tile(aes(fill = avg_rate)) +
    scale_fill_viridis() +
    labs(
      title = "Suicide Rates by Country",
      subtitle = "Year: {frame_time}",
      x = "Year",
      y = "Country",
      fill = "Suicide Rate\nper 100k"
    ) +
    theme_minimal() +
    transition_time(year)
  
  return(p)
}

# 4. GDP vs Suicide Rate scatter plot
create_gdp_scatter <- function(data) {
  ggplot(data, aes(x = gdp_per_capita, y = suicide_rate)) +
    geom_point(aes(color = sex, size = population), alpha = 0.6) +
    scale_x_log10(labels = comma) +
    scale_y_continuous(labels = comma) +
    theme_minimal() +
    labs(
      title = "GDP per Capita vs Suicide Rate",
      x = "GDP per Capita (log scale)",
      y = "Suicide Rate (per 100k population)",
      color = "Gender",
      size = "Population"
    )
}

# 5. Top countries bar chart
create_top_countries <- function(data, n = 10) {
  data %>%
    group_by(country) %>%
    summarise(avg_rate = mean(suicide_rate)) %>%
    arrange(desc(avg_rate)) %>%
    head(n) %>%
    ggplot(aes(x = reorder(country, avg_rate), y = avg_rate)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    theme_minimal() +
    labs(
      title = paste("Top", n, "Countries by Average Suicide Rate"),
      x = "Country",
      y = "Average Suicide Rate (per 100k population)"
    )
}

df <- process_suicide_data("suicidedata.csv")
head(df)
p1 <- create_time_series(df)
print(p1)
p2 <- create_age_boxplot(df)
print(p2)
p3 <- create_animated_map(df)
print(p3)
p4 <- create_gdp_scatter(df)
print(p4)
p5 <- create_top_countries(df)
print(p5)

