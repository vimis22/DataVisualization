library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)

create_time_series <- function(data) {
  ggplot(data %>% 
           group_by(year, sex) %>%
           summarise(avg_rate = mean(suicide_rate), .groups = 'drop'), 
         aes(x = year, y = avg_rate, color = sex)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = c("female" = "#2C7BB6", "male" = "#D7191C")) +
    scale_x_continuous(breaks = seq(1985, 2016, 5)) +
    scale_y_continuous(
      labels = function(x) paste0(x, " per 100k"),
      breaks = seq(0, 25, 5)
    ) +
    labs(
      title = "Global Suicide Rates Over Time by Gender",
      subtitle = "Analysis of suicide rates showing distinct gender-based trends",
      x = "Year",
      y = "Suicide Rate",
      color = "Gender"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "top"
    )
}

create_age_boxplot <- function(data) {
  ggplot(data, aes(x = age_group, y = suicide_rate, fill = sex)) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = c("female" = "#2C7BB6", "male" = "#D7191C")) +
    scale_y_continuous(
      labels = function(x) paste0(x, " per 100k"),
      breaks = seq(0, 100, 25)
    ) +
    labs(
      title = "Suicide Rates by Age Group and Gender",
      subtitle = "Distribution shows higher rates among older age groups",
      x = "Age Group",
      y = "Suicides per 100,000 population",
      fill = "Gender"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
}

create_gdp_scatter <- function(data) {
  ggplot(data, aes(x = gdp_per_capita, y = suicide_rate, color = sex)) +
    geom_point(alpha = 0.4, size = 2) +
    geom_smooth(method = "loess", se = TRUE, color = "black", size = 0.5) +
    scale_x_log10(labels = scales::dollar_format()) +
    scale_color_manual(values = c("female" = "#2C7BB6", "male" = "#D7191C")) +
    labs(
      title = "GDP per Capita vs Suicide Rate",
      subtitle = "Exploring economic relationships with suicide rates",
      x = "GDP per Capita (log scale)",
      y = "Suicides per 100,000 population",
      color = "Gender"
    ) +
    theme_minimal()
}

create_top_countries <- function(data, n = 10) {
  top_data <- data %>%
    group_by(country) %>%
    summarise(
      avg_rate = mean(suicide_rate),
      .groups = 'drop'
    ) %>%
    arrange(desc(avg_rate)) %>%
    head(n)

  ggplot(top_data, aes(x = reorder(country, avg_rate), y = avg_rate)) +
    geom_bar(stat = "identity", fill = "#2C3E50") +
    coord_flip() +
    scale_y_continuous(labels = function(x) paste0(x, " per 100k")) +
    labs(
      title = paste("Top", n, "Countries by Average Suicide Rate"),
      subtitle = "Countries with highest average suicide rates across all years",
      x = "Country",
      y = "Average Suicide Rate"
    ) +
    theme_minimal()
}

create_generation_trends <- function(data) {
  data %>%
    group_by(generation, year) %>%
    summarise(
      avg_rate = mean(suicide_rate),
      .groups = 'drop'
    ) %>%
    ggplot(aes(x = year, y = avg_rate, color = generation)) +
    geom_line() +
    geom_point(size = 2, alpha = 0.6) +
    scale_color_viridis_d() +
    labs(
      title = "Suicide Rates Across Generations",
      subtitle = paste("Generation ranges:",
                      "G.I. Generation (1901-1927)",
                      "Silent (1928-1945)",
                      "Boomers (1946-1964)",
                      "Generation X (1965-1980)",
                      "Millennials (1981-1996)",
                      sep = "\n"),
      x = "Year",
      y = "Average Suicide Rate per 100k"
    ) +
    theme_minimal() +
    theme(legend.position = "right")
}

create_animated_gdp_plot <- function(data) {
  anim <- data %>%
    group_by(year, country) %>%
    summarise(
      suicide_rate = mean(suicide_rate),
      gdp_per_capita = mean(gdp_per_capita),
      population = sum(population),
      .groups = 'drop'
    ) %>%
    ggplot(aes(x = gdp_per_capita, y = suicide_rate, 
               size = population, color = country)) +
    geom_point(alpha = 0.7) +
    scale_x_log10(labels = scales::dollar_format()) +
    scale_size_continuous(range = c(3, 15)) +
    scale_color_viridis_d() +
    labs(
      title = 'Economic Development and Suicide Rates',
      subtitle = 'Year: {frame_time}',
      x = 'GDP per Capita (log scale)',
      y = 'Suicide Rate per 100,000 population'
    ) +
    theme_minimal() +
    transition_time(year) +
    ease_aes('linear')
    
  return(anim)
}