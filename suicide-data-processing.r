# Required libraries
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

process_suicide_data <- function(file_path) {
  df <- read.csv(file_path) %>%
    mutate(
      year = as.numeric(year),
      suicide_rate = suicides_no / population * 100000,
      age_group = factor(age,
                         levels = c("5-14 years", "15-24 years", "25-34 years",
                                    "35-54 years", "55-74 years", "75+ years"),
                         ordered = TRUE
      ),
      gdp_per_capita.... = as.numeric(gsub(",", "", gdp_per_capita....)),
      generation = factor(generation)
    ) %>%
    filter(suicide_rate >= 0,
           population > 0,
           !is.na(suicide_rate),
           !is.na(gdp_per_capita....))
  
  return(df)
}

# 1. Time series visualization
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
      subtitle = "Average suicides per 100,000 population",
      x = "Year",
      y = "Suicide Rate",
      color = "Gender"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "top"
    )
}

# 2. Age group visualization
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

# 3. GDP scatter plot
create_gdp_scatter <- function(data) {
  # Sample data if too large
  if(nrow(data) > 10000) {
    data <- data %>% sample_n(10000)
  }
  
  ggplot(data, aes(x = gdp_per_capita...., y = suicide_rate, color = sex)) +
    geom_point(alpha = 0.4, size = 2) +
    geom_smooth(method = "loess", se = TRUE, color = "black", size = 0.5) +
    scale_x_log10(labels = scales::dollar_format()) +
    scale_color_manual(values = c("female" = "#2C7BB6", "male" = "#D7191C")) +
    labs(
      title = "GDP per Capita vs Suicide Rate",
      x = "GDP per Capita (log scale)",
      y = "Suicides per 100,000 population",
      color = "Gender"
    ) +
    theme_minimal()
}

# 4. Top countries visualization
create_top_countries <- function(data, n = 10) {
  data %>%
    group_by(country) %>%
    summarise(
      avg_rate = mean(suicide_rate),
      .groups = 'drop'
    ) %>%
    arrange(desc(avg_rate)) %>%
    head(n) %>%
    ggplot(aes(x = reorder(country, avg_rate), y = avg_rate)) +
    geom_bar(stat = "identity", fill = "#2C3E50") +
    coord_flip() +
    scale_y_continuous(labels = function(x) paste0(x, " per 100k")) +
    labs(
      title = paste("Top", n, "Countries by Average Suicide Rate"),
      x = "Country",
      y = "Average Suicide Rate"
    ) +
    theme_minimal()
}

# 5. Year-over-year change visualization
create_yearly_change <- function(data) {
  yearly_data <- data %>%
    group_by(year) %>%
    summarise(avg_rate = mean(suicide_rate), .groups = 'drop') %>%
    arrange(year) %>%
    mutate(
      change = ((avg_rate - lag(avg_rate)) / lag(avg_rate)) * 100,
      change_type = ifelse(change >= 0, "Increase", "Decrease")
    ) %>%
    filter(!is.na(change))
  
  ggplot(yearly_data, aes(x = year, y = change, fill = change_type)) +
    geom_col() +
    scale_fill_manual(values = c("Increase" = "#D7191C", "Decrease" = "#2C7BB6")) +
    labs(
      title = "Year-over-Year Change in Suicide Rates",
      x = "Year",
      y = "Percent Change (%)"
    ) +
    theme_minimal()
}

# 6. Suicide rates by generation over time
create_animated_plot <- renderPlotly({
  anim_data <- filtered_data() %>%
    group_by(year, country) %>%
    summarise(
      suicide_rate = mean(suicide_rate),
      gdp_per_capita.... = mean(gdp_per_capita....),
      total_suicides = sum(suicides_no),
      .groups = 'drop'
    ) %>%
    filter(country %in% (. %>% 
                         group_by(country) %>% 
                         summarise(total = sum(total_suicides)) %>% 
                         arrange(desc(total)) %>% 
                         head(10) %>% 
                         pull(country)))
  
  p <- ggplot(anim_data, 
              aes(x = gdp_per_capita...., 
                  y = suicide_rate,
                  size = total_suicides,
                  color = country)) +
    geom_point(alpha = 0.7) +
    scale_x_log10(labels = scales::dollar_format()) +
    scale_size_continuous(range = c(3, 15)) +
    scale_color_viridis_d() +
    labs(
      title = 'Suicide Rates and GDP Over Time',
      subtitle = 'Year: {frame_time}',
      x = 'GDP per Capita (log scale)',
      y = 'Suicide Rate per 100,000 population'
    ) +
    theme_minimal()
  
  ggplotly(p) %>% 
    animation_opts(frame = 100, transition = 0)
})

# 7. AI-generated analysis plot
create_ai_visualization <- function(data) {
  data %>%
    group_by(year, country) %>%
    summarise(
      suicide_rate = mean(suicide_rate),
      gdp_per_capita.... = mean(gdp_per_capita....),
      population = sum(population),
      .groups = 'drop'
    ) %>%
    ggplot(aes(x = gdp_per_capita...., y = suicide_rate)) +
    geom_point(aes(size = population, color = year), alpha = 0.6) +
    geom_smooth(method = "loess", color = "red", se = TRUE) +
    scale_x_log10(labels = scales::dollar_format()) +
    scale_size_continuous(range = c(2, 10), labels = scales::comma) +
    scale_color_viridis_c() +
    labs(
      title = "AI Analysis: Economic Development and Suicide Rates",
      subtitle = "Bubble size represents population, color indicates year",
      x = "GDP per Capita (log scale)",
      y = "Suicide Rate per 100,000 population"
    ) +
    theme_minimal()
}

# Load and process data
df <- process_suicide_data("data.csv")

# Create all visualizations
p1 <- create_time_series(df)
p2 <- create_age_boxplot(df)
p3 <- create_gdp_scatter(df)
p4 <- create_top_countries(df)
p5 <- create_yearly_change(df)
#p6 <- create_animated_plot(df)
p7 <- create_ai_visualization(df)

# Convert to interactive plots
p1_interactive <- ggplotly(p1)
p2_interactive <- ggplotly(p2)
p3_interactive <- ggplotly(p3)
p4_interactive <- ggplotly(p4)
p5_interactive <- ggplotly(p5)

# Display plots
print(p1_interactive)
print(p2_interactive)
print(p3_interactive)
print(p4_interactive)
print(p5_interactive)

print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
#print(p6)
print(p7)

# Save animation
anim_save("suicide_animation.gif", animation = p6)

# Save plots
ggsave("time_series.png", p1, width = 12, height = 6)
ggsave("age_groups.png", p2, width = 12, height = 6)
ggsave("gdp_scatter.png", p3, width = 12, height = 6)
ggsave("top_countries.png", p4, width = 12, height = 6)
ggsave("yearly_change.png", p5, width = 12, height = 6)
ggsave("ai_analysis.png", p7, width = 12, height = 6)
