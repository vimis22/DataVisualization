library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(tidyr)
library(scales)

create_time_series <- function(data) {
  data %>% 
    group_by(year) %>%
    summarise(suicide_rate = mean(suicide_rate)) %>%
    ggplot(aes(x = year, y = suicide_rate)) +
    geom_line(color = "#2C7BB6", size = 1) +
    geom_point(color = "#2C7BB6", size = 2) +
    scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 5)) +
    scale_y_continuous(labels = function(x) paste0(round(x, 1), " per 100k")) +
    labs(
      title = "Global Suicide Rates Over Time",
      subtitle = "Average suicide rates per 100,000 population",
      x = "Year",
      y = "Suicide Rate"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
}

create_gender_overview <- function(data, plot_type = "line") {
  if(plot_type == "line") {
    data %>% 
      group_by(year, sex) %>%
      summarise(suicide_rate = mean(suicide_rate), .groups = 'drop') %>%
      ggplot(aes(x = year, y = suicide_rate, color = sex)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_color_manual(values = c("Female" = "#FF9DA6", "Male" = "#4B9CD3")) +
      scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 5)) +
      labs(
        title = "Suicide Rates by Gender Over Time",
        subtitle = "Average rates per 100,000 population",
        x = "Year",
        y = "Suicide Rate",
        color = "Gender"
      ) +
      theme_minimal()
  } else if(plot_type == "pie") {
    data %>%
      group_by(sex) %>%
      summarise(suicide_rate = mean(suicide_rate)) %>%
      ggplot(aes(x = "", y = suicide_rate, fill = sex)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("Female" = "#FF9DA6", "Male" = "#4B9CD3")) +
      labs(
        title = "Distribution of Suicides by Gender",
        fill = "Gender"
      ) +
      theme_void()
  } else {
    data %>%
      group_by(sex) %>%
      summarise(suicide_rate = mean(suicide_rate)) %>%
      ggplot(aes(x = sex, y = suicide_rate, fill = sex)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Female" = "#FF9DA6", "Male" = "#4B9CD3")) +
      labs(
        title = "Average Suicide Rates by Gender",
        x = "Gender",
        y = "Suicide Rate (per 100k population)",
        fill = "Gender"
      ) +
      theme_minimal()
  }
}

create_age_overview <- function(data, plot_type = "bar") {
  if(plot_type == "pie") {
    data %>%
      group_by(age_group) %>%
      summarise(suicide_rate = mean(suicide_rate)) %>%
      ggplot(aes(x = "", y = suicide_rate, fill = age_group)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_viridis_d() +
      labs(
        title = "Distribution of Suicides by Age Group",
        fill = "Age Group"
      ) +
      theme_void()
  } else {
    data %>%
      group_by(age_group) %>%
      summarise(suicide_rate = mean(suicide_rate)) %>%
      ggplot(aes(x = age_group, y = suicide_rate, fill = age_group)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_d() +
      labs(
        title = "Average Suicide Rates by Age Group",
        x = "Age Group",
        y = "Suicide Rate (per 100k population)",
        fill = "Age Group"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
}

create_age_time_series <- function(data) {
  data %>%
    group_by(year, age_group) %>%
    summarise(suicide_rate = mean(suicide_rate), .groups = 'drop') %>%
    ggplot(aes(x = year, y = suicide_rate, color = age_group)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_viridis_d() +
    scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 5)) +
    labs(
      title = "Suicide Rates by Age Group Over Time",
      subtitle = "Average rates per 100,000 population",
      x = "Year", 
      y = "Suicide Rate",
      color = "Age Group"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
}

create_continent_bar <- function(data, plot_type = "bar") {
  continent_data <- data %>%
    group_by(continent) %>%
    summarise(suicide_rate = mean(suicide_rate), .groups = 'drop')
  
  if(plot_type == "bar") {
    ggplot(continent_data, aes(x = reorder(continent, -suicide_rate), y = suicide_rate, fill = continent)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_d() +
      labs(
        title = "Suicide Rates by Continent",
        x = "Continent",
        y = "Suicide Rate (per 100k population)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    ggplot(continent_data, aes(x = "", y = suicide_rate, fill = continent)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_viridis_d() +
      labs(
        title = "Distribution of Suicides by Continent",
        fill = "Continent"
      ) +
      theme_void()
  }
}

create_continent_gender_bar <- function(data) {
  data %>%
    group_by(continent, sex) %>%
    summarise(suicide_rate = mean(suicide_rate), .groups = 'drop') %>%
    ggplot(aes(x = reorder(continent, -suicide_rate), y = suicide_rate, fill = sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("Female" = "#FF9DA6", "Male" = "#4B9CD3")) +
    labs(
      title = "Suicide Rates by Continent and Gender",
      x = "Continent",
      y = "Suicide Rate (per 100k population)",
      fill = "Gender"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

create_continent_age_bar <- function(data) {
  data %>%
    group_by(continent, age_group) %>%
    summarise(suicide_rate = mean(suicide_rate), .groups = 'drop') %>%
    ggplot(aes(x = reorder(continent, -suicide_rate), y = suicide_rate, fill = age_group)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_viridis_d() +
    labs(
      title = "Suicide Rates by Continent and Age Group",
      x = "Continent",
      y = "Suicide Rate (per 100k population)",
      fill = "Age Group"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

create_continent_time_series <- function(data) {
  data %>%
    group_by(year, continent) %>%
    summarise(suicide_rate = mean(suicide_rate), .groups = 'drop') %>%
    ggplot(aes(x = year, y = suicide_rate, color = continent)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_viridis_d() +
    labs(
      title = "Suicide Rates by Continent Over Time",
      x = "Year",
      y = "Suicide Rate (per 100k population)",
      color = "Continent"
    ) +
    theme_minimal()
}

create_continent_choropleth <- function(data) {
  require(maps)
  require(sf)
  
  continent_data <- data %>%
    group_by(continent) %>%
    summarise(suicide_rate = mean(suicide_rate), .groups = 'drop')
  
  world <- map_data("world")
  world$continent <- countrycode::countrycode(
    world$region,
    origin = 'country.name',
    destination = 'continent'
  )
  
  world <- left_join(world, continent_data, by = "continent")
  
  ggplot(world, aes(x = long, y = lat, group = group, fill = suicide_rate)) +
    geom_polygon(color = "white", size = 0.1) +
    coord_map(projection = "mercator") +
    scale_fill_viridis_c(name = "Suicide Rate\n(per 100k)") +
    labs(title = "Suicide Rates by Continent") +
    theme_void()
}

create_country_bar <- function(data, top_n = 20) {
  country_continent <- data %>%
    group_by(country) %>%
    slice(1) %>%
    select(country, continent)
  
  data %>%
    group_by(country) %>%
    summarise(suicide_rate = mean(suicide_rate), .groups = 'drop') %>%
    left_join(country_continent, by = "country") %>%
    top_n(top_n, suicide_rate) %>%
    ggplot(aes(x = reorder(country, suicide_rate), y = suicide_rate, fill = continent)) +
    geom_bar(stat = "identity") +
    scale_fill_viridis_d() +
    coord_flip() +
    labs(
      title = paste("Top", top_n, "Countries by Suicide Rate"),
      x = "Country",
      y = "Suicide Rate (per 100k population)",
      fill = "Continent"
    ) +
    theme_minimal()
}

create_country_gender_proportion <- function(data) {
  data %>%
    group_by(country, sex) %>%
    summarise(suicide_rate = mean(suicide_rate), .groups = 'drop') %>%
    group_by(country) %>%
    mutate(total = sum(suicide_rate),
           proportion = suicide_rate / total) %>%
    ggplot(aes(x = reorder(country, -total), y = proportion, fill = sex)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Female" = "#FF9DA6", "Male" = "#4B9CD3")) +
    coord_flip() +
    labs(
      title = "Gender Distribution of Suicides by Country",
      x = "Country",
      y = "Proportion",
      fill = "Gender"
    ) +
    theme_minimal()
}

create_country_age_proportion <- function(data) {
  data %>%
    group_by(country, age_group) %>%
    summarise(suicide_rate = mean(suicide_rate), .groups = 'drop') %>%
    group_by(country) %>%
    mutate(total = sum(suicide_rate),
           proportion = suicide_rate / total) %>%
    ggplot(aes(x = reorder(country, -total), y = proportion, fill = age_group)) +
    geom_bar(stat = "identity") +
    scale_fill_viridis_d() +
    coord_flip() +
    labs(
      title = "Age Distribution of Suicides by Country",
      x = "Country",
      y = "Proportion",
      fill = "Age Group"
    ) +
    theme_minimal()
}

create_country_choropleth <- function(data) {
  require(maps)
  
  country_data <- data %>%
    group_by(country) %>%
    summarise(suicide_rate = mean(suicide_rate), .groups = 'drop')
  
  world <- map_data("world")
  world <- left_join(world, country_data, by = c("region" = "country"))
  
  ggplot(world, aes(x = long, y = lat, group = group, fill = suicide_rate)) +
    geom_polygon(color = "white", size = 0.1) +
    coord_map(projection = "mercator") +
    scale_fill_viridis_c(name = "Suicide Rate\n(per 100k)") +
    labs(title = "Suicide Rates by Country") +
    theme_void()
}

create_country_trends <- function(data) {
  country_trends <- data %>%
    group_by(country) %>%
    summarise(
      trend = coef(lm(suicide_rate ~ year))[2],
      p_value = summary(lm(suicide_rate ~ year))$coefficients[2,4],
      .groups = 'drop'
    ) %>%
    filter(p_value < 0.05) %>%
    arrange(trend)
  
  ggplot(country_trends, aes(x = reorder(country, trend), y = trend)) +
    geom_col(aes(fill = trend > 0)) +
    scale_fill_manual(values = c("TRUE" = "#FF9DA6", "FALSE" = "#4B9CD3")) +
    coord_flip() +
    labs(
      title = "Annual Change in Suicide Rate by Country",
      subtitle = "Only showing statistically significant trends (p < 0.05)",
      x = "Country",
      y = "Annual Change in Suicide Rate"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

create_gender_disparity_plot <- function(data) {
  data %>%
    group_by(continent, country, sex) %>%
    summarise(suicide_rate = mean(suicide_rate), .groups = 'drop') %>%
    pivot_wider(names_from = sex, values_from = suicide_rate) %>%
    mutate(disparity = Male - Female) %>%
    ggplot(aes(x = disparity, y = reorder(country, disparity))) +
    geom_point(aes(color = continent), size = 3) +
    geom_segment(aes(x = 0, xend = disparity, yend = country), 
                 color = "gray50") +
    scale_color_viridis_d() +
    labs(
      title = "Gender Disparity in Suicide Rates by Country",
      subtitle = "Male rate minus Female rate",
      x = "Gender Disparity (per 100k population)",
      y = "Country",
      color = "Continent"
    ) +
    theme_minimal()
}

create_gender_time_series <- function(data) {
  p <- data %>% 
    group_by(year, sex) %>%
    summarise(suicide_rate = mean(suicide_rate), .groups = 'drop') %>%
    ggplot(aes(x = year, y = suicide_rate, color = sex)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = c("Female" = "#FF9DA6", "Male" = "#4B9CD3")) +
    labs(
      title = "Gender-Specific Suicide Rates Over Time",
      subtitle = "Annual average rates by gender",
      x = "Year",
      y = "Suicide Rate (per 100k population)",
      color = "Gender"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
    
  return(p)
}

create_gender_disparity_evolution <- function(data) {
  data %>%
    group_by(year, sex) %>%
    summarise(suicide_rate = mean(suicide_rate), .groups = 'drop') %>%
    pivot_wider(names_from = sex, values_from = suicide_rate) %>%
    mutate(ratio = Male / Female) %>%
    ggplot(aes(x = year, y = ratio)) +
    geom_line(color = "#2C7BB6", size = 1) +
    geom_point(color = "#2C7BB6", size = 2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    labs(
      title = "Evolution of Gender Disparity in Suicide Rates",
      subtitle = "Male-to-Female suicide rate ratio",
      x = "Year",
      y = "Male-to-Female Ratio"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
}

create_age_violin_plot <- function(data) {
  ggplot(data, aes(x = age_group, y = suicide_rate, fill = age_group)) +
    geom_violin(trim = FALSE) +
    geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
    scale_fill_viridis_d() +
    labs(
      title = "Distribution of Suicide Rates by Age Group",
      subtitle = "Violin plot with embedded box plots",
      x = "Age Group",
      y = "Suicide Rate (per 100k population)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
}

create_animated_age_trends <- function(data) {
  p <- data %>%
    group_by(year, age_group) %>%
    summarise(
      suicide_rate = mean(suicide_rate),
      .groups = 'drop'
    ) %>%
    ggplot(aes(x = age_group, y = suicide_rate, fill = age_group)) +
    geom_col() +
    scale_fill_viridis_d() +
    labs(
      title = "Suicide Rates by Age Group",
      subtitle = "Year: {frame_time}",
      x = "Age Group",
      y = "Suicide Rate (per 100k population)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    transition_time(year) +
    ease_aes('linear')
    
  return(p)
}

create_age_heatmap <- function(data) {
  data %>%
    group_by(year, age_group) %>%
    summarise(suicide_rate = mean(suicide_rate), .groups = 'drop') %>%
    ggplot(aes(x = year, y = age_group, fill = suicide_rate)) +
    geom_tile() +
    scale_fill_viridis_c(name = "Suicide Rate\n(per 100k)") +
    labs(
      title = "Evolution of Age-Specific Suicide Rates",
      subtitle = "Heatmap showing temporal patterns by age group",
      x = "Year",
      y = "Age Group"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
}

create_age_gender_interaction <- function(data) {
  data %>%
    group_by(age_group, sex) %>%
    summarise(suicide_rate = mean(suicide_rate), .groups = 'drop') %>%
    ggplot(aes(x = age_group, y = suicide_rate, fill = sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("Female" = "#FF9DA6", "Male" = "#4B9CD3")) +
    labs(
      title = "Interaction between Age and Gender in Suicide Rates",
      subtitle = "Comparing gender differences across age groups",
      x = "Age Group",
      y = "Suicide Rate (per 100k population)",
      fill = "Gender"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
}

create_gdp_suicide_scatter <- function(data) {
  country_data <- data %>%
    group_by(country, continent) %>%
    summarise(
      suicide_rate = mean(suicide_rate),
      gdp_per_capita = mean(gdp_per_capita),
      population = mean(population),
      .groups = 'drop'
    )
  
  ggplot(country_data, 
         aes(x = gdp_per_capita, 
             y = suicide_rate, 
             size = population,
             color = continent)) +
    geom_point(alpha = 0.6) +
    scale_size_continuous(
      range = c(2, 20),
      labels = scales::comma,
      guide = guide_legend(title = "Population")
    ) +
    scale_x_log10(labels = scales::dollar_format()) +
    scale_color_viridis_d() +
    geom_smooth(aes(group = 1), 
                method = "lm", 
                color = "black", 
                size = 0.5,
                se = TRUE) +
    labs(
      title = "Relationship between GDP per Capita and Suicide Rate",
      subtitle = "Bubble size represents population, color represents continent",
      x = "GDP per Capita (log scale)",
      y = "Suicide Rate (per 100k population)",
      color = "Continent"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
}

create_animated_gdp_plot <- function(data) {
  p <- data %>%
    group_by(year, country, continent) %>%
    summarise(
      suicide_rate = mean(suicide_rate),
      gdp_per_capita = mean(gdp_per_capita),
      population = mean(population),
      .groups = 'drop'
    ) %>%
    ggplot(aes(x = gdp_per_capita, 
               y = suicide_rate,
               size = population,
               color = continent)) +
    geom_point(alpha = 0.6) +
    scale_size_continuous(range = c(2, 20)) +
    scale_x_log10(labels = scales::dollar_format()) +
    scale_color_viridis_d() +
    labs(
      title = "GDP per Capita vs Suicide Rate",
      subtitle = "Year: {frame_time}",
      x = "GDP per Capita (log scale)",
      y = "Suicide Rate (per 100k population)",
      color = "Continent",
      size = "Population"
    ) +
    theme_minimal() +
    transition_time(year) +
    ease_aes('linear')
  
  return(p)
}

create_gdp_correlation_analysis <- function(data) {
  correlations <- data %>%
    group_by(continent) %>%
    summarise(
      correlation = cor(gdp_per_capita, suicide_rate),
      .groups = 'drop'
    )
  
  ggplot(data, aes(x = gdp_per_capita, y = suicide_rate)) +
    geom_point(aes(color = continent), alpha = 0.6) +
    geom_smooth(aes(color = continent), method = "lm", se = TRUE) +
    scale_x_log10(labels = scales::dollar_format()) +
    scale_color_viridis_d() +
    facet_wrap(~continent) +
    labs(
      title = "GDP-Suicide Rate Correlation by Continent",
      subtitle = "Separate trend lines showing regional patterns",
      x = "GDP per Capita (log scale)",
      y = "Suicide Rate (per 100k population)",
      color = "Continent"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
}

create_gdp_quantile_analysis <- function(data) {
  data %>%
    mutate(gdp_quantile = cut(gdp_per_capita, 
                             breaks = quantile(gdp_per_capita, probs = seq(0, 1, 0.2)),
                             labels = c("Lowest 20%", "20-40%", "40-60%", "60-80%", "Top 20%"),
                             include.lowest = TRUE)) %>%
    ggplot(aes(x = gdp_quantile, y = suicide_rate, fill = gdp_quantile)) +
    geom_violin(trim = FALSE) +
    geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
    scale_fill_viridis_d() +
    labs(
      title = "Distribution of Suicide Rates by GDP Quantile",
      subtitle = "Showing relationship between economic development and suicide rates",
      x = "GDP per Capita Quantile",
      y = "Suicide Rate (per 100k population)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
}

create_economic_trajectory_plot <- function(data) {
  data %>%
    group_by(country, year) %>%
    summarise(
      gdp_per_capita = mean(gdp_per_capita),
      suicide_rate = mean(suicide_rate),
      .groups = 'drop'
    ) %>%
    ggplot(aes(x = gdp_per_capita, y = suicide_rate, group = country)) +
    geom_path(aes(color = country), alpha = 0.3) +
    geom_point(aes(color = country), size = 1) +
    scale_x_log10(labels = scales::dollar_format()) +
    labs(
      title = "Economic Development Trajectories and Suicide Rates",
      subtitle = "Path of each country's development and corresponding suicide rates",
      x = "GDP per Capita (log scale)",
      y = "Suicide Rate (per 100k population)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
}