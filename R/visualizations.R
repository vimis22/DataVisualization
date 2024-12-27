library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(tidyr)
library(scales)
library(ggalt)

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
    pie_data <- data %>%
      group_by(sex) %>%
      summarise(suicide_rate = mean(suicide_rate)) 
    
    plot_ly(pie_data, 
            labels = ~sex, 
            values = ~suicide_rate,
            type = 'pie',
            marker = list(colors = c("#FF9DA6", "#4B9CD3"))) %>%
      layout(title = "Distribution of Suicides by Gender",
             showlegend = TRUE)
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
    pie_data <- data %>%
      group_by(age_group) %>%
      summarise(suicide_rate = mean(suicide_rate))
    
    plot_ly(pie_data, 
            labels = ~age_group, 
            values = ~suicide_rate,
            type = 'pie',
            marker = list(colors = viridis(n = length(unique(data$age_group))))) %>%
      layout(title = "Distribution of Suicides by Age Group",
             showlegend = TRUE)
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
    pie_data <- continent_data %>%
    group_by(continent) %>%
    summarise(suicide_rate = mean(suicide_rate, na.rm = FALSE))

    plot_ly(
      pie_data,
      labels = ~continent,
      values = ~suicide_rate,
      type = 'pie',
      marker = list(colors = viridis::viridis(n = length(unique(continent_data$continent))))
    ) %>%
      layout(
        title = "Distribution of Suicides by Continent",
        showlegend = TRUE
      )
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
  p <- data %>%
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

  world_data <- prepare_map_data(data)
  
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
    filter(n() >= 5) %>%
    summarize(
      n_obs = n(),
      tryCatch({
        model <- lm(suicide_rate ~ year)
        estimate <- coef(model)[2]
        p_value <- summary(model)$coefficients[2,4]
        tibble(estimate = estimate, p_value = p_value)
      }, error = function(e) {
        tibble(estimate = NA_real_, p_value = NA_real_)
      }),
      .groups = 'drop'
    ) %>%
    filter(!is.na(estimate) & !is.na(p_value)) %>%
    mutate(p_adjusted = p.adjust(p_value, method = "holm")) %>%
    filter(p_adjusted < 0.05) %>%
    arrange(estimate) %>%
    mutate(country = factor(country, levels = country))

  ggplot(country_trends, aes(x = country, y = estimate, color = estimate)) +
    geom_point(stat = 'identity', size = 4) +
    geom_hline(yintercept = 0, color = "grey", size = 1) +
    geom_segment(aes(y = 0,
                     x = country,
                     yend = estimate,
                     xend = country),
                 size = 1) +
    scale_color_gradient(low = "green", high = "red") +
    labs(
      title = "Change per year (Suicides per 100k)",
      subtitle = "Of countries with significant trends (p < 0.05)",
      x = "Country",
      y = "Change Per Year (Suicides per 100k)"
    ) +
    scale_y_continuous(
      breaks = seq(-2, 2, 0.2),
      limits = c(-1.5, 1.5)
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    ) +
    coord_flip()
}

create_gender_disparity_plot <- function(data) {
  global_average <- data %>%
    summarize(
      suicide_rate = sum(as.numeric(suicides_no)) / sum(as.numeric(population)) * 100000
    ) %>%
    pull(suicide_rate)
  
  sex_country_data <- data %>%
    group_by(country, sex) %>%
    summarize(
      suicide_per_100k = sum(as.numeric(suicides_no)) / sum(as.numeric(population)) * 100000,
      .groups = 'drop'
    )
  
  sex_country_wide <- sex_country_data %>%
    pivot_wider(
      names_from = sex,
      values_from = suicide_per_100k
    ) %>%
    mutate(diff = Male - Female) %>%
    arrange(diff)
  
  plot_ly() %>%
    add_segments(
      data = sex_country_wide,
      x = ~Female, xend = ~Male,
      y = ~country, yend = ~country,
      color = I("grey80"),
      showlegend = FALSE
    ) %>%
    add_markers(
      data = sex_country_data %>% filter(sex == "Female"),
      x = ~suicide_per_100k, y = ~country,
      name = "Female",
      marker = list(color = "#FF9DA6", size = 8)
    ) %>%
    add_markers(
      data = sex_country_data %>% filter(sex == "Male"),
      x = ~suicide_per_100k, y = ~country,
      name = "Male", 
      marker = list(color = "#4B9CD3", size = 8)
    ) %>%
    add_segments(
      x = global_average, xend = global_average,
      y = sex_country_wide$country[1], yend = tail(sex_country_wide$country, 1),
      line = list(color = "grey35", dash = "dash"),
      showlegend = FALSE,
      name = "Global Average"
    ) %>%
    layout(
      title = list(
        text = "Gender Disparity in Suicide Rates by Country",
        font = list(size = 14)
      ),
      xaxis = list(
        title = "Suicides per 100k",
        range = c(0, max(sex_country_wide$Male) * 1.1),
        tickvals = seq(0, 80, 10)
      ),
      yaxis = list(
        title = "Country",
        categoryorder = "array",
        categoryarray = sex_country_wide$country
      ),
      showlegend = TRUE,
      legend = list(
        x = 0.85,
        y = 0.2
      ),
      margin = list(l = 150)
    )
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

create_animated_violin_plot <- function(data, selected_year = NULL) {
  base_plot <- data %>%
    mutate(year = as.integer(year)) %>%
    {if (!is.null(selected_year)) filter(., year == selected_year) else .} %>%
    ggplot(aes(x = age_group, y = suicide_rate, fill = age_group)) +
    geom_violin(trim = FALSE, alpha = 0.8) +
    geom_boxplot(width = 0.2, fill = "white", alpha = 0.5, outlier.shape = NA) +
    scale_fill_viridis_d() +
    scale_y_continuous(limits = c(0, 190)) +
    labs(
      title = "Distribution of Suicide Rates by Age Group",
      subtitle = "Year: {closest_state}",
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
    
  if (is.null(selected_year)) {
    base_plot + 
      transition_states(year, transition_length = 2, state_length = 1) +
      ease_aes('linear')
  } else {
    base_plot
  }
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
    scale_x_continuous(labels = scales::dollar_format()) +
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

create_animated_gdp_plot <- function(data, selected_year = NULL) {
  base_plot <- data %>%
    mutate(year = as.integer(year)) %>%
    {if (!is.null(selected_year)) filter(., year == selected_year) else .} %>%
    ggplot(aes(x = gdp_per_capita, 
               y = suicide_rate,
               size = population,
               color = continent)) +
    geom_point(alpha = 0.6) +
    scale_size_continuous(range = c(2, 20)) +
    scale_x_continuous(labels = scales::dollar_format()) +
    scale_y_continuous(limits = c(0, 160)) +
    scale_color_viridis_d() +
    labs(
      title = "GDP per Capita vs Suicide Rate",
      subtitle = if(is.null(selected_year)) "Year: {frame_time}" else paste("Year:", selected_year),
      x = "GDP per Capita (log scale)",
      y = "Suicide Rate (per 100k population)",
      color = "Continent",
      size = "Population"
    ) +
    theme_minimal()
    
  if (is.null(selected_year)) {
    base_plot + 
      transition_time(year) +
      ease_aes('linear')
  } else {
    base_plot
  }
}