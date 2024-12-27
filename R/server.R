server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$yearRange)
    data %>%
      filter(
        year >= input$yearRange[1],
        year <= input$yearRange[2]
      ) %>%
      {if (!is.null(input$countries) && length(input$countries) > 0)
        filter(., country %in% input$countries)
        else .} %>%
      {if (!is.null(input$continents) && length(input$continents) > 0)
        filter(., continent %in% input$continents)
        else .} %>%
      {if (!is.null(input$ageGroups) && length(input$ageGroups) > 0)
        filter(., age_group %in% input$ageGroups)
        else .} %>%
      {if (!is.null(input$gender) && length(input$gender) > 0)
        filter(., sex %in% input$gender)
        else .} %>%
      {if (!is.null(input$gdpRange))
        filter(., gdp_per_capita >= input$gdpRange[1], 
               gdp_per_capita <= input$gdpRange[2])
        else .}
  })

  rv <- reactiveValues(
    animating = FALSE,
    timer = NULL
  )
  
  observe({
    if (rv$animating) {
      rv$timer <- invalidateLater(100)
      isolate({
        newYear <- input$violinYearSlider + 1
        if (newYear > 2015) {
          newYear <- 1985
        }
        updateSliderInput(session, "violinYearSlider", value = newYear)
      })
    }
  })
  
  observeEvent(input$playPauseViolin, {
    rv$animating <- !rv$animating
    if (rv$animating) {
      updateActionButton(session, "playPauseViolin",
                        icon = icon("pause"))
    } else {
      updateActionButton(session, "playPauseViolin",
                        icon = icon("play"))
    }
  })
  
  observeEvent(input$resetViolin, {
    rv$animating <- FALSE
    updateActionButton(session, "playPauseViolin",
                      icon = icon("play"))
    updateSliderInput(session, "violinYearSlider",
                     value = 1985)
  })

  output$ageViolinPlotAnimated <- renderPlotly({
    year_data <- filtered_data() %>%
      filter(year == input$violinYearSlider)
    
    p <- create_animated_violin_plot(year_data, input$violinYearSlider)
    
    ggplotly(p) %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 50, r = 50, b = 100, t = 50)
      )
  })

  output$timeSeriesPlot <- renderPlotly({
    p <- create_time_series(filtered_data())
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h", y = -0.2))
  })

  output$genderPlot <- renderPlotly({
    p <- create_gender_overview(filtered_data(), input$genderPlotType)
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h", y = -0.2))
  })

  output$agePlot <- renderPlotly({
    p <- create_age_overview(filtered_data(), input$agePlotType)
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h", y = -0.2))
  })

  output$ageTimeSeries <- renderPlotly({
    p <- create_age_time_series(filtered_data())
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h", y = -0.2))
  })

  output$genderTimeSeriesOverview <- renderPlotly({
    p <- create_gender_overview(filtered_data(), input$genderTimeSeriesOverviewPlotType)
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h", y = -0.2))
  })

  output$continentBarPlot <- renderPlotly({
    p <- create_continent_bar(filtered_data(), input$continentPlotType)
    ggplotly(p)
  })

  output$continentGenderPlot <- renderPlotly({
    p <- create_continent_gender_bar(filtered_data())
    ggplotly(p)
  })

  output$continentAgePlot <- renderPlotly({
    p <- create_continent_age_bar(filtered_data())
    ggplotly(p)
  })

  output$continentTimeSeries <- renderPlotly({
    p <- create_continent_time_series(filtered_data())
    ggplotly(p)
  })

  output$continentMap <- renderPlotly({
    p <- create_continent_choropleth(filtered_data())
    ggplotly(p)
  })

  output$countryBarPlot <- renderPlotly({
    p <- create_country_bar(filtered_data(), input$topN)
    ggplotly(p)
  })

  output$countryGenderPlot <- renderPlotly({
    p <- create_country_gender_proportion(filtered_data())
    ggplotly(p)
  })

  output$countryAgePlot <- renderPlotly({
    p <- create_country_age_proportion(filtered_data())
    ggplotly(p)
  })

  output$countryMap <- renderPlotly({
    p <- create_country_choropleth(filtered_data())
    ggplotly(p)
  })

  output$countryTrends <- renderPlotly({
    p <- create_country_trends(filtered_data())
    ggplotly(p)
  })

  output$genderDisparityPlot <- renderPlotly({
    p <- create_gender_disparity_plot(filtered_data())
    ggplotly(p)
  })

  output$genderTimeSeries <- renderPlotly({
    p <- create_gender_time_series(filtered_data())
    ggplotly(p)
  })

  output$genderDisparityEvolution <- renderPlotly({
    p <- create_gender_disparity_evolution(filtered_data())
    ggplotly(p)
  })

  output$ageViolinPlot <- renderPlotly({
    p <- create_age_violin_plot(filtered_data())
    ggplotly(p)
  })

  output$animatedAgeTrends <- renderImage({
    outfile <- tempfile(fileext = '.gif')
    
    anim <- create_animated_age_trends(filtered_data())
    anim_save(outfile, animation = anim)
    
    list(src = outfile,
         contentType = 'image/gif',
         width = 800,
         height = 400,
         alt = "Animated age trends")
  }, deleteFile = TRUE)

  output$ageHeatmap <- renderPlotly({
    p <- create_age_heatmap(filtered_data())
    ggplotly(p)
  })

  output$gdpScatterPlot <- renderPlotly({
    p <- create_gdp_suicide_scatter(filtered_data())
    ggplotly(p)
  })

  output$animatedGdpPlot <- renderImage({
    outfile <- tempfile(fileext = '.gif')
    
    anim <- create_animated_gdp_plot(filtered_data())
    anim_save(outfile, animation = anim)
    
    list(src = outfile,
         contentType = 'image/gif',
         width = 800,
         height = 400,
         alt = "Animated GDP plot")
  }, deleteFile = TRUE)

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("suicide_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )

  observe({
    ranges <- get_data_ranges(data)
    
    updateSelectInput(session, "countries",
                     choices = ranges$countries)
    updateSelectInput(session, "continents",
                     choices = ranges$continents)
    updateSelectInput(session, "ageGroups",
                     choices = ranges$age_groups)
    updateSliderInput(session, "gdpRange",
                     min = floor(ranges$gdp_range[1]),
                     max = ceiling(ranges$gdp_range[2]))
  })
}