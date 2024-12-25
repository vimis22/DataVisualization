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
      {if (!is.null(input$ageGroups) && length(input$ageGroups) > 0)
        filter(., age_group %in% input$ageGroups)
        else .}
  })
  
  observe({
    updateSelectInput(session, "countries",
                     choices = sort(unique(data$country)),
                     selected = NULL)
    updateSelectInput(session, "ageGroups",
                     choices = sort(unique(data$age_group)),
                     selected = NULL)
  })
  
  output$timeSeriesPlot <- renderPlotly({
    p <- create_time_series(filtered_data())
    ggplotly(p)
  })
  
  output$topCountriesPlot <- renderPlotly({
    p <- create_top_countries(filtered_data())
    ggplotly(p)
  })

  output$ageDistributionPlot <- renderPlotly({
    p <- create_age_boxplot(filtered_data())
    ggplotly(p)
  })
  
  output$generationTrends <- renderPlotly({
    p <- create_generation_trends(filtered_data())
    ggplotly(p)
  })
  
  output$gdpScatter <- renderPlotly({
    p <- create_gdp_scatter(filtered_data())
    ggplotly(p)
  })
  
  output$gdpAnimation <- renderImage({

    anim <- create_animated_gdp_plot(filtered_data())
    

    outfile <- tempfile(fileext = '.gif')
    anim_save(outfile, animation = anim)
    
    list(src = outfile,
         contentType = 'image/gif',
         width = 800,
         height = 600,
         alt = "Animated GDP plot")
  }, deleteFile = TRUE)
}