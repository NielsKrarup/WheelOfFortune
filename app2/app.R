runApp(shinyApp(
  ui = fluidPage(
    headerPanel("Dynamic number of plots"),
    
    mainPanel(
      actionButton("addplot", "Add plot"),
      uiOutput("plots")
    )
  ),
  server = function(input, output, session) {
    
    # A variable that keeps track of the number of plots we have
    values <- reactiveValues(
      numPlots = 1
    )
    
    # Whenever the "add plot" button is pressed, increment num plots by 1
    observeEvent(input$addplot, {
      values$numPlots <- values$numPlots + 1
    })
    
    # Dynamically generate the UI that creates all the plots
    output$plots <- renderUI({
      # Create a list of `plotOutput` objects (for each plot, use a unique ID)
      plot_output_list <- lapply(1:values$numPlots, function(i) {
        plotname <- paste("plot", i, sep="")
        plotOutput(plotname, height = 280, width = 250)
      })
      
      # Place all the plot outputs inside a shiny `tagList()`
      do.call(tagList, plot_output_list)
    })
    
    # Every time the number of plots changes (button is clicked),
    # re-generate the render functions for all the plots
    observeEvent(values$numPlots, {
      for (i in 1:values$numPlots) {
        local({
          my_i <- i
          plotname <- paste("plot", my_i, sep="")
          
          output[[plotname]] <- renderPlot({
            plot(1:my_i, 1:my_i,
                 xlim = c(1, values$numPlots),
                 ylim = c(1, values$numPlots),
                 main = paste("1:", my_i, ".  n is ", values$numPlots, sep = "")
            )
          })
        })
      }
    })
  }
))
