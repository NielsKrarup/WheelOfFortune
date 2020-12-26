if (interactive()) {
  
  ui <- fluidPage(
    sliderInput("n", "Number of observations", 2, 1000, 500),
    plotOutput("plot")
  )
  
  server <- function(input, output, session) {
    
    observe({
      # Re-execute this reactive expression after 1000 milliseconds
      # Do something each time this is invalidated.
      # The isolate() makes this observer _not_ get invalidated and re-executed
      # when input$n changes.
      print(paste("The value of input$n is", isolate(input$n)))
    })
    
    rv <- reactiveValues(n = 1)
    
    # Generate a new histogram at timed intervals, but not when
    # input$n changes.
    output$plot <- renderPlot({
      if(rv$n < 5) invalidateLater(1000)
        # Re-execute this reactive expression after 2000 milliseconds
        hist(rnorm(input$n), main = paste("hej:", isolate(rv$n)))
        isolate({rv$n <- rv$n + 1 })
        print(rv$n)


    })
  }
  
  shinyApp(ui, server)
}