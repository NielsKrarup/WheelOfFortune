if (interactive()) {
  library(shiny)
  
  shinyApp(
    ui = fluidPage(
      useShinyjs(),  # Set up shinyjs
      actionButton("btn", "Click me"),
      textInput("element", "Watch what happens to me")
    ),
    server = function(input, output) {
      observeEvent(input$btn, {
        # Change the following line for more examples
        toggleState("element")
      })
    }
  )
}
