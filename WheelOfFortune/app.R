#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
    navbarPage(title = "NavBarPage_",
      tabPanel(title = "tabPanel1",
               numericInput(inputId = "n_q", 
                            label = "Number of questions / phrases", 
                            value = 1, min = 1, step = 1, 
                            width = "100px"),
               uiOutput("ui_phrases")
               ),
      tabPanel(title = "tabPanel2",
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        uiOutput("ui_cur_phrase"),
        textInput(inputId = "cur_guess", label = "Letter Guess", placeholder = "input a letter"),
        actionButton(inputId = "submit", label = "Submit!")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        fluidRow(
          #
          
        ),
        fluidRow(
          plotOutput("wrong_let_plot")
        )

      )
   )
)
)
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  rv <- reactiveValues()
  rv$df_guesses <- data.frame(let = character(),
                              phrase = character(),
                              status = character())
  
  #dynamically generated phrases for input
  output$ui_phrases <- renderUI({
    numPhrases <- as.integer(input$n_q)
    lapply(1:numPhrases, function(i) {
      textInput(inputId = paste0("phrase",i),label = paste("Phrase", i), value = NULL )
    })
  })
  
  #Select the current phrase to guess for
  output$ui_cur_phrase <- renderUI({
    req(input$n_q)
    selectInput(inputId = "cur_phrase",
                label   = "Phrase Guess:",choices = 1:as.integer(input$n_q))
  })
  
  observeEvent(input$submit,{
    #output spots of correct letter
    #add letter
    rv$df_guesses
  })
   
   output$wrong_let_plot <- renderPlot({
      # generate bins based on input$bins from ui.R
      plot(rnorm(1:10))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

