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

input <- list()
output <- list()
rv <- list()

input$phrase1 <- "HEJ Børn"

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
        textInput(inputId = "cur_guess_let", label = "Letter Guess", placeholder = "input a letter"),
        uiOutput("ui_submit")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        fluidRow(
          #
          h1(paste(input$phrase1)),
          plotOutput("announce_plot")
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
  #base plot for announcing 
  rv$p <- ggplot(data = data.frame())
  
  #Dynamic submit-button. 
  output$ui_submit <- renderUI({
    #req(input$cur_guess_let)
    validate(
      need(nchar(input$cur_guess_let) == 1, "Select Only one letter"),
      need(input$cur_guess_let != "K", "already tried!")
    )

    actionButton(inputId = "submit", label = "Submit!")
    
  })
  
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
  
  #################################  On submission
  observeEvent(input$submit,{
    #add letter
    rv$p_layer <- geom_label(data = data.frame(x = 0, y = 0, label = input$cur_guess_let),
                             aes(x = x, y = y, label = label))
    
    print(rv$df_guesses)
    rv$df_guesses <- rbind(rv$df_guesses,
                           data.frame(
                                let = input$cur_guess_let,
                                phrase = input$cur_phrase,
                                status = 1
                                )
                           )
    print(rv$df_guesses)                           
    shinyjs::reset("cur_guess_let")
  })
  
  observe({
    print(!is.character(input$cur_guess_let) | nchar(input$cur_guess_let) != 1)
    if(!is.character(input$cur_guess_let) | nchar(input$cur_guess_let) != 1){
      shinyjs::disable(id = "submit")
    }else{
      shinyjs::enable(id = "submit")
    } 
    
  })
   
   output$wrong_let_plot <- renderPlot({
      # generate bins based on input$bins from ui.R
      plot(rnorm(1:10))
   })
   
   output$announce_plot <- renderPlot({
     #
     rv$p + rv$p_layer
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

