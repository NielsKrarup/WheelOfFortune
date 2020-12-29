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
library(dplyr)

source(file = "planb.R")

LET_DK <- c(LETTERS, "Æ", "Ø", "Å")



# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
    navbarPage(title = "NavBarPage_",selected = "tabPanel1",
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
        #uiOutput("ui_submit"),
        actionButton(inputId = "submit", label = "Submit!"),
        uiOutput("ui_test")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        fluidRow(
          #
          uiOutput("see_phrase"),
          plotOutput("announce_correct"),
          uiOutput("announce_wrong")
        ),
        fluidRow(
          plotOutput("plot_overview")
        )

      )
   )
)
)
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  rv <- reactiveValues()
  rv$cnt <- 1
  rv$df_guesses <- data.frame(let = character(),
                              phrase = character(),
                              status = character())
  #base plot for announcing 
  rv$p <- ggplot(data = data.frame())
  

# Dynamic UI --------------------------------------------------------------
  #requierement for submit button
  output$ui_test <- renderUI({
    validate(
      need(nchar(input$cur_guess_let) == 1, "Select Only one letter"),
      need(input$cur_guess_let != "K", "already tried!")
    )
    NULL
  })
  
  #dynamically generated phrases for input
  output$ui_phrases <- renderUI({
    numPhrases <- as.integer(input$n_q)
    lapply(1:numPhrases, function(i) {
      tagList(
      textInput(inputId = paste0("phrase",i),label = paste("Phrase", i), value = "Køb en kat" ),
      textInput(inputId = paste0("phrase_hint",i), label = paste("Phrase", i, "hint"), placeholder = "Optional hint for phrase" )
      )
    })
  })
  
  #Select the current phrase to guess for
  output$ui_cur_phrase <- renderUI({
    req(input$n_q)
    selectInput(inputId = "cur_phrase",
                label   = "Phrase to guess for:",choices = 1:as.integer(input$n_q))
  })
  
  #See the selected phrase
  output$see_phrase <- renderUI({
    input$cur_guess_let
    h1("Hint: ", paste(input[[paste0("phrase_hint",input$cur_phrase)]]))
  })
  

  # Set Overview table for plotting Wrong/Right letters for each phrase -----------------------------------------------------------
  #needs number of phrases set
    observe({

      x <- (1:length(LET_DK)) %% 6
      x[x == 0] <- 6
      x
      y <- (0:(length(LET_DK)-1)) %/% 6
      y <- 6 - y
      y
      
      df <- data.frame(x = x, 
                       y = y, 
                       let = LET_DK, 
                       col = factor("grey", levels = c("grey", "green", "red")), 
                       alpha = 0.5)
      #cartesian product
      rv$overview_df_plot <- merge(tibble(id = 1:input$n_q), y = df)
        
    })
  
    # On Submission -----------------------------------------------------------
    
    observeEvent(input$submit,{
      
    #add letter
    print("looking in submit")
    phrase_tmp <- input[[paste0("phrase",input$cur_phrase)]]
    letter_tmp <- toupper(input$cur_guess_let)
    
    
    
    #Check letter guess
    obj <-  letter_checker(phrase = phrase_tmp, letter = letter_tmp)
    print(obj)
    
    #if correct
    n_occur <- length(obj$occurrences)
    rv$n_occ <- n_occur
    
    #set label as either the slots/placements or a "WRONG"
    if(obj$correct_bool){
      label <- as.character(obj$occurrences)
      #update overview table
      print("inside correct ifelse")
      rv$overview_df_plot[rv$overview_df_plot$id == input$cur_phrase & rv$overview_df_plot$let == letter_tmp, "col"] <- "green"
      rv$overview_df_plot[rv$overview_df_plot$id == input$cur_phrase & rv$overview_df_plot$let == letter_tmp, "alpha"] <- 1
      
    }else{
      label <- paste(letter_tmp, "is Wrong!")
      rv$overview_df_plot[rv$overview_df_plot$id == input$cur_phrase & rv$overview_df_plot$let == letter_tmp, "col"] <- "red"
      rv$overview_df_plot[rv$overview_df_plot$id == input$cur_phrase & rv$overview_df_plot$let == letter_tmp, "alpha"] <- 1
      
    }

    
    rv$p_layer_data <- data.frame(x = 1:n_occur, y = 0, label = label)
    
    rv$df_guesses <- rbind(rv$df_guesses,
                           data.frame(
                                let = input$cur_guess_let,
                                phrase = input$cur_phrase,
                                status = obj$correct_bool
                                )
                           )
    rv$cnt <- 1
    #conditional announce type, plot or text 


    shinyjs::reset("cur_guess_let")
    

    
  })
  
#Announcer
  output$announce_correct <- renderPlot({
    req(input$submit)
    
   if(isolate(rv$cnt < rv$n_occ)) invalidateLater(1000)

        p <- ggplot(data = subset(rv$p_layer_data, x <= isolate(rv$cnt)), aes(x = x, y = y, label = label)) +
              geom_text(size = 50) + xlim(c(0,(rv$n_occ + 1))) + theme_void()
        
        #increase counter
        isolate({rv$cnt <- rv$cnt + 1})
        p
  })
  
#Plot overview
  output$plot_overview <- renderPlot({
    req(input$submit)
    print(rv$overview_df_plot)
    #Overview plot
    ggplot(data = subset(rv$overview_df_plot, id == as.numeric(input$cur_phrase)), aes(x=x, y=y, label = let, col = col)) + 
      geom_text(alpha = subset(rv$overview_df_plot, id == as.numeric(input$cur_phrase))$alpha, size = 10) + 
      scale_color_manual(values = c("grey" = "grey", "green" = "green", "red" = "red")) + 
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
      
    
    
  })
  

  
  observe({

    if(!is.character(input$cur_guess_let) | nchar(input$cur_guess_let) != 1){

      shinyjs::disable(id = "submit")

    }else{

      shinyjs::enable(id = "submit")
    }
  })

   

} #server end

# Run the application 
shinyApp(ui = ui, server = server)

