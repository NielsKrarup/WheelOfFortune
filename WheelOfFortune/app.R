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
library(beepr)

source(file = "planb.R")

LET_DK <- c(LETTERS, "Æ", "Ø", "Å")



# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  navbarPage(
    title = "Wheel of Fortune!",
    selected = "Setup",
    tabPanel(
      title = "Setup",
      # Application title
      uiOutput("appTitleUI"),
      br(),
      column(
        width = 3,
        numericInput(
          inputId = "n_q",
          label = "Number of questions / phrases",
          value = 1,
          min = 1,
          step = 1,
          width = "100px"
        ),
        uiOutput("ui_phrases"),
        #text size
        numericInput(
          inputId = "aes_size",
          label = "Size of letters",
          value = 20,
          min = 1,
          step = 1,
          width = "100px"
        ),
        #width str
        numericInput(
          inputId = "aes_str_wrap",
          label = "Line width",
          value = 40,
          min = 1,
          step = 1,
          width = "100px"
        )
      ),
      column(7,
             imageOutput("image2")),
      column(
        2,
        p("LA Ring"),
        p('Sommerdag ved Roskilde Fjord (1900)'),
        p('Randers Kunstmuseum')
      )
    ),
    
    
    tabPanel(title = "GUESS! on screen",
             
             
             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                 uiOutput("ui_cur_phrase"),
                 textInput(
                   inputId = "cur_guess_let",
                   label = "Letter Guess",
                   placeholder = "input a letter"
                 ),
                 #uiOutput("ui_submit"),
                 actionButton(inputId = "submit", label = "Submit!"),
                 uiOutput("ui_test")
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 fluidRow(
                   #
                   #shiny::verbatimTextOutput("look"),
                   uiOutput("see_phrase"),
                   plotOutput("announce_correct"),
                   uiOutput("announce_wrong")
                 ),
                 fluidRow(plotOutput("plot_overview"))
                 
               )
             ))
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  # Reactive values ---------------------------------------------------------
  
  
  rv <- reactiveValues()
  #counter
  rv$cnt <- 1
  #list with phrases
  rv$list_phrases <- list()
  
  #(blank_sentence <- gsub(pattern = "[a-z]|[A-Z]", replacement = "_", x = sentence))
  
  observe({
    req(input$n_q)
    lapply(
      1:input$n_q, 
      FUN = function(i){
        rv[[paste0("plot_phrase",i)]] <- gsub(pattern = "[a-z]|[A-Z]|[øæå]|[ØÆÅ]", 
                                              replacement = "-", 
                                              x = input[[paste0("phrase", i)]] )
      } 
      )
  })
  
  #base plot for announcing
  rv$p <- ggplot(data = data.frame())
  
  
  # Pic ---------------------------------------------------------------------
  
  
  # pics
  output$image2 <- renderImage({
    #invalidate after 1 sec
    invalidateLater(1000)
    
    sec <- as.numeric(format(Sys.time(), format = "%S")) %% 10
    sample <- as.logical(runif(1) < 0.75)
    
    if (sec %in% c(8,9) && sample == 0) {
      return(
        list(
          src = "www/Hansen.jpg",
          contentType = "image/jpeg",
          height = "900",
          alt = "Face",
          style = "display: block; margin-left: auto; margin-right: auto;"
        )
      )
    } else {
      return(
        list(
          src = "www/LARing.jpg",
          width = "800",
          filetype = "image/jpeg",
          alt = "This is a chainring",
          style = "display: block; margin-left: auto; margin-right: auto;"
        )
      )
    }
    
  }, deleteFile = FALSE)
  
  
  
  # Dynamic UI --------------------------------------------------------------
  # Application title, with clock
  output$appTitleUI <- renderUI({
    invalidateLater(1000)
    h1(paste0(
      "EMIL HANSEN 30 SVIN - ",
      format(as.Date(Sys.Date(), format = "%d/%m/%Y"), "%Y"),
      "  -   UCT+1:   ",
      format(Sys.time(), format = "%H:%M:%S")
    ))
  })
  
  
  #requierement for submit button
  output$ui_test <- renderUI({
    validate(need(nchar(input$cur_guess_let) == 1, "Select Only one letter"))
    NULL
  })
  
  #dynamically generated phrases for input
  output$ui_phrases <- renderUI({
    numPhrases <- as.integer(input$n_q)
    
    lapply(1:numPhrases, function(i) {
      tagList(
        textInput(
          inputId = paste0("phrase", i),
          label = paste("Phrase", i),
          value = "Køb en kat"
        ),
        textInput(
          inputId = paste0("phrase_hint", i),
          label = paste("Phrase", i, "hint"),
          placeholder = "Optional hint for phrase"
        ),
        p("----------------------------------------")
      )
    })
  })
  
  #Select the current phrase to guess for
  output$ui_cur_phrase <- renderUI({
    req(input$n_q)
    
    selectInput(
      inputId = "cur_phrase",
      label   = "Phrase to guess for:",
      choices = 1:as.integer(input$n_q)
    )
  })
  
  #See the selected phrase
  output$see_phrase <- renderUI({
    input$cur_guess_let
    h1("Hint: ", paste(input[[paste0("phrase_hint", input$cur_phrase)]]))
  })
  
  output$look <- renderPrint({
    rv[["plot_phrase2"]]
    
  })
  
  
  # Set Overview table for plotting Wrong/Right letters for each phrase -------------------------------
  #needs number of phrases set
  observe({
    x <- (1:length(LET_DK)) %% 6
    x[x == 0] <- 6
    y <- (0:(length(LET_DK) - 1)) %/% 6
    y <- 6 - y
    
    df <- data.frame(
      x = x,
      y = y,
      let = LET_DK,
      col = factor("grey", levels = c("grey", "green", "red")),
      alpha = 0.5
    )
    
    #cartesian product, for each phrase
    rv$overview_df_plot <- merge(tibble(id = 1:input$n_q), y = df)
    
  })
  
  # On Submission -----------------------------------------------------------
  
  observeEvent(input$submit, {
    
    #what is correct underlying?
    phrase_underlying <- as.character(input[[paste0("phrase", input$cur_phrase)]])
    #shown phrase _a_a__
    phrase_tmp        <- as.character(rv[[paste0("plot_phrase",input$cur_phrase)]])

    #letter guessed for
    letter_tmp <- toupper(input$cur_guess_let)
    
    
    #Find Occurences of letter in phrase
    (obj1 <- gregexpr(pattern = letter_tmp, text = phrase_underlying, ignore.case = T)[[1]])
    
    #-1 if not found
    pos <- as.numeric(obj1)
    
    #update phrase with correctly guessed
    if( all(pos > 0) )
      for(i in seq_along(pos)){
        #position
        pos_ <- pos[i]
        #update with corresponding char from sentence, to get correct Upper/lower case
        substring(text = phrase_tmp, first = pos_, last = pos_) <- substring(text = phrase_underlying, 
                                                                             first = pos_, last = pos_)
      }
    print(phrase_tmp)
  
    #update guessing phrase
    rv[[paste0("plot_phrase",input$cur_phrase)]] <- phrase_tmp
    
    
    #PLOT
    #Check letter guess
    obj <-  letter_checker(phrase = phrase_underlying, letter = letter_tmp)
    
    #if correct
    n_occur <- length(obj$occurrences)
    rv$n_occ <- n_occur
    
    #set label as either the slots/placements or a "WRONG"
    if (obj$correct_bool) {
      label <- as.character(obj$occurrences)
      rv$beep <- sample(x = c(1, 2, 7),
                        size = 1,
                        replace = T)
      
      #update overview table
      print(rv$overview_df_plot[1,])
      rv$overview_df_plot[rv$overview_df_plot$id == input$cur_phrase &
                            rv$overview_df_plot$let == letter_tmp, "col"] <- "green"
      rv$overview_df_plot[rv$overview_df_plot$id == input$cur_phrase &
                            rv$overview_df_plot$let == letter_tmp, "alpha"] <- 1
      
    } else{
      label <- paste(letter_tmp, "is Wrong!")
      rv$beep <- 9
      
      rv$overview_df_plot[rv$overview_df_plot$id == input$cur_phrase &
                            rv$overview_df_plot$let == letter_tmp, "col"] <- "red"
      rv$overview_df_plot[rv$overview_df_plot$id == input$cur_phrase &
                            rv$overview_df_plot$let == letter_tmp, "alpha"] <- 1
    }
    
#beep
    #conditional announce type, plot or text
    beep(sound = rv$beep)
 
    shinyjs::reset("cur_guess_let")
    
  })
  
  

  #Announcer plot
  output$announce_correct <- renderPlot({
    req(rv[[paste0("plot_phrase",input$cur_phrase)]])
    #phrase plot
      ggplot(data = data.frame(lab = str_wrap(rv[[paste0("plot_phrase",input$cur_phrase)]], 
                                              width = as.integer(input$aes_str_wrap)), 
                               x = 0, y = 0), aes(label = lab, x = x, y = y)) + 
      geom_text(size = as.integer(input$aes_size)) + 
      theme(legend.position = "none",
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank())     
  })
  
  
  
  
  #Plot overview
  output$plot_overview <- renderPlot({
    req(input$submit)
    
    #Overview plot
    ggplot(data = subset(rv$overview_df_plot, id == as.numeric(input$cur_phrase)),
           aes(
             x = x,
             y = y,
             label = let,
             col = col
           )) +
      geom_text(
        alpha = subset(rv$overview_df_plot, id == as.numeric(input$cur_phrase))$alpha,
        size = 18
      ) +
      scale_color_manual(values = c(
        "grey" = "grey",
        "green" = "green",
        "red" = "red"
      )) +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()
      )
    
    
    
  })
  
  
  
  observe({
    if (!is.character(input$cur_guess_let) |
        nchar(input$cur_guess_let) != 1) {
      shinyjs::disable(id = "submit")
      
    } else{
      shinyjs::enable(id = "submit")
    }
  })
  
  
  
} #server end

# Run the application
shinyApp(ui = ui, server = server)
