
let <- LETTERS

foo_df_let <- function(let = NULL, max_pr_col = 6, phrase = 1){
  if(is.null(let)) return(data.frame(let = character(), x = numeric(), y = numeric()))
  len <- length(let)
  x <- (1:len) %% max_pr_col 
  x[x == 0] <- max_pr_col
  x
  y <- (0:(len-1)) %/% max_pr_col
  y <- max_pr_col - y
  y
  #y <- rev(y)
  
  df <- data.frame(let = let, x = x, y = y)
  
  return(df)
}

df_wrong_letters <- foo_df_let(let = letters)
df_wrong_letters
p <- ggplot(data = df_wrong_letters, aes(x = x, y = y))
    p + geom_label(aes(label = let), size = 25)

    
    rv <- list()
    rv$p_layer_data <- data.frame(x = 1:5, y = 0, label = "A")
    rv$cnt <- 3
    
    
    LET_DK <- c(LETTERS, "Æ", "Ø", "Å")
    
    x <- (1:length(LET_DK)) %% 6
    x[x == 0] <- 6
    x
    y <- (0:(length(LET_DK)-1)) %/% 6
    y <- 6 - y
    y
    
    df <- data.frame(x = x, y = y, let = LET_DK, col = factor("grey", levels = c("grey", "green", "red")), alpha = 0.5)
    df[10,"col"] <- "green"
    df[10,"alpha"] <- 1
    
    df[20,"col"] <- "red"
    df[20,"alpha"] <- 1
    
    overview_df_plot <- merge(tibble(id = 1:3), y = df)
    
    ggplot(data = overview_df_plot, aes(x=x, y=y, label = let, col = col)) + 
    geom_text(alpha = overview_df_plot$alpha, size = 10) + scale_color_manual(values = c("grey", "green", "red")) + 
      facet_grid(.~id)


# sim your Alpha
mtcars$Alpha <- runif(nrow(mtcars))

ggplot(data = mtcars, aes(mpg, hp)) +
  geom_point(alpha = Alpha)

path.package()
getwd()
library("beepr")
.Library
.libPaths()
