
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
    ggplot(data = rv$p_layer_data, aes(x=x, y=y)) + geom_point(size = NA) +
    geom_label(data = subset(rv$p_layer_data, x <= rv$cnt), aes(x=x, y=y, label=label))
p2
