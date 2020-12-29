#Wheel of fortune
#### woords: ----
DF  <-  "danmark for danskere"


#### function ----
letter_checker <- function(phrase = DF, letter = "X"){
  #Helper FUN
  uniqchars <- function(x) unique(strsplit(x, "")[[1]]) 
  #Guess to be converted to char and upper
  letter <- toupper(letter)
  if(!is.character(letter)){
    letter <- toupper(as.character(letter))
  } 
  
  #REMOVE SPACES, upper case letters
  phrase_no_space <- toupper(gsub(pattern = "\\s|_", replacement = "", x = phrase))
  
  #Find Occurences of letter in phrase
  obj1 <- gregexpr(pattern = letter, text = phrase_no_space)[[1]]
  
  occurrences <- as.numeric(obj1)
  
  correct_bool <- !any(occurrences == -1)

  
  #Total letters in phrase
  total_letters <- nchar(phrase_no_space)
  
  letters_in_phrase <- sort(uniqchars(phrase_no_space))
  
  out <- list(phrase = phrase, 
              letter = letter, 
              correct_bool = correct_bool, 
              occurrences = occurrences)
  
 #  df <- data.frame(Letter = letters_in_phrase,
 #                   Slot   = character(length(letters_in_phrase)), stringsAsFactors = FALSE)
 #  # loop over all letters
 #    for( i in seq_along(letters_in_phrase)){
 #      out <-  gregexpr(pattern = letters_in_phrase[i], phrase_no_space)[[1]]
 #      df$Slot[i] <- paste(as.character(out), collapse = ", ")
 #    }
 # df <- format(df,justify = "c") 
 # 
 # out <- df[df$Letter == letter,]
  
 #beeper
 
 # if( nrow(out) > 0 ) {
 #   #count no of correct 
 #   tmp <-  gregexpr(pattern = letter, phrase_no_space)[[1]]
 #   no_of_correct <- length(tmp)
 #   
 #   sound <- sample(x = c(1, 7, 10), size = 1)
 #   #repeats
 #   print(no_of_correct)
 #   for (i in 1:no_of_correct ){
 #       beepr::beep(sound = sound) 
 #       Sys.sleep(time = 0.5)
 #     } 
 #   } else beepr::beep(sound = 9) 
 
}


