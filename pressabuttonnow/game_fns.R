wrap_par <- 50

text_block <- function(a_vector_of_strings, wrap_to = wrap_par){
  # clear the console
  cat("\014")

  # padding
  cat("\n")
  
  # format text for readability
  a_vector_of_strings %>% 
    map_chr(~ str_wrap(.x, width = wrap_to)) %>% 
    paste(collapse = "\n\n")  %>% 
    cat()

  # padding
  cat("\n")
}

# menu
button_menu <- function(text_var, choice_var){
  # menu text must end in a comma and grammatically make sense to say 
  # menu text,
  # press a button now.

  menu_text <- paste(text_var, "press a button now.")  %>% 
    text_block()

  if (interactive()) {
    button_selection <- menu(choice_var, title = menu_text)
    if (button_selection == 0) {
      stop("You rage quit.")
    } else return(button_selection)
  } else stop("Interactivity borked.")

} 

# new node break
button_title <- function(title_txt) {
  cat("\n")
  cat(rep('-', wrap_par / 2))
  cat("\n")
 
  paste0('\n*** ', toupper(title_txt), ' ***\n') %>% 
  cat()

}

# display project log
game_log <- function(this_game_path) {
  cat('\n')
  cat(rep('-', wrap_par / 2))
  cat("\n\nProject Log\n\n")
  print(this_game_path %>% filter(from != to))

}

# end game - win

win_screen <- function(){
   button_title("You pressed buttons to the end of the game!")
}