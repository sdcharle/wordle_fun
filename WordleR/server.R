# Based on the breakout superstar game Wordle

library(shiny)

source("wordle_functions.R")

word_scores <- readRDS("data/word_scores.RDS") %>% 
  select(-l_count) %>% 
  mutate(add_score = as.numeric(add_score),
         mult_score = as.numeric(mult_score))

clean_text <- function(text) {
  text <- str_replace_all(str_to_lower(text), regex("[^a-z]"),'')
  text
}

filter_scores <- function(in_word, not_in_word, position_chars) {
  print(in_word)
  print(str_length(in_word))
  print(not_in_word)
  
  matcher <- c("^")
  
  in_word = unique(str_split(in_word,'')[[1]])
  not_in_word = unique(str_split(not_in_word,'')[[1]])
  
  for (c in position_chars) {
    if (c == '') { 
      matcher = c(matcher,'.')
    } else {
      matcher = c(matcher,c)
    }
  }
  matcher <- regex(paste(matcher, collapse = ''))

  check_in_word <- function(x) {
    length(intersect(str_split(x,'')[[1]], in_word)) == length(in_word)
  }
  
  check_not_in_word <- function(x) {
    length(intersect(str_split(x,'')[[1]], not_in_word)) == 0
  }
  
  word_scores$df_not_in <- map_lgl(word_scores$word, check_not_in_word)
  word_scores$df_in <-  map_lgl(word_scores$word, check_in_word)
  
  df <-word_scores %>% 
    filter(str_detect(word, matcher) &
             df_not_in &
             df_in) %>% 
    select(-df_in, -df_not_in)
  
  df
  
}

shinyServer(function(input, output) {

  output$filteredTable <- DT::renderDT({
    filter_scores(clean_text(input$in_word), 
                  clean_text(input$not_in_word), 
                  c(str_sub(clean_text(input$letter1),1,1),
                    str_sub(clean_text(input$letter2),1,1),
                    str_sub(clean_text(input$letter3),1,1),
                    str_sub(clean_text(input$letter4),1,1),
                    str_sub(clean_text(input$letter5),1,1))
                )    }, 
    selection = 'single', 
    filter = 'top', 
    extensions = c('Responsive','Scroller'),
    options = list(
      pageLength = 80,
      deferRender = TRUE,
      scrollY = 600,
      scroller = TRUE
    ) )

})
