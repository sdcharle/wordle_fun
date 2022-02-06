# Based on the breakout superstar game Wordle
# elements is not_r_c
library(shiny)
source("wordle_functions.R")

word_scores <- readRDS("data/word_scores.RDS")
so_far <- read_csv("data/wordles_so_far.txt")

word_scores <- word_scores %>% 
  mutate(previous_answer = word %in% so_far$word) %>% 
  mutate(log_freq = log(freq)) %>% 
  select(-l_count, -freq, -count) %>% 
  mutate(mult_score = as.numeric(mult_score)) %>% 
  mutate(rank = rank((-mult_score)))

clean_text <- function(text) {
  text <- str_replace_all(str_to_lower(text), regex("[^a-z]"),'')
  text
}

filter_scores <- function(not_in_word, position_chars, not_at_position) {

  # still filter for in word but add filtering based on position
  in_word =  unique(str_split(paste(not_at_position, collapse = ''), '')[[1]])
  print(glue("in word be:{in_word}"))
  not_in_word = unique(str_split(not_in_word,'')[[1]])

  matcher <- c("^")
  for (c in position_chars) {
    if (c == '') { 
      matcher = c(matcher,'.')
    } else {
      matcher = c(matcher,c)
    }
  }
  matcher <- regex(paste(matcher, collapse = ''))

  check_in_word <- function(x) {

    if (length(in_word) == 0) { 
      TRUE 
    } else {
      length(intersect(str_split(x,'')[[1]], in_word)) == length(in_word)
    }
  }
  
  check_not_in_word <- function(x) {
    length(intersect(str_split(x,'')[[1]], not_in_word)) == 0
  }
  
  neg_matcher = c()
  for (i in 1:5) {
    if(str_length(not_at_position[i]) == 0) {

      next
    }
    
    else {
      nm = '^.....'
      nm = glue("{str_sub(nm,1,i)}[{not_at_position[i]}]{str_sub(nm,i+1,5)}")
    }

    neg_matcher = c(neg_matcher, regex(paste(nm, collapse = '')) )
  }

  word_scores$df_not_in <- map_lgl(word_scores$word, check_not_in_word)
  word_scores$df_in <-  map_lgl(word_scores$word, check_in_word)
  # add the neg match based on
  
  df <- word_scores %>% 
    filter(str_detect(word, matcher) &
             df_not_in &
             df_in) %>% 
    select(-df_in, -df_not_in)
  
  for(nm in neg_matcher) {
    df <- df %>% 
      filter(!str_detect(word, nm))
  }
  df
}

shinyServer(function(input, output, session) {

  observeEvent(input$clearChoices, {
    updateTextInput(session, "not_in_word", value = "")
    updateTextInput(session, "letter1", value = "")
    updateTextInput(session, "letter2", value = "")
    updateTextInput(session, "letter3", value = "")
    updateTextInput(session, "letter4", value = "")
    updateTextInput(session, "letter5", value = "")
    updateTextInput(session, "not_1", value = "")
    updateTextInput(session, "not_2", value = "")
    updateTextInput(session, "not_3", value = "")
    updateTextInput(session, "not_4", value = "")
    updateTextInput(session, "not_5", value = "")
  })
  
  output$filteredTable <- DT::renderDT({
    filter_scores(clean_text(input$not_in_word), 
                  c(str_sub(clean_text(input$letter1),1,1),
                    str_sub(clean_text(input$letter2),1,1),
                    str_sub(clean_text(input$letter3),1,1),
                    str_sub(clean_text(input$letter4),1,1),
                    str_sub(clean_text(input$letter5),1,1)),
                  c(
                    clean_text(input$not_1),
                    clean_text(input$not_2),
                    clean_text(input$not_3),
                    clean_text(input$not_4),
                    clean_text(input$not_5)
                  )
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