NOTES <- "
R word list packages

words (scrabble dictionary)
Grady Ward's English words
english.words (CELEX database) in vwr package https://www.rdocumentation.org/packages/vwr/versions/0.3.0/topics/english.words
http://wordlist.aspell.net/
https://www.r-bloggers.com/2010/10/lists-of-english-words/
syn (kind of like wordnet-- though?)

sentences, fruit and words datasets are embedded in stringr

https://www.r-bloggers.com/2022/01/playing-wordle-in-r/
https://www.r-bloggers.com/2022/01/shinywordle-a-shiny-app-to-solve-the-game-worldle-and-the-power-of-regular-expressions/
https://screenrant.com/wordle-answers-updated-word-puzzle-guide/

So as a follow-up, it appears there are already at least 2 posts on R Bloggers about Wordle, 
but I'm going to save the links for now and not look at them 'til I've taken a crack at it.

words we've seen
cloud
wrung 

both 'low scores' as far as matching other things

Google book ngrams

ADD
Weight different positions differently (like starting letters more important
than ending)

"

library(words)
library(vwr)
library(tidyverse)
library(stringr)
library(glue)

data(english.words)
data(words)
length(english.words)
length(words)
five_ew <- english.words[str_length(english.words) == 5]
# about five thousand
five_ww <- words[str_length(words) == 5]
# 200? that sucks.

fww <- tibble(w = five_ww)
feww <- tibble(w = five_ew)
# too big - preprocess!
#freqs <- read_csv(glue("/cloud/project/WordleR/data/unigram_freq.csv"))
#freqs <- freqs %>% 
#  filter(str_length(word) == 5)
# filter list based on wordle hints
# let's compare dictionaries
wlist <- five_ew # big list

# missing some tho including some biggies!

wlist <- c(wlist,
           'roate',
           'aeros' )


# raw counts of letters
letters <- stringr::str_c(wlist, collape = '')
letters <- stringr::str_flatten(wlist, collapse = "")
a_count = str_count(letters, pattern = 'a')
# Next - weight by usage of the word in English
# what the fuck?
wlist <- wlist[wlist != 'fête']
# get it all
alphas <- 'abcdefghijklmnopqrstuvwxyz'

get_letter_stats <- function(word_frame, weighted = FALSE) {
  w_stats <- list()
  for (i in 1:26) {
    
    # count this letter across everyting
    all_count = str_count(letters, pattern = str_sub(alphas,i,i))
    # later - doesn't like glue bro
    df <- word_frame %>% 
      mutate(
        count1 =  as.integer(str_detect(word, regex(glue("^{str_sub(alphas,i,i)}")))),
        count2 = str_detect(word, regex(glue("^.{str_sub(alphas,i,i)}"))),
        count3 = str_detect(word, regex(glue("^..{str_sub(alphas,i,i)}"))),
        count4 = str_detect(word, regex(glue("^...{str_sub(alphas,i,i)}"))) ,
        count5 = str_detect(word, regex(glue("^....{str_sub(alphas,i,i)}")))
    )
    
    if(weighted) {
      df <- df %>% 
        mutate(count1 = as.integer(count1)*freq,
               count2 = as.integer(count2)*freq,
               count3 = as.integer(count3)*freq,
               count4 = as.integer(count4)*freq,
               count5 = as.integer(count5)*freq)
    } else {
      df <- df %>% 
        mutate(count1 = as.integer(count1)/(nrow(df)),
               count2 = as.integer(count2)/(nrow(df)),
               count3 = as.integer(count3)/(nrow(df)),
               count4 = as.integer(count4)/(nrow(df)),
               count5 = as.integer(count5)/(nrow(df)))
    }
    
    
    w_stats[[i]] = tibble(letter = str_sub(alphas,i,i),
                          all_count = all_count,
                          count1 = sum(df$count1),
                          count2 = sum(df$count2),
                          count3 = sum(df$count3),
                          count4 = sum(df$count4),
                          count5 = sum(df$count5))
  }
  
  letter_stats <- do.call(rbind, w_stats)
  letter_stats
}


# this approach bloze
word_score_add <- function(word) {
  score = 0
  for (i in 1:5) { # go over letters and sum up if they appear anywhere
    score = score + 
      letter_stats[letter_stats$letter == str_sub(word, i,i),]$count1 +
      letter_stats[letter_stats$letter == str_sub(word, i,i),]$count2 +
      letter_stats[letter_stats$letter == str_sub(word, i,i),]$count3 +
      letter_stats[letter_stats$letter == str_sub(word, i,i),]$count4 +
      letter_stats[letter_stats$letter == str_sub(word, i,i),]$count5 
  }
  score
}

# this based just on if there's a direct hit
# try weigh
word_score_mult <- function(word) {
  log_score = log(
    letter_stats[letter_stats$letter == str_sub(word,1,1),]$count1 *
      letter_stats[letter_stats$letter == str_sub(word,2,2),]$count2 * 
      letter_stats[letter_stats$letter == str_sub(word,3,3),]$count3 *
      letter_stats[letter_stats$letter == str_sub(word,4,4),]$count4 *
      letter_stats[letter_stats$letter == str_sub(word,5,5),]$count5 
  )
  log_score 
}

# check word against target
check_word <- function(word, target) {
  match_indexes = c()
  green_letters = c()
  overlaps = intersect(str_split(word,'')[[1]], str_split(target,'')[[1]])
  not_in_word = setdiff(str_split(word,'')[[1]], str_split(target,'')[[1]])
  for (i in 1:str_length(target)) {
    if (str_sub(word, i, i) == str_sub(target, i, i)) {
      match_indexes = c(match_indexes, i)
      green_letters = c(green_letters, str_split(target,'')[[1]][i])
      overlaps = setdiff(overlaps, str_sub(word, i,i))
    }
  }
  
  result = list()
  result$yellow = overlaps
  result$green = match_indexes
  result$green_letters = green_letters
  result$not_in_word = not_in_word
  result
}

# check word against pattern
# wordle_pattern is result from above
# don't do the compare, get regex then mass compare
# possibly - add spinner back!

filter_word_regex <- function(wordle_pattern) {
  # make regex!
  pattern = "^"
  for (i in 1:5) {
    if (i %in% wordle_pattern$green) {
      pattern = c(pattern, wordle_pattern$green_letters[match(i,wordle_pattern$green)])
    } else {
      pattern = c(pattern,'.')
    }
  }
  regex(paste(pattern, collapse = ''))
} 