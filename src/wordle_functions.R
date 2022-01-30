NOTES <- "
R word list packages

words (scrabble dictionary)
Grady Ward's English words
Don't forget Wordnet
english.words (CELEX database) in vwr package https://www.rdocumentation.org/packages/vwr/versions/0.3.0/topics/english.words
http://wordlist.aspell.net/
https://www.r-bloggers.com/2010/10/lists-of-english-words/
syn (kind of like wordnet-- though?)

sentences, fruit and words datasets are embedded in stringr

https://www.r-bloggers.com/2022/01/playing-wordle-in-r/
https://www.r-bloggers.com/2022/01/shinywordle-a-shiny-app-to-solve-the-game-worldle-and-the-power-of-regular-expressions/

So as a follow-up, it appears there are already at least 2 posts on R Bloggers about Wordle, 
but I'm going to save the links for now and not look at them 'til I've taken a crack at it.

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

# filter list based on wordle hints
# let's compare dictionaries
wlist <- five_ew # big list

# raw counts of letters
letters <- stringr::str_c(wlist, collape = '')
letters <- stringr::str_flatten(wlist, collapse = "")
a_count = str_count(letters, pattern = 'a')

# get it all
alphas <- 'abcdefghijklmnopqrstuvwxyz'
w_stats <- list()

for (i in 1:26) {
  all_count = str_count(letters, pattern = str_sub(alphas,i,i))
  count1 = sum(str_detect(wlist, regex(glue("^{str_sub(alphas,i,i)}"))))
  count2 = sum(str_detect(wlist, regex(glue("^.{str_sub(alphas,i,i)}"))))
  count3 = sum(str_detect(wlist, regex(glue("^..{str_sub(alphas,i,i)}"))))
  count4 = sum(str_detect(wlist, regex(glue("^...{str_sub(alphas,i,i)}"))))
  count5 = sum(str_detect(wlist, regex(glue("^....{str_sub(alphas,i,i)}"))))
  w_stats[[i]] = tibble(letter = str_sub(alphas,i,i),
                        all_count = all_count,
                        count1 = count1,
                        count2 = count2,
                        count3 = count3,
                        count4 = count4,
                        count5 = count5)
}

letter_stats <- do.call(rbind, w_stats)

# great, now what?

SO <- "

's''e' 'a' 'r' are good starting letters but what then? score all the words, 
based on the stats....




"

# additive

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

# multiplicative - overflows or underflows - use logs to our advantage?
# this based just on if there's a direct hit
word_score_mult <- function(word) {
  
  log_score = log(
    letter_stats[letter_stats$letter == str_sub(word,1,1),]$count1/(sum(letter_stats$count1) )*
      letter_stats[letter_stats$letter == str_sub(word,2,2),]$count2/(sum(letter_stats$count2) ) * 
      letter_stats[letter_stats$letter == str_sub(word,3,3),]$count3/(sum(letter_stats$count3)) *
      letter_stats[letter_stats$letter == str_sub(word,4,4),]$count4/(sum(letter_stats$count4) ) *
      letter_stats[letter_stats$letter == str_sub(word,5,5),]$count5/(sum(letter_stats$count5) ) 
  )
  log_score 
}

# Next - weight by usage of the word in English
# what the fuck?
wlist <- wlist[wlist != 'fête']

add_scores <- wlist %>% 
  map_chr(word_score_add)

mult_scores <- wlist %>% 
  map_chr(word_score_mult)

word_scores = tibble(word = wlist,
                     add_score = add_scores,
                     mult_score = mult_scores)

# words w/ 5 distinct letters only
unique_letters <- function(word) {
  length(unique(unlist(str_split(word,''))))
}

word_scores$l_count = map_int(word_scores$word, unique_letters)

best_scores <- word_scores %>% 
  filter(l_count == 5)

best_scores <- best_scores %>% 
  mutate(a_rank = rank(desc(add_score))) %>% 
  mutate(m_rank = rank((mult_score)))

group <- best_scores %>% filter(word %in% c('judge','devil','surly','heats', 'stead'))

# by English usage
