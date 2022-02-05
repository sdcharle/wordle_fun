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



words we've seen

Competition
https://github.com/Kinkelin/WordleCompetition

First 200

https://github.com/Kinkelin/WordleCompetition/blob/main/data/official/wordle_historic_words.txt



cloud
wrung 

both 'low scores' as far as matching other things

Google book ngrams

https://screenrant.com/wordle-answers-updated-word-puzzle-guide/
"
library(words)
library(vwr)
library(tidyverse)
library(stringr)
library(glue)
library(janitor)
source('/cloud/project/WordleR/wordle_functions.R')


unis <- read_csv("WordleR/data/unigram_freq.csv")  %>% 
  filter(str_length(word) == 5)

so_far <- read_csv("WordleR/data/wordles_so_far.txt")

word_scores <- tibble(word = wlist)

word_scores <- word_scores %>% left_join(unis)


# dropped 'wooer' which is in the so far list. Otherwise fuck it.

word_scores <- word_scores %>% 
  filter(!is.na(count)) %>% 
  mutate(freq = count/sum(count))


# words w/ 5 distinct letters only
unique_letters <- function(word) {
  length(unique(unlist(str_split(word,''))))
}

letter_stats <- get_letter_stats(word_scores, TRUE)

mult_scores <- word_scores$word %>% 
  map_chr(word_score_mult)

word_scores$mult_score =  mult_scores

word_scores$l_count = map_int(word_scores$word, unique_letters)

best_scores <- word_scores %>% 
  filter(l_count == 5)

best_scores <- best_scores %>% 
  mutate(rank = rank((mult_score)))

group <- best_scores %>% filter(word %in% c('judge','devil','surly','heats', 'stead'))

NOTES <- "

Some CS clown says 'later' is best(is it?)

Latest (weighted) says pores toils cores tares tones

"
