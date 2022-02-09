NOTES <- "
R word list packages
words (scrabble dictionary) <- not enough, forget it!
Grady Ward's English words
english.words (CELEX database) in vwr package https://www.rdocumentation.org/packages/vwr/versions/0.3.0/topics/english.words
http://wordlist.aspell.net/
https://www.r-bloggers.com/2010/10/lists-of-english-words/
syn (kind of like wordnet-- though?)

# actually sowpods is better dictionary: https://github.com/jesstess/Scrabble/blob/master/scrabble/sowpods.txt

sentences, fruit and words datasets are embedded in stringr (too small)

https://www.r-bloggers.com/2022/01/playing-wordle-in-r/
https://www.r-bloggers.com/2022/01/shinywordle-a-shiny-app-to-solve-the-game-worldle-and-the-power-of-regular-expressions/

I'm going to save the links for now and not look at them 'til I've taken a crack at it.

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

Dictionaries - my dictionary is missing words wordle accepts or words that
should be in there

"
library(words)
library(vwr)
library(tidyverse)
library(stringr)
library(glue)
library(janitor)
source('/cloud/project/WordleR/wordle_functions.R')

unis <- read_csv("/cloud/project/WordleR/data/unigram_freq.csv")  %>% 
  filter(str_length(word) == 5)

so_far <- read_csv("/cloud/project/WordleR/data/wordles_so_far.txt")

word_scores <- read_csv("/cloud/project/WordleR/data/sowpods.txt", col_names = FALSE) %>% 
  rename(word = X1) %>% 
  filter(str_length(word) == 5) %>% 
  mutate(word = str_to_lower(word))


#word_scores <- tibble(word = wlist)

word_scores <- word_scores %>% left_join(unis)

# dropped 'wooer' which is in the so far list. Otherwise fuck it.

# let's do wilding. Just give na's the fifth percentile of count.
word_scores <- word_scores %>% 
  replace_na(list(count = quantile(word_scores$count, .05, na.rm = T)))

word_scores <- word_scores %>% 
  mutate(freq = count/sum(count))


# find out how many is left after filtration

filter_count <- function(x) {
  nrow(word_scores %>% filter(!str_detect(word,regex(glue("[{x}]")))))
}


word_scores$exclude_count <- map_int(word_scores$word, filter_count)

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

group <- best_scores %>% filter(word %in% c('judge',
                                            'devil',
                                            'surly',
                                            'heats', 
                                            'stead',
                                            'salty',
                                            'roate',
                                            'later',
                                            'roast'))

# dump for the app to use!

# three - grams?

w <- word_scores$word

trigrams <- function(x) {
  c(str_sub(x,1,3),str_sub(x,2,4),str_sub(x,3,5))
}

trigrams('roast')

tg <- map(w,trigrams)

tg <- unlist(tg)

tg <- tibble(trigram = tg) %>% count(trigram)

saveRDS(word_scores, "/cloud/project/WordleR/data/word_scores.RDS")
saveRDS(tg, "/cloud/project/WordleR/data/trigrams.RDS")
NOTES <- "

Some CS clown says 'later' is best(is it?)

Latest (weighted) says pores toils cores tares tones

my cousin's 'ROAST' is pretty great
also 'ROATE' which is barely a word

"