NOTES <- "

https://www.r-bloggers.com/2010/10/lists-of-english-words/

# sowpods is better dictionary: https://github.com/jesstess/Scrabble/blob/master/scrabble/sowpods.txt

https://www.r-bloggers.com/2022/01/playing-wordle-in-r/
https://www.r-bloggers.com/2022/01/shinywordle-a-shiny-app-to-solve-the-game-worldle-and-the-power-of-regular-expressions/

Competition
https://github.com/Kinkelin/WordleCompetition
https://github.com/Kinkelin/WordleCompetition/blob/main/data/official/wordle_historic_words.txt

Google book ngrams

https://screenrant.com/wordle-answers-updated-word-puzzle-guide/

Other: 
https://github.com/first20hours/google-10000-english/blob/master/google-10000-english-usa-no-swears.txt

Actually - just use stuff from the word list.

"
library(tidyverse)
library(stringr)
library(glue)
library(janitor)
library(stringdist)
#library(hclust) - not avail wut?

source('/cloud/project/WordleR/wordle_functions.R')

unis <- read_csv("/cloud/project/WordleR/data/unigram_freq.csv")  %>% 
  filter(str_length(word) == 5)

so_far <- read_csv("/cloud/project/WordleR/data/wordles_so_far.txt")

#word_scores <- read_csv("/cloud/project/WordleR/data/sowpods.txt", col_names = FALSE) %>% 
#  rename(word = X1) %>% 
#  filter(str_length(word) == 5) %>% 
#  mutate(word = str_to_lower(word))

allowed <- readRDS('/cloud/project/WordleR/data/nyt_wordle_allowed_guesses.rds')
possible <- readRDS('/cloud/project/WordleR/data/nyt_wordle.rds')

allowed$possible_answer = FALSE
possible$possible_answer = TRUE

word_scores <- rbind(allowed, possible) %>% 
  mutate(word = str_to_lower(word))

word_scores <- word_scores %>% left_join(unis)


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

# actually just use possible answers here!
letter_stats <- get_letter_stats(word_scores %>% filter(possible_answer == TRUE), TRUE)

mult_scores <- word_scores$word %>% 
  map_chr(word_score_mult)

word_scores$mult_score =  mult_scores

word_scores$l_count = map_int(word_scores$word, unique_letters)

word_scores <- word_scores %>% 
  mutate(rank = rank((mult_score)))

best_scores <- word_scores %>% 
  filter(l_count == 5)

best_scores <- best_scores %>% 
  mutate(rank = rank((mult_score)))

#trigrams <- function(x) {
#  c(str_sub(x,1,3),str_sub(x,2,4),str_sub(x,3,5))
#}
#tg <- map(w,trigrams)
#tg <- unlist(tg)
#tg <- tibble(trigram = tg) %>% count(trigram)

saveRDS(word_scores, "/cloud/project/WordleR/data/word_scores.RDS")
#saveRDS(tg, "/cloud/project/WordleR/data/trigrams.RDS")
NOTES <- "
Some CS clown says 'later' is best(is it?)
some clown claiming 'salet' is best.
my cousin's 'ROAST' is pretty great
also 'ROATE' which is barely a word
toeas
You can get the word list from the Javascript.
"

library(stringdist)
# dude says JW is fly: https://amunategui.github.io/stringdist/

# hmmm...let's truncate words some, first? like top 1000???

subset <- word_scores %>% 
  arrange(desc(freq)) %>% 
  head(1000)
library(tictoc)
tic("get matrix")
distancemodels <- stringdistmatrix(subset$word, subset$word, method = "jw")
toc()
rownames(distancemodels) <- subset$word
hc <- hclust(as.dist(distancemodels))
# https://uc-r.github.io/hc_clustering
plot(hc)

twofer <- rect.hclust(hc,k = 25) # if ye want 20 clusts
# https://amunategui.github.io/stringdist/
# Can calculate various string distances based on edits (Damerau-Levenshtein, Hamming, Levenshtein, optimal sting alignment), 
# qgrams (qgram, cosine, jaccard distance) or 
# heuristic metrics (Jaro, Jaro-Winkler)

for (thing in twofer) {
  print(length(thing))
}

# not too even, yo....