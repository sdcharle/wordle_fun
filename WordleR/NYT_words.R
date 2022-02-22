# from this gent: https://www.r-bloggers.com/2022/02/wordle-data-analysis/
library(httr)

url = "https://www.nytimes.com/games/wordle/main.18637ca1.js"

wordle_script_text = GET(url) %>%
  content(as = "text", encoding = "UTF-8")

word_list = substr(
  wordle_script_text,
  # cigar is the first word
  str_locate(wordle_script_text, "cigar")[,"start"],
  # shave is the last word
  str_locate(wordle_script_text, "shave")[,"end"]) %>%
  str_remove_all("\"") %>%
  str_split(",") %>%
  data.frame() %>%
  select(word = 1) %>%
  mutate(word = toupper(word))

# heh! 10638

# do same for aahed to zymic

word_list_guess = substr(
  wordle_script_text,
  # cigar is the first word
  str_locate(wordle_script_text, "aahed")[,"start"],
  # shave is the last word
  str_locate(wordle_script_text, "zymic")[,"end"]) %>%
  str_remove_all("\"") %>%
  str_split(",") %>%
  data.frame() %>%
  select(word = 1) %>%
  mutate(word = toupper(word))

saveRDS(word_list, "/cloud/project/WordleR/data/nyt_wordle.rds")
saveRDS(word_list_guess, "/cloud/project/WordleR/data/nyt_wordle_allowed_guesses.rds")
