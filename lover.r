
# imports
library(genius)
library(ggwordcloud)
library(tidytext)
library(tidyverse)

# get Lover
# one row is one line of a song
lover = genius_album(artist = "Taylor Swift", album = "Lover")

# concat the lyrics into a single mega-string
lyrics = paste(lover$lyric, collapse = " ")

# basic cleaning
lyrics = gsub("'", "", lyrics)
lyrics = gsub("[[:punct:] ]+", " ", lyrics)
lyrics = toupper(lyrics)

# tokenize
words = strsplit(lyrics, split = " ") %>% unlist()
df = table(words) %>% as_tibble() %>% rename(word = words)

# make the stopwords look like our data
stopw = gsub("'", "", stop_words$word)
stopw = gsub("[[:punct:] ]+", " ", stopw)
stopw = toupper(stopw)
stopw = unique(stopw)

# remove stopwords from lyrics
df = filter(df, !(word %in% stopw)) %>% arrange(-n)

# word cloud!
ggplot(filter(df, n > 3), aes(label = word, size = n)) +
  geom_text_wordcloud() +
  scale_radius(range = c(0, 20), limits = c(0, NA)) +
  theme_minimal()

# output
ggsave("lovercloud.png")

