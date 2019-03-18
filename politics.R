
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("jsonlite")
library("NCmisc")
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidytext)
# coupled words analysis
library(widyr)
# plotting packages
library(igraph)
library(ggraph)

options(stringsAsFactors = FALSE)

#file.split("politics.json", size = 800000)

data = stream_in(file("data/politics.json"), pagesize = 5000)

dataframe = as.data.frame(data)

plot(dataframe$retrieved_on, dataframe$score)


commentdata <- data.frame(User = data$author, 
                          Date = as.POSIXct(data$created_utc, origin='1970-01-01'), 
                          Comment = data$body)
head(commentdata)

minimum = min(commentdata$Date)

maximum = max(commentdata$Date)

comments_top <- commentdata %>%
                dplyr::select(Comment) %>%
                unnest_tokens(word, Comment)

#clean the top comments

comments_top_clean <- comments_top %>%
  anti_join(as.data.frame(stop_words)) %>%
  filter(!word == "shit") %>%
  filter(!word == "gt") %>%
  filter(!word == "https") %>%
  filter(!word == "it's") %>%
  filter(!word == "amp") %>%
  filter(!word == "fucking") %>%
  filter(!word == "fuck") %>%
  filter(!word == "1") %>%
  filter(!word == "2") %>%
  filter(!word == "3") %>%
  filter(!word == "removed") %>%
  filter(!word == "deleted")
  
top_words <- comments_top_clean %>% 
  count(word, sort = TRUE) %>%
  top_n(100) %>%
  mutate(word = reorder(word, n))

head(top_words)

#word cloud

wordcloud(top_words$word, top_words$n, min.freq = 1,
            max.words = 75, random.order = FALSE, rot.per = 0.15, 
            colors = brewer.pal(8, "Dark2"))

#frequency graph
comments_top_clean %>% 
  count(word, sort = TRUE) %>%
  top_n(30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
    labs(x = "Words",
         y = "Count",
         title = "Most commonly used english words in /r/politics during December 2018")

comment_data_paired <- commentdata %>%
  dplyr::select(Comment) %>%
  mutate(Comment = removeWords(Comment, stop_words$word)) %>%
  mutate(Comment = gsub("\\brt\\b|\\bRT\\b", "", Comment)) %>%
  mutate(Comment = gsub("http://*", "", Comment)) %>%
  unnest_tokens(paired_words, Comment, token = "ngrams", n = 2)

comment_data_separated <- comment_data_paired %>%
  separate(paired_words, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE)

head(comment_data_separated, 10)

comment_data_separated %>%
  filter(n >= 2000) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(colour = "darkslategray4", size = 6) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 6) +
  labs(title = "Redditor Comment links on /r/politics",
    subtitle = "January 2019",
    x = "", y = "") +
  theme_void()
  
  



  