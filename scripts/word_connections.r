#Installed packages
install.packages('topicmodels')
#Libraries
library(tidyverse)
library(jsonlite)
library(tidytext)
library(ggplot2)
library(anytime)
library(topicmodels)


#Reading dataset from JSON
setwd("C:\\Users\\andrew\\Documents\\R\\Coursework")
the_donald <- stream_in(file("The_Donald_cleaned.json"))%>%filter(score>10)
the_donald <- stream_in(file("The_Donald_cleaned.json"))%>%select(-archived, -author_flair_background_color, -author_flair_css_class, -author_flair_richtext, -author_flair_template_id, -author_flair_text, -author_flair_text_color, -author_flair_type, -author_patreon_flair, -can_gild, -can_mod_post, -collapsed, -collapsed_reason, -controversiality, -distinguished, -removal_reason, -subreddit_id, -subreddit_name_prefixed, -subreddit_type)
stream_out(the_donald, file("The_Donald_short.json"))

#Remove bots
cleaned <- the_donald%>%
  filter(author !=  "AutoModerator")%>%
  filter(author !=  "TrumpBrickBot")%>%
  filter(author !=  "trumpcoatbot")%>%
  filter(author !=  "TrumpTrainBot")%>%
  filter(author !=  "BotForceOne")

#Remove deleted accounts
cleaned <- cleaned%>%
  filter(body != "[deleted]")

#Removed removed comments
cleaned <- cleaned%>%
  filter(body != "[removed]")

authors <- cleaned%>%select(author)%>%group_by(author)%>%count(author, sort=TRUE)

stream_out(cleaned, file("The_Donald_cleaned.json"))

#Need to purge deleted and removed comments
##Remove author called [deleted] and comments called [removed]
#Remove bots and automods
##AutoModerator
##TrumpBrickBot
##trumpcoatbot
##TrumpTrainBot
##BotForceOne

#Add my own stop words
stopWords <- stop_words
stop_words2 <- c("http","https","amp","x200b","www.youtube.com","youtu.be","en.wikipedia.org","gt","twitter.com","imgur.com","i.imgur.com","archive.is","magaimg.net","")
stopWords <- union(stopWords, data.frame("word" = stop_words2, "lexicon" = "test"))

words <- the_donald%>%select(author,body)%>%unnest_tokens(word,body)%>%anti_join(stopWords)

install.packages('widyr')
library(widyr)
word_pairs <- words%>%pairwise_count(word, author, sort = TRUE)

comments <- the_donald%>%
  select(body)%>%
  unnest_tokens(bigram,body,token="ngrams",n=2)%>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stopWords$word,
         !word2 %in% stopWords$word)%>%
  count(word1, word2, sort = TRUE)

library(igraph)
comments <- comments%>%unite("bigram", c(word1, word2), sep = " ") %>%
  filter(n > 20) %>%
  graph_from_data_frame()

install.packages('ggraph')
library(ggraph)
set.seed(123)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(comments, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()