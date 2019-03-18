
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

donaldData = stream_in(file("data/PUBATTLEGROUNDS.json"), pagesize = 5000)

donaldDataframe = as.data.frame(data)

plot(donaldDataframe$retrieved_on, donaldData$score)
Donaldcommentdata <- data.frame(User = donaldData$author, 
                          Date = as.POSIXct(donaldData$created_utc, origin='1970-01-01'), 
                          Comment = donaldData$body)
head(Donaldcommentdata)

Donaldminimum = min(Donaldcommentdata$Date)

Donaldmaximum = max(Donaldcommentdata$Date)

Donaldcommentdata = subset(Donaldcommentdata, User != "SavageAxeBot" & User != "KeepingdataDank" & User != "AutoModerator" 
                   & User != "dataMods" & User != "BattleBusBot" & User != "MemeInvestor_bot" & User != "Transcribot"
                   & User != "Transcribot" & User != "commonmisspellingbot" & User != "TiltedTowersBot" & User != "stormshieldonebot"
                   & User != "WikiTextBot" & User != "RemindMeBot" & User != "thank_mr_skeltal_bot" & User != "societybot"
                   & User != "rick_rolled_bot" & User != "NoSkinBot")

Donaldcomments_top <- Donaldcommentdata %>%
  dplyr::select(Comment) %>%
  unnest_tokens(word, Comment)

#clean the top comments

Donaldcomments_top_clean <- Donaldcomments_top %>%
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
  filter(!word == "deleted") %>%
  filter(!word == "x200b")

Donaldtop_words <- Donaldcomments_top_clean %>% 
  count(word, sort = TRUE) %>%
  top_n(100) %>%
  mutate(word = reorder(word, n))

head(Donaldtop_words)

#word cloud

wordcloud(Donaldtop_words$word, Donaldtop_words$n, min.freq = 1,
          max.words = 75, random.order = FALSE, rot.per = 0.15, 
          colors = brewer.pal(8, "Dark2"))

#frequency graph
Donaldcomments_top_clean %>% 
  count(word, sort = TRUE) %>%
  top_n(30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Words",
       y = "Count",
       title = "Most commonly used english words in /r/the_donald, December 2018")

Donaldcomment_data_paired <- Donaldcommentdata %>%
  dplyr::select(Comment) %>%
  mutate(Comment = removeWords(Comment, stop_words$word)) %>%
  mutate(Comment = gsub("\\brt\\b|\\bRT\\b", "", Comment)) %>%
  mutate(Comment = gsub("http://*", "", Comment)) %>%
  unnest_tokens(paired_words, Comment, token = "ngrams", n = 2)

Donald_Comments_Separated <- Donaldcomment_data_paired %>%
  separate(paired_words, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE)

head(Donald_Comments_Separated, 10)

Donald_Comments_Separated %>%
  filter(n >= 80) %>%
  filter(!word1 == "shit") %>%
  filter(!word1 == "gt") %>%
  filter(!word1 == "https") %>%
  filter(!word1 == "it's") %>%
  filter(!word1 == "amp") %>%
  filter(!word1 == "fucking") %>%
  filter(!word1 == "fuck") %>%
  filter(!word1 == "1") %>%
  filter(!word1 == "2") %>%
  filter(!word1 == "3") %>%
  filter(!word1 == "removed") %>%
  filter(!word1 == "deleted") %>%
  filter(!word1 == "x200b") %>%
  filter(!word2 == "shit") %>%
  filter(!word2 == "gt") %>%
  filter(!word2 == "https") %>%
  filter(!word2 == "it's") %>%
  filter(!word2 == "amp") %>%
  filter(!word2 == "fucking") %>%
  filter(!word2 == "fuck") %>%
  filter(!word2 == "1") %>%
  filter(!word2 == "2") %>%
  filter(!word2 == "3") %>%
  filter(!word2 == "removed") %>%
  filter(!word2 == "deleted") %>%
  filter(!word2 == "x200b") %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(colour = "darkslategray4", size = 4) +
  geom_node_text(aes(label = name), vjust = 1.2, size = 2.4) +
  labs(title = "Redditor Comment links on /r/PUBATTLEGROUNDS",
       subtitle = "January 2019",
       x = "", y = "") +
  theme_void()

