
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

dankMemesData = stream_in(file("data/dankmemes.json"), pagesize = 5000)

plot(dankMemesData$created_utc, dankMemesData$score)
dankMemesDataFrame <- data.frame(User = dankMemesData$author, 
                                Date = as.POSIXct(dankMemesData$created_utc, origin='1970-01-01'), 
                                Comment = dankMemesData$body,
                                Score = dankMemesData$score)

dankMemesFilter1 <- subset(dankMemesDataFrame, Date > "2018-12-01" & Date < "2018-12-07" & User != "SavageAxeBot
" & User != "KeepingDankMemesDank" & User != "AutoModerator" & User != "DankMemesMods"
             & User != "REEEEEEEEEpost" & User != "2Fdankmemes" & User != "deleted")
head(dankMemesFilter1)

averagescore = mean(dankMemesFilter1$Score)
averagescore = round(averagescore, 2)

dankMemesFilter1_top <- dankMemesFilter1 %>%
  dplyr::select(Comment) %>%
  unnest_tokens(word, Comment)

dankMemes_top <- dankMemesDataFrame %>%
  dplyr::select(Comment) %>%
  unnest_tokens(word, Comment)


#clean the top comments

dankmemesfilter1_top_clean <- dankMemesFilter1_top %>%
  anti_join(as.data.frame(stop_words)) %>%
  filter(!word == "shit") %>%
  filter(!word == "x200b") %>%
  filter(!word == "gt") %>%
  filter(!word == "https") %>%
  filter(!word == "it's") %>%
  filter(!word == "amp") %>%
  filter(!word == "fucking") %>%
  filter(!word == "fuck") %>%
  filter(!word == "1") %>%
  filter(!word == "www.reddit.com") %>%
  filter(!word == "2") %>%
  filter(!word == "3") %>%
  filter(!word == "removed") %>%
  filter(!word == "deleted") %>%
  filter(!word == "x200b")

dankmemes_top_clean <- dankMemes_top %>%
  anti_join(as.data.frame(stop_words)) %>%
  filter(!word == "shit") %>%
  filter(!word == "x200b") %>%
  filter(!word == "gt") %>%
  filter(!word == "https") %>%
  filter(!word == "it's") %>%
  filter(!word == "amp") %>%
  filter(!word == "fucking") %>%
  filter(!word == "fuck") %>%
  filter(!word == "1") %>%
  filter(!word == "www.reddit.com") %>%
  filter(!word == "2") %>%
  filter(!word == "3") %>%
  filter(!word == "removed") %>%
  filter(!word == "deleted") %>%
  filter(!word == "x200b")


DankMemesFilter1_top_words <- dankmemesfilter1_top_clean %>% 
  count(word, sort = TRUE) %>%
  top_n(200) %>%
  mutate(word = reorder(word, n))

head(DankMemesFilter1_top_words)

#word cloud

wordcloud(DankMemesFilter1_top_words$word, DankMemesFilter1_top_words$n, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.15, 
          colors = brewer.pal(8, "Dark2"))
#frequency graph
dankmemesfilter1_top_clean %>% 
  count(word, sort = TRUE) %>%
  top_n(30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Words",
       y = "Count",
       title = "Most commonly used english words in /r/dankmemes, December 1st-7th 2018")
