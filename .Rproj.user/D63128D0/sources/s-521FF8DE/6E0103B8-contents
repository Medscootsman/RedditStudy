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
createGraphs <- function(data, subreddit, minimumCountLinks) {
  
  dataframe = as.data.frame(data.frame(User = data$author, 
                                       Date = as.POSIXct(data$created_utc, origin='1970-01-01'), 
                                       Comment = data$body,
                                       Score = data$score))
  
  print(count(dataframe))
  
  dataframe = subset(dataframe, User != "SavageAxeBot" & User != "KeepingDankMemesDank" & User != "AutoModerator" 
                     & User != "DankMemesMods" & User != "Colorizebot" & User != "BattleBusBot" & User != "MemeInvestor_bot" & User != "Transcribot"
                     & User != "Transcribot" & User != "commonmisspellingbot" & User != "TiltedTowersBot" & User != "stormshieldonebot"
                     & User != "WikiTextBot" & User != "RemindMeBot" & User != "thank_mr_skeltal_bot" & User != "societybot"
                     & User != "rick_rolled_bot" & User != "NoSkinBot" & User != "REEEEEEEEEpost" & User != "2Fdankmemes" & User != "deleted" & User != "GlobalOffensiveBot")
  print(count(dataframe))
  
  dataFilter1_top <- dataframe %>%
    dplyr::select(Comment) %>%
    unnest_tokens(word, Comment)
  
  #clean the top comments
  
  datafilter1_top_clean <- dataFilter1_top %>%
    filter(!word == "'") %>%
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
  
  dataFilter1_top_words <- datafilter1_top_clean %>% 
    count(word, sort = TRUE) %>%
    top_n(200) %>%
    mutate(word = reorder(word, n))
  
  #word cloud
  
  wordcloud(dataFilter1_top_words$word, dataFilter1_top_words$n, min.freq = 1,
            max.words = 200, random.order = FALSE, rot.per = 0.25, 
            colors = brewer.pal(6, "Dark2"))
  
  #freq graph
  datafilter1_top_clean %>% 
    count(word, sort = TRUE) %>%
    top_n(30) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    labs(x = "Words",
         y = "Count",
         title = paste("Most commonly used english words in ", subreddit))
  
  Datacomment_data_paired <- dataframe %>%
    dplyr::select(Comment) %>%
    mutate(Comment = removeWords(Comment, stop_words$word)) %>%
    mutate(Comment = gsub("\\brt\\b|\\bRT\\b", "", Comment)) %>%
    mutate(Comment = gsub("http://*", "", Comment)) %>%
    unnest_tokens(paired_words, Comment, token = "ngrams", n = 2)
  
  data_Comments_Separated <- Datacomment_data_paired %>%
    separate(paired_words, c("word1", "word2"), sep = " ") %>%
    count(word1, word2, sort = TRUE)
  
  head(data_Comments_Separated)
  
  data_Comments_Separated %>%
    filter(n >= minimumCountLinks) %>%
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
    labs(title = paste("Redditor Comment links on ", subreddit),
         subtitle = "December 2018",
         x = "", y = "") +
    theme_void()
}

data = stream_in(file("data/dankmemes.json"), pagesize = 5000)

dankmemesAvgScore = mean(data$score)
dankmemesTotal = count(data)

createGraphs(data, "/r/DankMemes", 1000)

data = stream_in(file("data/The_Donald.json"), pagesize = 5000)

thedonaldAvgScore = mean(data$score)

createGraphs(data, "/r/TheDonald", 500)

data = stream_in(file("data/politics.json"), pagesize = 5000)

politicsAvgScore = mean(data$score)

createGraphs(data, "/r/politics", 1000)

data = stream_in(file("data/GlobalOffensive.json"), pagesize = 5000)

GlobalOffensiveAvgScore = mean(data$score)

createGraphs(data, "/r/GlobalOffensive", 400)

data = stream_in(file("data/LateStageCapitalism.json"), pagesize = 5000)

LateStageCapAvgScore = mean(data$score)

createGraphs(data, "/r/LateStageCapitalism", 50)

data = stream_in(file("data/PUBATTLEGROUNDS.json"), pagesize = 5000)

pubgAvgScore = mean(data$score)

createGraphs(data, "/r/PUBATTLEGROUNDS", 200)

data = stream_in(file("data/FortNiteBR.json"), pagesize = 5000)

fortniteBRScore = mean(data$score)

createGraphs(data, "/r/fortniteBR", 650)

avgsData <- round(c(dankmemesAvgScore, fortniteBRScore, GlobalOffensiveAvgScore, LateStageCapAvgScore, politicsAvgScore, pubgAvgScore, thedonaldAvgScore), 2)
avgLabel <- c("DankMemes", "FortniteBR", "GlobalOffensive", "LateStageCap", "Politics", "PUBATTLEGROUNDS", "TheDonald")

averages.data <- data.frame(avgLabel, avgsData)

averages.data2 <- averages.data[order(averages.data[,2], decreasing = TRUE),]

barplot(averages.data2$avgsData,
        main = "Average point score in studied Subreddits (December 2018)",
        ylab = "Point average",
        names.arg = averages.data2$avgLabel,
        col = "LightBlue")
