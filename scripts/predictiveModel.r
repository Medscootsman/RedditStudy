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
library(GGally)
library(RWeka)
library(qdap)
library(plotrix)
library(reshape2)
library(quanteda)
library(ggthemes)
library(dendextend)
library(ggthemes)

options(stringsAsFactors = FALSE)

comments = stream_in(file("data/unitedkingdom_short.json"), pagesize = 50)

dataframe = as.data.frame(data)

comments_specifics = data.frame(User = comments$author,
                                Date = as.POSIXct(comments$created_utc, origin='1970-01-01'),
                                Text = comments$body,
                                Score = comments$score,
                                UserBirthday = as.POSIXct(comments$author_created_utc, origin='1970-01-01'))

meanscore = mean(comments_specifics$Score)

#subsetting experiment

comments_above30 <- subset(comments_specifics, Score >= 30)

comments_low <- subset(comments_specifics, Score > 0 & score < 30)

#turn it into a corpus
body = Corpus(VectorSource(comments_above50$Text))


#clean it up
body = tm_map(body, tolower)

body = tm_map(body, removePunctuation)

body = tm_map(body, removeWords, stopwords("english"))

body = tm_map(body, removeWords, c("im", "myðŸ", "½hel", "x200b", "also", "its", "&qt", "gt", "just", "now", "like", "see", "know", "way", "get", "that", "use", "want", "can", "dont", "one", "say", "even", "thing", "go"))

#stemming
body = tm_map(body, stemDocument)

body = tm_map(body, function(x) iconv(enc2utf8(x), sub = "byte"))

term_count <- freq_terms(body, 20)

plot(term_count)

body_tdm <- TermDocumentMatrix(body)

body_dtm <- DocumentTermMatrix(body)


#creating term freq

