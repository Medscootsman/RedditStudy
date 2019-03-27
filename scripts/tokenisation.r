## Load the required libraries
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
library(stringr)
library(irlba)
library(e1071)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(biclust)
library(igraph)
library(fpc)
library(RCA)

comments = stream_in(file("data/FortNiteBR.json"), pagesize = 50)

dataframe = as.data.frame(data)

comments_specifics = data.frame(User = comments$author,
                                Date = as.POSIXct(comments$created_utc, origin='1970-01-01'),
                                Text = comments$body,
                                Score = comments$score,
                                UserBirthday = as.POSIXct(comments$author_created_utc, origin='1970-01-01'))

meanscore = mean(comments_specifics$Score)

#subsetting experiment

comments_below0 <- subset(comments_specifics, Score < 0)

comments_low <- subset(comments_specifics, Score > 0 & Score < 50)

comments_high <- subset(comments_specifics, Score >= 50)

comments_high_text = as.character(comments_high$Text)

corpus_High = Corpus(VectorSource(comments_high$Text))

highscoreTokens <-  tokens(comments_high_text, what="word",
                           remove_numbers = TRUE, remove_punct=TRUE, remove_symbols=TRUE, remove_hyphens=TRUE)
