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
library(stringr)

options(stringsAsFactors = FALSE)

comments = stream_in(file("data/GlobalOffensive.json"), pagesize = 50)

dataframe = as.data.frame(data)

comments_specifics = data.frame(User = comments$author,
                                Date = as.POSIXct(comments$created_utc, origin='1970-01-01'),
                                Text = comments$body,
                                Score = comments$score,
                                UserBirthday = as.POSIXct(comments$author_created_utc, origin='1970-01-01'))

comments_specifics = subset(comments_specifics, User != "SavageAxeBot" & User != "KeepingdataDank" & User != "AutoModerator" 
                            & User != "dataMods" & User != "BattleBusBot" & User != "MemeInvestor_bot" & User != "Transcribot"
                            & User != "Transcribot" & User != "commonmisspellingbot" & User != "TiltedTowersBot" & User != "stormshieldonebot"
                            & User != "WikiTextBot" & User != "RemindMeBot" & User != "thank_mr_skeltal_bot" & User != "societybot"
                            & User != "rick_rolled_bot" & User != "NoSkinBot")

meanscore = mean(comments_specifics$Score)

#subsetting experiment

comments_below0 <- subset(comments_specifics, Score < 0)

comments_low <- subset(comments_specifics, Score > 0 & Score < 50)

comments_high <- subset(comments_specifics, Score >= 50)


#turn it into a corpus
corpus_High = Corpus(VectorSource(comments_high$Text))

Corpus
#clean it up
corpus_High = tm_map(corpus_Low
                     , tolower)

corpus_High = tm_map(corpus_Low
                     , removePunctuation)

corpus_High= tm_map(corpus_High
                    , removeWords, stopwords("english"))

#stemming
corpus_High= tm_map(corpus_High
                    , stemDocument)

corpus_High= tm_map(corpus_High
                    , function(x) iconv(enc2utf8(x), sub = "byte"))

corpus_High= tm_map(corpus_High
                    , removeWords, c("im", "myðŸ", ",", "f", "u", "'", "½hel", "s", "t", "got", "day", "lol", "ã‚„", "x200b", "also", "its", "&qt", "gt", "just", "now", "like", "see", "know", "way", "get", "that", "use", "want", "can", "dont", "one", "say", "even", "thing", "go"))


#str_replace_all(body, "â", "")

#turn it into a corpus for low comments now
corpus_Low= Corpus(VectorSource(comments_below0$Text))


#clean it up
corpus_Low= tm_map(corpus_Low, tolower)

corpus_Low= tm_map(corpus_Low
                   , removePunctuation)

corpus_Low= tm_map(corpus_Low
                   , removeWords, stopwords("english"))

#stemming
corpus_Low= tm_map(corpus_Low
                   , stemDocument)

corpus_Low= tm_map(corpus_Low
                   , function(x) iconv(enc2utf8(x), sub = "byte"))

corpus_Low= tm_map(corpus_Low
                   , removeWords, c("im", "myðŸ", ",", "f", "u", "'", "½hel", "s", "t", "got", "day", "lol", "ã‚„", "x200b", "also", "its", "&qt", "gt", "just", "now", "like", "see", "know", "way", "get", "that", "use", "want", "can", "dont", "one", "say", "even", "thing", "go"))


#str_replace_all(body, "â", "")

#Low comments

term_count <- freq_terms(corpus_Low, 20)

term_count = subset(term_count, WORD != "ã‚„")

plot(term_count)

body_tdm <- TermDocumentMatrix(corpus_Low)

body_dtm <- DocumentTermMatrix(corpus_Low)

body_tdm2 <- removeSparseTerms(body_tdm, sparse = 0.9)

hc <- hclust(d = dist(body_tdm2, method = "euclidean"), method = "complete")
# Plot a dendrogram
plot(hc)


#high comments

term_count <- freq_terms(corpus_High, 20)

term_count = subset(term_count, WORD != "ã‚„")

plot(term_count)

body_tdm <- TermDocumentMatrix(corpus_High)

body_dtm <- DocumentTermMatrix(corpus_High)

body_tdm2 <- removeSparseTerms(body_tdm, sparse = 0.9)

hc <- hclust(d = dist(body_tdm2, method = "euclidean"), method = "complete")
# Plot a dendrogram
plot(hc)


#creating term freq

bodyMatrix <- as.matrix(body_tdm)

body_term_freq <- rowSums(bodyMatrix)

body_term_freq <- sort(body_term_freq, decreasing = T)

body_term_freq[1:10]

#Combine them

highcorp <- paste(corpus_High, collapse = "")

lowcorp <- paste(corpus_Low, collapse = "")

combined_corpus <- c(highcorp, lowcorp)

combined_corpus=Corpus(VectorSource(combined_corpus)) 

combined_corpus = tm_map(combined_corpus, tolower)

combined_tdm <- TermDocumentMatrix(combined_corpus)

combined_m = as.matrix(combined_tdm)

colnames(combined_m)=c("High score", "Low score")

#commonality cloud 
commonality.cloud(combined_m,
                  colors = "red",
                  max.words = 50)


common_words <- subset(combined_m, combined_m[, 1] > 0 & combined_m[, 2] > 0)

difference <- abs(common_words[, 1] - common_words[, 2])

common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 3],
                                   decreasing = T), ]
head(common_words)

top25_df <- data.frame(x = common_words[1:25, 1],
                       y = common_words[1:25, 2],
                       labels = rownames(common_words[1:25, ]))
# Make pyramid plot
pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels, 
             main = "Words in Common",
             gap = 1000,
             laxlab = NULL,
             raxlab = NULL, 
             unit = NULL,
             top.labels = c("High Score",
                            "Words",
                            "Low Score")
)


