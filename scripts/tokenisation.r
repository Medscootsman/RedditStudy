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

comments = stream_in(file("data/LateStageCapitalism.json"), pagesize = 50)

dataframe = as.data.frame(data)

meanscore = mean(comments$score)

comments_specifics = data.frame(User = comments$author,
                                Date = as.POSIXct(comments$created_utc, origin='1970-01-01'),
                                Text = comments$body,
                                Score = comments$score,
                                UserBirthday = as.POSIXct(comments$author_created_utc, origin='1970-01-01'))

comments_specifics$Popularity = ifelse(comments_specifics$Score > meanscore, "High", "Low")

comments_specifics = subset(comments_specifics, User != "SavageAxeBot" & User != "KeepingdataDank" & User != "AutoModerator" 
                            & User != "dataMods" & User != "BattleBusBot" & User != "MemeInvestor_bot" & User != "Transcribot"
                            & User != "Transcribot" & User != "commonmisspellingbot" & User != "TiltedTowersBot" & User != "stormshieldonebot"
                            & User != "WikiTextBot" & User != "RemindMeBot" & User != "thank_mr_skeltal_bot" & User != "societybot"
                            & User != "rick_rolled_bot" & User != "NoSkinBot")

comments_below0 <- subset(comments_specifics, Score < 0)

comments_low <- subset(comments_specifics, Score > 0 & Score < 50)

comments_high <- subset(comments_specifics, Score >= 50)

comments_high_text = as.character(comments_specifics$Text)

corpus_High = Corpus(VectorSource(comments_high$Text))

#tokenisation process

highscoreTokens <-  tokens(comments_high_text, what="word",
                           remove_numbers = TRUE, remove_punct=TRUE, remove_symbols=TRUE, remove_hyphens=TRUE)

#data prep

highscoreTokens <- tokens_tolower(highscoreTokens)

filterwords <- c("is", "is", "then", "also", "and", "im", "lol")

highscoreTokens <- tokens_select(highscoreTokens, stopwords(), selection = "remove")

highscoreTokens=tokens_remove(highscoreTokens, filterwords)

highscoreTokens=tokens_wordstem(highscoreTokens, language = "english")

highscoreTokens = tokens_ngrams(highscoreTokens, n = 1:2)

highscoretokensDFM = dfm(highscoreTokens, tolower = FALSE)

tokensSparse <- convert(highscoretokensDFM, "tm")

tm::removeSparseTerms(tokensSparse, 0.5)

dfm_trim(highscoretokensDFM, min_docfreq = 0.3)

x=dfm_trim(highscoretokensDFM, sparsity = 0.99)

df = convert(x, to="data.frame")

highscoreTokensDF = cbind(comments_specifics$Popularity, df)

highscoreTokensDF$minimum_wage = NULL

head(highscoreTokensDF)

names(highscoreTokensDF)[names(highscoreTokensDF) == "comments_specifics$Popularity"] <- "Popularity"
names(highscoreTokensDF)=make.names(names(highscoreTokensDF))

highscoreTokensDF$Popularity = factor(highscoreTokensDF$Popularity)

tree = rpart(formula = Popularity ~ ., data = highscoreTokensDF, method = "class",
             control = rpart.control(minsplit = 20, minbucket = round(minsplit/3), cp = 0.01,  
                                     maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                                     surrogatestyle = 0, maxdepth = 30))

printcp(tree)

plotcp(tree)

bestcp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
bestcp
ptree=prune(tree,cp=bestcp)
rpart.plot(ptree,cex = 0.6)
prp(ptree, faclen = 0, cex = 0.5, extra = 2)

#random forest

library(randomForest)

RF = randomForest(Popularity~., data=highscoreTokensDF)

varImpPlot = (RF)
