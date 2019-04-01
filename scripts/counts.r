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
library(tidyverse)
library(anytime)
# coupled words analysis
library(widyr)
# plotting packages
library(igraph)
library(ggraph)
library(plotrix)

#DANK MEMES
data = stream_in(file("data/dankmemes.json"), pagesize = 5000)

dankmemesAvgScore = mean(data$score)

dankmemesTotal = count(data)

dataframe = data.frame(User = data$author,
                                PostTime = as.POSIXct(data$created_utc, origin='1970-01-01'),
                                Text = data$body,
                                Score = data$score,
                                UserBirthday = as.POSIXct(data$author_created_utc, origin='1970-01-01'))

data = stream_in(file("data/The_Donald.json"), pagesize = 5000)

thedonaldAvgScore = mean(data$score)

thedonaldTotal = count(data)

data = stream_in(file("data/politics.json"), pagesize = 5000)

politicsAvgScore = mean(data$score)

politicsTotal = count(data)

data = stream_in(file("data/GlobalOffensive.json"), pagesize = 5000)

GlobalOffensiveAvgScore = mean(data$score)

GlobalOffensiveTotal = count(data)

data = stream_in(file("data/LateStageCapitalism.json"), pagesize = 5000)

LateStageCapAvgScore = mean(data$score)

LateStageCapTotal = count(data)

data = stream_in(file("data/PUBATTLEGROUNDS.json"), pagesize = 5000)

pubgAvgScore = mean(data$score)

pubgTotal = count(data)

data = stream_in(file("data/FortNiteBR.json"), pagesize = 5000)

fortniteBRScore = mean(data$score)

fortniteBRTotal = count(data)

data = stream_in(file("data/unitedkingdom.json"), pagesize = 5000)

ukTotal = count(data)

ukAvgScore = mean(data$score)

data = stream_in(file("data/canada.json"))

canadaScore = mean(data$score)

canadaTotal = count(data)

data = stream_in(file("data/australia.json"))

1user <- subset(data, data$author == "RedderBarron")
australiaTotal = count(data)

ausScore = mean(data$score)

totalsData <- round(c(dankmemesTotal$n, fortniteBRTotal$n, GlobalOffensiveTotal$n, LateStageCapTotal$n, politicsTotal$n, pubgTotal$n, thedonaldTotal$n, ukTotal$n, australiaTotal$n, canadaTotal$n), 2)
totalsLabel <- c("DankMemes", "FortniteBR", "GlobalOffensive", "LateStageCap", "Politics", "PUBATTLEGROUNDS", "TheDonald", "UK", "Australia", "Canada")

totals.data <- data.frame(totalsLabel, totalsData)

totals.data2 <- totals.data[order(totals.data[,2], decreasing = TRUE),]

barplot(totals.data2$totalsData,
        main = "Total Comments per Subreddit (December 2018)",
        ylab = "Point average",
        names.arg = totals.data2$totalsLabel,
        col = "LightBlue")

#pie chart

pie(totals.data2$totalsData, totals.data2$totalsLabel, explode = 3, main = "Total Comments per Subreddit (December 2018)")
