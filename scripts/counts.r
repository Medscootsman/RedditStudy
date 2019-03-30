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
library(plotrix)

data = stream_in(file("data/dankmemes.json"), pagesize = 5000)

dankmemesAvgScore = mean(data$score)

dankmemesTotal = count(data)

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

totalsData <- round(c(dankmemesTotal$n, fortniteBRTotal$n, GlobalOffensiveTotal$n, LateStageCapTotal$n, politicsTotal$n, pubgTotal$n, thedonaldTotal$n, ukTotal$n), 2)
totalsLabel <- c("DankMemes", "FortniteBR", "GlobalOffensive", "LateStageCap", "Politics", "PUBATTLEGROUNDS", "TheDonald", "UK")

totals.data <- data.frame(totalsLabel, totalsData)

totals.data2 <- totals.data[order(totals.data[,2], decreasing = TRUE),]

barplot(totals.data2$totalsData,
        main = "Total Comments per Subreddit (December 2018)",
        ylab = "Point average",
        names.arg = totals.data2$totalsLabel,
        col = "LightBlue")

#pie chart

pie3D(totals.data2$totalsData, totals.data2$totalsLabel, explod = 0.1, main = "Total Comments per Subreddit (December 2018)")
