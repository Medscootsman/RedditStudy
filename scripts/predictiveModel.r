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

data = stream_in(file("data/unitedkingdom_short.json"), pagesize = 50)

dataframe = as.data.frame(data)

createGraphs(data, "/r/unitedkingdom", 75)

