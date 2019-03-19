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
options(stringsAsFactors = FALSE)

comments = stream_in(file("data/unitedkingdom_short.json"), pagesize = 50)

dataframe = as.data.frame(data)

comments_specifics = data.frame(User = comments$author,
                                Date = as.POSIXct(comments$created_utc, origin='1970-01-01'),
                                Score = comments$score,
                                UserBirthday = as.POSIXct(comments$author_created_utc, origin='1970-01-01'))

