#Libraries
library(tidyverse)
library(jsonlite)
library(tidytext)
library(ggplot2)
library(anytime)

#Reading dataset from JSON
setwd("D:\\Users\\Andrew\\Documents\\RStudio")
r_comments_all <- stream_in(file("unitedkingdom.json"))%>%filter(score>=0)%>%select(author, author_created_utc, body, created_utc, edited, gilded, id, is_submitter, link_id, no_follow, parent_id, permalink, score, send_replies, stickied, subreddit)

r_comments_all <- r_comments_all%>%filter(!body=="[deleted]",!body=="[removed]")

top_authors <- r_comments_all%>%select(author,score)%>%group_by(author)%>%count(score)%>%arrange(desc(n))

#Dankmemesbots r_comments_all <- r_comments_all%>%filter(!author=="AutoModerator", !author=="KeepingDankMemesDank", !author=="DankMemesMods", !author=="reeeeeeeeeepost")
#FortNiteBRbots r_comments_all <- r_comments_all%>%filter(!author=="AutoModerator",!author=="TiltedTowersBot",!author=="NoSkinBot", !author=="Niccorazi-", !author=="supremedecline")
r_comments_all <- r_comments_all%>%filter(!author=="InYourDomix",!author=="AutoModerator")

stream_out(r_comments_all, file("unitedkingdom_short.json"))

