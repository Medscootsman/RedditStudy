#Load Libraries
#JSON
library(jsonlite)
#Plotting
library(ggplot2)

#Data folder location
dataFolder = "../../data/"

#Subreddits to parse
subreddits = c("australia_short", "canada_short", "dankmemes_short", "FortNiteBR_short", "funny_short", "GlobalOffensive_short", "LateStageCapitalism_short", "politics_short", "PUBATTLEGROUNDS_short", "RocketLeague_short", "tf2_short", "The_Donald_short", "unitedkingdom_short")

subs = c("LateStageCapitalism_short", "PUBATTLEGROUNDS_short")

#Loads JSON files
for(i in subs){
  #Load in the file at index
  comments <- stream_in(file(paste(dataFolder, i, ".json", sep = "")))
  
  #Add the scores to the previous scores
  if(exists("score")){
    score <- score + comments$score
  }
  else{
    score <- comments$score
  }
  
  #Remove comments to save memory for next loaded subreddit
  #rm(comments)
}

#Plot graph
plot <- ggplot(comments, aes(x=nchar(comments$body), y=comments$score))
plot <- plot + geom_point()
plot <- plot + stat_smooth(method="lm",col="red")
plot <- plot + xlab('Comment Length (Number of Characters)') + ylab('Score')
plot