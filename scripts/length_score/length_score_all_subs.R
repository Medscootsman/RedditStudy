#Load Libraries
#JSON
library(jsonlite)
#Plotting
library(ggplot2)

#Data folder location
dataFolder = "../../data/"

#Subreddits to parse
subreddits = c("australia_short", "canada_short", "dankmemes_short", "FortNiteBR_short", "funny_short", "GlobalOffensive_short", "LateStageCapitalism_short", "politics_short", "PUBATTLEGROUNDS_short", "RocketLeague_short", "tf2_short", "The_Donald_short", "unitedkingdom_short")

#Loads JSON files
for(i in subreddits){
  #Load in the file at index
  comments <- stream_in(file(paste(dataFolder, i, ".json", sep = "")))
  print(i)
  
  #Add the rows to the previous rows
  if(exists("score")){
    score <- c(score, comments$score)
    body <- c(body, comments$body)
  }
  else{
    score <- comments$score
    body <- comments$body
  }

}

#Make comments now a data frame with score and body rows
comments <- data.frame(score, body, stringsAsFactors = FALSE)

#Plot graph
plot <- ggplot(comments, aes(x=nchar(comments$body), y=comments$score))
plot <- plot + geom_point()
plot <- plot + stat_smooth(method="lm",col="red")
plot <- plot + xlab('Comment Length (Number of Characters)') + ylab('Score')
plot