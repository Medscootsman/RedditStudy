#Load Libraries
#JSON
library(jsonlite)
#Plotting
library(ggplot2)

#Data folder location
dataFolder = "../../data/"
#Subreddit to parse
subreddit = "LateStageCapitalism_short"

#Loads JSON file
comments <- stream_in(file(paste(dataFolder,subreddit, ".json", sep = "")))

#Plot graph
plot <- ggplot(comments, aes(x=nchar(comments$body), y=comments$score))
plot <- plot + geom_point()
plot <- plot + stat_smooth(method="lm",col="red")
plot <- plot + xlab('Comment Length (Number of Characters)') + ylab('Score')
plot