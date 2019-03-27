#ULTIMATE REGRESSION SCRIPT

#Packages
install.packages('jsonlite')
install.packages('ggplot2')
install.packages('tidyverse')
install.packages('tidytext')
install.packages('anytime')


#Libraries
library(tidyverse)
library(jsonlite)
library(tidytext)
library(ggplot2)
library(anytime)


#Read the data in from a JSON file
setwd("D:\\Users\\Andrew\\Documents\\RStudio")
r_comments_all <- stream_in(file("politics_short.json"))


#Add a column which lists how far down the comment is in the comment chain

max_tier <- 10 #How far down the chain to go

tier_table <- r_comments_all%>%
  left_join(r_comments_all%>%select(id,parent_id)%>%
              mutate(parent_id2=parent_id)%>%mutate(parent_id=paste("t1_",id, sep=""))%>%select(-id))

for(val in 3:max_tier){
  col_name <- paste0("parent_id",val)
  col_name2 <- paste0("parent_id",val-1)
  tier_table <- tier_table%>%left_join(r_comments_all%>%select(id,parent_id)%>%mutate(!!quo_name(col_name) := parent_id)%>%mutate(!!quo_name(col_name2) := paste("t1_",id, sep=""))%>%select(-id,-parent_id))
}

tier_table <- tier_table%>%mutate(tier = max_tier+1)
for(val in 1:max_tier){
  ifelse(val == 1,var_name <- rlang::sym("parent_id"),var_name <- rlang::sym(paste0("parent_id",val)))
  tier_table <- tier_table%>%mutate(tier = ifelse(is.na(!!var_name),tier,ifelse(link_id == !!var_name,val,tier)))
}

tier_table <- tier_table%>%select(-parent_id2,-parent_id3,-parent_id4,-parent_id5,-parent_id6,-parent_id7,-parent_id8,-parent_id9,-parent_id10)
tier_table <- tier_table%>%filter(!tier == 11) #Select only the first ten chain comments


#Add column for time of day comment was made, and time since post was made

tier_table <- tier_table%>%
  left_join(tier_table%>%group_by(link_id)%>%summarize(min=min(created_utc)))%>%
  mutate(time_since_post=(created_utc-min))%>%
  select(-min)
  
tier_table <- tier_table%>%mutate(time_of_day=as.POSIXct(created_utc, origin="1970-01-01"))


#Pick columns for regression
tier_table <- tier_table%>%select(-edited, -gilded, -id, -is_submitter, -link_id, -no_follow, -parent_id, -permalink, -send_replies, -stickied, -subreddit)


#Regression summaries and correlations
summary(lm(score ~ time_since_post, tier_table))


#Regression Plots

#1: Interesting graphs based on time


#AVERAGE NUMBER OF COMMENTS PER DAY OF WEEK
numposts.vs.dayofweek <-tier_table%>%
  filter(tier==1)%>%
  mutate(Date=as.Date(time_of_day))%>%
  group_by(Date)%>%
  tally()%>%
  mutate(Day=format(Date,"%A"))%>%
  group_by(Day)%>%
  summarize(Mean = mean(n, na.rm=TRUE))

numposts.vs.dayofweek$Day <- factor(numposts.vs.dayofweek$Day, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
numposts.vs.dayofweek <- numposts.vs.dayofweek[order(numposts.vs.dayofweek$Day), ]

ggplot(numposts.vs.dayofweek, aes(x=Day, y=Mean)) + geom_point() + 
  labs(x="Day of Week", y="Average Number of Comments", title="Average Number of Comments vs Day of Week", subtitle = "")




#AVERAGE SCORE OF COMMENT PER DAY OF WEEK
score.vs.dayofweek <- tier_table%>%
  filter(tier==1)%>%
  mutate(Date=as.Date(time_of_day))%>%
  mutate(Day=format(Date,"%A"))%>%
  group_by(Day)%>%
  summarize(Mean = mean(score, na.rm=TRUE))

score.vs.dayofweek$Day <- factor(score.vs.dayofweek$Day, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
score.vs.dayofweek <- score.vs.dayofweek[order(score.vs.dayofweek$Day), ]

ggplot(score.vs.dayofweek, aes(x=Day, y=Mean)) + geom_point() + 
  labs(x="Day of Week", y="Average Comment Score", title="Average Comment Score vs Day of Week", subtitle = "")




#AVERAGE NUMBER OF COMMENTS PER HOUR IN A DAY
numposts.vs.timeofday <- tier_table%>%
  select(time_of_day,tier)%>%
  filter(tier==1)%>%
  mutate(Date=as.Date(time_of_day))%>%
  mutate(Day=format(time_of_day,"%A"))%>%
  mutate(Time=format(time_of_day,"%H"))%>%
  group_by(Date, Day, Time)%>%
  tally()

numposts.vs.timeofday <- numposts.vs.timeofday%>%group_by(Time, Day)%>%summarize(Mean = mean(n, na.rm=TRUE))

numposts.vs.timeofday$Day <- factor(numposts.vs.timeofday$Day, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
numposts.vs.timeofday <- numposts.vs.timeofday[order(numposts.vs.timeofday$Day), ]

numposts.vs.timeofday.plot <- ggplot(numposts.vs.timeofday, aes(x=Time, y=Mean, col=Day, group=Day)) + geom_line() + geom_point() + 
  labs(x="Time of the Day (24h)", y="Avg Number of Comments", title="Number of posts vs Time of Day", subtitle = "")
numposts.vs.timeofday.plot




#AVERAGE SCORE VS HOUR IN DAY FOR ALL DAYS IN WEEK
avgscore.vs.timeofday <- tier_table%>%
  select(time_of_day,tier,score)%>%
  filter(tier==1)%>%
  mutate(Date=as.Date(time_of_day))%>%
  mutate(Day=format(time_of_day,"%A"))%>%
  mutate(Time=format(time_of_day,"%H"))%>%
  group_by(Date, Day, Time)%>%
  select(-time_of_day,-tier)%>%
  summarize(Mean = mean(score, na.rm=TRUE))

avgscore.vs.timeofday <- avgscore.vs.timeofday%>%
  ungroup()%>%
  group_by(Time,Day)%>%
  summarize(Mean2 = mean(Mean, na.rm=TRUE))%>%
  arrange(Day)

avgscore.vs.timeofday$Day <- factor(avgscore.vs.timeofday$Day, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
avgscore.vs.timeofday <- avgscore.vs.timeofday[order(avgscore.vs.timeofday$Day), ]

avgscore.vs.timeofday.plot <- ggplot(avgscore.vs.timeofday, aes(x=Time, y=Mean2, col=Day, group=Day)) + geom_line() + geom_point() + 
  labs(x="Time of the Day (24h)", y="Avg Comment Score", title="Avg Comment Score vs Time of Day", subtitle = "Avg for All Weekdays for December 2018")
avgscore.vs.timeofday.plot





#TRYING SOMETHING NEW, CONVERT SCORE INTO HIGH OR LOW CATEGORIES
#Everyday of the week
avgscore.vs.timeofday2 <- tier_table%>%
  select(time_of_day,tier,score)%>%
  filter(tier==1)%>%
  mutate(Date=as.Date(time_of_day))%>%
  mutate(Day=format(time_of_day,"%A"))%>%
  mutate(Time=format(time_of_day,"%H"))%>%
  mutate(score=ifelse(score>50,1,0))%>%
  group_by(Date, Day, Time)%>%
  summarize(Mean = mean(score, na.rm=TRUE))

avgscore.vs.timeofday2 <- avgscore.vs.timeofday2%>%
  ungroup()%>%
  group_by(Time,Day)%>%
  summarize(Mean2 = mean(Mean, na.rm=TRUE))%>%
  arrange(Day)

avgscore.vs.timeofday2$Day <- factor(avgscore.vs.timeofday2$Day, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
avgscore.vs.timeofday2 <- avgscore.vs.timeofday2[order(avgscore.vs.timeofday2$Day), ]

avgscore.vs.timeofday.plot2 <- ggplot(avgscore.vs.timeofday2, aes(x=Time, y=Mean2*100, col=Day, group=Day)) + geom_line() + geom_point() + 
  labs(x="Time of the Day (24h)", y="% Successful Comments", title="% Successful Comments vs Time of Day", subtitle = "% Successful Comments for All Weekdays for December 2018")
avgscore.vs.timeofday.plot2


#Average of all days
avgscore.vs.timeofday3 <- tier_table%>%
  select(time_of_day,tier,score)%>%
  filter(tier==1)%>%
  mutate(Date=as.Date(time_of_day))%>%
  mutate(Day=format(time_of_day,"%A"))%>%
  mutate(Time=format(time_of_day,"%H"))%>%
  group_by(Date, Day, Time)%>%
  summarize(Mean = mean(score, na.rm=TRUE))

avgscore.vs.timeofday3 <- avgscore.vs.timeofday3%>%
  ungroup()%>%
  group_by(Time)%>%
  summarize(Mean2 = mean(Mean, na.rm=TRUE))

avgscore.vs.timeofday.plot3 <- ggplot(avgscore.vs.timeofday3, aes(x=Time, y=Mean2)) + geom_line() + geom_point() + 
  labs(x="Time of the Day (24h)", y="Avg Score", title="Avg Score vs Time of Day", subtitle = "Avg Score vs Time of day")
avgscore.vs.timeofday.plot3



test <- tier_table%>%
  select(created_utc,score)%>%
  mutate(time_of_day=format(as.POSIXct(created_utc, origin="1970-01-01"),"%H"))%>%
  group_by(time_of_day)%>%summarize(Mean = mean(score, na.rm=TRUE))%>%mutate(time_of_day=as.POSIXct(time_of_day,format='%H'))

test$test <- c(1:24)

ggplot(test, aes(x=test,y=Mean)) + geom_point() + geom_smooth()

#Avg score vs date
ggplot(tier_table%>%
         filter(tier==1)%>%
         mutate(time_of_day=as.Date(time_of_day))%>%
         group_by(time_of_day)%>%
         summarize(Mean = mean(score, na.rm=TRUE)), aes(x=time_of_day, y=Mean)) + geom_point()







#Select only top tier comments, select only up to one day, successful score > 50
ggplot(tier_table%>%
         filter(tier==1)%>%
         filter(time_since_post<86400)%>%
         filter(score>=50)%>%
         filter(time_of_day<'2018-12-02')%>%
         mutate(time_since_post=floor(time_since_post/60))%>%
         group_by(time_since_post)%>%
         summarize(Mean = mean(score, na.rm=TRUE)), aes(x=time_since_post, y=Mean)) + geom_point() +
  labs(x="Time Since Original Post (minutes)", y="Mean Score", title="Mean Score vs Time Since Post", subtitle = "")



#EXPORT PICS
png("avgscore-vs-timeofday.png", width=3000, height=2000, res=300)
avgscore.vs.timeofday.plot
dev.off()