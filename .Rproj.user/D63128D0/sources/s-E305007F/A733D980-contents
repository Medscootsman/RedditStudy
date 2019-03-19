#Libraries
library(tidyverse)
library(jsonlite)
library(tidytext)
library(ggplot2)
library(anytime)

#Reading dataset from JSON
setwd("C:\\Users\\andrew\\Documents\\R\\Coursework")
r_comments_all <- stream_in(file("politics_short0.json"))%>%select(score,body,id,link_id,parent_id,permalink)%>%filter(nchar(body)<100)

nrow(r_comments_all)

summary(lm(1:47295 ~ score,tier_table%>%filter(tier==1)%>%select(score)%>%filter(score>0)%>%arrange(desc(score))))
ggplot(tier_table%>%filter(tier==1)%>%select(score)%>%filter(score>0)%>%arrange(desc(score)),aes(x=1:47295,y=score)) + 
  geom_point() + xlim(0,100) + geom_smooth(formula="y~log(x)+x", method="lm")

#Adding a column for which tier the comments are in
tier_table <- r_comments_all%>%
  left_join(r_comments_all%>%select(id,parent_id)%>%
              mutate(parent_id2=parent_id)%>%mutate(parent_id=paste("t1_",id, sep=""))%>%select(-id))

max_tier <- 10

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
tier_table <- tier_table%>%filter(!tier == 11)
tier_table <- tier_table%>%select(-parent_id, -link_id)



#PLOTTING COMMENT TIER VS MEAN SCORE
plot.lm <- lm(Mean ~ I(tier^-2)*tier, tier_table%>%group_by(tier)%>%summarize(Mean = mean(score, na.rm=TRUE)))
resid(plot.lm)

data.frame("tier"=2:11, "residual"=resid(plot.lm))

ggplot(data.frame("tier"=1:10, "residual"=resid(plot.lm)), aes(x=tier, y=residual)) + geom_point() + geom_abline(slope=0,intercept=0,color="blue")

summary(lm(Mean ~ I(tier^-2)*tier, tier_table%>%group_by(tier)%>%summarize(Mean = mean(score, na.rm=TRUE))))
ggplot(tier_table%>%group_by(tier)%>%summarize(Mean = mean(score, na.rm=TRUE)), aes(x=tier, y=Mean)) + geom_point(size=1) + geom_smooth(formula = y ~ I(x^-2)*x, method = "lm",show.legend = TRUE) + labs(x="Comment Tier", y="Mean Score", title="Mean Score vs Comment Tier", subtitle = "Using y ~ I(x^-2)*x model")

  #CONCLUSION
  ##DECENT FIT, GRAPH OF RESIDUALS INDICATES MISSING PREDICTOR

#PLOTTING SCORE VS TIME SINCE ORIGINAL POST 

meanscore.timeafterpost <- tier_table%>%
  select(score,link_id,created_utc,tier)%>%
  filter(tier==1)%>%
  left_join(tier_table%>%
              filter(tier==1)%>%
              group_by(link_id)%>%
              summarize(min=min(created_utc)))%>%
  mutate(dif=(created_utc-min))%>%
  mutate(dif = floor(dif/300))%>%
  group_by(dif)%>%
  summarize(Mean = mean(score, na.rm=TRUE))
  #group_by(dif)%>%
  #summarize(Mean = mean(Mean, na.rm=TRUE))



lm.score.time <- lm(Mean ~ I(dif^-2)/dif, meanscore.timeafterpost%>%filter(dif>0))
resid(lm.score.time)

ggplot(data.frame("tier"=1:956, "residual"=resid(lm.score.time)), aes(x=tier, y=residual)) + geom_point() + geom_abline(slope=0,intercept=0,color="blue")
ggplot(meanscore.timeafterpost%>%filter(dif>0),aes(x=dif,y=Mean)) + 
  geom_point() + xlim(0,288) + 
  geom_smooth(formula="y~(I(x^-2))/x + x", method="lm") + 
  labs(x="Time Since Original Post (5 minute intervals)", y="Mean Score", title="Mean Score vs Time Since Post", subtitle = "Using y~I(x^-2)/x + x model")




#PLOTTING SCORE AGAINST COMMENT LENGTH
score.vs.length <- tier_table%>%select(score,body,tier)%>%filter(tier==1)%>%mutate(len=nchar(body))%>%mutate(len=floor(len/100))%>%select(-body)%>%group_by(len)%>%summarize(Mean = mean(score, na.rm=TRUE))
summary(lm(Mean ~ len,score.vs.length))

resid(lm(Mean ~ len, score.vs.length))
ggplot(score.vs.length,aes(x=len,y=Mean)) + geom_point() + geom_smooth(formula="y ~ x", method="lm") + xlim(0,20)

#Seems to be no correlation


#create list of top 100 words
#Add new column to frame with percent of comment with top words?
#plot against score

#SUPPLEMENT STOP WORDS WITH MY OWN WORDS
stopWords <- stop_words
stop_words2 <- c("http","https","amp","x200b","gt")
stopWords2 <- rbind(stopWords, data.frame("word"=stop_words2,"lexicon"="test"))

top_words <- tier_table%>%filter(tier==1)%>%select(body)%>%unnest_tokens(word,body)%>%filter(!word %in% stopWords2$word)%>%filter(!grepl("[.''_0123456789]",word))%>%group_by(word)%>%count(word)
top_words <- top_words%>%filter(n>260)%>%arrange(desc(n))%>%select(-n)

top.word.counts <- tier_table%>%
  filter(tier==1)%>%select(-tier)%>%
  unnest_tokens(word,body)%>%inner_join(top_words)%>%group_by(id)%>%count(id)

combined <- tier_table%>%filter(tier==1)%>%left_join(top.word.counts)
combined <- combined%>%mutate(n=ifelse(is.na(n),0,n))
combined <- combined%>%mutate(num=n/nchar(body))
combined <- combined%>%group_by(n)%>%summarize(Mean = mean(score, na.rm=TRUE))

summary(lm(Mean~I(n^2)+n,combined))
ggplot(combined,aes(x=n,y=Mean)) + geom_point() + xlim(0,6) + geom_smooth(formula="y~I(x^2)+x", method="lm")

#EXPORT PICS
png("meanscore_vs_time2.png", width=3000, height=2000, res=300)
ggplot(tier_table%>%group_by(tier)%>%summarize(Mean = mean(score, na.rm=TRUE)), aes(x=tier, y=Mean)) + geom_point(size=1) + geom_smooth(formula = y ~ I(x^-2)*x, method = "lm",show.legend = TRUE) + labs(x="Comment Tier", y="Mean Score", title="Mean Score vs Comment Tier", subtitle = "Using y ~ I(x^-2)*x model")
dev.off()
