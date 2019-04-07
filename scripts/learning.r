#ULTIMATE REGRESSION SCRIPT

#Packages
install.packages('jsonlite')
install.packages('ggplot2')
install.packages('tidyverse')
install.packages('tidytext')
install.packages('anytime')

install.packages('keras')
install.packages('dplyr')
install.packages('purrr')


#Libraries
library(tidyverse)
library(jsonlite)
library(tidytext)
library(ggplot2)
library(anytime)
library(stringr)

library(keras)
install_keras()
library(dplyr)
library(purrr)

#-----FOLLOWING A TUTORIAL-----

imdb <- dataset_imdb(num_words = 10000)

word_index <- dataset_imdb_word_index()

word_index_df <- data.frame(
  word = names(word_index),
  idx = unlist(word_index, use.names = FALSE),
  stringsAsFactors = FALSE
)

# The first indices are reserved  
word_index_df <- word_index_df %>% mutate(idx = idx + 3)
word_index_df <- word_index_df %>%
  add_row(word = "<PAD>", idx = 0)%>%
  add_row(word = "<START>", idx = 1)%>%
  add_row(word = "<UNK>", idx = 2)%>%
  add_row(word = "<UNUSED>", idx = 3)

word_index_df <- word_index_df %>% arrange(idx)

decode_review <- function(text){
  paste(map(text, function(number) word_index_df %>%
              filter(idx == number) %>%
              select(word) %>% 
              pull()),
        collapse = " ")
}

c(train_data, train_labels) %<-% imdb$train
c(test_data, test_labels) %<-% imdb$test

paste0("Training entries: ", length(train_data), ", labels: ", length(train_labels))
length(train_data[[1]])
length(train_data[[2]])

train_data <- pad_sequences(
  train_data,
  value = word_index_df %>% filter(word == "<PAD>") %>% select(idx) %>% pull(),
  padding = "post",
  maxlen = 256
)

test_data <- pad_sequences(
  test_data,
  value = word_index_df %>% filter(word == "<PAD>") %>% select(idx) %>% pull(),
  padding = "post",
  maxlen = 256
)

#BUILD THE NEURAL NETWORK

vocab_size <- 10000

model <- keras_model_sequential()
model %>% 
  layer_embedding(input_dim = vocab_size, output_dim = 16) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% summary()



#ADD LOSS FUNCTION AND OPTIMIZER

model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = 'accuracy'
)



#CREATE A VALIDATION SET

x_val <- train_data[1:10000, ]
partial_x_train <- train_data[10001:nrow(train_data), ]

y_val <- train_labels[1:10000]
partial_y_train <- train_labels[10001:length(train_labels)]



#TRAIN THE MODEL

history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 40,
  batch_size = 512,
  validation_data = list(x_val, y_val),
  verbose=1
)



#EVALUATE THE MODEL

results <- model %>% evaluate(test_data, test_labels)
results

plot(history)

#-----THE REAL THING-----

setwd("D:\\Users\\Andrew\\Documents\\RStudio")
r_comments_all <- stream_in(file("politics_short.json"))

r_comments_all%>%mutate(len = nchar(body))%>%summarize(Mean = mean(len, na.rm=TRUE))

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
tier_table <- tier_table%>%filter(tier==1)

#Creating a comments set from the same time each day
tier_table2 <- tier_table%>%mutate(time = format(time_of_day,"%H:%M:%S"))%>%filter(time_since_post<3600,time>"11:00:00",time< "23:00:00")
comments.all <- tier_table2%>%select(body,score)%>%mutate(Label=ifelse(score>=20,1,0))%>%select(-score)

#Creating a comment set which includes post name
post.names <- tier_table%>%left_join(tier_table%>%mutate(post=substr(permalink,29,nchar(permalink)-9))%>%select(post,id)%>%mutate(post = str_replace_all(post,"[[:punct:] 0123456789]+"," ")))
comments.all <- post.names%>%select(body,post,score)%>%mutate(Label=ifelse(score>=50,1,0))%>%select(-score)

#comments.all%>%group_by(Label)%>%tally()%>%mutate(percent=(n/1506774)*100) #Checking to see percent high to low, you need a score of 3 to make it 50/50 :P

comments.high <- comments.all%>%filter(Label==1)%>%head(20000)
comments.low <- comments.all%>%filter(Label==0)%>%head(20000)

comments.combined <- union_all(comments.high,comments.low)
comments.combined.labels <- comments.combined$Label

comments.combined <- comments.combined%>%mutate(body = tolower(body))%>%mutate(body = str_replace_all(body,"[[:punct:] 0123456789]+"," "))
comments.combined <- comments.combined%>%mutate(post = tolower(post))%>%mutate(post = str_replace_all(post,"[[:punct:] 0123456789]+"," "))
comments.combined <- comments.combined%>%select(-Label)

comments.combined <- comments.combined%>%mutate(body = str_replace_all(body,"\n",""))
comments.combined <- comments.combined%>%mutate(body = str_replace(gsub("\\s+", " ", str_trim(body)), "B", "b"))
comments.combined <- comments.combined%>%mutate(post = str_replace_all(post,"\n",""))
comments.combined <- comments.combined%>%mutate(post = str_replace(gsub("\\s+", " ", str_trim(post)), "B", "b"))

comments.combined$combine <- paste(comments.combined$post,comments.combined$body)

word.index <- comments.all%>%mutate(body = str_replace_all(body,"[[:punct:] 0123456789]+"," "))%>%unnest_tokens(word,body)%>%group_by(word)%>%tally()%>%arrange(desc(n))%>%head(50000)%>%select(-n)


test.frame <- str_split(comments.combined$combine, " ")

#CREATE MATRICES
test.matrix <- matrix(0, 40000, 128)
test.matrix2 <- matrix("", 40000, 128)

#EXPORT DATA TO BE PROCESSED IN C++
for(va in 1:40000){
  for(va2 in 1:ifelse(length(test.frame[[va]])>128,128,length(test.frame[[va]]))){
    test.matrix2[va,va2]=test.frame[[va]][va2]
  }
}
write.table(test.matrix2, file="data.txt", row.names=FALSE, col.names=FALSE, na="",quote = FALSE, sep=",") #Export word matrix
write.table(word.index, file="dataword.txt", row.names=FALSE, col.names=FALSE, na="") #Export dictionary

#IMPORT DATA FROM C++
comment.matrix.integer <- read.table("post_and_comments.txt",sep=",",header=FALSE)
for(val in 1:40000){
  for(val2 in 1:128){
    test.matrix[val,val2]=comment.matrix.integer[val,val2]
  }
}

vocab_size <- 10001

model <- keras_model_sequential()
model %>% 
  layer_embedding(input_dim = vocab_size, output_dim = 64)%>%
  layer_global_average_pooling_1d()%>%
  layer_dropout(0.7)%>%
  layer_dense(units = 64, activation = "relu")%>%
  layer_dropout(0.7)%>%
  layer_dense(units = 64, activation = "relu")%>%
  layer_dropout(0.7)%>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% summary()

#ADD LOSS FUNCTION AND OPTIMIZER

model %>% compile(
  optimizer = 'rmsprop',
  loss = 'binary_crossentropy',
  metrics = 'accuracy'
)

#Randomize matrix rows
set.seed(123)
test.matrix3 <- test.matrix[sample(nrow(test.matrix)),]
set.seed(123)
comments.combined.labels2 <- comments.combined.labels[sample(length(comments.combined.labels))]

#CREATE A VALIDATION SET

x_val <- test.matrix3[1:4000,]
partial_x_train <- test.matrix3[4001:38000,]

y_val <- comments.combined.labels2[1:4000]
partial_y_train <- comments.combined.labels2[4001:38000]

x_test <- test.matrix3[38001:40000,]
y_test <- comments.combined.labels2[38001:40000]


#TRAIN THE MODEL

history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 20,
  batch_size = 128,
  validation_data = list(x_val, y_val),
  verbose=1
)



#EVALUATE THE MODEL
count <- 0
for(val in 1:2000){
  if(y_test[val] == 1){
    count <- count + 1
  }
}
count
x_test

results <- model %>% evaluate(x_test, y_test)
results

plots <- plot(history)
plots <- ggplot(data.frame(history)%>%mutate(DataMetric = paste(metric,data)),aes(x=epoch,y=value,group=DataMetric,colour=DataMetric)) + geom_point() + geom_line() +
  labs(x="epoch", y="value", title="History of model fit", subtitle = "using post + comment data")

#EXPORT PICS
png("keras_training.png", width=3000, height=2000, res=300)
plots
dev.off()