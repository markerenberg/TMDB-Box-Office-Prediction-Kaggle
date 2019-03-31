#-------------------------------
# TMDB Kaggle Competition
# Mark Erenberg
#-------------------------------


library(data.table)
library(ggplot2)
library(tidyverse)
library(stringi)
library(lubridate)
library(randomForest)
library(scales)
library(DT)

setwd("C:\\Users\\marke\\Downloads\\TMDB Kaggle")
train_raw <- read.csv("train.csv",header = TRUE,stringsAsFactors = F)
df = train_raw
n = nrow(df)

cast = unlist(as.list(df$cast))

# Creating a dataframe for each row, with 3 columns:
# C1: Name, C2: Gender, C3: Order
cast_lists = lapply(cast,function(x){unlist(as.list(strsplit(x,'},')[[1]]))})
cast_dfs = lapply(cast_lists,function(x){
  as.data.frame(cbind(
    # extract name
    unlist(lapply(x,function(y){
      substr(y,str_locate(y,'name')[,2]+5,str_locate(y,'order')-5)})), 
    # extract gender
    unlist(lapply(x,function(y){
      substr(y,str_locate(y,'gender')[,2]+4,str_locate(y,'gender')[,2]+4)})),
    # extract order
    unlist(lapply(x,function(y){
      substr(y,str_locate(y,'order')[,2]+4,str_locate(y,'profile_path')-4)}))),
    stringsAsFactors = F)
})

# Counting how many times a cast member appears in a movie:
cast_members = unique(unlist(lapply(cast_dfs,function(x){x[,1]})))
cast_tally = unlist(lapply(cast_members,function(actor){
  sum(unlist(lapply(cast_dfs,function(df){
    sum(rowSums(df == actor))
  })
  ),na.rm=T)}
))

# Putting cast_member and cast_tally in a dataframe, sort by cast_tally DESC
cast = as.data.frame(cbind(cast_members,as.numeric(cast_tally)),stringsAsFactors = F)
names(cast) = c('cast_members','cast_tally')
cast$cast_tally = as.numeric(cast$cast_tally)
cast = cast[order(-cast_tally),]

# Bar Graph of Top Actors
ggplot(data=cast,aes(x=cast_members,y=cast_tally))+
  geom_bar(data=cast, 
           aes(fill=cast_members),stat='identity',show.legend=F)+
  ggtitle('Actor Count')+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab('Count')+
  xlab('Actor')

# Get list of top 300 actors (after removing empty strings)
if(length(which(cast$cast_members == '')) != 0){
  cast = cast[-which(cast$cast_members == ''),]}
topactors = cast[1:301,1]

# Create a dataframe that contains dummy variables for every actor's
#   apperance in a movie
actors_df = as.data.frame(seq(1:n))
for(actor in topactors){
  old_names = names(actors_df)
  actor_count = unlist(lapply(cast_dfs,function(df){
    sum(rowSums(df == actor),na.rm=T)
  }))
  actors_df = cbind(actors_df,actor_count)
  names(actors_df) = c(old_names,actor)
}


# Get Male to Female ratio
actors_df$males = unlist(lapply(cast_dfs,function(df){sum(df$V2 == 2,na.rm=T)}))
actors_df$females = unlist(lapply(cast_dfs,function(df){sum(df$V2 == 1,na.rm=T)}))
actors_df$gender_ratio = ifelse(is.finite(actors_df$males/actors_df$females),
                                actors_df$males/actors_df$females, 0)

# M:F ratio in top 10 actors
actors_df$top10males = unlist(lapply(cast_dfs,function(df){sum(df$V2[1:11] == 2,na.rm=T)}))
actors_df$top10females = unlist(lapply(cast_dfs,function(df){sum(df$V2[1:11] == 1,na.rm=T)}))
actors_df$top10gender_ratio = ifelse(
  is.finite(actors_df$top10males/actors_df$top10females),
  actors_df$top10males/actors_df$top10females, 
  0)



# Create a list for each movie, containing all keywords for that movie
keyw = df$Keywords
key_lists = lapply(keyw,function(x){unlist(as.list(strsplit(x,'},')[[1]]))})
key_df = lapply(key_lists,function(lst){
  # extract names
  unlist(lapply(lst,function(strng){
    # if string in list is last string, remove the '}]"' characters
    ifelse(match(strng,lst)==length(lst),
           substr(strng,str_locate(strng,'name')[,2]+5,nchar(strng)-3),     
           substr(strng,str_locate(strng,'name')[,2]+5,nchar(strng)-1))
  }
  ))
})

# To count how many times a keyword appears in a movie:
unique_keynames = unique(unlist(key_df))
keyword_tally = unlist(lapply(unique_keynames,function(key){
  sum(unlist(lapply(key_df,function(lst){
    sum(lst == key, na.rm=T)
  })
  ),na.rm=T)}
))

# Create a dataframe containing each keyword and the keyword_tally
keywords = as.data.frame(cbind(unique_keynames,keyword_tally),
                         stringsAsFactors = F)
names(keywords) = c('keyword','keyword_tally')
keywords = keywords[order(-keyword_tally),]


# Get list of top 300 keywords (after removing empty strings)
if(length(which(keywords$keyword == '')) != 0){
  keywords = keywords[-which(keywords$keyword == ''),]  
}
topkeys = keywords[1:301,1]

# Create a dataframe that contains dummy variables for every keyword's
#   apperance in a movie description
keyword_df = as.data.frame(seq(1:n))
for(key in topkeys){
  old_names = names(keyword_df)
  key_count = unlist(lapply(key_df,function(lst){
    sum(lst == key,na.rm=T)
  }))
  keyword_df = cbind(keyword_df,key_count)
  names(keyword_df) = c(old_names,key)
}

# Append Cast and Keyword variables to train/test dataframe
cast_key_vars = cbind(actors_df[,-1],keyword_df[,-1])
df = cbind(df,cast_key_vars)