#-------------------------------
# TMDB Kaggle Competition
# Mark Erenberg
#-------------------------------


library(data.table)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(tidyverse)
library(stringi)
library(lubridate)
library(scales)
library(DT)
library(dplyr)
library(stringr)
library(jsonlite)

setwd("C:\\Users\\marke\\Downloads\\TMDB Kaggle")
train_raw <- read.csv("train.csv",header = TRUE,stringsAsFactors = F)
df = train_raw

cast = unlist(as.list(df$cast))
keyw = df$Keywords
crew = df$crew

# Returns a dataframe for each row, with 3 columns:
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

# To count how many times a cast member appears in a movie:
cast_members = unique(unlist(lapply(cast_dfs,function(x){x[,1]})))
cast_tally = unlist(lapply(cast_members,function(actor){
  sum(unlist(lapply(cast_dfs,function(df){
    sum(rowSums(df == actor))
  })
  ),na.rm=T)}
))

cast = as.data.frame(cbind(cast_members,as.numeric(cast_tally)),stringsAsFactors = F)
colnames(cast) = c('cast_members','cast_tally')
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

# Get list of top 300 actors
topactors = cast[1:301,1]
cast[cast$members == ''] 


