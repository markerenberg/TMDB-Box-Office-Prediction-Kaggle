library(tidyverse)
library(stringi)
library(stringr)
library(jsonlite)


setwd("C:\\Users\\marke\\Downloads\\TMDB Kaggle")
train_raw = read.csv("train.csv",header=T)




# Returns a dataframe for each row in Cast columns
# Each dataframe has 3 columns:
# C1: Name, C2: Gender, C3: Order

cast = unlist(as.list(train_raw$cast))
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
      substr(y,str_locate(y,'order')[,2]+4,str_locate(y,'profile_path')-4)}))
  ))
})