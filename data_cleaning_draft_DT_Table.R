##This is a draft code generally separating some json columns, and put into DT tables for easy glimpse purpose

library(tidyverse)
library(DT)
library(dplyr)
library(stringr)
#File name and path need to be modified accordingly
train <- read.csv(file="Desktop/ST4248/project/train.csv",header = TRUE)
train %>% glimpse()

##cast
cast_list <- list()
for (i in seq_along(train$cast)) {
  cast_list[[i]] <- train$cast[[i]] %>%
    str_extract_all('(?<=\\{).*?(?=\\})') %>%  #extract everything between {}
    str_split("(?<=[:digit:]|[:punct:]), ",
              n=Inf,simplify = TRUE) %>%       #split on ","
    str_extract_all('(?<=\\:[:space:]).*') %>% #get the part after the semicolon
    str_replace_all("'|\"","") %>% #clean the unwanted punctuation
    matrix( ncol = 8,  byrow = TRUE,dimnames=list(c(),
    c("cast_id","character","credit_id","gender","id",
    "name","order","profile_path"))) %>% #convert to matrix
    as_tibble(stringsAsFactors = FALSE)#convert the matrix to tibble
}

# Name the list
names(cast_list) <- c(1:3000) #name the list with the sequential number of the list element
# Create df with a column that identifies each movie
cast_df <- bind_rows(cast_list, .id = 'movie_id')#create single df with a column that identifies the movies
cast_df %>% 
  head(10) %>% 
  DT::datatable()

## Genres
genres_list <- list()
for (i in seq_along(train$genres)) {
  genres_list[[i]] <- train$genres[[i]] %>%
    str_extract_all('(?<=\\{).*?(?=\\})') %>% 
    str_split("(?<=[:digit:]|[:punct:]), ",n=Inf,simplify = TRUE) %>% 
    str_extract_all('(?<=\\:[:space:]).*') %>% 
    str_replace_all("[:punct:]","") %>% 
    matrix( ncol = 2,  byrow = TRUE,dimnames=list(c(), c("id","name"))) %>% 
    as_tibble(stringsAsFactors = FALSE)
}
names(genres_list) <- c(1:3000)
genres_df <- bind_rows(genres_list, .id = 'movie_id')
genres_df %>% 
  head(10) %>% 
  DT::datatable()

## Production companies
production_companies_list <- list()
for (i in seq_along(train$production_companies)) {
  production_companies_list[[i]] <- train$production_companies[[i]] %>%
    str_extract_all('(?<=\\{).*?(?=\\})') %>% 
    str_split("(?<=[:digit:]|[:punct:]), ",n=Inf,simplify = TRUE) %>% 
    str_extract_all('(?<=\\:[:space:]).*') %>% 
    str_replace_all("[:punct:]","") %>% 
    matrix( ncol = 2,  byrow = TRUE,dimnames=list(c(),c("name","id"))) %>% 
    as_tibble(stringsAsFactors = FALSE)
}
names(production_companies_list) <- c(1:3000)
production_companies_df <- bind_rows(production_companies_list, .id = 'movie_id')
production_companies_df %>% 
  head(10) %>% 
  DT::datatable()

## Crew
crew_list <- list()
for (i in seq_along(train$crew)) {
  crew_list[[i]] <- train$crew[[i]] %>%
    str_extract_all('(?<=\\{).*?(?=\\})') %>% 
    str_split("(?<=[:digit:]|[:punct:]), ",n=Inf,simplify = TRUE) %>% 
    str_extract_all('(?<=\\:[:space:]).*') %>% 
    str_replace_all("[:punct:]","") %>% 
    matrix( ncol = 7,  byrow = TRUE,dimnames=list(c(),c("credit_id","department","gender","id","job","name","profile_path"))) %>% 
    as_tibble(stringsAsFactors = FALSE)
}
names(crew_list) <- c(1:3000)
crew_df <- bind_rows(crew_list, .id = 'movie_id')
crew_df%>% 
  head(10) %>% 
  DT::datatable()
