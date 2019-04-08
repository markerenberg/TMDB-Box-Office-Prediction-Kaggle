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
  data.frame(
    # extract id
    ID=as.numeric(unlist(lapply(x,function(y){
      t = gsub(".*gender': \\d+, 'id': (\\d+),.*", '\\1', y)
      t = ifelse(str_detect(t, "\\d+"), t, NA)

      }))),
    # extract name
    NAME=unlist(lapply(x,function(y){
      substr(y,str_locate(y,"'name")[,2]+5,str_locate(y,"order':")-5)})),
    # extract gender
    GENDER=unlist(lapply(x,function(y){
      substr(y,str_locate(y,'gender')[,2]+4,str_locate(y,'gender')[,2]+4)})),
    # extract order
    ORDER=unlist(lapply(x,function(y){
      substr(y,str_locate(y,'order')[,2]+4,str_locate(y,'profile_path')-4)})))
})

# Counting how many times a cast member appears in a movie:
# Putting cast_member and cast_tally in a dataframe, sort by cast_tally DESC

cast_dfs_unfold = bind_rows(cast_dfs, .id="column_label")
cast = plyr::count(cast_dfs_unfold, c("ID", "NAME"))
colnames(cast) = c("ID", "cast_members", "cast_tally")
cast = cast[order(-cast$cast_tally),]


# Bar Graph of Top Actors
ggplot(data=cast[1:100,],aes(x=cast_members,y=cast_tally))+
  geom_bar(data=cast[1:100,],
           aes(fill=cast_members),stat='identity',show.legend=F)+
  ggtitle('Actor Count')+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab('Count')+
  xlab('Actor')

# Get list of top 300 actors (after removing empty strings and NA)
if(length(which(cast$cast_members == '')) != 0){
  cast = cast[-which(cast$cast_members == ''),]}

if(length(which(is.na(cast$cast_members))) != 0){
  cast = cast[-which(is.na(cast$cast_members)),]}

topactors = cast[1:300,1:2]

# Create a dataframe that contains dummy variables for every actor's
#   apperance in a movie
actors_df = as.data.frame(seq(1:n))
names(actors_df) = "column_label"

for (i in 1:300) {
    actor = topactors[i, ]
    cast_dfs_unfold$column_label = as.numeric(cast_dfs_unfold$column_label)
    y = cast_dfs_unfold%>% group_by(column_label) %>% summarize(actor=sum(actor[1,1] %in% ID))
    names(y)[2] = actor[1,2]
    actors_df = merge(actors_df, y, by="column_label")
}


# Get Male to Female ratio
actors_df$males = unlist(lapply(cast_dfs,function(df){sum(df$GENDER == 2,na.rm=T)}))
actors_df$females = unlist(lapply(cast_dfs,function(df){sum(df$GENDER == 1,na.rm=T)}))
actors_df$gender_ratio = ifelse(is.finite(actors_df$males/actors_df$females),
                                actors_df$males/actors_df$females, 0)

# M:F ratio in top 10 actors
actors_df$top10males = unlist(lapply(cast_dfs,function(df){sum(df$GENDER[1:11] == 2,na.rm=T)}))
actors_df$top10females = unlist(lapply(cast_dfs,function(df){sum(df$GENDER[1:11] == 1,na.rm=T)}))
actors_df$top10gender_ratio = ifelse(
  is.finite(actors_df$top10males/actors_df$top10females),
  actors_df$top10males/actors_df$top10females,
  0)



# Create a list for each movie, containing all keywords for that movie
keyw = df$Keywords
key_lists = lapply(keyw,function(x){unlist(as.list(strsplit(x,'},')[[1]]))})

key_df = lapply(key_lists,function(lst) {
    data.frame(
        # extract id
        ID = as.numeric(unlist(lapply(lst,function(y) {
            t = gsub(".*'id': (\\d+), 'name'.*", '\\1', y)
            t = ifelse(str_detect(t, "\\d+"), t, NA)

        }))),
        # extract names
        NAME=unlist(lapply(lst,function(strng){
            # if string in list is last string, remove the '}]"' characters
            ifelse(match(strng,lst)==length(lst),
                   substr(strng,str_locate(strng,'name')[,2]+5,nchar(strng)-3),
                   substr(strng,str_locate(strng,'name')[,2]+5,nchar(strng)-1))
        }
    )))
})

# To count how many times a keyword appears in a movie:
# Create a dataframe containing each keyword and the keyword_tally
key_df_unfold = bind_rows(key_df, .id="column_label")
keywords = plyr::count(key_df_unfold, c("ID", "NAME"))
colnames(keywords) = c("ID", "keyword", "keyword_tally")
keywords = keywords[order(-keywords$keyword_tally),]





# Get list of top 300 keywords (after removing empty strings)
if(length(which(keywords$keyword == '')) != 0){
  keywords = keywords[-which(keywords$keyword == ''),]
}

if(length(which(is.na(keywords$keyword))) != 0){
  keywords = keywords[-which(is.na(keywords$keyword)),]}

topkeys = keywords[1:300,1:2]

# Create a dataframe that contains dummy variables for every keyword's
#   apperance in a movie description

keyword_df = as.data.frame(seq(1:n))
names(keyword_df) = "column_label"

for (i in 1:300) {
    key = topkeys[i, ]
    key_df_unfold$column_label = as.numeric(key_df_unfold$column_label)
    y = key_df_unfold %>% group_by(column_label) %>% summarize(key=sum(key[1,1] %in% ID))
    names(y)[2] = key[1,2]
    keyword_df = merge(keyword_df, y, by="column_label")
    cat(i)
}


# 3) Crew

# Creating a dataframe for each row, with 3 columns:
# C1: Name, C2: Gender, C3: Department
crew = unlist(as.list(df$crew))
crew_lists = lapply(toString(crew),function(x){unlist(as.list(strsplit(x,'},')[[1]]))})
crew_dfs = lapply(crew_lists,function(x){
  data.frame(
    # extract id
    ID=as.numeric(unlist(lapply(x,function(y){
      t = gsub(".*gender': \\d+, 'id': (\\d+),.*", '\\1', y)
      t = ifelse(str_detect(t, "\\d+"), t, NA)

      }))),
    # extract name
    Name=unlist(lapply(x,function(y){
      substr(y,str_locate(y,'name')[,2]+5,str_locate(y,'profile_path')-5)})), 
    # extract gender
    Gender=unlist(lapply(x,function(y){
      substr(y,str_locate(y,'gender')[,2]+4,str_locate(y,'gender')[,2]+4)})),
    # extract department
    Department=unlist(lapply(x,function(y){
      substr(y,str_locate(y,'department')[,2]+5,str_locate(y,'gender')-5)})))
})


# Counting how many times a crew member appears in a movie:
# Putting crew_member and crew_tally in a dataframe, sort by crew_tally DESC


crew_dfs_unfold = bind_rows(crew_dfs, .id="column_label")
crew = plyr::count(crew_dfs_unfold, c("ID", "Name"))
colnames(crew) = c("ID", "crew_members", "crew_tally")
crew = crew[order(-crew$crew_tally),]


# Get list of top 300 crew members (after removing empty strings)
if(length(which(crew$crew_members == '')) != 0){
  crew = crew[-which(crew$crew_members == ''),]}

if(length(which(is.na(crew$crew_members))) != 0){
  crew = crew[-which(is.na(crew$crew_members)),]}
topcrewmembers = crew[1:300,1:2]


# Create a dataframe that contains dummy variables for every crew's
# apperance in a movie

crews_df = as.data.frame(seq(1:n))
names(crews_df) = "column_label"

for (i in 1:300) {
    cr = topcrewmembers[i, ]
    crew_dfs_unfold$column_label = as.numeric(crew_dfs_unfold$column_label)
    y = crew_dfs_unfold%>% group_by(column_label) %>% summarize(cr=sum(cr[1,1] %in% ID))
    names(y)[2] = paste0('CREW ',cr[1,2])
    crews_df = merge(crews_df, y, by="column_label")
}



# Get Male to Female ratio
crews_df$crew_males = unlist(lapply(crew_dfs,function(df){sum(df$Gender == 2,na.rm=T)}))
crews_df$crew_females = unlist(lapply(crew_dfs,function(df){sum(df$Gender == 1,na.rm=T)}))
crews_df$crew_gender_ratio = ifelse(is.finite(crews_df$crew_males/crews_df$crew_females),
                                    crews_df$crew_males/crews_df$crew_females, 0)

# M:F ratio in top 10 crews
crews_df$top10crewmales = unlist(lapply(crew_dfs,function(df){sum(df$Gender[1:11] == 2,na.rm=T)}))
crews_df$top10crewfemales = unlist(lapply(crew_dfs,function(df){sum(df$Gender[1:11] == 1,na.rm=T)}))
crews_df$top10crewgender_ratio = ifelse(
  is.finite(crews_df$top10crewmales/crews_df$top10crewfemales),
  crews_df$top10crewmales/crews_df$top10crewfemales, 
  0)


cast_key_crew_vars = cbind(actors_df[,-1],keyword_df[,-1],crews_df[,-1])
df = cbind(df,cast_key_crew_vars)
df = select(df, -cast, -Keywords, -crew)