#-------------------------------
#This is a draft code generally separating some json columns
#Creating dummy variables based on common genere, production_companies etc.
#Creating new variables such as size of cast, size of crew etc.
#-------------------------------
#Modified on 31 MAR 2019
#new column: ratio of size of each department to total size of crew
#new column: ratio of male crew to (male+female)
#new column: ratio of male cast to (male+female)

library(data.table)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringi)
library(lubridate)
library(scales)
library(DT)
library(dplyr)
library(stringr)
library(jsonlite)
library(randomForest)

setwd("Desktop/ST4248/project")
train_raw <- read.csv("train.csv",header = TRUE,stringsAsFactors = F)

train_raw %>% glimpse()

##Cleaning of training data
train <- train_raw %>%
  separate(belongs_to_collection, 'idPart', sep = 'name', remove = TRUE) %>%  # Get the collection ID
  separate(release_date, c('releaseMonth', 'releaseDay', 'releaseYear'), sep = '/', remove = TRUE) %>%  # Separate the release_date
  mutate(collectionID = ifelse(is.na(idPart) == FALSE, gsub("\\D", "", idPart), idPart), # Get digitis from collection
         collectionID = ifelse(is.na(collectionID) == TRUE, 0, collectionID), # If collection value is NA the movie is not part of collection
         mainSpokenLanguage = substr(spoken_languages,17,18), # This contains the ISO value for the first spoken language
         mainSpokenLanguage = ifelse(is.na(mainSpokenLanguage), 'NA', mainSpokenLanguage),
         spokenEn = ifelse(mainSpokenLanguage == 'en', TRUE, FALSE), # Hot vec for spoken language == en
         partOfCollection = ifelse(is.na(idPart) == FALSE, TRUE, FALSE),  # Hot vec for is in collection
         hasHomePage = ifelse(is.na(homepage) == FALSE, TRUE, FALSE), # Hot vec for has homepage
         hasTagline = ifelse(is.na(tagline) == FALSE, TRUE, FALSE), # Hot vec for has tagline
         hasOverview = ifelse(is.na(overview) == FALSE, TRUE, FALSE), # Hot vec for has overview
         genres = ifelse(is.na(genres) == TRUE, 'NoGen', genres), # Hot vecs for the different genres
         genComedy = ifelse(stri_detect_fixed(genres, 'Comedy'),TRUE, FALSE),
         genDrama = ifelse(stri_detect_fixed(genres, 'Drama'),TRUE, FALSE),
         genThriller = ifelse(stri_detect_fixed(genres, 'Comedy'),TRUE, FALSE),
         genAction = ifelse(stri_detect_fixed(genres, 'Action'),TRUE, FALSE),
         genAnimation = ifelse(stri_detect_fixed(genres, 'Comedy'),TRUE, FALSE),
         genHorror = ifelse(stri_detect_fixed(genres, 'Horror'),TRUE, FALSE),
         genDocumentary = ifelse(stri_detect_fixed(genres, 'Documentary'),TRUE, FALSE),
         genAdventure = ifelse(stri_detect_fixed(genres, 'Adventure'),TRUE, FALSE),
         genCrime = ifelse(stri_detect_fixed(genres, 'Crime'),TRUE, FALSE),
         genMystery = ifelse(stri_detect_fixed(genres, 'Mystery'),TRUE, FALSE),
         genFantasy = ifelse(stri_detect_fixed(genres, 'Fantasy'),TRUE, FALSE),
         genWar = ifelse(stri_detect_fixed(genres, 'War'),TRUE, FALSE),
         genScienceFiction = ifelse(stri_detect_fixed(genres, 'Science Fiction'),TRUE, FALSE),
         genRomance = ifelse(stri_detect_fixed(genres, 'Romance'),TRUE, FALSE),
         genMusic = ifelse(stri_detect_fixed(genres, 'Music'),TRUE, FALSE),
         genWestern = ifelse(stri_detect_fixed(genres, 'Western'),TRUE, FALSE),
         genFamily = ifelse(stri_detect_fixed(genres, 'Family'),TRUE, FALSE),
         genHistory = ifelse(stri_detect_fixed(genres, 'Comedy'),TRUE, FALSE),
         genForeign = ifelse(stri_detect_fixed(genres, 'Foreign'),TRUE, FALSE),
         genTVMovie = ifelse(stri_detect_fixed(genres, 'TV Movie'),TRUE, FALSE),
         genNoGen = ifelse(genres == 'NoGen', TRUE, FALSE),
         production_companies = ifelse(is.na(production_companies) == TRUE, 'NoProd', production_companies), # Hot vecs for the most popular production companies
         prodUniversal = ifelse(stri_detect_fixed(production_companies, 'Universal Pictures'),TRUE, FALSE),
         prodParamount = ifelse(stri_detect_fixed(production_companies, 'Paramount Pictures'),TRUE, FALSE),
         prodTCF = ifelse(stri_detect_fixed(production_companies, 'Twentieth Century Fox Film Corporation'),TRUE, FALSE),
         prodColumbia = ifelse(stri_detect_fixed(production_companies, 'Columbia Pictures'),TRUE, FALSE),
         prodWarner = ifelse(stri_detect_fixed(production_companies, 'Warner Bros.'),TRUE, FALSE),
         prodNLC = ifelse(stri_detect_fixed(production_companies, 'New Line Cinema'),TRUE, FALSE),
         prodDisney = ifelse(stri_detect_fixed(production_companies, 'Walt Disney Pictures'),TRUE, FALSE),
         prodColumbiaPictures = ifelse(stri_detect_fixed(production_companies, 'Columbia Pictures Corporation'),TRUE, FALSE),
         prodTriStar = ifelse(stri_detect_fixed(production_companies, 'TriStar Pictures'),TRUE, FALSE),
         prodMGM = ifelse(stri_detect_fixed(production_companies, 'Metro-Goldwyn-Mayer (MGM)'),TRUE, FALSE),
         prodUnitedArtists = ifelse(stri_detect_fixed(production_companies, 'United Artists'),TRUE, FALSE),
         prodMiramax = ifelse(stri_detect_fixed(production_companies, 'Miramax Films'),TRUE, FALSE),
         prodTouchstone = ifelse(stri_detect_fixed(production_companies, 'Touchstone Pictures  '),TRUE, FALSE),
         prodFoxSearchlight = ifelse(stri_detect_fixed(production_companies, 'Fox Searchlight Pictures'),TRUE, FALSE),
         releaseYear = ifelse(as.integer(releaseYear) <= 18, paste0('20', releaseYear), paste0('19', releaseYear)), # Year of relese
         release_date = as.Date(paste(releaseYear, releaseMonth, releaseDay, sep = '-')), 
         age = as.integer(today() - release_date) / 365, # Age of movie in years
         quarterRelease = quarter(release_date), # Relese quarter
         weekRelease = week(release_date), # Relese week
         dayRelease = wday(release_date), # Relese day of week
         runtime = ifelse(is.na(runtime) == TRUE, 0, runtime), # If runtime is missing set it to zero, this will be fixed later
         sizeOfCast = str_count(cast, 'cast_id'), # Size of cast
         sizeOfCrew = str_count(crew, 'name'), # Size of crew
         sizeOfDirecting=ifelse(is.na(sizeOfCrew)==TRUE,0,str_count(crew,'Directing')/sizeOfCrew), #department size ratio
         sizeOfWriting=ifelse(is.na(sizeOfCrew)==TRUE,0,str_count(crew,'Writing')/sizeOfCrew),
         sizeOfProduction=ifelse(is.na(sizeOfCrew)==TRUE,0,str_count(crew,'Production')/sizeOfCrew),
         sizeOfSound=ifelse(is.na(sizeOfCrew)==TRUE,0,str_count(crew,'Sound')/sizeOfCrew),
         sizeOfCamera=ifelse(is.na(sizeOfCrew)==TRUE,0,str_count(crew,'Camera')/sizeOfCrew),
         sizeOfEditing=ifelse(is.na(sizeOfCrew)==TRUE,0,str_count(crew,'Editing')/sizeOfCrew),
         sizeOfArt=ifelse(is.na(sizeOfCrew)==TRUE,0,str_count(crew,'Art')/sizeOfCrew),
         sizeOfCostumeMakeUp=ifelse(is.na(sizeOfCrew)==TRUE,0,str_count(crew,'Costume MakeUp')/sizeOfCrew),
         sizeOfLighting=ifelse(is.na(sizeOfCrew)==TRUE,0,str_count(crew,'Lighting')/sizeOfCrew),
         sizeOfVisualEffects=ifelse(is.na(sizeOfCrew)==TRUE,0,str_count(crew,'Visual Effects')/sizeOfCrew),
         sizeOfActors=ifelse(is.na(sizeOfCrew)==TRUE,0,str_count(crew,'Actors')/sizeOfCrew),
         sizeOfCrew = ifelse(is.na(sizeOfCrew), 0, sizeOfCrew),
         numberOfKeywords = str_count(Keywords, 'name'), # Get nmber of keywords by conting how many "name" instances there is
         numberOfKeywords = ifelse(is.na(numberOfKeywords) == TRUE, 0, numberOfKeywords),
         numberOfProductionCompanies = str_count(production_companies, 'name'), # Get nmber of production companies by conting how many "name" instances there is
         numberOfProductionCompanies = ifelse(is.na(numberOfProductionCompanies) == TRUE, 0, numberOfProductionCompanies),
         numberOfProductionCountries = str_count(production_countries, 'name'), # Get nmber of production countries by conting how many "name" instances there is
         numberOfProductionCountries = ifelse(is.na(numberOfProductionCountries) == TRUE, 0, numberOfProductionCountries),
         numberOfGenres = str_count(genres, 'name'), # Get nmber of genres by conting how many "name" instances there is
         collectionID = as.factor(collectionID)) %>%  # Make collectionID a factor
  group_by(collectionID) %>%
  mutate(sizeOfCollection = n()) %>%
  ungroup() %>%
  mutate(sizeOfCollection = ifelse(sizeOfCollection > 1000, 0, sizeOfCollection)) %>% # Most movies are not in a collection. Collection size for the biggest collection i set to zero
  select(-idPart, -homepage, -imdb_id, -poster_path, -original_title, -genres, -overview, # Drop all unwanted columns
         -tagline, -production_companies, -status, -spoken_languages, -production_countries, -releaseYear, -releaseMonth, -releaseDay,
         -title, -collectionID, -mainSpokenLanguage)

train %>% glimpse()

df = train
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
actors_df$actor_males = unlist(lapply(cast_dfs,function(df){sum(df$V2 == 2,na.rm=T)}))
actors_df$actor_females = unlist(lapply(cast_dfs,function(df){sum(df$V2 == 1,na.rm=T)}))
actors_df$actor_gender_ratio = ifelse(is.finite(actors_df$males/actors_df$females),
                                      actors_df$males/actors_df$females, 0)

# M:F ratio in top 10 actors
actors_df$actor_top10males = unlist(lapply(cast_dfs,function(df){sum(df$V2[1:11] == 2,na.rm=T)}))
actors_df$actor_top10females = unlist(lapply(cast_dfs,function(df){sum(df$V2[1:11] == 1,na.rm=T)}))
actors_df$actor_top10gender_ratio = ifelse(
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



# Creating a dataframe for each row, with 3 columns:
# C1: Name, C2: Gender, C3: Department
crew = unlist(as.list(df$crew))
crew_lists = lapply(toString(crew),function(x){unlist(as.list(strsplit(x,'},')[[1]]))})
crew_dfs = lapply(crew_lists,function(x){
  as.data.frame(cbind(
    # extract name
    unlist(lapply(x,function(y){
      substr(y,str_locate(y,'name')[,2]+5,str_locate(y,'profile_path')-5)})), 
    # extract gender
    unlist(lapply(x,function(y){
      substr(y,str_locate(y,'gender')[,2]+4,str_locate(y,'gender')[,2]+4)})),
    # extract department
    unlist(lapply(x,function(y){
      substr(y,str_locate(y,'department')[,2]+5,str_locate(y,'gender')-5)}))),
    stringsAsFactors = F)
})


# Counting how many times a crew member appears in a movie:
crew_members = unique(unlist(lapply(crew_dfs,function(x){x[,1]})))
crew_tally = unlist(lapply(crew_members,function(crew_member){
  sum(unlist(lapply(crew_dfs,function(df){
    sum(rowSums(df == crew_member))
  })
  ),na.rm=T)}
))

# Putting crew_member and crew_tally in a dataframe, sort by crew_tally DESC
crew = as.data.frame(cbind(crew_members,as.numeric(cast_tally)),stringsAsFactors = F)
colnames(crew) = c('crew_members','crew_tally')
crew$crew_tally = as.numeric(crew$crew_tally)
crew = crew[order(-crew$crew_tally),]

# Bar Graph of Top Crew Members
ggplot(data=crew,aes(x=crew_members,y=crew_tally))+
  geom_bar(data=crew, 
           aes(fill=crew_members),stat='identity',show.legend=F)+
  ggtitle('Crew Member Count')+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab('Count')+
  xlab('Crew Member')


# Get list of top 300 actors (after removing empty strings)
if(length(which(crew$crew_members == '')) != 0){
  crew = crew[-which(crew$crew_members == ''),]}
topcrewmembers = crew[1:301,1]


# Create a dataframe that contains dummy variables for every crew's
# apperance in a movie
crews_df = as.data.frame(seq(1:n))
for(crews in topcrewmembers){
  old_cnames = names(crews_df)
  crew_count = unlist(lapply(crew_dfs,function(df){
    sum(rowSums(df == crews),na.rm=T)
  }))
  crews_df = cbind(crews_df,crew_count)
  names(crews_df) = c(old_cnames,crews)
}


# Get Male to Female ratio
crews_df$crew_males = unlist(lapply(crew_dfs,function(df){sum(df$V2 == 2,na.rm=T)}))
crews_df$crew_females = unlist(lapply(crew_dfs,function(df){sum(df$V2 == 1,na.rm=T)}))
crews_df$crew_gender_ratio = ifelse(is.finite(crews_df$males/crews_df$females),
                                    crews_df$males/crews_df$females, 0)

# M:F ratio in top 10 crews
crews_df$top10males = unlist(lapply(crew_dfs,function(df){sum(df$V2[1:11] == 2,na.rm=T)}))
crews_df$top10females = unlist(lapply(crew_dfs,function(df){sum(df$V2[1:11] == 1,na.rm=T)}))
crews_df$top10gender_ratio = ifelse(
  is.finite(crews_df$top10males/crews_df$top10females),
  crews_df$top10males/crews_df$top10females, 
  0)

# Append Cast, Keyword, and Crew variables to train/test dataframe
cast_key_crew_vars = cbind(actors_df[,-1],keyword_df[,-1],crews_df[,-1])
train_df = cbind(df,cast_key_crew_vars)
