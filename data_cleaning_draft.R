##This is a draft code generally separating some json columns
##Creating dummy variables based on common genere, production_companies etc.
##Creating new variables such as size of cast, size of crew etc.

library(data.table)
library(tidyverse)
library(stringi)
library(lubridate)
library(randomForest)
library(scales)
library(DT)
library(dplyr)
library(stringr)
#File name and path need to be modified accordingly
train_raw <- read.csv(file="train.csv",header = TRUE)

train_raw %>% glimpse()

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
         -tagline, -production_companies, -status, -spoken_languages, -cast, -crew, -Keywords, 
         -production_countries, -releaseYear, -releaseMonth, -releaseDay,
         -title, -collectionID, -mainSpokenLanguage)

train %>% glimpse()