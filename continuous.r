train_raw <- read.csv(file="train.csv",header = TRUE)


# Correct some values
train_raw[1336, ]$runtime = 130
train_raw[train_raw$id == 16,]$revenue = 192864          # Skinning

# Fill 0 budget with correct values
train_raw[train_raw$id == 90,]$budget = 30000000         # Sommersby
train_raw[train_raw$id == 118,]$budget = 60000000        # Wild Hogs
train_raw[train_raw$id == 149,]$budget = 18000000        # Beethoven
train_raw[train_raw$id == 313,]$revenue = 12000000       # The Cookout
train_raw[train_raw$id == 451,]$revenue = 12000000       # Chasing Liberty
train_raw[train_raw$id == 464,]$budget = 20000000        # Parenthood
train_raw[train_raw$id == 470,]$budget = 13000000        # The Karate Kid, Part II
train_raw[train_raw$id == 513,]$budget = 930000          # From Prada to Nada
train_raw[train_raw$id == 797,]$budget = 8000000         # Welcome to Dongmakgol
train_raw[train_raw$id == 819,]$budget = 90000000        # Alvin and the Chipmunks: The Road Chip
train_raw[train_raw$id == 850,]$budget = 90000000        # Modern Times
train_raw[train_raw$id == 1112,]$budget = 7500000        # An Officer and a Gentleman
train_raw[train_raw$id == 1131,]$budget = 4300000        # Smokey and the Bandit
train_raw[train_raw$id == 1359,]$budget = 10000000       # Stir Crazy
train_raw[train_raw$id == 1542,]$budget = 1              # All at Once
train_raw[train_raw$id == 1542,]$budget = 15800000       # Crocodile Dundee II
train_raw[train_raw$id == 1571,]$budget = 4000000        # Lady and the Tramp
train_raw[train_raw$id == 1714,]$budget = 46000000       # The Recruit
train_raw[train_raw$id == 1721,]$budget = 17500000       # Cocoon
train_raw[train_raw$id == 1865,]$revenue = 25000000      # Scooby-Doo 2: Monsters Unleashed
train_raw[train_raw$id == 2268,]$budget = 17500000       # Madea Goes to Jail budget
train_raw[train_raw$id == 2491,]$revenue = 6800000       # Never Talk to Strangers
train_raw[train_raw$id == 2602,]$budget = 31000000       # Mr. Holland's Opus
train_raw[train_raw$id == 2612,]$budget = 15000000       # Field of Dreams
train_raw[train_raw$id == 2696,]$budget = 10000000       # Nurse 3-D
train_raw[train_raw$id == 2801,]$budget = 10000000       # Fracture

# test_raw[test_raw$id == 3889,]$budget = 15000000       # Colossal
# test_raw[test_raw$id == 6733,]$budget = 5000000        # The Big Sick
# test_raw[test_raw$id == 3197,]$budget = 8000000        # High-Rise
# test_raw[test_raw$id == 6683,]$budget = 50000000       # The Pink Panther 2
# test_raw[test_raw$id == 5704,]$budget = 4300000        # French Connection II
# test_raw[test_raw$id == 6109,]$budget = 281756         # Dogtooth
# test_raw[test_raw$id == 7242,]$budget = 10000000       # Addams Family Values
# test_raw[test_raw$id == 7021,]$budget = 17540562       #  Two Is a Family
# test_raw[test_raw$id == 5591,]$budget = 4000000        # The Orphanage
# test_raw[test_raw$id == 4282,]$budget = 20000000       # Big Top Pee-wee



# split the date string and turn it into 3 number tuple, missing dates are replaced with NAs
x = t(sapply(strsplit(train_raw$release_date, "/", fixed=TRUE), function(x) if(length(x)==3) {as.numeric(x)} else {c(NA,NA,NA)} ))
# handle the truncated year, assume there are no movies are in the DB with the date past 2020
x[,3] = ifelse(x[,3]>20,x[,3]+1900,x[,3]+2000)
# now make a 4-digit year date string and parse it in a normal way
train_raw$release_date <- as.Date(paste(x[,3],x[,1],x[,2],sep="-"))

library(lubridate)
train_raw$release_year <- year(train_raw$release_date)


library(classInt)
x <- classIntervals(train_raw$popularity, 4, style = 'quantile')
train_raw$pop_bin = ifelse(train_raw$popularity>=x$brks[4], 4, ifelse(train_raw$popularity>=x$brks[3], 3, ifelse(train_raw$popularity>=x$brks[2], 2, 1)))

x <- classIntervals(train_raw$budget, 4, style = 'quantile')
train_raw$budget_bin = ifelse(train_raw$budget>=x$brks[4], 4, ifelse(train_raw$budget>=x$brks[3], 3, ifelse(train_raw$budget>=x$brks[2], 2, 1)))

train_raw$log_budget = log1p(train_raw$budget)
train_raw$log_popularity = log1p(train_raw$popularity)


# Filling missing runtime with mean value
train_raw$runtime[is.na(train_raw$runtime)]=0
x=train_raw$runtime[train_raw$runtime>0]
train_raw$runtime[train_raw$runtime==0]=mean(x)

train_raw$budget_runtime_ratio = train_raw$budget/train_raw$runtime
train_raw$budget_popularity_ratio = train_raw$budget/train_raw$popularity
train_raw$budget_year_ratio = train_raw$budget/(train_raw$release_year*train_raw$release_year)
train_raw$releaseYear_popularity_ratio = train_raw$release_year/train_raw$popularity
train_raw$releaseYear_popularity_ratio2 = train_raw$popularity/train_raw$release_year
train_raw$year_to_log_budget = train_raw$release_year / log_budget
train_raw$year_to_log_popularity = train_raw$release_year / log_popularity



meanruntimeByYear = train_raw %>% group_by(release_year) %>% summarize(meanruntimeByYear = mean(runtime, na.rm = TRUE))
meanPopularityByYear = train_raw %>% group_by(release_year) %>% summarize(meanPopularityByYear = mean(popularity, na.rm = TRUE))
meanBudgetByYear = train_raw %>% group_by(release_year) %>% summarize(meanBudgetByYear = mean(budget, na.rm = TRUE))

train_raw = merge(train_raw, meanruntimeByYear, by="release_year")
train_raw = merge(train_raw, meanPopularityByYear, by="release_year")
train_raw = merge(train_raw, meanBudgetByYear, by="release_year")


# id
# pop_bin
# budget_bin
# log_budget
# log_popularity
# budget_runtime_ratio
# budget_popularity_ratio
# budget_year_ratio
# releaseYear_popularity_ratio
# releaseYear_popularity_ratio2
# year_to_log_budget
# year_to_log_popularity
# meanruntimeByYear
# meanPopularityByYear
# meanBudgetByYear
zy = train_raw[c(2, 25:38)]