#-------------------------------
#xgBoost on train_df
#-------------------------------
library(data.table)
library(mlr)
library(stringr)
library(xgboost)
library(caretEnsemble)

#convert all logical features into factors
df=data.frame(lapply(df, function(x) if(is.logical(x)) {
  return(as.factor(x))
} else{
  return(x)
}
), stringsAsFactors = FALSE)

for (i in 1:nrow(df)) {
  if(df[i,"budget"] > 1000 & df[i,"revenue"] < 100){
    df[i,"revenue"] = df[i,"revenue"] * 10^6
  }
}


set.seed(100)
ind_train_2=sample(200,100)
test_df = df[-ind_train_2,]
train_df=df[ind_train_2,]

#set variable names
setcol <- names(train_df)

#convert data frame to data table
setDT(train_df)
setDT(test_df)
#as.matrix(train_df)

#check missing values 
table(is.na(train_df))
sapply(train_df, function(x) sum(is.na(x))/length(x))*100

table(is.na(test_df))
sapply(test_df, function(x) sum(is.na(x))/length(x))*100

#quick data cleaning
#remove extra character from target variable
test_df [,revenue := substr(revenue,start = 1,stop = nchar(revenue)-1)]

#remove leading whitespaces
char_col <- colnames(train_df)[ sapply (train_df,is.character)]
for(i in char_col) set(train_df,j=i,value = str_trim(train_df[[i]],side = "left"))
for(i in char_col) set(test_df,j=i,value = str_trim(test_df[[i]],side = "left"))

#set all missing value as "Missing" 
train_df[is.na(train_df)] <- "Missing" 
test_df[is.na(test_df)] <- "Missing" 

#should be using one hot encoding but already done in data cleaning
#so just convert the whole data frame to numeric
labels <- log1p(as.numeric(train_df$revenue))
ts_label <- as.numeric(test_df$revenue)

train_df=train_df %>% within(rm("id","revenue"))
test_df=test_df %>% within(rm("id","revenue"))

#new_tr <- model.matrix(~.+0,data = train_df[,-c("revenue"),with=F])
new_tr<-sapply(train_df,function(x) as.numeric(as.character(x)))
new_ts<-sapply(test_df,function(x) as.numeric(as.character(x)))

#preparing matrix 
dtrain <- xgb.DMatrix(data = new_tr,label = labels)
dtest <- xgb.DMatrix(data = new_ts)

#default parameters
param <- list(booster="gbtree",
              eta=0.03,
              colsample_bytree = 0.3,
              max_depth = 6,
              min_child_weight = 2,
              base_score = mean(labels),
              subsample = 0.9)

set.seed(1235)
xgb1 <- xgb.train(data=dtrain, params = param, nrounds= 276,print_every_n = 50)

xgbcv <- xgb.cv( params = param, data = dtrain, nrounds = 276, 
                 nfold = 5, showsd = T, stratified = T, 
                 print.every.n = 10, early.stop.round = 20, maximize = F)
#best iteration: 250

#model prediction
xgbpred <- exp(predict (xgb1,dtest))-1
#mse
mean((xgbpred-ts_label)^2, na.rm = TRUE)



