# CitiBike Competition Kaggle
# Code started 8/9/14
# https://www.kaggle.com/c/bike-sharing-demand 

df <- read.csv("./data/train.csv")

#######################################################################
# data cleaning
#######################################################################
library(lubridate)
# parsing timestamp var 
df$date <- as.Date(df$datetime, format="%Y-%m-%d")
df$week_day <- weekdays(df$date)
df$hour <- hour(as.POSIXlt(df$datetime))
df$am_pm <- ifelse(df$hour %in% 0:11, "am", "pm")  
df$month <- month(as.POSIXlt(df$datetime))
df$day <- day(as.POSIXlt(df$datetime))

# describing season var 
for(i in 1:dim(df)[1]){
  if(df$season[i] == 1){
    df$season_char[i] <- "spring"
  } else if(df$season[i] == 2){
    df$season_char[i] <- "summer"
  } else if(df$season[i] == 3){
    df$season_char[i] <- "fall"
  } else if(df$season[i] == 4){
    df$season_char[i] <- "winter"
  } else {
    df$season_char[i] <- " "
  }
}
table(df$season)
table(df$season_char)
rm(i)

#######################################################################
# train-test data (70%-30%) 
#######################################################################
set.seed(123)
dfTrain <- df[sample(1:nrow(df), nrow(df)*0.7, replace=FALSE),]
dfTest <- df[!row.names(df) %in% row.names(dfTrain),]
save(dfTrain, file="./data/dfTrain.Rda")
save(dfTest, file="./data/dfTest.Rda")







