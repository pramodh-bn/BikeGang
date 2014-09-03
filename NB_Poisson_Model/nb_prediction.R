# testing prediciton with NB model  

# NB model 
library(MASS)
nb <- glm.nb(count ~ season_char + month + day 
             + hour + am_pm + as.factor(weather)
             + atemp + humidity + windspeed, 
             data = rtestTrain)
summary(nb)

dfTest$nb.pred <- predict (nb, dfTest, type ="response")
mean(dfTest$count)
mean(dfTest$nb.pred)

##################################################################
# reading in Kaggle test dataset 
rtest <- read.csv("./data/test.csv")

# cleaning up test dataset 
library(lubridate)
# parsing timestamp var 
rtest$date <- as.Date(rtest$datetime, format="%Y-%m-%d")
rtest$week_day <- weekdays(rtest$date)
rtest$hour <- hour(as.POSIXlt(rtest$datetime))
rtest$am_pm <- ifelse(rtest$hour %in% 0:11, "am", "pm")  
rtest$month <- month(as.POSIXlt(rtest$datetime))
rtest$day <- day(as.POSIXlt(rtest$datetime))

# describing season var 
for(i in 1:dim(rtest)[1]){
  if(rtest$season[i] == 1){
    rtest$season_char[i] <- "spring"
  } else if(rtest$season[i] == 2){
    rtest$season_char[i] <- "summer"
  } else if(rtest$season[i] == 3){
    rtest$season_char[i] <- "fall"
  } else if(rtest$season[i] == 4){
    rtest$season_char[i] <- "winter"
  } else {
    rtest$season_char[i] <- " "
  }
}
table(rtest$season)
table(rtest$season_char)

#############################################################################
## outputting predictions 
rtest$count <- predict(nb, rtest, type ="response")

# submission dataset 
sub <- rtest[, c(1,18)]
write.csv(sub, file="./submissions/nb_sub_2014-08-27.csv", row.names=FALSE)
