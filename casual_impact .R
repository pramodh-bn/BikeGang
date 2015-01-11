### Casual Impact Model 
## http://google.github.io/CausalImpact/CausalImpact.html 

## !!! not really applicable since produces negative predictions

#install.packages("devtools")
#library(devtools)
#devtools::install_github("google/CausalImpact")
library(CausalImpact)

## To make the casual impact model work: 
# 1. generate 'basic' predictions using any method (i.e., poisson model) in test data
# 2. those predictors will be used as 'intervention' values in test data
# 3. combine test and train data (count as outcome in train data, and pred.count
# as outcome in test data)
# 4. run the casual impact model 
# 5. output pre-intervention values as a final result 

## First will test the model on only train data 
# Loading data 
load("./data/dfTrain.Rda")  # created in import_clean.R
load("./data/dfTest.Rda")  # created in import_clean.R

## Predicting outcome using Negative BInomial model 
# main code in nb_prediction.R

library(MASS)
nb <- glm.nb(count ~ season_char + month + day 
             + hour + am_pm + as.factor(weather)
             + atemp + humidity + windspeed, 
             data = dfTrain)
summary(nb)

dfTest$nb.pred <- predict (nb, dfTest, type ="response")
dfTrain$nb.pred <- predict (nb, dfTrain, type ="response")
mean(dfTest$count)
mean(dfTest$nb.pred)

# checking RMSLE for the prediction accuracy 
# P.S. lower values are better
library(Metrics)
rmsle(dfTest$count, dfTest$nb.pred)

## Creating matrxi for the Casual Impact 
dfTrain <- dfTrain[order(dfTrain[,"datetime"],decreasing=FALSE),]
dfTest <- dfTest[order(dfTest[,"datetime"],decreasing=FALSE),]
m1 <- as.matrix(dfTrain[, c("count","temp")])
colnames(m1)[1] <- "count_new"
m2 <- as.matrix(dfTest[, c("nb.pred","temp")])
colnames(m2)[1] <- "count_new"
all <- rbind(m1, m2)
# Causal Impact simeple model (only temp as predictor)
pre.period <- c(1, 7620)
post.period <- c(7621, 10886)
impact <- CausalImpact(all, pre.period, post.period)
plot(impact)
summary(impact)
summary(impact, "report")
names(impact$series)
preds <- impact$series$point.pred
preds_postive <- ifelse(preds < 0, 0, preds)
mean(preds_postive)

rmsle(dfTest$count, preds_postive[7621:10886])

## Model is not useful since predicted values can be negative 

