# loading clean data 
load("./data/dfTrain.Rda")  # created in import_clean.R
load("./data/dfTest.Rda")  # created in import_clean.R

#######################################################################
# exploratory analysis 
#######################################################################
summary(dfTrain[,-1])

# check if "count" is the same as sum of "casual" and "registered"
head(cbind(dfTrain$count, dfTrain$registered+dfTrain$casual), 20)  # <- yes!! 

library(caret)
featurePlot(x = dfTrain[,3:8], y = as.numeric(dfTrain$count), plot="pairs")
featurePlot(x = dfTrain[,c(9:11,15)], y = as.numeric(dfTrain$count), plot="pairs")

# individual plots and tets (outcome vs predictor)
library(ggplot2)
# seasons 
(ggplot(dfTrain, aes(x=season_char, y=count, fill=season_char)) + geom_boxplot()
 + ggtitle("Seasons vs. Count"))
aovFit <- aov(count ~ season_char, data=dfTrain)
summary(aovFit)

# holiday 
(ggplot(dfTrain, aes(x=as.factor(holiday), y=count, fill=as.factor(holiday))) 
 + geom_boxplot() + ggtitle("Holiday vs. Count"))
wilcox.test(dfTrain$holiday, dfTrain$count, paired=FALSE)

# workingday 
(ggplot(dfTrain, aes(x=as.factor(workingday), y=count, fill=as.factor(workingday))) 
 + geom_boxplot() + ggtitle("Workingday vs. Count"))
wilcox.test(dfTrain$workingday, dfTrain$count, paired=FALSE)

# weather 
#1: Clear, Few clouds, Partly cloudy, Partly cloudy 
#2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist 
#3: Light Snow, Light Rain + Thunderstorm + Scatt clouds, Light Rain + Scatt clouds 
#4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog 
(ggplot(dfTrain, aes(x=as.factor(weather), y=count, fill=as.factor(weather))) 
 + geom_boxplot() + ggtitle("Weather vs. Count"))
aovFit <- aov(count ~ weather, data=dfTrain)
summary(aovFit)

# temperature 
(ggplot(dfTrain, aes(x=temp, y=count)) + geom_point(shape=1) 
 + geom_smooth(method=lm, se=FALSE) + ggtitle("Temp vs. Count"))
corFit <- cor.test(dfTrain$temp, dfTrain$count, method="spearman")
corFit$estimate
corFit$p.value

# ambient temperature 
(ggplot(dfTrain, aes(x=atemp, y=count)) + geom_point(shape=1) 
 + geom_smooth(method=lm, se=FALSE) + ggtitle("Ambient Temp vs. Count"))
corFit <- cor.test(dfTrain$atemp, dfTrain$count, method="spearman")
corFit$estimate
corFit$p.value

# humidity
(ggplot(dfTrain, aes(x=humidity, y=count)) + geom_point(shape=1) 
 + geom_smooth(method=lm, se=FALSE) + ggtitle("Humidity vs. Count"))
corFit <- cor.test(dfTrain$humidity, dfTrain$count, method="spearman")
corFit$estimate
corFit$p.value

# windspeed
(ggplot(dfTrain, aes(x=windspeed, y=count)) + geom_point(shape=1) 
 + geom_smooth(method=lm, se=FALSE) + ggtitle("Windspeed vs. Count"))
corFit <- cor.test(dfTrain$windspeed, dfTrain$count, method="spearman")
corFit$estimate
corFit$p.value

# week_day
(ggplot(dfTrain, aes(x=as.factor(week_day), y=count, fill=as.factor(week_day))) 
 + geom_boxplot() + ggtitle("Week_Day vs. Count") 
 + scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", 
                             "Thursday", "Friday", "Saturday", "Sunday")))
aovFit <- aov(count ~ as.factor(week_day), data=dfTrain)
summary(aovFit)

# hour 
(ggplot(dfTrain, aes(x=hour, y=count)) + geom_point(shape=1) 
 + geom_smooth(method=lm, se=FALSE) + ggtitle("Hour vs. Count"))
corFit <- cor.test(dfTrain$hour, dfTrain$count, method="spearman")
corFit$estimate
corFit$p.value

# Am/Pm
(ggplot(dfTrain, aes(x=as.factor(am_pm), y=count, fill=as.factor(am_pm))) 
 + geom_boxplot() + ggtitle("AM/PM vs. Count"))
ampm <- ifelse(dfTrain$am_pm == "am", 0, 1)
wilcox.test(ampm, dfTrain$count, paired=FALSE)





