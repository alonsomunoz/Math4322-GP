## Cleaning the file from missing data and setting variables to right type
data_clean = na.omit(Video_Games_Sales_as_at_22_Dec_2016)
data_clean$Platform = as.factor(data_clean$Platform)
data_clean$Year_of_Release = as.factor(data_clean$Year_of_Release)
data_clean$Genre =  as.factor(data_clean$Genre)
data_clean$Publisher =  as.factor(data_clean$Publisher)
data_clean$Developer =  as.factor(data_clean$Developer)
data_clean$Rating =  as.factor(data_clean$Rating)
data_clean$User_Score =  as.double(data_clean$User_Score)

## Predictors  ---  Critic_Score+Platform+Genre+Year_of_Release+Publisher
data_lm = lm(Global_Sales~ Critic_Score+Platform+Genre+Year_of_Release+Publisher, data = data_clean)
summary(data_lm)
par(mfrow = c(2,2))
plot(data_lm)

## Changed from linear model to tree
library(tree)
library(ISLR)

train = sample(1:nrow(data_clean),nrow(data_clean)*.80)
data_tree = tree(Global_Sales~ Critic_Score+Platform+Genre+Year_of_Release,data=data_clean)
plot(data_tree)
text(data_tree,pretty = 0)
summary(data_tree)

## Changed from regualr tree to randomforest 
library(randomForest)
##df = subset(mydata, select = -c(x,z) )
set.seed(10)
data_just_global =  subset(data_clean, select = -c(1,5,6,7,8,9,15)) 
train = sample(1:nrow(data_just_global),nrow(data_just_global)*.80)
p = ncol(data_just_global) - 1
data_randomforest = randomForest(Global_Sales~., data = data_just_global, subset = train,importance = TRUE)
data_randomforest
varImpPlot(data_randomforest)
