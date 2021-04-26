# Cleaning the file from missing data and setting variables to right type
data_clean = na.omit(Video_Games_Sales_as_at_22_Dec_2016)
data_clean$Platform = as.factor(data_clean$Platform)
data_clean$Year_of_Release = as.factor(data_clean$Year_of_Release)
data_clean$Genre =  as.factor(data_clean$Genre)
data_clean$Publisher =  as.factor(data_clean$Publisher)
data_clean$Developer =  as.factor(data_clean$Developer)
data_clean$Rating =  as.factor(data_clean$Rating)
data_clean$User_Score =  as.double(data_clean$User_Score)

# Question One ------------------------------------------------------------

## 1 removes Name, 5 removes Publisher, 6 removes na sales, 7 removes eu sales
##, 8 removes jp sales, 9 removes other sales, 15 removes developer
data_global = subset(data_clean, select = -c(1,5,6,7,8,9,15))
set.seed(10)

# Predictors  ---  Critic_Score+Platform+Genre+Year_of_Release+Publisher
data_lm = lm(Global_Sales~., data = data_global)
summary(data_lm)
par(mfrow = c(2,2))
plot(data_lm)

# Changed from linear model to tree
library(tree)
library(ISLR)
train = sample(1:nrow(data_global),nrow(data_global)*.80)
data_tree = tree(Global_Sales~.,data=data_global)
plot(data_tree)
text(data_tree,pretty = 0)
summary(data_tree)

# Changed from regualr tree to randomforest 
library(randomForest)
train = sample(1:nrow(data_global),nrow(data_global)*.80)
data_randomforest = randomForest(Global_Sales~., data = data_global, subset = train,importance = TRUE)
data_randomforest
varImpPlot(data_randomforest)

# Question Two ------------------------------------------------------------

data_success = data_global
data_success$Success <- ifelse(data_success$Global_Sales > 1, 1,0)
data_success$Success = as.factor(data_success$Success)

data_glm = glm(Success~.-Global_Sales, data= data_success, family = "binomial")
summary(data_glm)
plot(data_glm)
