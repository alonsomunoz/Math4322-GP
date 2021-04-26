# Cleaning the file from missing data and setting variables to right type
data_clean = na.omit(Video_Games_Sales_as_at_22_Dec_2016)
data_clean$Platform = as.factor(data_clean$Platform)
data_clean$Year_of_Release = as.factor(data_clean$Year_of_Release)
data_clean$Genre =  as.factor(data_clean$Genre)
data_clean$Publisher =  as.factor(data_clean$Publisher)
data_clean$Developer =  as.factor(data_clean$Developer)
data_clean$Rating =  as.factor(data_clean$Rating)
data_clean$User_Score =  as.double(data_clean$User_Score)

# Linear model ------------------------------------------------------------


## 1 removes Name, 5 removes Publisher, 6 removes na sales, 7 removes eu sales
##, 8 removes jp sales, 9 removes other sales,12 removes critic count
##, 14 removes user count, 15 removes developer
data_global = subset(data_clean, select = -c(1,5,6,7,8,9,15))
par(mfrow = c(2,2))
set.seed(1)

# Predictors  ---  Critic_Score+Platform+Genre+Year_of_Release+Publisher
train = sample(1:nrow(data_global),nrow(data_global)*.80)
data_lm = lm(Global_Sales~., data = data_global,subset = train)
summary(data_lm)
plot(data_lm)

# doing step function and stuff, trying to figure out best predictors
library(leaps)
data_lm_reg = regsubsets(Global_Sales~., data = data_global, really.big = TRUE)
data_lm_res = summary(data_lm_reg )
data_lm_res 
data_lm_stat = cbind(data_lm_res$rsq, data_lm_res$adjr2, data_lm_res$cp,data_lm_res$bic)
colnames(data_lm_stat) = c("rsq","Adjr2","Cp","BIC")
data_lm_stat

step(data_lm)

# getting mse for linear model
data_lm_yhat = predict.lm(data_lm, newdata = data_global[-train,])
data_lm_test = data_global[-train,"Global_Sales"]
mean((data_lm_yhat-data_lm_test)^2)

# Random Forest Model -----------------------------------------------------

# Changed from regular tree to randomforest 
library(randomForest)
set.seed(111)
train = sample(1:nrow(data_global),nrow(data_global)*.80)
data_randomforest = randomForest(Global_Sales~.-User_Count -Critic_Count, data = data_global, subset = train,importance = TRUE)
data_randomforest
varImpPlot(data_randomforest)

data_randomforest_yhat = predict(data_randomforest, newdata = data_global[-train,])
data_randomforest_test = data_global[-train,"Global_Sales"]
mean((data_randomforest_yhat-data_randomforest_test$Global_Sales)^2)


