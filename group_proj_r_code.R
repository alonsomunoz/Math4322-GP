# Cleaning the file from missing data and setting variables to right type
data_clean = na.omit(Video_Games_Sales_as_at_22_Dec_2016)
data_clean$Platform = as.factor(data_clean$Platform)
data_clean$Year_of_Release = as.factor(data_clean$Year_of_Release)
data_clean$Genre =  as.factor(data_clean$Genre)
data_clean$Publisher =  as.factor(data_clean$Publisher)
data_clean$Developer =  as.factor(data_clean$Developer)
data_clean$Rating =  as.factor(data_clean$Rating)
data_clean$User_Score =  as.double(data_clean$User_Score)

## removing these observations have these years of release because when spliting the training and
## testing data, one observation ends in one and there isn't another observation for the other.
## then the predict method doesn't work at all. Have the table showing how much observations are in
## each year so that we decide which years we are going to remove.

data_clean = data_clean[data_clean$Year_of_Release != "1985",]
data_clean = data_clean[data_clean$Year_of_Release != "1988",]
data_clean = data_clean[data_clean$Year_of_Release != "1992",]
data_clean = data_clean[data_clean$Year_of_Release != "1994",]

table(data_clean$Year_of_Release)

# Linear model ------------------------------------------------------------

## 1 removes Name, 5 removes Publisher, 6 removes na sales, 7 removes eu sales
##, 8 removes jp sales, 9 removes other sales,12 removes critic count
##, 14 removes user count, 15 removes developer
data_global = subset(data_clean, select = -c(1,5,6,7,8,9,12,14,15))
par(mfrow = c(2,2))
set.seed(1241)

# Predictors  ---  Critic_Score+Platform+Genre+Year_of_Release+Publisher
train = sample(1:nrow(data_global),nrow(data_global)*.80)
data_lm = lm(Global_Sales~., data = data_global,subset = train)
summary(data_lm)
plot(data_lm)

# Converted the global_sales to log(global_sales)
data_lm = lm(log(Global_Sales)~., data = data_global,subset = train)
summary(data_lm)
plot(data_lm)

# getting mse for linear model when using log(global_sales)
data_lm_yhat = predict.lm(data_lm, newdata = data_global[-train,])
data_lm_test = data_global[-train,"Global_Sales"]
lm_mse = mean((data_lm_yhat-data_lm_test$Global_Sales)^2)
lm_mse


# trying to do some polynomials for numeric predictors just critic score
ploy_lm_mse = rep(0,5)
for(i in 1:5){
  train = sample(1:nrow(data_global),nrow(data_global)*.80)
  poly_lm = lm(log(Global_Sales)~Platform+Year_of_Release+Genre+Rating+User_Score+poly(Critic_Score,i), data = data_global, subset = train)
  ##plot(poly_lm)
  data_lm_yhat = predict.lm(data_lm, newdata = data_global[-train,])
  data_lm_test = data_global[-train,"Global_Sales"]
  lm_mse = mean((data_lm_yhat-data_lm_test$Global_Sales)^2)
  ploy_lm_mse[i] = lm_mse
}
ploy_lm_mse
# trying polynominals

# Trying to use step function ---------------------------------------------

# doing step function and stuff, trying to figure out best predictors
library(leaps)
data_lm_reg = regsubsets(Global_Sales~., data = data_global, really.big = TRUE)
data_lm_res = summary(data_lm_reg )
data_lm_res 
data_lm_stat = cbind(data_lm_res$rsq, data_lm_res$adjr2, data_lm_res$cp,data_lm_res$bic)
colnames(data_lm_stat) = c("rsq","Adjr2","Cp","BIC")
data_lm_stat

# trying to remove things from linear model but doesn't work at all, says all needed.
step(data_lm) 

# Random Forest Model -----------------------------------------------------

# Changed from regular tree to randomforest 
library(randomForest)
train = sample(1:nrow(data_global),nrow(data_global)*.80)
data_randomforest = randomForest(Global_Sales~., data = data_global, subset = train,importance = TRUE)
data_randomforest
varImpPlot(data_randomforest)

data_randomforest_yhat = predict(data_randomforest, newdata = data_global[-train,])
data_randomforest_test = data_global[-train,"Global_Sales"]
random_forest_mse = mean((data_randomforest_yhat-data_randomforest_test$Global_Sales)^2)
random_forest_mse

