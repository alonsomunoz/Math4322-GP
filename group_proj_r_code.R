# Cleaning the file from missing data and setting variables to right type
data_clean = na.omit(Video_Games_Sales_as_at_22_Dec_2016)
data_clean$Platform = as.factor(data_clean$Platform)
##data_clean$Platform <- factor(data_clean$Platform, levels = c("2600","3DO","3DS","DC","DS","GB","GBA","GC","GEN","GG","N64","NES","NG","PC","PCFX","PS","PS2","PS3","PS4","PSP","PSV","SAT","SCD","SNES","TG16","Wii","WiiU","WS","X360","XB","XOne" ), ordered= TRUE)
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
data_clean = data_clean[data_clean$Year_of_Release != "N/A",]

## removing these because when spliting into train and test sets, the train set oesn't have that 
## category while the testing set does and no prediction can be made since the training set never
## had that specific rating.
data_clean = data_clean[data_clean$Rating != "AO",]
data_clean = data_clean[data_clean$Rating != "K-A",]
data_clean = data_clean[data_clean$Rating != "RP",]

## 1 removes Name, 5 removes Publisher, 6 removes na sales, 7 removes eu sales
##, 8 removes jp sales, 9 removes other sales,12 removes critic count
##, 14 removes user count, 15 removes developer
data_global = subset(data_clean, select = -c(1,5,6,7,8,9,12,14,15))

## added this line in because of how she told us to change year_of_release from a factor to numerical
data_global$Year_of_Release = as.numeric(data_global$Year_of_Release)

## Just to check amounts of variables and see if there are two few observations
table(Video_Games_Sales_as_at_22_Dec_2016$Platform)
table(Video_Games_Sales_as_at_22_Dec_2016$Genre)
table(Video_Games_Sales_as_at_22_Dec_2016$Year_of_Release)
table(Video_Games_Sales_as_at_22_Dec_2016$Rating)

table(data_clean$Rating)
table(data_global$Rating)
table(data_global$Genre)
table(data_global$Platform)

par(mfrow = c(2,2))
set.seed(12423)

# Linear model ------------------------------------------------------------

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
lm_mse = mean((log(data_lm_test$Global_Sales)-data_lm_yhat)^2)
lm_mse
lm_mse_sqrt_exp = sqrt(exp(lm_mse)) ## just processing the mse
lm_mse_sqrt_exp

# trying polynomial stuff -------------------------------------------------

# trying to do some polynomials for numeric predictors just critic score
poly_lm_mse = rep(0,5)
for(i in 1:5){
  train = sample(1:nrow(data_global),nrow(data_global)*.80)
  poly_lm = lm(log(Global_Sales)~Platform+Year_of_Release+Genre+Rating+User_Score+poly(Critic_Score,i), data = data_global, subset = train)
  ##plot(poly_lm)
  data_lm_yhat = predict.lm(data_lm, newdata = data_global[-train,])
  data_lm_test = data_global[-train,"Global_Sales"]
  lm_mse = mean((data_lm_yhat-data_lm_test$Global_Sales)^2)
  poly_lm_mse[i] = lm_mse
}
ploy_lm_mse
# trying polynominals

# Trying to use step function ---------------------------------------------

## 

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
data_randomforest = randomForest(log(Global_Sales)~., data = data_global, subset = train,importance = TRUE)
data_randomforest
varImpPlot(data_randomforest)

data_randomforest_yhat = predict(data_randomforest, newdata = data_global[-train,])
data_randomforest_test = data_global[-train,"Global_Sales"]
random_forest_mse = mean((log(data_randomforest_test$Global_Sales)-data_randomforest_yhat)^2)
random_forest_mse
rf_mse_sqrt_exp = sqrt(exp(random_forest_mse)) ## just processing the mse
rf_mse_sqrt_exp

# Paper Code --------------------------------------------------------------

seeds = floor(runif(10, min=0, max=9999999))

## Linear Model 10 times
linear_model_10_mse = rep(0,10)

for(i in 1:10){
  set.seed(seeds[i])
  
  train = sample(1:nrow(data_global),nrow(data_global)*.80)
  data_lm = lm(log(Global_Sales)~., data = data_global,subset = train)
  data_lm_yhat = predict.lm(data_lm, newdata = data_global[-train,])
  data_lm_test = data_global[-train,"Global_Sales"]
  linear_model_10_mse[i] = mean((log(data_lm_test$Global_Sales)-data_lm_yhat)^2)
}

## Linear Model Full
lm_full = lm(Global_Sales~.,data = data_global)
plot(lm_full)

summary(lm_full)
linear_model_10_mse
mean(linear_model_10_mse)

## Try to remove some variable but it did not remove any variables
step(lm_full)

## Random Forest 10 times
rf_model_10_mse = rep(0,10)

for(i in 1:10){
  set.seed(seeds[i])
  
  train = sample(1:nrow(data_global),nrow(data_global)*.80)
  data_randomforest = randomForest(log(Global_Sales)~., data = data_global, subset = train,importance = TRUE)
  data_randomforest_yhat = predict(data_randomforest, newdata = data_global[-train,])
  data_randomforest_test = data_global[-train,"Global_Sales"]
  rf_model_10_mse[i] = mean((log(data_randomforest_test$Global_Sales)-data_randomforest_yhat)^2)
}

## Random Forest Full Model
rf_full = randomForest(log(Global_Sales)~., data = data_global,importance = TRUE)
rf_full
varImpPlot(rf_full)
rf_model_10_mse
mean(rf_model_10_mse)
