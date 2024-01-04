remove(list=ls())

setwd("C:\\Users\\Aanchal Dusija\\Desktop\\DATA SCIENCE PROJECT\\CODE")
getwd()

# Importing the dataset
data <- read.csv("C:\\Users\\Aanchal Dusija\\Desktop\\DATA SCIENCE PROJECT\\CODE\\DATA SCIENCE EXCEL FLIGHTS_2.csv",
                 header = TRUE, sep = ",")
#View(data)

# Calculating the departure delay (in mins)
sched_dep_time <- data$CRS_DEP_TIME
sched_dep_time_mins <- 0
act_dep_time_mins <- 0
act_dep_time <- data$DEP_TIME
for(i in 1:NROW(sched_dep_time)){
  if(is.na(act_dep_time[i]) == TRUE){
    act_dep_time_mins[i] <- NA}
  else if(sched_dep_time[i] %/% 100 == 0 & act_dep_time[i] %/% 1000 != 0) {
    sched_dep_time_mins[i] <- (sched_dep_time[i] %% 100) + ((24 + sched_dep_time[i] %/% 100) * 60)
  } else {
    sched_dep_time_mins[i] <- (sched_dep_time[i] %% 100) + (sched_dep_time[i] %/% 100) * 60 
  }
}
#View(sched_dep_time_mins)
#View(sched_dep_time)

act_dep_time_mins <- 0
for(i in 1:NROW(act_dep_time)){
  if(is.na(act_dep_time[i]) == TRUE){
    act_dep_time_mins[i] <- NA
  }
  else if(act_dep_time[i] %/% 100 == 0 & sched_dep_time[i] %/% 1000 != 0){
    act_dep_time_mins[i] <- (act_dep_time[i] %% 100) + ((24 + act_dep_time[i] %/% 100) * 60)
  } else {
    act_dep_time_mins[i] <- (act_dep_time[i] %% 100) + (act_dep_time[i] %/% 100) * 60 
  }
}
#View(act_dep_time_mins)

for(i in 1:NROW(act_dep_time)){
  if(is.na(act_dep_time_mins[i]) == TRUE){
    act_dep_time_mins[i] <- NA
  } else if(act_dep_time_mins[i] - sched_dep_time_mins[i] < -400){
    act_dep_time_mins[i] <- (act_dep_time[i] %% 100) + ((24 + act_dep_time[i] %/% 100) * 60)
  } else{}
}

dep_delay = 0
for(i in 1:NROW(act_dep_time_mins)) {
  if(is.na(act_dep_time_mins[i]) == TRUE){
    dep_delay[i] <- NA
  }
  else{
    dep_delay[i] <- act_dep_time_mins[i] - sched_dep_time_mins[i]
  }
}
#View(dep_delay)


# Categorising delayed departures into 0s and 1s if greater than 15 minutes
dep_delay_new = 0
for(i in 1:NROW(dep_delay)){
  if(is.na(dep_delay[i]) == TRUE){
    dep_delay_new[i] <- dep_delay[i]
  } else if(dep_delay[i] >= 15){
    dep_delay_new[i] <- 1
  } else {
    dep_delay_new[i] <- 0
  }
}
#View(dep_delay_new)



# Making the departure delays into bands of 15 minutes
dep_delay_group = 0
for(i in 1:NROW(dep_delay)){
  value = trunc(dep_delay[i] / 15)
  dep_delay_group[i] = value
}
#View(dep_delay_group)



#Viewing unique carriers present in the data

t <- unique(data$OP_UNIQUE_CARRIER)
#View(t)


# Calculating percentage of missing values

sum(is.na(dep_delay)) # There are 1681 missing values
length(dep_delay) #There are 99,990 observations in our data

missing_value_percent <- sum(is.na(dep_delay))/length(dep_delay)
missing_value_percent # 1.6811 percent values are missing


## Descriptive Statistics

# Summary of the data
# Univariate Analysis

summary(data)
head(data)
dim(data)

# Checking the class of each variable
sapply(data, class)

# Scatterplot matrix

# To plot only delays not early departures

delay_mins = 0
for(i in 1:NROW(dep_delay)){
  if(is.na(dep_delay[i]) == TRUE){
    delay_mins[i] <- NA
    data$FL_DATE[i] <- NA
  } else if(dep_delay[i] >= 0){
    delay_mins = c(delay_mins,dep_delay[i],data$FL_DATE[i])
  } else {}
}

# Scatterplot of Positive Delays

delay_plot = plot(delay_mins)


# Understanding the data structure

install.packages("Hmisc")
library(Hmisc)
describe(data)

## IMPUTING MISSING VALUES
set.seed(123)
data1 <- data
data1 <- cbind(data1,dep_delay,dep_delay_new,dep_delay_group)
data1 <- subset(data1, data1$dep_delay > 0)

library(Hmisc)
set.seed(123)
d<-aregImpute(~dep_delay+TAXI_OUT+TAXI_IN+DISTANCE+CARRIER_DELAY+WEATHER_DELAY+
                NAS_DELAY+SECURITY_DELAY+LATE_AIRCRAFT_DELAY, data = data1, 
              n.impute = 5,nk=0, match = 'closest')
imputed <- impute.transcan(d, imputation=3, data=data1, list.out=TRUE,
                           pr=FALSE, check=FALSE)
#View(imputed)
imputed<-data.frame(imputed)
# View(imputed)
data1<-cbind(data1,imputed)
data1<-data1[,-c(97,28,31,42,44,45,46,47,48)]
#View(data1)

# Boxplot of positive delays

boxplot(delay_mins, outline = TRUE, col = 'BLUE')


## Model 1 (Model with positive delays and outliers)

set.seed(123)
model_1<-lm(data1$dep_delay~data1$TAXI_OUT+data1$TAXI_IN+data1$DISTANCE+data1$CARRIER_DELAY+data1$WEATHER_DELAY+
              data1$NAS_DELAY+data1$SECURITY_DELAY+data1$LATE_AIRCRAFT_DELAY)
summary(model_1)  




# Removing Outliers
set.seed(123)
temp_data1 <- data1
#temp_data1[is.na(temp_data1)] <- 0 
set.seed(123)
temp_model1 <- lm(temp_data1$dep_delay~temp_data1$TAXI_OUT+temp_data1$TAXI_IN+temp_data1$DISTANCE+temp_data1$CARRIER_DELAY+temp_data1$WEATHER_DELAY+
                    temp_data1$NAS_DELAY+temp_data1$SECURITY_DELAY+temp_data1$LATE_AIRCRAFT_DELAY)

cooksdistance<-cooks.distance(temp_model1)
#View(cooksdistance)
cutoff<-1
out_df<-temp_data1
out_df<-cbind(out_df,cooksdistance)
out_df<-subset(out_df,cooksdistance > cutoff)
#View(out_df)


summary(dep_delay)
q1 <- -6
q3 <- 7
IQR <- q3-q1
lower_iqr <- q1 - (1.5 * IQR)
upper_iqr <- q3 + (1.5 * IQR)
out_iqr <- subset(data1, data1$dep_delay < lower_iqr | data1$dep_delay > upper_iqr)
#View(out_iqr)

set.seed(123)
data1 <- cbind(data1,cooksdistance)
data2 <- subset(data1,(data1$cooksdistance < cutoff) & (data1$dep_delay > lower_iqr | data1$dep_delay < upper_iqr))
#View(data2)

# Partitioning the data into train and test data

set.seed(111)
train_data<-sample(1:nrow(data2),0.8*nrow(data2),replace = FALSE)
train_data<-data2[train_data,]  
dim(train_data)
train_data1<-train_data[,c("dep_delay","DISTANCE","NAS_DELAY","TAXI_OUT","CARRIER_DELAY","SECURITY_DELAY","TAXI_IN","WEATHER_DELAY","LATE_AIRCRAFT_DELAY")]
dim(train_data1)
#View(train_data1)

test_data<-data2[!(rownames(data2) %in% rownames(train_data)),]
dim(test_data)
test_data1<-test_data[,c("dep_delay","DISTANCE","NAS_DELAY","TAXI_OUT","CARRIER_DELAY","SECURITY_DELAY","TAXI_IN","WEATHER_DELAY","LATE_AIRCRAFT_DELAY")]
dim(test_data1)
#View(test_data1)


# Model 2 (Model with positive delays and without the outliers)
set.seed(111)
model_2<-lm(dep_delay~., data = train_data1)
summary(model_2)

# MODEL PLOTS

par(mfrow = c(2, 2))
plot(model_2)
# Plots:
# 1.residuals vs Leverage
# 2.residuals vs Fitted
# 3.Normal Q-Q plot
# 4.square root of standardised residuals vs fitted values

# STEPWISE REGRESSION

library(MASS)
set.seed(111)
step.model <- stepAIC(model_2, direction = "both", 
                      trace = FALSE)
summary(step.model)

# Checking assumptions of regression
par(mfrow=c(1,1))
# 1. Checking whether residuals are normal  
qqnorm(step.model$residuals)

 # The plot indicates the residuals are normally distributed

# 2. To check the constant variance assumption
plot(step.model$fitted.values,step.model$residuals)

# Hence the errors dont have constant variance

# 3. To check if the regressors are independent

library(MASS)
install.packages("car")
library(car)
vif(step.model)
#View(vif(step.model))



# Correlation between independent variables
par(mfrow=c(1,1))
df <- data.frame(data2$TAXI_OUT, data2$TAXI_IN, data2$DISTANCE,
                 data2$CARRIER_DELAY, data2$WEATHER_DELAY, data2$NAS_DELAY, data2$SECURITY_DELAY, data2$LATE_AIRCRAFT_DELAY)
#View(df)

m <- cor(df, method = "pearson", use = "complete.obs")
#View(m)

install.packages("corrplot")
library(corrplot)
c1<-corrplot(m, method = "number", use = "complete.obs")
#View(c1)

# Correlation between dep_delay(dependent) and independent variables

df2<-data.frame(data2$dep_delay,data2$TAXI_OUT, data2$TAXI_IN, data2$DISTANCE,
                data2$CARRIER_DELAY, data2$WEATHER_DELAY, data2$NAS_DELAY,data2$SECURITY_DELAY, data2$LATE_AIRCRAFT_DELAY)
#View(df2)
m1<-cor(df2, method="pearson", use = "complete.obs")
#View(m1)

install.packages("corrplot")
library(corrplot)
c2<-corrplot(m1, method = "number", use = "complete.obs")
#View(c2)

# Factors contributing to delays

check_data <- data.frame(data2$CARRIER_DELAY, data2$WEATHER_DELAY, data2$LATE_AIRCRAFT_DELAY)
#View(check_data)
causes <- sapply(check_data, which.max)
#View(causes)

percent_causes <- data.frame(causes,(causes/NROW(check_data))*100)
#View(percent_causes)
colnames(percent_causes)[1]<-"Count"
colnames(percent_causes)[2]<-"Percentage"


################# Calculating airline-wise delays

data<-cbind(data,dep_delay,dep_delay_new)
airlinewise_df <- data.frame(data$OP_UNIQUE_CARRIER,data$dep_delay,data$dep_delay_new)
#View(airlinewise_df)

# Total no.of flights for each airline (unique carrier)
total<- tapply(airlinewise_df$data.dep_delay_new,airlinewise_df$data.OP_UNIQUE_CARRIER,length)
total

#Total no. of flight delays for each airline (unique carrier)

data3<-subset(data2, data2$dep_delay>15)
delays<-tapply(data3$dep_delay_new,data3$OP_UNIQUE_CARRIER,sum)
delays

# Percentage of flights delayed for each airline

percentage_delays <- (delays/total)*100
percentage_delays

# Minimum delay airlinewise

min_delay<-tapply(airlinewise_df$data.dep_delay,airlinewise_df$data.OP_UNIQUE_CARRIER,min, na.rm = TRUE)
min_delay

# Maximum delay airlinewise

max_delay<-tapply(airlinewise_df$data.dep_delay,airlinewise_df$data.OP_UNIQUE_CARRIER,max, na.rm = TRUE)
max_delay

# Mean delay airlinewise

mean_delay<-tapply(airlinewise_df$data.dep_delay,airlinewise_df$data.OP_UNIQUE_CARRIER,mean, na.rm = TRUE)
mean_delay

# Rank of delays

ranked_delays <- percentage_delays[order(percentage_delays)]
# View(ranked_delays)

# Data frame of airlinewise delays

airlinewise_df1 <- data.frame(delays,total,percentage_delays,min_delay,max_delay,mean_delay)
# View(airlinewise_df1)



## STEP MODEL RMSE for test data

#install.packages("Metrics")
library(Metrics)
predictions_step<-predict(step.model, newdata = test_data1)
#View(predictions_step)
# predicting for NA values
#test_data1_linear<-test_data1
#test_data1_linear[is.na(test_data1_linear)]<-0
#View(test_data1_linear)
#predictions_step0<-predict(step.model, newdata = test_data1_linear)
# View(predictions_step0)

#rmse(test_data1_linear$dep_delay,predictions_step0)
rmse(test_data1$dep_delay, predictions_step)

# RMSE for model_2
pred_mod2<-predict(model_2, newdata = test_data1)
rmse(test_data1$dep_delay, pred_mod2)


################################# LOGISTIC REGRESSION #######################################


library(ggplot2)
# TRAIN DATA LOGISTIC
df_glm_train <-cbind(train_data[,89],train_data1[,-1]) 
#View(df_glm_train)
#Changing the name of the first column to "dep_delay_new1"
colnames(df_glm_train)[1]<-"dep_delay_new1"

# TEST DATA LOGISTIC
df_glm_test<-cbind(test_data[,89],test_data1[,-1])
colnames(df_glm_test)[1]<-"dep_delay_new2"
#View(df_glm_test)

# TEST DATA LOGISTIC WITHOUT NAs
#df_glm_test0<-cbind(test_data[,98],test_data1_linear[,-1])
#colnames(df_glm_test0)[1]<-"dep_delay_new3"

# CONVERTING THE DEPENDENT VARIABLE INTO A FACTOR

df_glm_train$dep_delay_new1<-as.factor(df_glm_train$dep_delay_new1)
class(df_glm_train$dep_delay_new1)
levels(df_glm_train$dep_delay_new1)

df_glm_test$dep_delay_new2<-as.factor(df_glm_test$dep_delay_new2)
class(df_glm_test$dep_delay_new2)
levels(df_glm_test$dep_delay_new2)

# LOGISTIC REGRESSION MODEL
set.seed(123)
glm_train<-glm(dep_delay_new1~.,data = df_glm_train, family = binomial(link="logit"))
summary(glm_train)

# predict
predicted_log<-predict(glm_train, newdata = df_glm_test, type = "response")
# View(predicted_log)

# predicting with complete test_data set (NO NA values)
#predicted_log0<- predict(glm_train, newdata = df_glm_test0, type = "response")
# View(predicted_log0)

## Finding the Optimal Cut off
install.packages("InformationValue")
library(InformationValue)

optCutOff <- optimalCutoff(df_glm_test$dep_delay_new2, predicted_log)[1] 
optCutOff
# optCutOff is 0.47

greater<-subset(predicted_log,predicted_log>=optCutOff)
length(greater)
lesser<-subset(predicted_log,predicted_log<optCutOff)
length(lesser)

# Misclassification Error
library(InformationValue)
misClassError(df_glm_test$dep_delay_new2, predicted_log, threshold = optCutOff)

#Confusion Matrix
confusionMatrix(df_glm_test$dep_delay_new2, predicted_log, threshold = optCutOff)
accu<-(2692+2972)/(2692+2972+447+668)
accu


# Concordance
Concordance(df_glm_test$dep_delay_new2, predicted_log)

# Area under ROC curve
#install.packages("plotROC")
library(plotROC)
plotROC(df_glm_test$dep_delay_new2, predicted_log)

###### DECISION TREES (REGRESSION)

#install.packages("tree")
#library(tree)
library(rpart)
library(rpart.plot)

dec_tree_fit<-rpart(dep_delay~., data = train_data1)
plot(dec_tree_fit)
text(dec_tree_fit, pretty = 0)
plotcp(dec_tree_fit)

# Pruning the tree
dec_tree_fit1<-prune.rpart(dec_tree_fit, cp = 0.011)
plot(dec_tree_fit1)
text(dec_tree_fit1, pretty = 0)

# rpart.plot
rpart.plot(dec_tree_fit)

#set.seed(123)
#dec_tree_fit<-tree(dep_delay~., data = train_data1)
#plot(dec_tree_fit)
#text(dec_tree_fit,pretty = 0)

# Pruning the tree
#prune.tree(dec_tree_fit)
#plot(prune.tree(dec_tree_fit))

## Trial Prune
#library(rpart)
#prune.try<-rpart(dep_delay~., data = train_data1)
#rsq.rpart(prune.try)

# rpart plot
#plot(dec_tree_fit)
#text(dec_tree_fit, pretty = 0)
#rpart.plot(dec_tree_fit) 

# RMSE for test_data
predicted_tree<-predict(dec_tree_fit, newdata = test_data1)
library(Metrics)
rmse(test_data1$dep_delay ,predicted_tree)
#View(predicted_tree)

# DECISION TREE (CLASSIFICATION)
#library(tree)
library(rpart)
library(rpart.plot)

# TRAIN DATA
df_class_train<-cbind(train_data[,89],train_data1[,-1])
colnames(df_class_train)[1]<-"dep_delay_new3"
# View(df_class_train)

# TEST DATA 
df_class_test<-cbind(test_data[,89],test_data1[,-1])
colnames(df_class_test)[1]<-"dep_delay_new4"
# View(df_mlm_test)

# DECISION TREE CLASSIFICATION MODEL

library(rpart)
library(rpart.plot)

set.seed(123)
dec_tree_fit_classif<-rpart(dep_delay_new3~., data = df_class_train, method = "class")
plot(dec_tree_fit_classif)
text(dec_tree_fit_classif, pretty = 0)
plotcp(dec_tree_fit_classif)

# Plotting using rpart.plot
library(rpart.plot)
rpart.plot(dec_tree_fit_classif)

#set.seed(123)
#dec_tree_fit_classif<-tree(dep_delay_group4~., data = df_class_train)
#plot(dec_tree_fit_classif)
#text(dec_tree_fit_classif,pretty = 0)


#prune.tree(dec_tree_fit_classif)
#plot(prune.tree(dec_tree_fit_classif))

# Pruning the tree
set.seed(123)
dec_tree_fit_classif1<-prune.rpart(dec_tree_fit_classif, cp = 0.021)
plot(dec_tree_fit_classif1)
text(dec_tree_fit_classif1, pretty = 0)
rpart.plot(dec_tree_fit_classif1)

# Predicting
predicted_tree_classif<-predict(dec_tree_fit_classif, newdata = df_class_test, type = "class")
# View(predicted_tree_classif)

# Misclassification Error
library(InformationValue)
misClassError(l, l1)


# Confusion matrix
library(Metrics)
dec_mat<-table(predicted_tree_classif, df_class_test$dep_delay_new4)
dec_mat

accu1<-(2698+2896)/(2698+2896+744+441)
accu1

# Concordance
Concordance(l, l1)


# Area under ROC curve
#install.packages("plotROC")
library(plotROC)
plotROC(df_class_test$dep_delay_new4, predicted_tree_classif)

l<-ifelse(df_class_test$dep_delay_new4=="1",1,0)
l1<-ifelse(predicted_tree_classif=="1",1,0)
plotROC(l,l1)


#rmse(df_class_test$dep_delay_group5 ,predicted_tree_classif)
#15*rmse(df_class_test$dep_delay_group5 ,predicted_tree_classif)



###### MULTINOMIAL LOGISTIC REGRESSION
# install.packages("nnet")
# library(nnet)
# 
# # TRAIN DATA MULTINOMIAL
# 
# df_mlm_train<-cbind(train_data[,90],train_data1[,-1])
# # View(df_mlm_train)
# # Changing the name of the column to "dep_delay_group1"
# colnames(df_mlm_train)[1]<-"dep_delay_group1"
# 
# # TEST DATA MULTINOMIAL
# df_mlm_test<-cbind(test_data[,90],test_data1[,-1])
# colnames(df_mlm_test)[1]<-"dep_delay_group2"
# # View(df_mlm_test)
# 
# # TEST DATA MULTINOMIAL WITHOUT NAs
# #df_mlm_test0<-cbind(test_data[,99],test_data1_linear[,-1])
# #colnames(df_mlm_test0)[1]<-"dep_delay_group3"
# # View(df_mlm_test0)
# 
# # CONVERTING THE DEPENDENT VARIABLE INTO A FACTOR
# df_mlm_train$dep_delay_group1<-as.factor(df_mlm_train$dep_delay_group1)
# class(df_mlm_train$dep_delay_group1)
# levels(df_mlm_train$dep_delay_group1)
# 
# df_mlm_test$dep_delay_group2<-as.factor(df_mlm_test$dep_delay_group2)
# class(df_mlm_test$dep_delay_group2)
# levels(df_mlm_test$dep_delay_group2)

# Multinomial Logistic Model
# set.seed(123)
# mlm_train<-multinom(dep_delay_group1~.,data = df_mlm_train)
# summary(mlm_train)
# 
# # predict
# 
# library(Metrics)
# predictions_mlm<-predict(mlm_train, newdata = df_mlm_test)
# #View(predictions_mlm)
# 
# # Checking the Confusion matrix and misclassification error
# table(predictions_mlm, df_mlm_test$dep_delay_group2)
# View(table(predictions_mlm, df_mlm_test$dep_delay_group2))

############ BAGGING

#install.packages("randomForest")
library(randomForest)
set.seed(123)

bag.model<-randomForest(dep_delay~.,data = train_data1, mtry=8, na.action = na.omit, importance = TRUE)
bag.model

# PREDICTING 
bag.predict<-predict(bag.model, newdata = test_data1)
#View(bag.predict)

# PREDICTING WITH 0s in place of NAs
#bag.predict0<-predict(bag.model, newdata = test_data1_linear)
#View(bg.predict0)

# Plotting predicted values against the dependent variable
plot(bag.predict, test_data1$dep_delay)
abline(0,1)

# RMSE for test_data
library(Metrics)
rmse(test_data1$dep_delay, bag.predict)

# Variable Importance Plot
varImpPlot(bag.model)

# Comparing RMSE of Decision Trees (Regression) with the above Bagging algorithm
rmse(test_data1$dep_delay ,predicted_tree)
# Hence, Mean Squared Error for BAGGING is less than Mean Squared Error 
# computed using Decision Tree

############# RANDOM FOREST
library(randomForest)
set.seed(123)
rf.model<-randomForest(dep_delay~., data = train_data1, mtry = 6, 
                       importance = TRUE)
rf.model

# PREDICTING 
rf.predict<-predict(rf.model, newdata = test_data1)
#View(rf.predict)

# PREDICTING WITH 0s in place of NAs
#rf.predict0<-predict(rf.model, newdata = test_data1_linear)
#View(rf.predict0)

# Plotting predicted values against the dependent variable
plot(rf.predict, test_data1$dep_delay)
abline(0,1)

# RMSE 
rmse(test_data1$dep_delay, rf.predict)

# Variable Importance Plot
varImpPlot(rf.model)

######
rmse(test_data1$dep_delay ,predicted_tree)
# Accuracy for Decision Tree = 29.38 

rmse(test_data1$dep_delay, bag.predict)
# Accuracy for Bagging = 12.50163

rmse(test_data1$dep_delay, rf.predict)
# Accuracy for Random Forest = 12.45464

# Variable Importance
importance(rf.model)
varImpPlot(rf.model)
# As shown by the Variable importance graph, 
#late aircraft delay and carrier delay are the most important variables

# Out of Bag Error
plot(rf.model) #The graph becomes stable around 200 tress so we can 
# reduce the number of trees to 200 

city<-tapply(data2$dep_delay_new, data2$ORIGIN, sum)
View(city)

# AUROC Curves

#predicted_list<-list(predicted_log, predicted_tree_classif)
#View(predicted_list)

set.seed(123)
glm_train<-glm(dep_delay_new1~.,data = df_glm_train, family = binomial(link="logit"))
summary(glm_train)


predicted_log<-predict(glm_train, newdata = df_glm_test, type = "response")
View(predicted_log)
pred_log<-ifelse(predicted_log>0.47,1,0)
View(pred_log)
#pred_log<-as.factor(pred_log)

set.seed(123)
dec_tree_fit_classif<-rpart(dep_delay_new2~., data = df_glm_train, method = "class")
predicted_tree_classif<-predict(dec_tree_fit_classif, newdata = df_glm_test, type = "class")
#predicted_tree_classif<-as.double(predicted_tree_classif)
class(predicted_tree_classif)
View(predicted_tree_classif)
#predicted_tree_classif1<-ifelse(predicted_tree_classif==1,0,1)
#View(predicted_tree_classif1)

predicted_list1<-list(pred_log, predicted_tree_classif)
View(predicted_list1)

m1<-length(predicted_list1)
m1



actuals_list1<-rep(list(df_glm_test$dep_delay_new2),m1)
View(actuals_list1)

library(ROCR)
pred2<-prediction(predicted_list1, actuals_list1)
rocs<-performance(pred2, "tpr", "fpr")
plot(rocs, col = as.list(1:m1), main = "Set of ROC Curves")
legend(x="bottomright", legend = c("Binary Logistic Regression","Decision Trees"),
       fill = 1:m1)