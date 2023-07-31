#Package load
packages <- c("FSA", "FSAdata", "magrittr", "dplyr", "tidyr", "plyr", "tidyverse", "psych", "Hmisc",
              "plotrix", "ggplot2", "moments", "readxl", "readr", "epiDisplay", "corrplot", "gmodels", 
              "ggpubr", "car", "AICcmodavg", "ISLR", "caret", "textreg", "pROC", "glmnet", "Metrics")

install.packages(packages)
lapply(packages, require, character.only = TRUE)

college <-College

#------------------------------
#data library: 
#Private = A factor with levels No and Yes indicating private or public university, Apps = Number of applications received,
#Accept = Number of applications accepted, Enroll = Number of new students enrolled, Top10perc = Pct. new students from top 10% of H.S. class
#Top25perc = Pct. new students from top 25% of H.S. class, F.Undergrad = Number of fulltime undergraduates, P.Undergrad = Number of parttime undergraduates
#Outstate = Out-of-state tuition, Room.Board = Room and board costs, Books = Estimated book costs, Personal = Estimated personal spending, PhD = Pct. of faculty with Ph.D.'s
#Terminal = Pct. of faculty with terminal degree, S.F.Ratio = Student/faculty ratio, perc.alumni = Pct. alumni who donate, Expend = Instructional expenditure per student
#Grad.Rate = Graduation rate
#-------------------------------

#Overview of dataset
describe(college)
summary(college)
headTail(college,6)
college
sum(is.na(college))

#Label private and non-private
college$Private <- factor(college$Private, levels = c("No","Yes"), labels = c("Non-Private", "Private")) 
view(college)

lm.fit <- lm(Grad.Rate~., data = college)
lm.fit
summary(lm.fit)

# -- Ridge Regression
#Test and train split - 70% Train 30% Test
set.seed(160)
trainsample <- college$Private %>% createDataPartition (p = 0.7, list = FALSE)

train <- college[trainsample,]
test <- college[-trainsample,]
x_train <- model.matrix(Grad.Rate~.,train)[,-19]
x_test <- model.matrix(Grad.Rate~., test)[,-19]
y_train <- train$Grad.Rate
y_test <- test$Grad.Rate

#Ridge regression model 
#Cross validation - 20 fold
set.seed(112)
cross_v_ridge <- cv.glmnet(x_train, y_train, alpha=0, nfold = 20)
cross_v_ridge

best_model_1se <- cross_v_ridge$lambda.1se
best_model_min <- cross_v_ridge$lambda.min
plot(cross_v_ridge)
#Lambda.min = 3.27, lambda.max = 33.44 
#Plot shows mean-squared error scale differs after first dashline at log of 1, with 17 features. Suggests 17 features should be retained.

#ridge regression - min
ridge_min <- glmnet(x_train, y_train, alpha = 0, lambda = best_model_min)
coef(ridge_min)
ridge_min

#ridge regression - 1se
ridge_1se <- glmnet(x_train, y_train, alpha = 0, lambda = best_model_1se)
coef(ridge_1se)
ridge_1se

#Performance of the fit model by RMSE ---- Train Dataset
#RMSE performance of fit - min
RMSE_ridge_min_train_model <- predict(ridge_min, newx = x_train)
RMSE_train_ridge_min <- rmse(y_train, RMSE_ridge_min_train_model)
RMSE_train_ridge_min
#RMSE performance of fit - 1se
RMSE_ridge_1se_train_model <- predict(ridge_1se, newx = x_train)
RMSE_train_ridge_1se <- rmse(y_train, RMSE_ridge_1se_train_model)
RMSE_train_ridge_1se
#ridgemin = 12.96, ridge 1se = 13.76

#Performance of the fit model by RMSE ---- Test Dataset
#RMSE performance of fit - min
RMSE_ridge_min_test_model <- predict(ridge_min, newx = x_test)
RMSE_test_ridge_min <- rmse(y_test, RMSE_ridge_min_test_model)
RMSE_test_ridge_min

#RMSE performance of fit - 1se
RMSE_ridge_1se_test_model <- predict(ridge_1se, newx = x_test)
RMSE_test_ridge_1se <- rmse(y_test, RMSE_ridge_1se_test_model)
RMSE_test_ridge_1se
#ridge min = 12.15, ridge 1se = 12.67

#LASSO regression model 
#Cross Validation
cross_v_lasso <- cv.glmnet(x_train, y_train, alpha=1, nfolds=20)
cross_v_lasso
cross_v_lasso$lambda.min
cross_v_lasso$lambda.1se
plot(cross_v_lasso)
#Lasso min value =0.28, 1se 2.2. 11 variables change at the log of -1 and increase with less variables. (12 variables is a good fit number for the model) while 1se suggests 6,5 a good fit number.

#Lasso performance of fit - min
lasso_min <- glmnet(x_train, y_train, alpha = 1, lambda = cross_v_lasso$lambda.min)
lasso_min
coef(lasso_min)

#Lasso performance of fit - 1se
lasso_1se <- glmnet(x_train, y_train, alpha = 1, lambda = cross_v_lasso$lambda.1se)
coef(lasso_1se)
lasso_1se

#Performance of fit model by RMSE ---- Train Dataset
#RMSE performance of fit - min
RMSE_lasso_min_train_model <- predict(lasso_min, newx = x_train)
RMSE_train_lasso_min <- rmse(y_train, RMSE_lasso_min_train_model)
RMSE_train_lasso_min

#RMSE performance of fit - 1se
RMSE_lasso_1se_train_model <- predict(lasso_1se, newx = x_train)
RMSE_train_lasso_1se <- rmse(y_train, RMSE_lasso_1se_train_model)
RMSE_train_lasso_1se

#Performance of the fit model by RMSE ---- Test Dataset
#RMSE performance of fit - min
RMSE_lasso_min_test_model <- predict(lasso_min, newx = x_test)
RMSE_test_lasso_min <- rmse(y_test, RMSE_lasso_min_test_model)
RMSE_test_lasso_min

#RMSE performance of fit - 1se
RMSE_lasso_1se_test_model <- predict(lasso_1se, newx = x_test)
RMSE_test_lasso_1se <- rmse(y_test, RMSE_lasso_1se_test_model)
RMSE_test_lasso_1se
#RMSE train min = 12.95, RMSE train 1se = 13.65, RMSE test min = 12.19, RMSE test 1se = 12.76

#Stepwise Selection - AIC Comparison
full_model = lm(Grad.Rate ~ ., data = train)
summary(full_model)

model.final = step(full_model)
summary(model.final)

# -- Forward Selection
install.packages("olsrr")
library(olsrr)
forward_fit_p <- ols_step_forward_p(full_model, r = .05)
forward_fit_aic <- ols_step_forward_aic(full_model)
forward_fit_aic
forward_fit_p
## Forward fit suggests, f.undergrad with least followed by expend, apps, p.undergrad, etc. at the range of 4350 +. 

# -- Backward Selection - Least incremental predictor power
#p value - prem (p value for removal)
backward_fit_p <- ols_step_backward_p (full_model, prem = .05)
backward_fit_p

#AIC 
backward_fit_aic <- ols_step_backward_aic (full_model, prem = .05)
backward_fit_aic
#Backwise suggests, private, phD, Personal, Top10persc with 4350+

#Stepwise regression using p-values
both_fit_p <- ols_step_both_p (full_model, pent = .05, prem = .05)
both_fit_p
#Both direction suggests, expend, apps, p.undergrad, etc. with range 4350+

#Comparing Regression Models
#Forward model
model_1 <- lm(Grad.Rate~ Outstate + Top25perc + perc.alumni + Room.Board + P.Undergrad + Apps
              + Expend + F.Undergrad, data = train)
summary(model_1)
#Backward model
model_2 <- lm(Grad.Rate~ Accept + Enroll + Books + S.F.Ratio + Terminal + Top10perc +
                Personal + PhD + Private, data = train)
summary(model_2)
#Both model
model_3 <- lm(Grad.Rate~ Outstate + Top25perc + perc.alumni + Room.Board + P.Undergrad + 
                Apps + Expend, data = train)
summary(model_3)

#Lasso Model
model_4 <- lm(Grad.Rate~ Top10perc + Top25perc + Outstate + Room.Board + perc.alumni, data = train)

model_AIC <- AIC(model_1, model_2, model_3, model_4)
model_BIC <- BIC(model_1, model_2, model_3, model_4)

data.frame(model_AIC, model_BIC)
#Comparing the models, model_1 & 2 = Forward, both model performs the best. But, personally, the best model of choice is Lasso as the AIC, BIC is similar yet, the number of variables has the least. 