#Package install and load
packages <- c("FSA", "FSAdata", "magrittr", "dplyr", "tidyr", "plyr", "tidyverse", "psych", "Hmisc",
              "plotrix", "ggplot2", "moments", "readxl", "readr", "epiDisplay", "corrplot", "gmodels", 
              "ggpubr", "car", "AICcmodavg", "ISLR", "caret")

install.packages(packages)
lapply(packages, require, character.only = TRUE)
#Data load - college admission from ISLR Library
#Statistics for a large number of US Colleges from the 1995 issue of US News and World Report.
college <- College

#data library: 
#777 observations, 18 variables
#Private = A factor with levels No and Yes indicating private or public university, Apps = Number of applications received,
#Accept = Number of applications accepted, Enroll = Number of new students enrolled, Top10perc = Pct. new students from top 10% of H.S. class
#Top25perc = Pct. new students from top 25% of H.S. class, F.Undergrad = Number of fulltime undergraduates, P.Undergrad = Number of parttime undergraduates
#Outstate = Out-of-state tuition, Room.Board = Room and board costs, Books = Estimated book costs, Personal = Estimated personal spending, PhD = Pct. of faculty with Ph.D.'s
#Terminal = Pct. of faculty with terminal degree, S.F.Ratio = Student/faculty ratio, perc.alumni = Pct. alumni who donate, Expend = Instructional expenditure per student
#Grad.Rate = Graduation rate

view(college)
describe(college)

headTail(college,6)

class(college)
str(college)
summary(college)

sum(is.na(college))

#Label private and non-private into 0 and 1. 
college$Private <- factor(college$Private, levels = c("No","Yes"), labels = c("Non-Private", "Private")) 
view(college)

#Filter data to private
table(college$Private)
status_private <- college %>% 
  filter(Private == "Private")

#Plot Private Graduation Rate per school
ggplot(data = status_private, aes(x = Grad.Rate)) +
  geom_bar() + ggtitle ("Private Graduation Rate per School") + xlab("Graduation Rate") + ylab("Count")

#Filter data to public
status_non_private <- college %>% 
  filter(Private == "Non-Private")
status_non_private

#Plot Public Graduation Rate per school
ggplot(data = status_non_private, aes(x = Grad.Rate)) +
  geom_bar() + ggtitle ("Public Graduation Rate per School") + xlab("Graduation Rate") + ylab("Count")

#Under assumption, private will accept more students (differentiating the number)
pairs(~ Apps + Enroll + F.Undergrad + P.Undergrad + Outstate + Personal + Grad.Rate, data = college)

#Correlation plot
par(mfrow = c(1, 4))

#Fulltime vs. personal 
ggplot(college,aes(x = F.Undergrad, y = Personal, color = Private)) +
  geom_jitter(width = 0, height = 0.09, alpha = 0.7)

#Fulltime vs. outstate
ggplot(college,aes(x = F.Undergrad, y = Outstate, color = Private)) +
  geom_jitter(width = 0, height = 0.09, alpha = 0.7)

#Part Time vs. Personal
ggplot(college,aes(x = P.Undergrad, y = Personal, color = Private)) +
  geom_jitter(width = 0, height = 0.09, alpha = 0.7)

#Part time vs. Outstate
ggplot(college,aes(x = P.Undergrad, y = Outstate, color = Private)) +
  geom_jitter(width = 0, height = 0.09, alpha = 0.7)

#Graduation Rate vs. Fulltime
ggplot(college,aes(x = Grad.Rate, y = Outstate, color = Private)) +
  geom_jitter(width = 0, height = 0.09, alpha = 0.7)


#Boxplots for underlying variables: F.Undergrad, P.Undergrad, Personal, Outstate - Checking for Boundary Overlaps
ggplot(data=college, aes(x=Private, y=F.Undergrad, fill = Private)) + geom_boxplot(width=0.5)
ggplot(data=college, aes(x=Private, y=P.Undergrad, fill = Private)) + geom_boxplot(width=0.5/length(unique(college$Private)))
ggplot(data=college, aes(x=Private, y=Personal, fill = Private)) + geom_boxplot(width=0.5/length(unique(college$Private)))
ggplot(data=college, aes(x=Private, y=Outstate, fill = Private)) + geom_boxplot(width=0.5/length(unique(college$Private)))
#Personal Overlaps - Confirms not suitable for choices

#Train and Test - 70% Train, 30% Test
set.seed(156)
trainsamples <- college$Private %>% createDataPartition (p = 0.7, list = FALSE)
train_data_college <- college[trainsamples,]
testdata <- college[-trainsamples, ]
head(train_data_college)

#Logistic Regression Modelling
install.packages("texreg")
library(texreg)
#Model 1 - P.Undergrad + Outstate
Logist_Model1 <- glm(Private ~ P.Undergrad + Outstate, data = train_data_college, family = binomial(link = "logit"))
screenreg(Logist_Model1)
summary(Logist_Model1)
#P-Value = AIC 332.29,Undergrad = 1.09e-16, Outstate = 2e-16, < alpha

confint(Logist_Model1)
confint.default(Logist_Model1)
exp(coef(Logist_Model1))
#Intercept = 0.04, P.Undergrad = 0.998, Outstate 1.00
#P.Undergrad is a good indicator considered for predictor.

#Model 2 - F.Undergrad + P.Undergrad + Outstate
Logist_Model2 <- glm(Private ~ F.Undergrad + P.Undergrad + Outstate, data = train_data_college, family = binomial(link = "logit"))
screenreg(Logist_Model2)
summary(Logist_Model2)
#AIC = 230.13

confint(Logist_Model2)
confint.default(Logist_Model2)
exp(coef(Logist_Model2))
#Intercept = 0.055, F.Undergrad = 0.9994, P.Undergrad = 0.9998, Outstate = 1.00

#Model 3 - F.Undergrad + Outstate
Logist_Model3 <- glm(Private ~ F.Undergrad + Outstate, data = train_data_college, family = binomial(link = "logit"))
screenreg(Logist_Model3)
summary(Logist_Model3)

confint(Logist_Model3)
confint.default(Logist_Model3)
exp(coef(Logist_Model3))

#Model 4 - Logistic Function all variables
Logist_Model4 <- glm(Private ~ ., data = train_data_college, family = binomial(link = "logit"))
screenreg(Logist_Model4)
summary(Logist_Model4)
#AIC = 228.98

confint(Logist_Model4)
confint.default(Logist_Model4)
exp(coef(Logist_Model4))
#Intercept = 0.054, F.Undergrad = 0.9994, Outstate = 1.00
install.packages('caret')

#Import required library
library(caret)


#Test Predictions
test_probabilities <- predict(Logist_Model3, newdata = testdata, type = "response")
test_class_prob <- as.factor(ifelse(test_probabilities > 0.5, yes = 1, no = 0))
Con_Matrix_test <- table(actual = testdata$Private, expected.value = test_probabilities)

confusionMatrix(test_class_prob, testdata$Private, positive = "Yes")
sum(diag(Con_Matrix_test)) / sum(Con_Matrix_test)

#Train Predictions
train_probabilities <- predict(Logist_Model3, newdata = train_data_college, type = "response")
class_probabilities <- as.factor(ifelse(train_probabilities > 0.5, yes = 1, no = 0))
tibble::glimpse(class_probabilities)

#Confusion Matrix
Con_Matrix <- table(actual = train_data_college$Private, expected.value = class_probabilities)
Con_Matrix
confusionMatrix(class_probab, train_data_college$Private, positive = "Yes")
#Non-Private 127 22
#Private     15 381

sum(diag(Con_Matrix)) / sum(Con_Matrix)
#0.932

#ROC Curve
install.packages("pROC")
library(pROC)
ROC_Private <- roc(testdata$Private, test_probabilities)
plot(ROC_Private) 

print(auc(ROC_Private))
#Under the curve - Almost a perfect triangle
