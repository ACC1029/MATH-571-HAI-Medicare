#all libraries needed for the below
library(tidyverse)
library(glmnet)
library(caret)
library(ggplot2)


#trying this will all features to begin with and will repeat with the correlated variables removed 
###############load
#load the CSV provided by Anna, housed in the git repository, as a tibble
#hai_6_all_data_nona <- read_csv("hai_6_all_data_nona.csv")


###############make sure the categorical values are factors
##commenting this out now that I'm pulling from Anna's latest code where this is factor-zed 
# hai_6_all_data_nona$mortality <- factor(hai_6_all_data_nona$mortality)
# hai_6_all_data_nona$readmission <- factor(hai_6_all_data_nona$readmission)
# hai_6_all_data_nona$effectiveness <- factor(hai_6_all_data_nona$effectiveness)
# hai_6_all_data_nona$timeliness <- factor(hai_6_all_data_nona$timeliness)
# hai_6_all_data_nona$patient_exp <- factor(hai_6_all_data_nona$patient_exp)
# hai_6_all_data_nona$sir_comp <- factor(hai_6_all_data_nona$sir_comp)
# hai_6_all_data_nona$val_care_cat <- factor(hai_6_all_data_nona$val_care_cat)

###############recode
#look at the distribution of scores
hai_6_all_data_nona %>%
  select(provider_id, sir_score) %>%
  count(sir_score)

min(hai_6_all_data_nona$sir_score)
#0
max(hai_6_all_data_nona$sir_score)
#5.219
median(hai_6_all_data_nona$sir_score)
#.83
mean(hai_6_all_data_nona$sir_score)
#.863589

ggplot(data=hai_6_all_data_nona, aes(hai_6_all_data_nona$sir_score)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")

#are there any that are exactly = the mean?
hai_6_all_data_nona %>%
  select(provider_id,sir_score) %>%
  filter(sir_score == mean(hai_6_all_data_nona$sir_score)) 
#no there are none that are exactly equal to the mean

#recode
#http://rprogramming.net/recode-data-in-r/
hai_6_nona_recoded <- hai_6_all_data_nona
hai_6_nona_recoded$above_avg_score <- NA
hai_6_nona_recoded$above_avg_score[hai_6_nona_recoded$sir_score > mean(hai_6_all_data_nona$sir_score)] <- TRUE
hai_6_nona_recoded$above_avg_score[hai_6_nona_recoded$sir_score < mean(hai_6_all_data_nona$sir_score)] <- FALSE

hai_6_nona_recoded %>%
  select(provider_id,above_avg_score) %>%
  count(above_avg_score)


#above_avg_score     n
#1            F     1649
#2            T  1419


##############################We're first going to try lasso to predict the actual HAI 6 measure SIR score
###############separate into test and train
#set a seed for reproducibility
set.seed(1) 

#get the num
hai_6_inTrain <- createDataPartition(y = hai_6_nona_recoded$above_avg_score,
                                     ## the outcome data are needed
                                     p = .8,
                                     ## The percentage of data in the
                                     ## training set
                                     list = FALSE)

hai_6_training <- hai_6_nona_recoded[ hai_6_inTrain,]

hai_6_testing <- hai_6_nona_recoded[-hai_6_inTrain,]

nrow(hai_6_training)
#2456
nrow(hai_6_testing)
#612

#let's look at the distriibution of some of these features within test and train
#hospital owner
##full (no na) dataset
hai_6_all_data_nona %>%
  select(provider_id, hospital_owner) %>%
  count(hospital_owner)

##train
ggplot(data=hai_6_training, aes(hai_6_training$hospital_owner)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")
##test
ggplot(data=hai_6_testing, aes(hai_6_testing$hospital_owner)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")

##hospital_type
#train
ggplot(data=hai_6_training, aes(hai_6_training$hospital_type)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")
#test
ggplot(data=hai_6_testing, aes(hai_6_testing$hospital_type)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")

##hospital_rating
#train
ggplot(data=hai_6_training, aes(hai_6_training$hospital_rating)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")
#test
ggplot(data=hai_6_testing, aes(hai_6_testing$hospital_rating)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")

##hospital owner
#train
ggplot(data=hai_6_training, aes(hai_6_training$mortality)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")

#test
ggplot(data=hai_6_testing, aes(hai_6_testing$mortality)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")

#I'd like to get this all onto one plot, but it's taking more fiddling than I want to do right now
#ggplot(histogram, aes(hospital_owner)) + 
#  theme(axis.text.x=element_text(angle=90,hjust=1)) +
#  geom_histogram(data = hai_6_training, stat = "count", fill = "yello", alpha = 0.2) + 
#  geom_histogram(data = hai_6_testing, stat = "count", fill = "blue", alpha = 0.2) +
# geom_histogram(data = hai_6_all_data_nona, stat = "count", fill = "green", alpha = 0.2)  

###############create the logistic regression with all the factors
form_all_fact <- hai_6_training$above_avg_score ~ hai_6_training$hospital_owner + hai_6_training$hospital_type + hai_6_training$hospital_rating + hai_6_training$mortality 
  + hai_6_training$readmission + hai_6_training$effectiveness + hai_6_training$timeliness 
  + hai_6_training$spend_score + hai_6_training$payment + hai_6_training$val_care_cat

  
fit_all_fact <- glm(formula = form_all_fact, family = binomial(link = "logit"), data =  hai_6_training)
summary(fit_all_fact)
#AIC: 3397.6

#look at it!
plot(fit_all_fact)

###############predict

pred_all_fact <- predict(fit_all_fact, type="response")
#cfm <- confusionMatrix(pred_all_fact, hai_6_training$above_avg_score)
#table(pred = pred_all_fact, true = hai_6_training$above_avg_score)
pred_all_fact

#tf_pred_for_step <- pred_for_step
all_predictions <- ifelse(pred_all_fact > 0.5, TRUE, FALSE) 
all_predictions
train_all_cfm <- confusionMatrix(all_predictions, hai_6_training$above_avg_score)
train_all_cfm
# Confusion Matrix and Statistics
# 
# Reference
# Prediction FALSE TRUE
# FALSE  1173  959
# TRUE    147  177
# 
# Accuracy : 0.5497          
# 95% CI : (0.5297, 0.5695)
# No Information Rate : 0.5375          
# P-Value [Acc > NIR] : 0.116

###############test predictions
test_pred_all_fact <- predict(fit_all_fact, newdata = hai_6_testing, type="response")
#cfm <- confusionMatrix(pred_all_fact, hai_6_training$above_avg_score)
#table(pred = pred_all_fact, true = hai_6_training$above_avg_score)
test_pred_all_fact

#tf_pred_for_step <- pred_for_step
test_all_predictions <- ifelse(test_pred_all_fact > 0.5, TRUE, FALSE) 
test_all_predictions
test_all_cfm <- confusionMatrix(test_all_predictions, hai_6_testing$above_avg_score)
test_all_cfm

###############validate the fit



###############create the logistic regression using forward stepwise feature selection
# form_all_fact <- hai_6_training$above_avg_score ~ hai_6_training$hospital_type+ hai_6_training$hospital_owner+ hai_6_training$mortality+ hai_6_training$readmission+
# hai_6_training$effectiveness+ hai_6_training$timeliness+ 
# hai_6_training$patient_exp+ hai_6_training$spend_score


fit_for_step <- step(glm(formula = form_all_fact, family = binomial(link = "logit"), data =  hai_6_training), direction = "forward")
# Start:  AIC=3397.6
# hai_6_training$above_avg_score ~ hai_6_training$hospital_owner + 
#   hai_6_training$hospital_type + hai_6_training$hospital_rating + 
#   hai_6_training$mortality



summary(fit_for_step)


#look at it!
plot(fit_for_step)

###############predict


###############test predictions

###############validate the fit

###############create the logistic regression using backward stepwise feature selection
# form_all_fact <- hai_6_training$above_avg_score ~ hai_6_training$hospital_type+ hai_6_training$hospital_owner+ hai_6_training$mortality+ hai_6_training$readmission+
# hai_6_training$effectiveness+ hai_6_training$timeliness+ 
# hai_6_training$patient_exp+ hai_6_training$spend_score


fit_back_step <- step(glm(formula = form_all_fact, family = binomial(link = "logit"), data =  hai_6_training), direction = "backward")
# AIC=3390.25
#hai_6_training$above_avg_score ~ hai_6_training$mortality

summary(fit_back_step)


#look at it!
plot(fit_back_step)

###############predict


###############test predictions

###############validate the fit

###############create the logistic regression using bidirectional stepwise feature selection
# form_all_fact <- hai_6_training$above_avg_score ~ hai_6_training$hospital_type+ hai_6_training$hospital_owner+ hai_6_training$mortality+ hai_6_training$readmission+
# hai_6_training$effectiveness+ hai_6_training$timeliness+ 
# hai_6_training$patient_exp+ hai_6_training$spend_score


fit_both_step <- step(glm(formula = form_all_fact, family = binomial(link = "logit"), data =  hai_6_training), direction = "both")
# AIC=3390.25
#hai_6_training$above_avg_score ~ hai_6_training$mortality

summary(fit_both_step)

#look at it!
plot(fit_both_step)

###############predict
pred_both_step <- predict(fit_both_step, type = "response")
pred_both_step

#tf_pred_for_step <- pred_for_step
both_predictions <- ifelse(pred_both_step > 0.5, TRUE, FALSE) 
both_predictions
train_cfm <- confusionMatrix(both_predictions, hai_6_training$above_avg_score)
train_cfm
#          Reference
# Prediction FALSE TRUE
# FALSE  1166  963
# TRUE    154  173
# 
# Accuracy : 0.5452         
# 95% CI : (0.5253, 0.565)
# No Information Rate : 0.5375         
# P-Value [Acc > NIR] : 0.2271         
# 
# Kappa : 0.0375         
# Mcnemar's Test P-Value : <2e-16         
#                                          
#             Sensitivity : 0.8833         
#             Specificity : 0.1523         
#          Pos Pred Value : 0.5477         
#          Neg Pred Value : 0.5291         
#              Prevalence : 0.5375         
#          Detection Rate : 0.4748         
#    Detection Prevalence : 0.8669         
#       Balanced Accuracy : 0.5178          
        
#table(pred = pred_for_step, hai_6_training$above_avg_score)
#table(pred = pred_for_step)
#pred_for_step[5]
##
# make class predictions for the testing set
# y_pred_class = logreg.predict(X_test)
# Classification accuracy: percentage of correct predictions
# 
# calculate accuracy
# from sklearn import metrics
# print(metrics.accuracy_score(y_test, y_pred_class))

###############test predictions
test_pred_both_step <- predict(fit_both_step, newdata = hai_6_testing$above_avg_score, type = "response")
test_pred_both_step

#tf_pred_for_step <- pred_for_step
predictions <- ifelse(test_pred_both_step > 0.5, TRUE, FALSE) 
predictions

test_cfm <- confusionMatrix(test_pred_both_step, hai_6_testing$above_avg_score)
test_cfm

###############validate the fit
