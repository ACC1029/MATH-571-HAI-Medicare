#all libraries needed for the below
library(tidyverse)
library(glmnet)
library(caret)
library(ggplot2)


#trying this will all features to begin with and will repeat with the correlated variables removed 
###############load
#load the CSV provided by Anna, housed in the git repository, as a tibble
hai_6_all_data_nona <- read_csv("hai_6_all_data_nona.csv")

###############make sure the categorical values are factors
hai_6_all_data_nona$mortality_code <- factor(hai_6_all_data_nona$mortality_code)
hai_6_all_data_nona$readmission_code <- factor(hai_6_all_data_nona$readmission_code)
hai_6_all_data_nona$effectiveness_of_care_code <- factor(hai_6_all_data_nona$effectiveness_of_care_code)
hai_6_all_data_nona$timeliness_of_care_code <- factor(hai_6_all_data_nona$timeliness_of_care_code)
hai_6_all_data_nona$patient_experience_code <- factor(hai_6_all_data_nona$patient_experience_code)
hai_6_all_data_nona$SIR_compared_to_national_code <- factor(hai_6_all_data_nona$SIR_compared_to_national_code)
hai_6_all_data_nona$value_of_care_category_code <- factor(hai_6_all_data_nona$value_of_care_category_code)

###############recode
#look at the distribution of scores
hai_6_all_data_nona %>%
  select(provider_id, SIR_score) %>%
  count(SIR_score)

min(hai_6_all_data_nona$SIR_score)
#0
max(hai_6_all_data_nona$SIR_score)
#5.219
median(hai_6_all_data_nona$SIR_score)
#.83
mean(hai_6_all_data_nona$SIR_score)
#.863589

ggplot(data=hai_6_all_data_nona, aes(hai_6_all_data_nona$SIR_score)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")

#are there any that are exactly = the mean?
hai_6_all_data_nona %>%
  select(provider_id,SIR_score) %>%
  filter(SIR_score == mean(hai_6_all_data_nona$SIR_score)) 
#no there are none that are exactly equal to the mean

#recode
#http://rprogramming.net/recode-data-in-r/
hai_6_nona_recoded <- hai_6_all_data_nona
hai_6_nona_recoded$above_avg_score <- NA
hai_6_nona_recoded$above_avg_score[hai_6_nona_recoded$SIR_score > mean(hai_6_all_data_nona$SIR_score)] <- TRUE
hai_6_nona_recoded$above_avg_score[hai_6_nona_recoded$SIR_score < mean(hai_6_all_data_nona$SIR_score)] <- FALSE

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

##hospital_overall_rating
#train
ggplot(data=hai_6_training, aes(hai_6_training$hospital_overall_rating)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")
#test
ggplot(data=hai_6_testing, aes(hai_6_testing$hospital_overall_rating)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")

##hospital owner
#train
ggplot(data=hai_6_training, aes(hai_6_training$mortality_code)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")

#test
ggplot(data=hai_6_testing, aes(hai_6_testing$mortality_code)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")

#I'd like to get this all onto one plot, but it's taking more fiddling than I want to do right now
#ggplot(histogram, aes(hospital_owner)) + 
#  theme(axis.text.x=element_text(angle=90,hjust=1)) +
#  geom_histogram(data = hai_6_training, stat = "count", fill = "yello", alpha = 0.2) + 
#  geom_histogram(data = hai_6_testing, stat = "count", fill = "blue", alpha = 0.2) +
# geom_histogram(data = hai_6_all_data_nona, stat = "count", fill = "green", alpha = 0.2)  

###############create the logistic regression with all the factors
form_all_fact <- hai_6_training$above_avg_score ~ hai_6_training$hospital_owner + hai_6_training$hospital_overall_rating + hai_6_training$mortality_code 
  + hai_6_training$readmission_code + hai_6_training$effectiveness_of_care_code + hai_6_training$effectiveness_of_care_code + hai_6_training$timeliness_of_care_code 
  + hai_6_training$spend_score + hai_6_training$payment + hai_6_training$value_of_care_category_code

  
fit_all_fact <- glm(formula = form_all_fact, family = binomial(link = "logit"), data =  hai_6_training)
summary(fit_all_fact)


#look at it!
plot(fit_all_fact)

###############predict

pred_all_fact <- predict(fit_all_fact, type="response")
cfm <- confusionMatrix(pred_all_fact, hai_6_training$above_avg_score)
table(pred = pred_all_fact, true = hai_6_training$above_avg_score)


###############test predictions

###############validate the fit



###############create the logistic regression using forward stepwise feature selection
# form_all_fact <- hai_6_training$above_avg_score ~ hai_6_training$hospital_type+ hai_6_training$hospital_owner+ hai_6_training$mortality_code+ hai_6_training$readmission_code+
# hai_6_training$effectiveness_of_care_code+ hai_6_training$timeliness_of_care_code+ 
# hai_6_training$patient_experience_code+ hai_6_training$spend_score


fit_for_step <- step(glm(formula = form_all_fact, family = binomial(link = "logit"), data =  hai_6_training), direction = "forward")
# AIC=3396.51
# hai_6_training$above_avg_score ~ hai_6_training$hospital_owner + 
#   hai_6_training$hospital_overall_rating + hai_6_training$mortality_code

summary(fit_for_step)


#look at it!
plot(fit_for_step)

###############predict


###############test predictions

###############validate the fit

###############create the logistic regression using backward stepwise feature selection
# form_all_fact <- hai_6_training$above_avg_score ~ hai_6_training$hospital_type+ hai_6_training$hospital_owner+ hai_6_training$mortality_code+ hai_6_training$readmission_code+
# hai_6_training$effectiveness_of_care_code+ hai_6_training$timeliness_of_care_code+ 
# hai_6_training$patient_experience_code+ hai_6_training$spend_score


fit_back_step <- step(glm(formula = form_all_fact, family = binomial(link = "logit"), data =  hai_6_training), direction = "backward")
# AIC=3396.51
# hai_6_training$above_avg_score ~ hai_6_training$hospital_owner + 
#   hai_6_training$hospital_overall_rating + hai_6_training$mortality_code
summary(fit_back_step)


#look at it!
plot(fit_for_step)

###############predict


###############test predictions

###############validate the fit

###############create the logistic regression using bidirectional stepwise feature selection
# form_all_fact <- hai_6_training$above_avg_score ~ hai_6_training$hospital_type+ hai_6_training$hospital_owner+ hai_6_training$mortality_code+ hai_6_training$readmission_code+
# hai_6_training$effectiveness_of_care_code+ hai_6_training$timeliness_of_care_code+ 
# hai_6_training$patient_experience_code+ hai_6_training$spend_score


fit_both_step <- step(glm(formula = form_all_fact, family = binomial(link = "logit"), data =  hai_6_training), direction = "both")
# AIC=3390.25
#hai_6_training$above_avg_score ~ hai_6_training$mortality_code
summary(fit_both_step)

#look at it!
plot(fit_for_step)

###############predict
pred_for_step <- predict(fit_for_step, type = "response")
pred_for_step

#tf_pred_for_step <- pred_for_step
predictions <- ifelse(pred_for_step > 0.5, TRUE, FALSE) 
predictions
train_cfm <- confusionMatrix(predictions, hai_6_training$above_avg_score)
train_cfm
# Accuracy : 0.5476          
# 95% CI : (0.5277, 0.5675)
# No Information Rate : 0.5375          
# P-Value [Acc > NIR] : 0.1607          
# 
# Kappa : 0.0403          
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.9000          
#             Specificity : 0.1382          
#          Pos Pred Value : 0.5482          
#          Neg Pred Value : 0.5433          
#              Prevalence : 0.5375          
#          Detection Rate : 0.4837          
#    Detection Prevalence : 0.8823          
#       Balanced Accuracy : 0.5191          
        
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
test_pred_for_step <- predict(fit_for_step, newdata = hai_6_testing$above_avg_score, type = "response")
test_pred_for_step

#tf_pred_for_step <- pred_for_step
predictions <- ifelse(pred_for_step > 0.5, TRUE, FALSE) 
predictions

test_cfm <- confusionMatrix(predictions, hai_6_testing$above_avg_score)
test_cfm

###############validate the fit
