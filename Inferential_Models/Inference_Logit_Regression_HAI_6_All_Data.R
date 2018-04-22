#all libraries needed for the below
library(tidyverse)
library(glmnet)
library(caret)
library(ggplot2)
#library(popbio)
#library(logihist)
library(leaps)

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

###############create the logistic regression with all the factors
form_all_fact <- hai_6_nona_recoded$above_avg_score ~ hai_6_nona_recoded$hospital_owner + hai_6_nona_recoded$hospital_type + hai_6_nona_recoded$hospital_rating + hai_6_nona_recoded$mortality 
+ hai_6_nona_recoded$readmission + hai_6_nona_recoded$effectiveness + hai_6_nona_recoded$timeliness 
+ hai_6_nona_recoded$spend_score + hai_6_nona_recoded$payment + hai_6_nona_recoded$val_care_cat


fit_all_fact <- glm(formula = form_all_fact, family = binomial(link = "logit"), data =  hai_6_nona_recoded)
summary(fit_all_fact)
#AIC: 4240.3

#look at it!
plot(fit_all_fact)

###############predict with all the factors
pred_all_fact <- predict(fit_all_fact, type="response")
#cfm <- confusionMatrix(pred_all_fact, hai_6_nona_recoded$above_avg_score)
#table(pred = pred_all_fact, true = hai_6_nona_recoded$above_avg_score)
pred_all_fact

#tf_pred_for_step <- pred_for_step
all_predictions <- ifelse(pred_all_fact > 0.5, TRUE, FALSE) 
all_predictions
train_all_cfm <- confusionMatrix(all_predictions, hai_6_nona_recoded$above_avg_score)
train_all_cfm

# Confusion Matrix and Statistics
# 
# Reference
# Prediction FALSE TRUE
# FALSE  1428 1159
# TRUE    221  260
# 
# Accuracy : 0.5502          
# 95% CI : (0.5324, 0.5679)
# No Information Rate : 0.5375          
# P-Value [Acc > NIR] : 0.08158         
# 
# Kappa : 0.0516          
# Mcnemar's Test P-Value : < 2e-16         
#                                           
#             Sensitivity : 0.8660          
#             Specificity : 0.1832          
#          Pos Pred Value : 0.5520          
#          Neg Pred Value : 0.5405          
#              Prevalence : 0.5375          
#          Detection Rate : 0.4654          
#    Detection Prevalence : 0.8432          
#       Balanced Accuracy : 0.5246          
#                                           
#        'Positive' Class : FALSE

###############create the logistic regression with forward stepwise feature selection
fit_for_step <- step(glm(formula = form_all_fact, family = binomial(link = "logit"), data =  hai_6_nona_recoded), direction = "forward")
summary(fit_for_step)
#Start:  AIC=4240.33
# hai_6_nona_recoded$above_avg_score ~ hai_6_nona_recoded$hospital_owner + 
#   hai_6_nona_recoded$hospital_type + hai_6_nona_recoded$hospital_rating + 
#   hai_6_nona_recoded$mortality


#look at it!
plot(fit_for_step)

###############predict with forward selected model
pred_for_step <- predict(fit_for_step, type="response")
pred_for_step

#tf_pred_for_step <- pred_for_step
for_predictions <- ifelse(pred_for_step > 0.5, TRUE, FALSE) 
for_predictions
for_cfm <- confusionMatrix(for_predictions, hai_6_nona_recoded$above_avg_score)
for_cfm
# Reference
# Prediction FALSE TRUE
# FALSE  1428 1159
# TRUE    221  260
# 
# Accuracy : 0.5502          
# 95% CI : (0.5324, 0.5679)
# No Information Rate : 0.5375          
# P-Value [Acc > NIR] : 0.08158         
# 
# Kappa : 0.0516          
# Mcnemar's Test P-Value : < 2e-16         
#                                           
#             Sensitivity : 0.8660          
#             Specificity : 0.1832          
#          Pos Pred Value : 0.5520          
#          Neg Pred Value : 0.5405          
#              Prevalence : 0.5375          
#          Detection Rate : 0.4654          
#    Detection Prevalence : 0.8432          
#       Balanced Accuracy : 0.5246          
#                                           
#        'Positive' Class : FALSE    

###############create the logistic regression with backward stepwise feature selection
fit_back_step <- step(glm(formula = form_all_fact, family = binomial(link = "logit"), data =  hai_6_nona_recoded), direction = "backward")
summary(fit_back_step)
#Start:  AIC=4231.9
# hai_6_nona_recoded$above_avg_score ~ hai_6_nona_recoded$hospital_owner + 
#   hai_6_nona_recoded$hospital_type + hai_6_nona_recoded$hospital_rating + 
#   hai_6_nona_recoded$mortality


#look at it!
plot(fit_back_step)

###############predict with backward selected model
pred_back_step <- predict(fit_back_step, type="response")
pred_back_step

#tf_pred_for_step <- pred_for_step
back_predictions <- ifelse(pred_back_step > 0.5, TRUE, FALSE) 
back_predictions
back_cfm <- confusionMatrix(back_predictions, hai_6_nona_recoded$above_avg_score)
back_cfm
# Confusion Matrix and Statistics
# 
# Reference
# Prediction FALSE TRUE
# FALSE  1469 1210
# TRUE    180  209
# 
# Accuracy : 0.5469          
# 95% CI : (0.5291, 0.5647)
# No Information Rate : 0.5375          
# P-Value [Acc > NIR] : 0.151           
# 
# Kappa : 0.0402          
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.8908          
#             Specificity : 0.1473          
#          Pos Pred Value : 0.5483          
#          Neg Pred Value : 0.5373          
#              Prevalence : 0.5375          
#          Detection Rate : 0.4788          
#    Detection Prevalence : 0.8732          
#       Balanced Accuracy : 0.5191          
#                                           
#        'Positive' Class : FALSE     

###############create the logistic regression with bidirectional stepwise feature selection
fit_both_step <- step(glm(formula = form_all_fact, family = binomial(link = "logit"), data =  hai_6_nona_recoded), direction = "both")
summary(fit_both_step)
#Start:  AIC=4231.9
#hai_6_nona_recoded$above_avg_score ~ hai_6_nona_recoded$mortality


#look at it!
plot(fit_both_step)

###############predict with bidirectional selected model
pred_both_step <- predict(fit_both_step, type="response")
pred_both_step

#tf_pred_for_step <- pred_for_step
both_predictions <- ifelse(pred_both_step > 0.5, TRUE, FALSE) 
both_predictions
both_cfm <- confusionMatrix(both_predictions, hai_6_nona_recoded$above_avg_score)
both_cfm

# Confusion Matrix and Statistics
# 
# Reference
# Prediction FALSE TRUE
# FALSE  1469 1210
# TRUE    180  209
# 
# Accuracy : 0.5469          
# 95% CI : (0.5291, 0.5647)
# No Information Rate : 0.5375          
# P-Value [Acc > NIR] : 0.151           
# 
# Kappa : 0.0402          
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.8908          
#             Specificity : 0.1473          
#          Pos Pred Value : 0.5483          
#          Neg Pred Value : 0.5373          
#              Prevalence : 0.5375          
#          Detection Rate : 0.4788          
#    Detection Prevalence : 0.8732          
#       Balanced Accuracy : 0.5191          
#                                           
#        'Positive' Class : FALSE


################################Plot the logistic transformation of our varibles uisng popbio package
#rating
logi.hist.plot2(hai_6_nona_recoded$hospital_rating, hai_6_nona_recoded$above_avg_score)

#spend score
logi.hist.plot2(hai_6_nona_recoded$spend_score, hai_6_nona_recoded$above_avg_score)

#payments

######################Looking at the data with best subset in mind
##We cannot use this method of feature selection because we have classifiers
# best.subset <- regsubsets(form_all_fact, hai_6_nona_recoded)
# best.subset.summary <- summary(best.subset)
# best.subset.summary$outmat
# 
# best.subset.by.adjr2 <- which.max(best.subset.summary$adjr2)
# best.subset.by.adjr2
# #hospital_owner
# best.subset.by.cp <- which.min(best.subset.summary$cp)
# best.subset.by.cp
# 
# best.subset.by.bic <- which.min(best.subset.summary$bic)
# best.subset.by.bic
