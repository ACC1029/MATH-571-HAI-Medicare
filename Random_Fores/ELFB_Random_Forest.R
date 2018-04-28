library(rpart)
library(caret)
library(randomForest)
library(MASS)
form <- hai_6_all_data_nona$sir_score ~ hai_6_all_data_nona$hospital_owner + hai_6_all_data_nona$hospital_type + hai_6_all_data_nona$hospital_rating + hai_6_all_data_nona$mortality 
+ hai_6_all_data_nona$readmission + hai_6_all_data_nona$effectiveness + hai_6_all_data_nona$timeliness +  hai_6_all_data_all_data_nona$patient_exp
+ hai_6_all_data_nona$spend_score + hai_6_all_data_nona$payment + hai_6_all_data_nona$val_care_cat


train <- sample(1:nrow(hai_6_all_data_nona),nrow(hai_6_all_data_nona)/2)
test <- 
forest_hai_6 <- randomForest(form, data=hai_6_all_data_nona, importance = TRUE)

summary(forest_hai_6)

predict(forest_hai_6)

pred_forest <- predict(forest_hai_6, type = "response")
#cfm <- confusionMatrix(pred_all_fact, hai_6_nona_recoded$above_avg_score)
#table(pred = pred_all_fact, true = hai_6_nona_recoded$above_avg_score)
pred_forest

#tf_pred_for_step <- pred_for_step
pred_forest <- ifelse(pred_forest > 0.5, TRUE, FALSE) 
forest_cfm <- confusionMatrix(all_predictions, hai_6_nona_recoded$above_avg_score)
forest_cfm
