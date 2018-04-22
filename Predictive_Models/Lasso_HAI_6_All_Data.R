#all libraries needed for the below
library(tidyverse)
library(glmnet)
library(caret)
library(ggplot2)


###############load
#load the CSV provided by Anna, housed in the git repository, as a tibble
hai_6_all_data_nona <- read_csv("adjusted_data/hai_6_all_data_nona.csv")


##############################We're first going to try lasso to predict the actual HAI 6 measure SIR score
###############separate into test and train
#get the num
hai_6_inTrain <- createDataPartition(y = hai_6_all_data_nona$SIR_score,
                                     ## the outcome data are needed
                                     p = .8,
                                     ## The percentage of data in the
                                     ## training set
                                     list = FALSE)

hai_6_training <- hai_6_all_data_nona[ hai_6_inTrain,]

hai_6_testing <- hai_6_all_data_nona[-hai_6_inTrain,]

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
ggplot(data=hai_6_testing, aes(hai_6_training$hospital_owner)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")

##hospital_type
#train
ggplot(data=hai_6_training, aes(hai_6_training$hospital_type)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")
#test
ggplot(data=hai_6_testing, aes(hai_6_training$hospital_type)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")

##hospital_overall_rating
#train
ggplot(data=hai_6_training, aes(hai_6_training$hospital_overall_rating)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")
#test
ggplot(data=hai_6_testing, aes(hai_6_training$hospital_overall_rating)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")

##hospital owner
#train
ggplot(data=hai_6_training, aes(hai_6_training$mortality_code)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")

#test
ggplot(data=hai_6_testing, aes(hai_6_training$hospital_owner)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_histogram(stat = "count")

#I'd like to get this all onto one plot, but it's taking more fiddling than I want to do right now
#ggplot(histogram, aes(hospital_owner)) + 
#  theme(axis.text.x=element_text(angle=90,hjust=1)) +
#  geom_histogram(data = hai_6_training, stat = "count", fill = "yello", alpha = 0.2) + 
#  geom_histogram(data = hai_6_testing, stat = "count", fill = "blue", alpha = 0.2) +
# geom_histogram(data = hai_6_all_data_nona, stat = "count", fill = "green", alpha = 0.2)  

###############create the lasso model
#without trying to find our own lambda, per the glmet help: "Typical usage is to have the program compute its own lambda sequence based on nlambda and lambda.min.ratio. Supplying a value of lambda overrides this. "
#lambdas <- c(1000:1)

#I get this warning because I have factors: In data.matrix(.) : NAs introduced by coercion
train_x <- hai_6_training %>% select(hospital_type
                                     , hospital_owner
                                     , hospital_overall_rating
                                     , mortality_code
                                     , readmission_code
                                     , timeliness_of_care_code
                                     , patient_experience_code
                                     , spend_score
                                      , payment
                                    , value_of_care_category_code
                                      ) %>% data.matrix()

train_y <- hai_6_training$SIR_score

#Error in elnet(x, is.sparse, ix, jx, y, weights, offset, type.gaussian,  : 
##NA/NaN/Inf in foreign function call (arg 5)
lasso_model <- glmnet(train_x, train_y, family="gaussian", alpha = 1)
plot(lasso_model)
###############testing the lasso model




