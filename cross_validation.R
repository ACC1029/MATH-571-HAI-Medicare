# Run loading data before this script

glm.fit=glm(I(sir_score^2) ~ hospital_type + hospital_owner + mortality + readmission + 
              effectiveness + timeliness + patient_exp + sir_comp + spend_score + 
              hospital_type:sir_comp + timeliness:sir_comp + hospital_type:hospital_owner + 
              hospital_owner:spend_score + hospital_type:readmission + 
              mortality:spend_score + readmission:sir_comp + hospital_owner:sir_comp + 
              hospital_type:timeliness + effectiveness:sir_comp + I(spend_score^.5), 
            data = hai_6_all_data_nona
)
summary(glm.fit)
length(glm.fit$coefficients)
glm.fit$rank

set.seed(17)
cv.error.10=rep(0,10)
cv.r_squared=rep(0,10)
for (i in 1:10){
  glm.fit=glm(I(sir_score^2) ~ hospital_type + hospital_owner + mortality + readmission + 
                effectiveness + timeliness + patient_exp + spend_score + 
                hospital_type:hospital_owner + 
                hospital_owner:spend_score + hospital_type:readmission + 
                mortality:spend_score + 
                hospital_type:timeliness + I(spend_score^.5), 
              data = hai_6_all_data_nona
  )
  cv.r_squared[i] = (1 - glm.fit$deviance/glm.fit$null.deviance)
  cv.error.10[i]=cv.glm(hai_6_all_data_nona,glm.fit,K=10)$delta[1]
}
cv.error.10
cv.r_squared


set.seed(17)
cv.error.10=rep(0,10)
cv.r_squared=rep(0,10)
for (i in 1:10){
  glm.fit=glm(sir_score ~ hospital_type + hospital_owner + mortality + readmission + 
                effectiveness + timeliness + patient_exp + spend_score + 
                hospital_type:hospital_owner + 
                hospital_owner:spend_score + hospital_type:readmission + 
                mortality:spend_score +
                hospital_type:timeliness + I(spend_score^.5), 
              data = hai_6_all_data_nona
  )
  cv.r_squared[i] = (1 - glm.fit$deviance/glm.fit$null.deviance)
  cv.error.10[i]=cv.glm(hai_6_all_data_nona,glm.fit,K=10)$delta[1]
}
cv.r_squared
cv.error.10



## Since the test MSE is significantly smaller when changing I(sir_score^2), compared to the smaller drop in Adjusted R^2
## So we are removing the square of the response from our model.  