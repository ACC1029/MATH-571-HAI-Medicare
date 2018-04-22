# Call after running new data if in a new session

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

lm.fit = lm(sir_score ~ hospital_type + hospital_owner + mortality + readmission + 
              effectiveness + timeliness + patient_exp + spend_score + 
              hospital_owner:spend_score +
              mortality:spend_score, 
            data = hai_6_all_data_nona)
summary(lm.fit)


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
