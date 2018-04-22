library(car)
# combine all the data for providers with HAI 6 SIR scores and pneumonia payment category, no tribal or childrens
hai_6_all_data <- hai_reduced_spread_nona %>%
  inner_join(hosp_gen_info_recoded, by = "provider_id") %>%
  inner_join(mspb_reduced, by = "provider_id") %>%
  inner_join(pay_val_care_recoded, by = "provider_id") %>%
  filter(Measure == "HAI_6" & !is.na(SIR_score) & payment_measure_id == "PAYM_30_PN", 
         hospital_owner != "Tribal" & hospital_type != "Childrens") %>%
  select(provider_id, hospital_type, hospital_owner, hospital_rating = hospital_overall_rating, 
         mortality = mortality_code, readmission = readmission_code, effectiveness = effectiveness_of_care_code, 
         timeliness = timeliness_of_care_code, patient_exp = patient_experience_code, sir_score = SIR_score, 
         spend_score, payment, val_care_cat = value_of_care_category_code
  ) %>%
  transform(hospital_rating = as.numeric(hospital_rating))

glimpse(hai_6_all_data)

# change NA to means and modes
hai_6_all_data_nona <- hai_6_all_data %>%
  replace_na(list(hospital_rating = 3, mortality = 0, spend_score = 0.99, payment = 17163, 
                  mortality = 0, readmission = 1, effectiveness = 0, timeliness = 0,
                  patient_exp = -1, sir_comp = 0, val_care_cat = 0))


# model with all variables
lm_fit <- lm(sir_score ~ ., data = hai_6_all_data_nona)
summary(lm_fit)

# perform forward selection, taking into account interaction terms
step_model_for <- step(lm_fit, scope = . ~ .^2, direction = 'forward')

initial_forward_step <- lm(sir_score ~ hospital_type + hospital_owner + hospital_rating + 
                             mortality + readmission + effectiveness + timeliness + patient_exp + 
                             spend_score + payment + val_care_cat + hospital_owner:spend_score + 
                             hospital_type:hospital_owner + hospital_type:readmission + 
                             hospital_type:patient_exp + hospital_owner:timeliness + hospital_rating:spend_score + 
                             hospital_type:mortality + readmission:payment + hospital_rating:effectiveness, 
                           data = hai_6_all_data_nona
)
summary(initial_forward_step)

step_model_back <- step(lm_fit, scope = . ~ .^2, direction = 'backward')

initial_back_step <- lm(sir_score ~ hospital_type + hospital_rating + timeliness + payment,
                   data = hai_6_all_data_nona
)
summary(initial_back_step)


for_step_inter_trans <- lm(I(sir_score^2) ~ hospital_type + hospital_owner + hospital_rating + 
                             mortality + readmission + effectiveness + timeliness + patient_exp + 
                             spend_score + payment + val_care_cat + hospital_owner:spend_score + 
                             hospital_type:hospital_owner + hospital_type:readmission + 
                             hospital_type:patient_exp + hospital_owner:timeliness + hospital_rating:spend_score + 
                             hospital_type:mortality + readmission:payment + hospital_rating:effectiveness, 
                           data = hai_6_all_data_nona
                            )
summary(for_step_inter_trans)

trun_mod <- lm(I(sir_score^2) ~ spend_score, data = hai_6_all_data_nona)

leveragePlots(trun_mod)
qqPlot(trun_mod, main="QQ Plot")
outlierTest(trun_mod)

cutoff <- 4/((nrow(hai_6_all_data_nona)-length(trun_mod$coefficients)-2)) 
plot(trun_mod, which = 4, cook.levels = cutoff)

plot(hai_6_all_data_nona$spend_score, resid(for_step_inter_trans))


# try spend_score transformation
plot(hai_6_all_data_nona$spend_score, hai_6_all_data_nona$sir_score)

