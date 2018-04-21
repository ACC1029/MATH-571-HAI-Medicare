
# combine all the data for providers with HAI 6 SIR scores and pneumonia payment category, no tribal or childrens
hai_6_all_data <- hai_reduced_spread_nona %>%
  inner_join(hosp_gen_info_recoded, by = "provider_id") %>%
  inner_join(mspb_reduced, by = "provider_id") %>%
  inner_join(pay_val_care_recoded, by = "provider_id") %>%
  filter(Measure == "HAI_6" & !is.na(SIR_score) & payment_measure_id == "PAYM_30_PN", 
         hospital_owner != "Tribal" & hospital_type != "Childrens") %>%
  select(hospital_type, hospital_owner, hospital_rating = hospital_overall_rating, 
         mortality = mortality_code, readmission = readmission_code, effectiveness = effectiveness_of_care_code, 
         timeliness = timeliness_of_care_code, patient_exp = patient_experience_code, 
         sir_comp = SIR_compared_to_national_code, sir_score = SIR_score, spend_score, payment, 
         val_care_cat = value_of_care_category_code
  ) %>%
  transform(hospital_rating = as.numeric(hospital_rating))

glimpse(hai_6_all_data)

# change NA to means and modes
hai_6_all_data_nona <- hai_6_all_data %>%
  replace_na(list(hospital_rating = 3, mortality = 0, spend_score = 0.99, payment = 17163, 
                  mortality = 0, readmission = 1, effectiveness = 0, timeliness = 0,
                  patient_exp = -1, sir_comp = 0, val_care_cat = 0))


# model with all variables
lm_fit <- lm(sir_score ~ hospital_rating + mortality + readmission + effectiveness +
               timeliness + patient_exp + spend_score + payment + 
               val_care_cat, data = hai_6_all_data_nona)
summary(lm_fit)

# perform forward selection, taking into account interaction terms
step_model <- step(initial_fit, scope = . ~ .^2, direction = 'forward')

initial_forward_step <- lm(sir_score ~ hospital_type + hospital_owner + mortality + readmission + 
                     effectiveness + timeliness + patient_exp + sir_comp + spend_score,
             data = hai_6_all_data_nona
)
summary(initial_forward_step)

initial_back_step <- lm(sir_score ~ timeliness + patient_exp + sir_comp + spend_score,
                   data = hai_6_all_data_nona
)
summary(initial_back_step)


initial_for_step_inter <- lm(sir_score ~ hospital_type + hospital_owner + mortality + readmission + 
                               effectiveness + timeliness + patient_exp + sir_comp + spend_score + 
                               hospital_type:sir_comp + timeliness:sir_comp + hospital_type:hospital_owner + 
                               hospital_owner:spend_score + hospital_type:readmission + 
                               mortality:spend_score + readmission:sir_comp + hospital_owner:sir_comp + 
                               hospital_type:timeliness + effectiveness:sir_comp, data = hai_6_all_data_nona)
summary(initial_for_step_inter)

for_step_inter_trans <- lm(I(sir_score^2) ~ hospital_type + hospital_owner + mortality + readmission + 
                               effectiveness + timeliness + patient_exp + sir_comp + spend_score + 
                               hospital_type:sir_comp + timeliness:sir_comp + hospital_type:hospital_owner + 
                               hospital_owner:spend_score + hospital_type:readmission + 
                               mortality:spend_score + readmission:sir_comp + hospital_owner:sir_comp + 
                               hospital_type:timeliness + effectiveness:sir_comp + I(spend_score^.5), 
                               data = hai_6_all_data_nona
                            )
summary(for_step_inter_trans)

plot(for_step_inter_trans, which=c(1,2,4,5))


# try spend_score transformation
plot(log10(hai_6_all_data_nona$spend_score), hai_6_all_data_nona$sir_score)
