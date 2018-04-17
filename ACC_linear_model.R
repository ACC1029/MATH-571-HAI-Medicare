# variables we are using initially:
  # spending per patient 
  # hospital type
  # hospital owner
  # mortality
  # hospital overall rating
  # payment
  # value of care category
  # sir score compared to national


# combine all the data for providers with HAI 6 SIR scores and pneumonia payment category, no tribal or childrens
hai_6_all_data <- hai_reduced_spread_nona %>%
  inner_join(hosp_gen_info_recoded, by = "provider_id") %>%
  inner_join(mspb_reduced, by = "provider_id") %>%
  inner_join(pay_val_care_recoded, by = "provider_id") %>%
  select(provider_id, Measure, payment_measure_id, hospital_type, hospital_owner, hospital_overall_rating, 
         mortality_code, readmission_code, effectiveness_of_care_code, timeliness_of_care_code,
         patient_experience_code, SIR_compared_to_national_code, SIR_score, spend_score, payment, 
         value_of_care_category_code
         ) %>%
  filter(Measure == "HAI_6" & !is.na(SIR_score) & payment_measure_id == "PAYM_30_PN", 
         hospital_owner != "Tribal" & hospital_type != "Childrens") %>%
  transform(hospital_overall_rating = as.numeric(hospital_overall_rating)) %>%
  arrange(provider_id)

# change NA to means and modes
hai_6_all_data_nona <- hai_6_all_data %>%
  # filter(provider_id == 30078) %>%
  replace_na(list(hospital_overall_rating = 3, mortality_code = 0, spend_score = 0.99, payment = 17163, 
                  mortality_code = 0, readmission_code = 1, effectiveness_of_care_code = 0, timeliness_of_care_code = 0,
                  patient_experience_code = -1, SIR_compared_to_national_code = 0, value_of_care_category_code = 0))


glimpse(hai_6_all_data_nona)
sapply(hai_6_all_data_nona, class)

# means:
## overall mean of payment, spending score, and overall hospital rating
hai_6_all_data %>%
  summarise(payment_mean = mean(payment, na.rm = TRUE),
            spend_score_mean = mean(spend_score, na.rm = TRUE),
            hospital_overall_rating_mean = mean(as.integer(hospital_overall_rating), na.rm = TRUE))

# modes: 
## mortality_code: 0, readmission_code: 1.00, effectiveness_of_care_code: 0, timeliness_of_care_code: 0, 
## patient_experience_code: -1.00, SIR_compared_to_national_code: 0, value_of_care_category_code: 0

hai_6_all_data %>%
  filter(!is.na(value_of_care_category_code)) %>%
  count(value_of_care_category_code) %>%
  filter(n == max(n)) %>%
  select(value_of_care_category_code)

# this does not seem to work, see patient_experience_code's mode when calculated individually
cat_modes <- hai_6_all_data %>%
  filter(!is.na(mortality_code), !is.na(readmission_code), !is.na(effectiveness_of_care_code),
         !is.na(timeliness_of_care_code), !is.na(patient_experience_code), !is.na(SIR_compared_to_national_code),
         !is.na(value_of_care_category_code)) %>%
  count(mortality_code, readmission_code, effectiveness_of_care_code, timeliness_of_care_code,
        patient_experience_code, SIR_compared_to_national_code, value_of_care_category_code) %>%
  filter(n == max(n)) %>%
  select(mortality_code, readmission_code, effectiveness_of_care_code, timeliness_of_care_code,
         patient_experience_code, SIR_compared_to_national_code, value_of_care_category_code)

# make correlation matrix
library(corrplot)

# all predictors
predictors <- hai_6_all_data_nona %>%
  select(overall_rating = hospital_overall_rating, mortality = mortality_code, readmission = readmission_code,
         effectiveness = effectiveness_of_care_code, timeliness = timeliness_of_care_code, 
         pat_experience = patient_experience_code, sir_compare = SIR_compared_to_national_code, 
         spend_score, payment, value_cat = value_of_care_category_code)

# minus overall rating
predictors <- hai_6_all_data_nona %>%
  select(mortality = mortality_code, readmission = readmission_code,
         effectiveness = effectiveness_of_care_code, timeliness = timeliness_of_care_code, 
         pat_experience = patient_experience_code, sir_compare = SIR_compared_to_national_code, 
         spend_score, payment, value_cat = value_of_care_category_code)

# minus payment
predictors <- hai_6_all_data_nona %>%
  select(mortality = mortality_code, readmission = readmission_code,
         effectiveness = effectiveness_of_care_code, timeliness = timeliness_of_care_code, 
         pat_experience = patient_experience_code, sir_compare = SIR_compared_to_national_code, 
         spend_score, value_cat = value_of_care_category_code)

# minus value of care category
predictors <- hai_6_all_data_nona %>%
  select(mortality = mortality_code, readmission = readmission_code,
         effectiveness = effectiveness_of_care_code, timeliness = timeliness_of_care_code, 
         pat_experience = patient_experience_code, sir_compare = SIR_compared_to_national_code, 
         spend_score)

M <- cor(predictors)
corrplot(M, type = "lower", order = "hclust", tl.col = "black", tl.srt = 45)

hai_6_all_data_nona_nocor <- hai_6_all_data_nona %>%
  select(SIR_score, hospital_type, hospital_owner, mortality = mortality_code, readmission = readmission_code,
         effectiveness = effectiveness_of_care_code, timeliness = timeliness_of_care_code, 
         pat_experience = patient_experience_code, sir_compare = SIR_compared_to_national_code, 
         spend_score)

glimpse(hai_6_all_data_nona_nocor)

initial_fit <- lm(SIR_score ~ ., data = hai_6_all_data_nona_nocor)
summary(initial_fit)

install.packages("car")

car::vif(initial_fit)
