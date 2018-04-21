install.packages("ggplot2")

library(ggplot2)
library(tidyverse)
library(corrplot)

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

# means:
## overall mean of payment, spending score, and overall hospital rating
hai_6_all_data %>%
  summarise(payment_mean = mean(payment, na.rm = TRUE),
            spend_score_mean = mean(spend_score, na.rm = TRUE),
            hospital_overall_rating_mean = mean(as.integer(hospital_rating), na.rm = TRUE))

# modes: 
## mortality: 0, readmission: 1.00, effectiveness: 0, timeliness: 0, 
## patient_exp: -1.00, sir_comp: 0, val_care_cat: 0

hai_6_all_data %>%
  filter(!is.na(val_care_cat)) %>%
  count(val_care_cat) %>%
  filter(n == max(n)) %>%
  select(val_care_cat)

# change NA to means and modes
hai_6_all_data_nona <- hai_6_all_data %>%
  replace_na(list(hospital_rating = 3, mortality = 0, spend_score = 0.99, payment = 17163, 
                  mortality = 0, readmission = 1, effectiveness = 0, timeliness = 0,
                  patient_exp = -1, sir_comp = 0, val_care_cat = 0))

which(!complete.cases(hai_6_all_data_nona))

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

glimpse(hai_6_all_data_nona)
sapply(hai_6_all_data_nona, class)

# plots of all variables
hai_6_all_data_nona %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# plots of all numeric variables against sir_score
hai_6_all_data_nona %>%
  keep(is.numeric) %>% 
  gather(-sir_score, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = sir_score)) +
  geom_point(shape = 1) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

  # based on above, might try transforming payment, spend score, mortality, hosp rating, val care
  # also consider making patient_exp and readmission or timeliness or value of care interaction

# model with all variables
lm_fit <- lm(sir_score ~ ., data = hai_6_all_data_nona)
summary(lm_fit)

# vif of model with all variables
vif <- car::vif(lm_fit)
vif
summary(vif)
  ## This does not show any high factors -- 
    # 1) the largest VIF is less than 10
    # 2) the mean VIF is not substantially greater than 1

# make correlation matrix
# all predictors
predictors <- hai_6_all_data_nona %>%
  select(hospital_rating, payment)

# minus overall rating
predictors <- hai_6_all_data_nona %>%
  select(mortality, readmission, effectiveness, timeliness, patient_exp, sir_comp, 
         spend_score, payment, val_care_cat)

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
  select(sir_score, hospital_type, hospital_owner, mortality, readmission,
         effectiveness, timeliness, patient_exp, sir_comp, spend_score)

which(!complete.cases(hai_6_all_data_nona_nocor))
glimpse(hai_6_all_data_nona_nocor)

# plots of all variables
hai_6_all_data_nona_nocor %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# plots of all numeric variables against sir_score
hai_6_all_data_nona_nocor %>%
  keep(is.numeric) %>% 
  gather(-sir_score, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = sir_score)) +
  geom_point(shape = 1) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

initial_fit <- lm(sir_score ~ ., data = hai_6_all_data_nona_nocor)
summary(initial_fit)

install.packages("car")
car::vif(initial_fit)

# perform forward selection, taking into account interaction terms
step_model <- step(initial_fit, scope = . ~ .^2, direction = 'forward')
summary(step_model)

step_model <- lm(sir_score ~ hospital_type + hospital_owner + mortality + readmission + 
                   effectiveness + timeliness + patient_exp + sir_comp + spend_score + 
                   hospital_type:sir_comp + timeliness:sir_comp + hospital_type:hospital_owner + 
                   hospital_owner:spend_score + hospital_type:readmission + 
                   mortality:spend_score + readmission:sir_comp + hospital_owner:sir_comp + 
                   hospital_type:timeliness + effectiveness:sir_comp, data = hai_6_all_data_nona_nocor)
summary(step_model)


plot(predict(step_model), resid(step_model))

plot(hai_6_all_data_nona_nocor$mortality, resid(step_model))

plot(step_model, which=c(1,2,4,5))

plot(hai_6_all_data_nona_nocor$hospital_type, resid(step_model))

#============================#
# Assumptions to test:
## Constant variance
## Normality
## Independence

