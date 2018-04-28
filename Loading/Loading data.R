library(tidyverse)

#--loading raw data--#

hai_raw <- read_csv("Healthcare_Associated_Infections_-_Hospital.csv",
                    na = c("", "NA"),
                    col_names = c(
                      "provider_id", "hospital_name", "address", "city", "state", "zip_code",
                      "county_name", "phone_number", "measure_name", "measure_id",
                      "compared_to_national", "score", "footnote", "measure_start_date",
                      "measure_end_date", "location"
                    ),
                    col_types = cols(
                      provider_id = col_integer(),
                      hospital_name = col_character(),
                      address = col_character(),
                      city = col_character(),
                      state = col_character(),
                      zip_code = col_integer(),
                      county_name = col_character(),
                      phone_number = col_double(),
                      measure_name = col_character(),
                      measure_id = col_character(),
                      compared_to_national = col_character(),
                      score = col_character(),
                      footnote = col_character(),
                      measure_start_date = col_character(),
                      measure_end_date = col_character(),
                      location = col_character()
                    ),
                    skip = 1
                    )

hosp_gen_info_raw <- read_csv("Hospital_General_Information.csv",
                              na = c("", "NA"),
                              col_names = c(
                                "provider_id", "hospital_name", "address", "city", "state", "zip_code", "county_name", 
                                "phone_number", "hospital_type", "hospital_owner", "emergency_services", 
                                "meets_meaningful_use", "hospital_overall_rating", "hospital_overall_rating_footnote",
                                "mortality", "mortality_footnote", "safety_of_care", "safety_of_care_footnote", 
                                "readmission", "readmission_footnote", "patient_experience", "patient_experience_footnote", 
                                "effectiveness_of_care", "effectiveness_of_care_footnote", "timeliness_of_care", 
                                "timeliness_of_care_footnote", "efficient_use_of_medical_imaging", 
                                "efficient_use_of_medical_imaging_footnote", "location"
                              ),
                              col_types = cols(
                                provider_id = col_integer(),
                                hospital_name = col_character(),
                                address = col_character(),
                                city = col_character(),
                                state = col_character(),
                                zip_code = col_integer(),
                                county_name = col_character(),
                                phone_number = col_double(),
                                hospital_type = col_character(),
                                hospital_owner = col_character(),
                                emergency_services = col_character(),
                                meets_meaningful_use = col_character(),
                                hospital_overall_rating = col_character(),
                                hospital_overall_rating_footnote = col_character(),
                                mortality = col_character(),
                                mortality_footnote = col_character(),
                                safety_of_care = col_character(),
                                safety_of_care_footnote = col_character(),
                                readmission = col_character(),
                                readmission_footnote = col_character(),
                                patient_experience = col_character(),
                                patient_experience_footnote = col_character(),
                                effectiveness_of_care = col_character(),
                                effectiveness_of_care_footnote = col_character(),
                                timeliness_of_care = col_character(),
                                timeliness_of_care_footnote = col_character(),
                                efficient_use_of_medical_imaging = col_character(),
                                efficient_use_of_medical_imaging_footnote = col_character(),
                                location = col_character()
                              ),
                              skip = 1
)

mspb_raw = read_csv("Medicare_Hospital_Spending_Per_Patient_-_Hospital.csv",
                    na = c("", "NA"),
                    col_names = c(
                      "provider_id", "hospital_name", "address", "city", "state", "zip_code",
                      "county_name", "phone_number", "measure_name", "measure_id", "spend_score", 
                      "footnote", "measure_start_date", "measure_end_date", "location"
                    ),
                    col_types = cols(
                      provider_id = col_integer(),
                      hospital_name = col_character(),
                      address = col_character(),
                      city = col_character(),
                      state = col_character(),
                      zip_code = col_integer(),
                      county_name = col_character(),
                      phone_number = col_double(),
                      measure_name = col_character(),
                      measure_id = col_character(),
                      spend_score = col_character(),
                      footnote = col_character(),
                      location = col_character(),
                      measure_start_date = col_character(),
                      measure_end_date = col_character()
                    ),
                    skip = 1
)

payment_value_care_raw <- read_csv("Payment_and_value_of_care_-_Hospital.csv",
                                   na = c("", "NA"),
                                   col_names = c(
                                     "provider_id", "hospital_name", "address", "city", "state", "zip_code",
                                     "county_name", "phone_number", "payment_measure_name", "payment_measure_id",
                                     "payment_category", "denominator", "payment", "lower_estimate", 
                                     "higher_estimate", "payment_footnote", "value_of_care_display_name",
                                     "value_of_care_display_id", "value_of_care_category", "value_of_care_footnote",
                                     "measure_start_date", "measure_end_date", "location"
                                   ),
                                   col_types = cols(
                                     provider_id = col_integer(),
                                     hospital_name = col_character(),
                                     address = col_character(),
                                     city = col_character(),
                                     state = col_character(),
                                     zip_code = col_integer(),
                                     county_name = col_character(),
                                     phone_number = col_double(),
                                     payment_measure_name = col_character(),
                                     payment_measure_id = col_character(),
                                     payment_category = col_character(),
                                     denominator = col_character(),
                                     payment = col_character(),
                                     lower_estimate = col_character(),
                                     higher_estimate = col_character(),
                                     payment_footnote = col_character(),
                                     value_of_care_display_name = col_character(),
                                     value_of_care_display_id = col_character(),
                                     value_of_care_category = col_character(),
                                     value_of_care_footnote = col_character(),
                                     measure_start_date = col_character(),
                                     measure_end_date = col_character(),
                                     location = col_character()
                                   ),
                                   skip = 1
)

hosp_gen_info_reduced <- dplyr::select(hosp_gen_info_raw,-hospital_overall_rating_footnote, -mortality_footnote, 
         -safety_of_care_footnote, -readmission_footnote, -patient_experience_footnote, 
         -effectiveness_of_care_footnote, -timeliness_of_care_footnote,
         -efficient_use_of_medical_imaging_footnote) %>%
  mutate(mortality = replace(mortality, mortality == "Not Available", NA), 
         hospital_overall_rating = replace(hospital_overall_rating, hospital_overall_rating == "Not Available", NA),
         safety_of_care = replace(safety_of_care, safety_of_care == "Not Available", NA),
         patient_experience = replace(patient_experience, patient_experience == "Not Available", NA),
         timeliness_of_care = replace(timeliness_of_care, timeliness_of_care == "Not Available", NA),
         readmission = replace(readmission, readmission == "Not Available", NA),
         effectiveness_of_care = replace(effectiveness_of_care, effectiveness_of_care == "Not Available", NA),
         efficient_use_of_medical_imaging = replace(efficient_use_of_medical_imaging, efficient_use_of_medical_imaging == "Not Available", NA)
  )

hai_reduced <- dplyr::select(hai_raw, provider_id, measure_name, measure_id, compared_to_national, score) %>%
  mutate(measure_name = replace(measure_name, measure_name == "Not Available", NA), 
         measure_id = replace(measure_id, measure_id == "Not Available", NA),
         compared_to_national = replace(compared_to_national, compared_to_national == "Not Available", NA),
         score = replace(score, score == "Not Available", NA)) %>%
  arrange(provider_id, measure_id)

mspb_reduced <- dplyr::select(mspb_raw, provider_id, spend_score) %>%
  mutate(spend_score = replace(spend_score, spend_score == "Not Available", NA),
         spend_score = as.double(spend_score)) %>%
  arrange(provider_id)

pay_val_care_reduced <- dplyr::select(payment_value_care_raw, provider_id, payment_measure_id,
                               payment_category, denominator, payment, lower_estimate, higher_estimate,
                               value_of_care_display_id, value_of_care_category) %>%
  mutate(payment_category = replace(payment_category, payment_category == "Not Available", NA),
         denominator = replace(denominator, denominator == "Not Available", NA),
         payment = replace(payment, payment == "Not Available", NA),
         lower_estimate = replace(lower_estimate, lower_estimate == "Not Available", NA),
         higher_estimate = replace(higher_estimate, higher_estimate == "Not Available", NA),
         value_of_care_category = replace(value_of_care_category, value_of_care_category == "Not Available", NA)
  ) %>%
  arrange(provider_id, payment_measure_id)

# HAI: break up measure into measure and its type
hai_reduced <- hai_reduced %>% 
  mutate("Measure" = substring(text = measure_id, first = 1, last = 5),
         "Type" = substring(text = measure_id, first = 7, last = length(measure_id)))

# HAI: Spread
hai_reduced_spread <- hai_reduced %>%
  # filter(provider_id == 10005) %>%
  unite(provider_hai, provider_id, Measure) %>%
  select(provider_hai, Type, score, compared_to_national) %>%
  gather(variable, value, -(provider_hai:Type)) %>%
  unite(temp, Type, variable) %>%
  spread(temp, value)

## Break out provider-type again and drop provider-type column
hai_reduced_spread <- hai_reduced_spread %>%
  mutate("provider_id"= as.integer(str_sub(provider_hai, 1, unlist(lapply(strsplit(provider_hai, ''),
                                                                          function(provider_hai) which(provider_hai == '_')))[c(TRUE,FALSE)]-1)),
         "Measure" = str_sub(provider_hai, unlist(lapply(strsplit(provider_hai, ''),
                                                         function(provider_hai) which(provider_hai == '_')))[c(TRUE,FALSE)]+1, 
                             str_length(provider_hai)),
         CI_LOWER_score = as.double(CI_LOWER_score),
         CI_UPPER_score = as.double(CI_UPPER_score),
         DOPC_DAYS_score = as.integer(DOPC_DAYS_score),
         ELIGCASES_score = as.double(ELIGCASES_score),
         NUMERATOR_score = as.integer(NUMERATOR_score),
         SIR_score = as.double(SIR_score)) %>%
  select(provider_id, Measure, everything(), -provider_hai) %>%
  arrange(provider_id)

# remove rows other than for SIR measures
hai_sir <- hai_reduced %>%
  filter(Type == "SIR")

# HAI: remove providers that have no SIR scores at all
no_score_providers <- hai_sir %>%
  filter(is.na(score)) %>%
  select(Measure, provider_id, score) %>%
  count(score, provider_id) %>%
  group_by(provider_id) %>%
  arrange(desc(n)) %>%
  filter(n == 6)

# HAI: recode compared to national
hai_reduced_spread <- hai_reduced_spread %>%
  mutate(SIR_compared_to_national_code = dplyr::recode(SIR_compared_to_national, 
                                                "No Different than National Benchmark" = 0,
                                                "Better than the National Benchmark" = 1,
                                                "Worse than the National Benchmark" = -1)
         ) %>%
  mutate(SIR_compared_to_national_code = factor(SIR_compared_to_national_code))


# HAI: filter down to rows that are populated
hai_reduced_spread_nona <- hai_reduced_spread %>%
  left_join(no_score_providers, by = "provider_id") %>%
  filter(is.na(n))

# Hosp gen info: Recode variables with national comparisons
hosp_gen_info_recoded <- hosp_gen_info_reduced %>%
  mutate(mortality_code = dplyr::recode(mortality, 
                                 "Above the national average" = 1,
                                 "Same as the national average" = 0, 
                                 "Below the national average" = -1),
         safety_of_care_code = dplyr::recode(safety_of_care,
                                      "Above the national average" = 1,
                                      "Same as the national average" = 0,
                                      "Below the national average" = -1),
         patient_experience_code = dplyr::recode(patient_experience,
                                          "Above the national average" = 1,
                                          "Same as the national average" = 0,
                                          "Below the national average" = -1),
         timeliness_of_care_code = dplyr::recode(timeliness_of_care,
                                          "Above the national average" = 1,
                                          "Same as the national average" = 0,
                                          "Below the national average" = -1),
         readmission_code = dplyr::recode(readmission,
                                   "Above the national average" = 1,
                                   "Same as the national average" = 0,
                                   "Below the national average" = -1),
         effectiveness_of_care_code = dplyr::recode(effectiveness_of_care,
                                             "Above the national average" = 1,
                                             "Same as the national average" = 0,
                                             "Below the national average" = -1),
         efficient_use_of_medical_imaging_code = dplyr::recode(efficient_use_of_medical_imaging,
                                                        "Above the national average" = 1,
                                                        "Same as the national average" = 0,
                                                        "Below the national average" = -1)
  ) %>%
  mutate(mortality_code = factor(mortality_code), safety_of_care_code = factor(safety_of_care_code),
         patient_experience_code =  factor(patient_experience_code), 
         timeliness_of_care_code = factor(timeliness_of_care_code), 
         readmission_code = factor(readmission_code), effectiveness_of_care_code = factor(effectiveness_of_care_code),
         efficient_use_of_medical_imaging_code = factor(efficient_use_of_medical_imaging_code)) 

# Payment value of care: makes dollar numeric and break up measures into measure and their types
pay_val_care_reduced <- pay_val_care_reduced %>% 
  mutate(payment = as.numeric(sub(',', '', sub('\\$', '', pay_val_care_reduced$payment))),
         lower_estimate = as.numeric(sub(',', '', sub('\\$', '', pay_val_care_reduced$lower_estimate))),
         higher_estimate = as.numeric(sub(',', '', sub('\\$', '', pay_val_care_reduced$higher_estimate))),
         "payment_measure" = substring(text = payment_measure_id, first = 1, last = 7),
         "payment_type" = substring(text = payment_measure_id, first = 9, last = length(payment_measure_id)),
         "value_of_care_measure" = substring(text = value_of_care_display_id, first = 1, last = 12),
         "value_of_care_type" = substring(text = value_of_care_display_id, first = 14, last = length(value_of_care_display_id))
  )

# Payment value of care: Recode ordered categories. I think it's ok to ignore payment level in value of care category because it's captured in the payment category.
pay_val_care_recoded <- pay_val_care_reduced %>%
  mutate(payment_category_code = dplyr::recode(payment_category,
                                        "Greater than the National Average Payment" = 1,
                                        "No different than the national average payment" = 0,
                                        "Less than the National Average Payment" = -1,
                                        "Number of Cases Too Small" = -2),
         value_of_care_category_code = dplyr::recode(value_of_care_category,
                                              "Average complications and higher payment" = 0,
                                              "Average complications and average payment" = 0,
                                              "Average complications and lower payment " = 0,
                                              "Average mortality and higher payment" = 0,
                                              "Average mortality and average payment" = 0,
                                              "Average mortality and lower payment" = 0,
                                              "Better complications and higher payment" = 1,
                                              "Better complications and average payment" = 1,
                                              "Better complications and lower payment" = 1,
                                              "Better mortality and higher payment" = 1,
                                              "Better mortality and average payment" = 1,
                                              "Better mortality and lower payment" = 1,
                                              "Worse complications and higher payment" = -1,
                                              "Worse complications and average payment" = -1,
                                              "Worse complications and lower payment" = -1,
                                              "Worse mortality and higher payment" = -1,
                                              "Worse mortality and average payment" = -1,
                                              "Worse mortality and lower payment" = -1
         )
  ) %>%
  mutate(payment_category_code = replace(payment_category_code, payment_category_code == -2, NA),
         payment_category_code = factor(payment_category_code),
         value_of_care_category_code = factor(value_of_care_category_code))

# join all the data together on provider_id, limit to providers that don't have all na HAIs
all_data <- hosp_gen_info_reduced %>%  
  inner_join(hai_reduced_spread_nona, by = c("provider_id" = "provider_id")) %>%
  inner_join(pay_val_care_reduced, by = c("provider_id" = "provider_id")) %>%
  inner_join(mspb_reduced, by = c("provider_id" = "provider_id"))


propmiss <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      nmiss=sum(is.na(x)), 
      n=length(x), 
      propmiss=sum(is.na(x))/length(x)
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  return(d[order(d$propmiss), ])
}

# get proportions of missing data in other variables for HAI 6 rows
all_data_hai6 <- hosp_gen_info_reduced %>%  
  inner_join(hai_reduced_spread_nona, by = c("provider_id" = "provider_id")) %>%
  inner_join(pay_val_care_reduced, by = c("provider_id" = "provider_id")) %>%
  inner_join(mspb_reduced, by = c("provider_id" = "provider_id")) %>%
  filter(Measure == "HAI_6")

hai6_miss <- propmiss(all_data_hai6)

hai6_miss %>%
  filter(variable == "hospital_overall_rating" | variable == "patient_experience" | 
           variable == "readmission" | variable == "effectiveness_of_care" | variable == "mortality" |
           variable == "timeliness_of_care" | variable == "spend_score" | variable == "payment" |
           variable == "value_of_care_category" | variable == "safety_of_care" | 
           variable == "efficient_use_of_medical_imaging" | variable == "SIR_compared_to_national" |
           variable == "SIR_score") %>%
  ggplot(mapping = aes(x = variable, y = propmiss)) +
  geom_col(fill = "red") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=15)) +
  labs(title = "Missing Data by Variable for HAI 6", x = "Variable", y = "Proportion Missing")
           
all_data <- hosp_gen_info_reduced %>%  
  inner_join(hai_reduced_spread_nona, by = c("provider_id" = "provider_id")) %>%
  inner_join(pay_val_care_reduced, by = c("provider_id" = "provider_id")) %>%
  inner_join(mspb_reduced, by = c("provider_id" = "provider_id"))
           
           
