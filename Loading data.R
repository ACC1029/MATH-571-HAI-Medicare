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
                      "county_name", "phone_number", "measure_name", "measure_id", "score", 
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
                      score = col_character(),
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

mspb_reduced <- dplyr::select(mspb_raw, provider_id, score)

pay_val_care_reduced <- dplyr::select(payment_value_care_raw, provider_id, payment_measure_name, payment_measure_id,
                               payment_category, denominator, payment, lower_estimate, higher_estimate,
                               value_of_care_display_name, value_of_care_display_id, value_of_care_category,
                               location)

# join all the data together on provider_id
all_data <- hosp_gen_info_reduced %>%  
  inner_join(hai_reduced, by = c("provider_id" = "provider_id")) %>%
  inner_join(pay_val_care_reduced, by = c("provider_id" = "provider_id")) %>%
  inner_join(mspb_reduced, by = c("provider_id" = "provider_id"))