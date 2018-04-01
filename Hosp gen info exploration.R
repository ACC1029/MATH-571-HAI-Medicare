# Recode variables with national comparisons
hosp_gen_info_recoded <- hosp_gen_info_reduced %>%
  mutate(mortality_code = recode(mortality, 
                                 "Above the national average" = 1,
                                 "Same as the national average" = 0, 
                                 "Below the national average" = -1),
         safety_of_care_code = recode(safety_of_care,
                                      "Above the national average" = 1,
                                      "Same as the national average" = 0,
                                      "Below the national average" = -1),
         patient_experience_code = recode(patient_experience,
                                          "Above the national average" = 1,
                                          "Same as the national average" = 0,
                                          "Below the national average" = -1),
         timeliness_of_care_code = recode(timeliness_of_care,
                                          "Above the national average" = 1,
                                          "Same as the national average" = 0,
                                          "Below the national average" = -1),
         readmission_code = recode(readmission,
                                   "Above the national average" = 1,
                                   "Same as the national average" = 0,
                                   "Below the national average" = -1),
         effectiveness_of_care_code = recode(effectiveness_of_care,
                                             "Above the national average" = 1,
                                             "Same as the national average" = 0,
                                             "Below the national average" = -1),
         efficient_use_of_medical_imaging_code = recode(efficient_use_of_medical_imaging,
                                                        "Above the national average" = 1,
                                                        "Same as the national average" = 0,
                                                        "Below the national average" = -1)
  )

# how many hospitals per state?
hosp_gen_info_reduced %>%
  select(provider_id, state) %>%
  count(state)

# what types of hospitals are there and how many?
hosp_gen_info_reduced %>%
  select(provider_id, hospital_type) %>%
  count(hospital_type)

# what types of hospitals are there per state?
hosp_gen_info_reduced %>%
  select(provider_id, hospital_type, state) %>%
  count(state, hospital_type)

# what hospital owners are there and how many?
hosp_gen_info_reduced %>%
  select(provider_id, hospital_owner) %>%
  count(hospital_owner)

# what types of hospital owners are there per state?
hosp_gen_info_reduced %>%
  select(provider_id, hospital_owner, state) %>%
  count(state, hospital_owner)

# how many hospitals have emergency services?
hosp_gen_info_reduced %>%
  select(provider_id, emergency_services) %>%
  count(emergency_services)

# how many hospitals meet meaningful use requirements?
hosp_gen_info_reduced %>%
  select(provider_id, meets_meaningful_use) %>%
  count(meets_meaningful_use)

# what is the distribution of overall ratings?
hosp_gen_info_reduced %>%
  select(provider_id, hospital_overall_rating) %>%
  count(hospital_overall_rating)

# what are the mortality comparisons?
hosp_gen_info_reduced %>%
  select(provider_id, mortality) %>%
  count(mortality)

hosp_gen_info_recoded %>%
  select(provider_id, mortality) %>%
  count(mortality)


# what are the safety of care comparisons?
hosp_gen_info_reduced %>%
  select(provider_id, safety_of_care) %>%
  count(safety_of_care)

# what are the readmission comparisons?
hosp_gen_info_reduced %>%
  select(provider_id, readmission) %>%
  count(readmission)

# what are the patient experience comparisons?
hosp_gen_info_reduced %>%
  select(provider_id, patient_experience) %>%
  count(patient_experience)

# what are the effectiveness of care comparisons?
hosp_gen_info_reduced %>%
  select(provider_id, effectiveness_of_care) %>%
  count(effectiveness_of_care)

# what are the timeliness of care comparisons?
hosp_gen_info_reduced %>%
  select(provider_id, timeliness_of_care) %>%
  count(timeliness_of_care)

# what are the efficient use of medical imaging of care comparisons?
hosp_gen_info_reduced %>%
  select(provider_id, efficient_use_of_medical_imaging) %>%
  count(efficient_use_of_medical_imaging)