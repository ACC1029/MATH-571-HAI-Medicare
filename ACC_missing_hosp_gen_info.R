# providers missing HAI_6 score by hospital type
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score)) %>%
  count(hospital_type)

# providers missing HAI_6 score and mortality by hospital type
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score), is.na(mortality_code)) %>%
  count(hospital_type)

# providers missing HAI_6 score and safety of care by hospital type
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score), is.na(safety_of_care_code)) %>%
  count(hospital_type)

# providers missing HAI_6 score and patient experience by hospital type
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score), is.na(patient_experience_code)) %>%
  count(hospital_type)

# providers missing HAI_6 score and timeliness of care by hospital type
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score), is.na(timeliness_of_care_code)) %>%
  count(hospital_type)

# providers missing HAI_6 score and readmission by hospital type
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score), is.na(readmission_code)) %>%
  count(hospital_type)

# providers missing HAI_6 score and effectiveness of care by hospital type
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score), is.na(effectiveness_of_care_code)) %>%
  count(hospital_type)

# providers missing HAI_6 score and efficient use of medical imaging by hospital type
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score), is.na(efficient_use_of_medical_imaging_code)) %>%
  count(hospital_type)





# providers missing HAI_6 score and mortality by hospital owner
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score), is.na(mortality_code)) %>%
  count(hospital_owner)

# providers missing HAI_6 score and safety of care by hospital owner
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score), is.na(safety_of_care_code)) %>%
  count(hospital_owner)

# providers missing HAI_6 score and patient experience by hospital owner
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score), is.na(patient_experience_code)) %>%
  count(hospital_owner)

# providers missing HAI_6 score and timeliness of care by hospital owner
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score), is.na(timeliness_of_care_code)) %>%
  count(hospital_owner)

# providers missing HAI_6 score and readmission by hospital owner
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score), is.na(readmission_code)) %>%
  count(hospital_owner)

# providers missing HAI_6 score and effectiveness of care by hospital owner
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score), is.na(effectiveness_of_care_code)) %>%
  count(hospital_owner)

# providers missing HAI_6 score and efficient use of medical imaging by hospital owner
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score), is.na(efficient_use_of_medical_imaging_code)) %>%
  count(hospital_owner)



# providers not missing HAI_6 score by hospital type
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", !is.na(SIR_score)) %>%
  count(hospital_type)


# providers not missing HAI_6 score and mortality by hospital type
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", !is.na(SIR_score), !is.na(mortality_code)) %>%
  count(hospital_type)

# providers not missing HAI_6 score and safety of care by hospital type
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", !is.na(SIR_score), !is.na(safety_of_care_code)) %>%
  count(hospital_type)

# providers not missing HAI_6 score and patient experience by hospital type
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", !is.na(SIR_score), !is.na(patient_experience_code)) %>%
  count(hospital_type)

# providers not missing HAI_6 score and timeliness of care by hospital type
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", !is.na(SIR_score), !is.na(timeliness_of_care_code)) %>%
  count(hospital_type)

# providers not missing HAI_6 score and readmission by hospital type
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", !is.na(SIR_score), !is.na(readmission_code)) %>%
  count(hospital_type)

# providers not missing HAI_6 score and effectiveness of care by hospital type
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", !is.na(SIR_score), !is.na(effectiveness_of_care_code)) %>%
  count(hospital_type)

# providers not missing HAI_6 score and efficient use of medical imaging by hospital type
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", !is.na(SIR_score), !is.na(efficient_use_of_medical_imaging_code)) %>%
  count(hospital_type)





# providers not missing HAI_6 score and mortality by hospital owner
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", !is.na(SIR_score), !is.na(mortality_code)) %>%
  count(hospital_owner)

# providers not missing HAI_6 score and safety of care by hospital owner
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", !is.na(SIR_score), !is.na(safety_of_care_code)) %>%
  count(hospital_owner)

# providers not missing HAI_6 score and patient experience by hospital owner
hosp_gen_in1fo_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", !is.na(SIR_score), !is.na(patient_experience_code)) %>%
  count(hospital_owner)

# providers not missing HAI_6 score and timeliness of care by hospital owner
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", !is.na(SIR_score), !is.na(timeliness_of_care_code)) %>%
  count(hos11pital_owner)

# providers not missing HAI_6 score and readmission by hospital owner
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", !is.na(SIR_score), !is.na(readmission_code)) %>%
  count(hospital_owner)

# providers not missing HAI_6 score and effectiveness of care by hospital owner
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", !is.na(SIR_score), !is.na(effectiveness_of_care_code)) %>%
  count(hospital_owner)

# providers not missing HAI_6 score and efficient use of medical imaging by hospital owner
hosp_gen_info_recoded %>%
  inner_join(hai_reduced_spread, by = "provider_id") %>%
  filter(Measure == "HAI_6", !is.na(SIR_score), !is.na(efficient_use_of_medical_imaging_code)) %>%
  count(hospital_owner)


# mortality mode for acute care hospitals
hosp_gen_info_recoded %>%
  filter(hospital_type == "Acute Care Hospitals", !is.na(mortality_code)) %>%
  count(mortality_code) %>%
  filter(n == max(n))

# mortality mode for Critical Access Hospitals hospitals
hosp_gen_info_recoded %>%
  filter(hospital_type == "Critical Access Hospitals", !is.na(mortality_code)) %>%
  count(mortality_code) %>%
  filter(n == max(n))

# safety of care mode for acute care hospitals
hosp_gen_info_recoded %>%
  filter(hospital_type == "Acute Care Hospitals", !is.na(safety_of_care_code)) %>%
  count(safety_of_care_code) %>%
  filter(n == max(n))

# safety of care mode for Critical Access Hospitals hospitals
hosp_gen_info_recoded %>%
  filter(hospital_type == "Critical Access Hospitals", !is.na(safety_of_care_code)) %>%
  count(safety_of_care_code) %>%
  filter(n == max(n))

# patient experience mode for acute care hospitals
hosp_gen_info_recoded %>%
  filter(hospital_type == "Acute Care Hospitals", !is.na(patient_experience_code)) %>%
  count(patient_experience_code) %>%
  filter(n == max(n))

# patient experience mode for Critical Access Hospitals hospitals
hosp_gen_info_recoded %>%
  filter(hospital_type == "Critical Access Hospitals", !is.na(patient_experience_code)) %>%
  count(patient_experience_code) %>%
  filter(n == max(n))

# timeliness of care mode for acute care hospitals
hosp_gen_info_recoded %>%
  filter(hospital_type == "Acute Care Hospitals", !is.na(timeliness_of_care_code)) %>%
  count(timeliness_of_care_code) %>%
  filter(n == max(n))

# timeliness of care mode for Critical Access Hospitals hospitals
hosp_gen_info_recoded %>%
  filter(hospital_type == "Critical Access Hospitals", !is.na(timeliness_of_care_code)) %>%
  count(timeliness_of_care_code) %>%
  filter(n == max(n))

# readmission mode for acute care hospitals
hosp_gen_info_recoded %>%
  filter(hospital_type == "Acute Care Hospitals", !is.na(readmission_code)) %>%
  count(readmission_code) %>%
  filter(n == max(n))

# readmission mode for Critical Access Hospitals hospitals
hosp_gen_info_recoded %>%
  filter(hospital_type == "Critical Access Hospitals", !is.na(readmission_code)) %>%
  count(readmission_code) %>%
  filter(n == max(n))

# effectiveness of care mode for acute care hospitals
hosp_gen_info_recoded %>%
  filter(hospital_type == "Acute Care Hospitals", !is.na(effectiveness_of_care_code)) %>%
  count(effectiveness_of_care_code) %>%
  filter(n == max(n))

# effectiveness of care mode for Critical Access Hospitals hospitals
hosp_gen_info_recoded %>%
  filter(hospital_type == "Critical Access Hospitals", !is.na(effectiveness_of_care_code)) %>%
  count(effectiveness_of_care_code) %>%
  filter(n == max(n))

# efficient use of medical imaging mode for acute care hospitals
hosp_gen_info_recoded %>%
  filter(hospital_type == "Acute Care Hospitals", !is.na(efficient_use_of_medical_imaging_code)) %>%
  count(efficient_use_of_medical_imaging_code) %>%
  filter(n == max(n))

# efficient use of medical imaging mode for Critical Access Hospitals hospitals
hosp_gen_info_recoded %>%
  filter(hospital_type == "Critical Access Hospitals", !is.na(readmission_code)) %>%
  count(efficient_use_of_medical_imaging_code) %>%
  filter(n == max(n))


# mode for Government - Federal hospitals
hosp_gen_info_recoded %>%
  filter(hospital_owner == "Government - Federal", !is.na(effectiveness_of_care_code)) %>%
  count(effectiveness_of_care_code) %>%
  filter(n == max(n))

# mode for Government - Hospital District or Authority hospitals
hosp_gen_info_recoded %>%
  filter(hospital_owner == "Government - Hospital District or Authority", !is.na(effectiveness_of_care_code)) %>%
  count(effectiveness_of_care_code) %>%
  filter(n == max(n))

# mode for Government - Local hospitals
hosp_gen_info_recoded %>%
  filter(hospital_owner == "Government - Local", !is.na(effectiveness_of_care_code)) %>%
  count(effectiveness_of_care_code) %>%
  filter(n == max(n))

# mode for Government - State hospitals
hosp_gen_info_recoded %>%
  filter(hospital_owner == "Government - State", !is.na(effectiveness_of_care_code)) %>%
  count(effectiveness_of_care_code) %>%
  filter(n == max(n))

# mode for Physician hospitals
hosp_gen_info_recoded %>%
  filter(hospital_owner == "Physician", !is.na(effectiveness_of_care_code)) %>%
  count(effectiveness_of_care_code) %>%
  filter(n == max(n))

# mode for Proprietary hospitals
hosp_gen_info_recoded %>%
  filter(hospital_owner == "Proprietary", !is.na(effectiveness_of_care_code)) %>%
  count(effectiveness_of_care_code) %>%
  filter(n == max(n))

# mode for Tribal hospitals
hosp_gen_info_recoded %>%
  filter(hospital_owner == "Tribal", !is.na(effectiveness_of_care_code)) %>%
  count(effectiveness_of_care_code) %>%
  filter(n == max(n))

# mode for Voluntary non-profit - Church hospitals
hosp_gen_info_recoded %>%
  filter(hospital_owner == "Voluntary non-profit - Church", !is.na(effectiveness_of_care_code)) %>%
  count(effectiveness_of_care_code) %>%
  filter(n == max(n))

# mode for Voluntary non-profit - Other hospitals
hosp_gen_info_recoded %>%
  filter(hospital_owner == "Voluntary non-profit - Other", !is.na(effectiveness_of_care_code)) %>%
  count(effectiveness_of_care_code) %>%
  filter(n == max(n))

# mode for Voluntary non-profit - Private hospitals
hosp_gen_info_recoded %>%
  filter(hospital_owner == "Voluntary non-profit - Private", !is.na(effectiveness_of_care_code)) %>%
  count(effectiveness_of_care_code) %>%
  filter(n == max(n))
