all_data <- hosp_gen_info_reduced %>%  
  inner_join(hai_reduced, by = c("provider_id" = "provider_id")) %>%
  inner_join(pay_val_care_reduced, by = c("provider_id" = "provider_id")) %>%
  inner_join(mspb_reduced, by = c("provider_id" = "provider_id"))

hosp_gen_info_reduced %>%  
  inner_join(hai_reduced_spread_nona, by = "provider_id") %>%
  filter(is.na(efficient_use_of_medical_imaging)) %>%
  select(provider_id) %>%
  distinct(provider_id)
  
hai_reduced_spread %>%
  filter(Measure == "HAI_6", is.na(SIR_score))
         
# of the 1737 providers that don't have HAI_6 scores, 1391 also don't have spending per patient scores
hai_reduced_spread %>%
  inner_join(mspb_reduced, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score), is.na(score))

# 263 provider with HAI_6 scores don't have spending per patient scores
hai_reduced_spread %>%
  inner_join(mspb_reduced, by = "provider_id") %>%
  filter(Measure == "HAI_6", !is.na(SIR_score), is.na(score))

# distribution of hospitals types that are missing HAI 6 scores and spending per patient scores
hai_reduced_spread %>%
  inner_join(mspb_reduced, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score), is.na(score)) %>%
  inner_join(hosp_gen_info_recoded, by = "provider_id") %>%
  select(provider_id, hospital_type) %>%
  count(hospital_type)

# distribution of hospitals owners that are missing HAI 6 scores and spending per patient scores
hai_reduced_spread %>%
  inner_join(mspb_reduced, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score), is.na(score)) %>%
  inner_join(hosp_gen_info_recoded, by = "provider_id") %>%
  select(provider_id, hospital_owner) %>%
  count(hospital_owner)

# distribution of emergency services that are missing HAI 6 scores and spending per patient scores
hai_reduced_spread %>%
  inner_join(mspb_reduced, by = "provider_id") %>%
  filter(Measure == "HAI_6", is.na(SIR_score), is.na(score)) %>%
  inner_join(hosp_gen_info_recoded, by = "provider_id") %>%
  select(provider_id, emergency_services) %>%
  count(emergency_services)

# distribution of hospitals types that are missing HAI 6 scores
hai_reduced_spread %>%
  filter(Measure == "HAI_6", is.na(SIR_score)) %>%
  inner_join(hosp_gen_info_recoded, by = "provider_id") %>%
  select(provider_id, hospital_type) %>%
  count(hospital_type)

# distribution of hospitals owners that are missing HAI 6 scores
hai_reduced_spread %>%
  filter(Measure == "HAI_6", is.na(SIR_score)) %>%
  inner_join(hosp_gen_info_recoded, by = "provider_id") %>%
  select(provider_id, hospital_owner) %>%
  count(hospital_owner)

# distribution of emergency services that are missing HAI 6 scores
hai_reduced_spread %>%
  filter(Measure == "HAI_6", is.na(SIR_score)) %>%
  inner_join(hosp_gen_info_recoded, by = "provider_id") %>%
  select(provider_id, emergency_services) %>%
  count(emergency_services)

# distribution of emergency services that are not missing HAI 6 scores
hai_reduced_spread %>%
  filter(Measure == "HAI_6", !is.na(SIR_score)) %>%
  inner_join(hosp_gen_info_recoded, by = "provider_id") %>%
  select(provider_id, emergency_services) %>%
  count(emergency_services)

# distribution of emergency services that are not missing HAI 6 scores or spending per patient scores
hai_reduced_spread %>%
  inner_join(mspb_reduced, by = "provider_id") %>%
  filter(Measure == "HAI_6", !is.na(SIR_score), !is.na(score)) %>%
  inner_join(hosp_gen_info_recoded, by = "provider_id") %>%
  select(provider_id, hospital_owner) %>%
  count(hospital_owner)

# what is the mean spending score of acute hospitals?
mspb_reduced %>%
  inner_join(hosp_gen_info_recoded, by = "provider_id") %>%
  filter(hospital_type == "Acute Care Hospitals", !is.na(score)) %>%
  summarize(mean = mean(score))

# what is the mean spending score of critical access hospitals?
mspb_reduced %>%
  inner_join(hosp_gen_info_recoded, by = "provider_id") %>%
  filter(hospital_type == "Critical Access Hospitals", !is.na(score)) %>%
  summarize(mean = mean(score))

# what is the mean spending score of hospital owner?
mspb_reduced %>%
  inner_join(hosp_gen_info_recoded, by = "provider_id") %>%
  filter(hospital_owner == "Voluntary non-profit - Private", !is.na(score)) %>%
  summarize(mean = mean(score))

# what is the mean spending score of emergency services?
mspb_reduced %>%
  inner_join(hosp_gen_info_recoded, by = "provider_id") %>%
  filter(emergency_services == "true", !is.na(score)) %>%
  summarize(mean = mean(score))

mspb_reduced %>%
  inner_join(hosp_gen_info_recoded, by = "provider_id") %>%
  filter(!is.na(score)) %>%
  summarize(mean = mean(score))

mspb_reduced %>%
  filter(!is.na(score)) %>%
  summarize(mean = mean(score), min = min(score), max = max(score))

