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

hosp_gen_info_reduced <- select(hosp_gen_info_raw, -hospital_overall_rating_footnote, -mortality_footnote, 
                                -safety_of_care_footnote, -readmission_footnote, -patient_experience_footnote, 
                                -effectiveness_of_care_footnote, -timeliness_of_care_footnote,
                                -efficient_use_of_medical_imaging_footnote)
hai_reduced <- select(hai_raw, provider_id, measure_name, measure_id, compared_to_national, score)
mspb_reduced <- select(mspb_raw, provider_id, score)
pay_val_care_reduced <- select(payment_value_care_raw, provider_id, payment_measure_name, payment_measure_id,
                               payment_category, denominator, payment, lower_estimate, higher_estimate,
                               value_of_care_display_name, value_of_care_display_id, value_of_care_category,
                               location)

# join all the data together on provider_id
all_data <- hosp_gen_info_reduced %>%  
  inner_join(hai_reduced, by = c("provider_id" = "provider_id")) %>%
  inner_join(pay_val_care_reduced, by = c("provider_id" = "provider_id")) %>%
  inner_join(mspb_reduced, by = c("provider_id" = "provider_id"))

#--Exploration--#

# HAI

hai_reduced %>%
  select(provider_id, measure_id) %>%
  count(measure_id)

# need to spread out HAI measures
spread_hai <- hai_reduced %>%
  spread(measure_id, score)

# break up measure into measure and its type
measure_type <- hai_reduced %>% 
  mutate("Measure" = substring(text = measure_id, first = 1, last = 5),
         "Type" = substring(text = measure_id, first = 7,last = length(measure_id)))
measure_type

# 36 different measures
measures <- unique(filter(hai_reduced)[,c("measure_id", "measure_name")])

# 6 different SIR measures
sir_measures <- unique(filter(measure_type, Type == "SIR")[,c("Measure", "measure_name")])

#measure_type_SIR
measure_type_SIR <- measure_type %>%
  select(Measure, Type, score) %>%
  filter(Type == "SIR" & score != "Not Available") %>%
  group_by(Measure)

measure_type_SIR_prov <- measure_type %>%
  select(Measure, Type, score, provider_id) %>%
  filter(Type == "SIR" & score != "Not Available")
measure_type_SIR_prov

HAI_1 <- measure_type_SIR %>%
  filter(Measure == "HAI_1")

ggplot(measure_type_SIR, (aes(Measure, as.double(score)))) + 
  geom_boxplot() +
  labs(title = "SIR Score per Measure", x = "Measure", y = "Score")

ggplot(measure_type_SIR_prov, aes(Measure)) + 
  geom_bar() +
  labs(title = "Count of Providers Reporting per Measure", y = "Provider Count")

# summary of SIR
measure_type_SIR %>%
  group_by(Measure) %>%
  summarise(mean = mean(as.double(score)), sd = sd(as.double(score)),
            IQR = IQR(as.double(score)),
            min = min(as.double(score)), max = max(as.double(score))
            )

#4,806 hospitals
hospitals <- unique(filter(hai_reduced)[,c("provider_id")])
hospitals

# select provider_id and HAI score
hai_reduced %>%
  select(provider_id, score) %>%
  filter(score != "Not Available") %>%
  arrange(provider_id)

# how many measures are there per provider -- 36 for all
hai_reduced %>%
  select(provider_id, measure_id) %>%
  group_by(provider_id) %>%
  count(provider_id)

#what is the averge number of measures hospitals have scores for? -- 24.06
ave_meas_hosp <- hai_reduced %>%
  select(provider_id, measure_id, score) %>%
  filter(score != "Not Available") %>%
  group_by(provider_id) %>%
  count(provider_id)
mean(ave_meas_hosp$n)

#what is the average number of hospitals each measure (with scores) is reported by? #2698.67
ave_hosp_meas <- hai_reduced %>%
  select(provider_id, measure_id, score) %>%
  filter(score != "Not Available") %>%
  group_by(measure_id) %>%
  count(measure_id)
mean(ave_hosp_meas$n)

prov_meas_score <- hai_reduced %>%
  select(provider_id, measure_id, score) %>%
  filter(score != "Not Available")
prov_meas_score

ggplot(data = prov_meas_score, mapping = aes(x = measure_id)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggtitle("Number of Providers with Scores per Measure ID")


#-------Spending per Patient--------------#

mspb_scores <- mspb_reduced %>%
  select(score, measure_id) %>%
  filter(score != "Not Available") 
mspb_scores

ggplot(mspb_scores, aes(measure_id, as.double(score)), xlab = "Measure ID") + 
  geom_boxplot() +
  labs(title = "Medicare spending per beneficiary scores", x = "Measure ID", y = "Score")




#---------Hospital General Information-----------#
spec(hosp_gen_info_raw)

# how many hospitals per state?
hosp_gen_info_raw %>%
  select(provider_id, state) %>%
  count(state)

# what types of hospitals are there and how many?
hosp_gen_info_raw %>%
  select(provider_id, hospital_type) %>%
  count(hospital_type)

# what types of hospitals are there per state?
hosp_gen_info_raw %>%
  select(provider_id, hospital_type, state) %>%
  count(state, hospital_type)

#what hospital owners are there and how many?
hosp_gen_info_raw %>%
  select(provider_id, hospital_owner) %>%
  count(hospital_owner)

# what types of hospital owners are there per state?
hosp_gen_info_raw %>%
  select(provider_id, hospital_owner, state) %>%
  count(state, hospital_owner)

# how many hospitals have emergency services?
hosp_gen_info_raw %>%
  select(provider_id, emergency_services) %>%
  count(emergency_services)

# how many hospitals meet meaningful use requirements?
hosp_gen_info_raw %>%
  select(provider_id, meets_meaningful_use) %>%
  count(meets_meaningful_use)

# what is the distribution of overall ratings?
hosp_gen_info_raw %>%
  select(provider_id, hospital_overall_rating) %>%
  count(hospital_overall_rating)

# what are the mortality comparisons?
hosp_gen_info_raw %>%
  select(provider_id, mortality) %>%
  count(mortality)

# what are the safety of care comparisons?
hosp_gen_info_raw %>%
  select(provider_id, safety_of_care) %>%
  count(safety_of_care)

# what are the readmission comparisons?
hosp_gen_info_raw %>%
  select(provider_id, readmission) %>%
  count(readmission)

# what are the patient experience comparisons?
hosp_gen_info_raw %>%
  select(provider_id, patient_experience) %>%
  count(patient_experience)

# what are the effectiveness of care comparisons?
hosp_gen_info_raw %>%
  select(provider_id, effectiveness_of_care) %>%
  count(effectiveness_of_care)

# what are the timeliness of care comparisons?
hosp_gen_info_raw %>%
  select(provider_id, timeliness_of_care) %>%
  count(timeliness_of_care)

# what are the efficient use of medical imaging of care comparisons?
hosp_gen_info_raw %>%
  select(provider_id, efficient_use_of_medical_imaging) %>%
  count(efficient_use_of_medical_imaging)


#-------Payment value of care--------#

# what are the measure names?
payment_value_care_raw %>%
  select(provider_id, payment_measure_name) %>%
  count(payment_measure_name)

# what are the payment categories (comparisons)?
payment_value_care_raw %>%
  select(provider_id, payment_category) %>%
  count(payment_category)

# distribution of payment
payment <- as.numeric(sub(',', '', sub('\\$', '', pay_val_care_reduced$payment)))

ggplot(payment_value_care_raw, aes(provider_id, as.numeric(sub(',', '', sub('\\$', '', pay_val_care_reduced$payment))))) +
         labs(x = "Provider ID", y = "Payment") + 
  geom_boxplot() 

# what are the value of care display names?
payment_value_care_raw %>%
  select(provider_id, value_of_care_display_name) %>%
  count(value_of_care_display_name)

# what are the value of care display ids?
payment_value_care_raw %>%
  select(provider_id, value_of_care_display_id) %>%
  count(value_of_care_display_id)

# 