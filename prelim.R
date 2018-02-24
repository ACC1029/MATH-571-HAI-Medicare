library(tidyverse)

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

hai_reduced <- select(hai_raw, provider_id, state, measure_id, measure_name, compared_to_national, score)

#break up measure into measure and its type
measure_type <- hai_reduced %>% 
  mutate("Measure" = substring(text = measure_id, first = 1, last = 5),
         "Type" = substring(text = measure_id, first = 7,last = length(measure_id)))

# 36 different measures
measures <- unique(filter(hai_reduced)[,c("measure_id", "measure_name")])

# 6 different SIR measures
sir_measures <- unique(filter(measure_type, Type == "SIR")[,c("Measure", "measure_name")])

#measure_type_SIR
measure_type_SIR <- measure_type %>%
  select(Measure, Type, score) %>%
  filter(Type == "SIR" & score != "Not Available") %>%
  group_by(Measure)

measure_type_SIR_proc <- measure_type %>%
  select(Measure, Type, score, provider_id) %>%
  filter(Type == "SIR" & score != "Not Available")
measure_type_SIR_proc

HAI_1 <- measure_type_SIR %>%
  filter(Measure == "HAI_1")

ggplot(measure_type_SIR, (aes(Measure, as.double(score)))) + 
  geom_boxplot() +
  labs(title = "SIR Score per Measure", x = "Measure", y = "Score")

ggplot(measure_type_SIR_proc, aes(Measure)) + 
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

ggplot(data = prov_meas_score, mapping = aes(x = measure_id)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggtitle("Number of Providers with Scores per Measure ID")

hospital_info_raw <- read_csv("https://data.medicare.gov/resource/rbry-mqwu.csv",
                              na = c("", "NA"),
                              col_types = cols(
                              address = col_character(),
                              city = col_character(),
                              county_name = col_character(),
                              effectiveness_of_care_national_comparison = col_character(),
                              effectiveness_of_care_national_comparison_footnote = col_character(),
                              efficient_use_of_medical_imaging_national_comparison = col_character(),
                              efficient_use_of_medical_imaging_national_comparison_footnote = col_character(),
                              emergency_services = col_character(),
                              hospital_name = col_character(),
                              hospital_overall_rating = col_integer(),
                              hospital_overall_rating_footnote = col_character(),
                              hospital_ownership = col_character(),
                              hospital_type = col_character(),
                              location = col_character(),
                              location_address = col_character(),
                              location_city = col_character(),
                              location_state = col_character(),
                              location_zip = col_integer(),
                              meets_criteria_for_meaningful_use_of_ehrs = col_character(),
                              mortality_national_comparison = col_character(),
                              mortality_national_comparison_footnote = col_character(),
                              patient_experience_national_comparison = col_character(),
                              patient_experience_national_comparison_footnote = col_character(),
                              phone_number = col_double(),
                              phone_number_type = col_character(),
                              provider_id = col_integer(),
                              readmission_national_comparison = col_character(),
                              readmission_national_comparison_footnote = col_character(),
                              safety_of_care_national_comparison = col_character(),
                              safety_of_care_national_comparison_footnote = col_character(),
                              state = col_character(),
                              timeliness_of_care_national_comparison = col_character(),
                              timeliness_of_care_national_comparison_footnote = col_character(),
                              zip_code = col_integer()
                              )
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

mspb_reduced <- select(mspb_raw, provider_id, measure_id, state, score)

mspb_scores <- mspb_reduced %>%
  select(score, measure_id) %>%
  filter(score != "Not Available") 

ggplot(mspb_scores, aes(measure_id, as.double(score)), xlab = "Measure ID") + 
  geom_boxplot() +
  labs(title = "Medicare spending per beneficiary scores", x = "Measure ID", y = "Score")

