example <- hai_reduced %>%
  filter(provider_id == 10001) %>%
  select(provider_id, measure_id, score)


example <- hai_reduced_spread %>%
  filter(provider_id == 10001) %>%
  select(provider_id, Measure, CI_LOWER_score, CI_UPPER_score, DOPC_DAYS_score, 
         ELIGCASES_score, NUMERATOR_score, SIR_score
         )

hai_reduced_spread %>%
  filter(is.na(SIR_score))

# there are 17116 missing SIR scores
hai_sir %>% 
  filter(is.na(score)) %>%
  select(score) %>%
  count(score)

# there are 11720 with SIR scores
hai_sir %>% 
  filter(!is.na(score)) %>%
  select(score)

# count hospitals per HAI Measure score not NA
hai_measures_plot <- hai_sir %>%
  filter(!is.na(score)) %>%
  count(provider_id, Measure)

ggplot(hai_measures_plot, aes(Measure)) +
  geom_bar() +
  labs(title = "Count of Providers Reporting per Measure", y = "Provider Count")


# there are 1633 with score of 0.000
hai_reduced %>% 
  filter(score == "0.000") %>%
  arrange(provider_id)



# TODO: remove NAs for this plot
ggplot(hai_sir, aes(Measure)) + 
  geom_bar() +
  labs(title = "Count of Providers Reporting per Measure", y = "Provider Count")

#what is the averge number of measures hospitals have scores for? -- 24.06
ave_meas_hosp <- hai_reduced %>%
  select(provider_id, measure_id, score) %>%
  filter(score != "Not Available") %>%
  group_by(provider_id) %>%
  count(provider_id)
mean(ave_meas_hosp$n)

# summary of SIR
hai_sir %>%
  filter(!is.na(score)) %>%
  group_by(Measure) %>%
  summarise(mean = mean(as.double(score)), sd = sd(as.double(score)),
            IQR = IQR(as.double(score)),
            min = min(as.double(score)), max = max(as.double(score))
            )

# how many measures are there per provider -- 36 for all
hai_reduced %>%
  select(provider_id, measure_id) %>%
  group_by(provider_id) %>%
  count(provider_id)

# how many measures per provider with reported score
hai_reduced %>%
  select(provider_id, measure_id, score) %>%
  filter(!is.na(score)) %>%
  group_by(provider_id) %>%
  count(provider_id)

# which Measure is missing the most scores?  --HAI 4 with 4038
hai_sir %>%
  filter(is.na(score)) %>%
  select(Measure) %>%
  group_by(Measure) %>%
  count(Measure)

# there are 1712 providers with no SIR scores  -- TODO: should we remove these providers altogether?
hai_sir %>%
  filter(is.na(score)) %>%
  select(Measure, provider_id, score) %>%
  count(score, provider_id) %>%
  group_by(provider_id) %>%
  arrange(desc(n)) %>%
  filter(n == 6)

# there are 2373 providers with at least one SIR score missing -- TODO: should we replace them with 0?
hai_sir %>%
  filter(is.na(score)) %>%
  select(Measure, provider_id, score) %>%
  count(score, provider_id) %>%
  group_by(provider_id) %>%
  arrange(desc(n)) %>%
  filter(n < 6)

prov_meas_score <- hai_sir %>%
  select(provider_id, measure_id, score) %>%
  filter(!is.na(score))

ggplot(data = prov_meas_score, mapping = aes(x = measure_id)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggtitle("Number of Providers with Scores per Measure ID")

# find all measures that don't have a score, and see if they have a numerator and denominator
## the numbers seem too small to conclusively say that the NAs can be replaced with 0
no_score_measures <- hai_reduced_spread %>%
  filter(is.na(SIR_score) & !is.na(ELIGCASES_score)) %>%
  select(provider_id, SIR_score, ELIGCASES_score, NUMERATOR_score) %>%
  arrange(provider_id)
no_score_measures

yes_score_measures <- hai_reduced_spread %>%
  filter(!is.na(SIR_score)) %>%
  select(provider_id, SIR_score, ELIGCASES_score, NUMERATOR_score) %>%
arrange(provider_id)
yes_score_measures


# Vizualizations and summary stats
hai_reduced_spread %>%
  filter(Measure == "HAI_6" & is.na(SIR_score))

hai_reduced_spread %>%
  filter(!is.na(SIR_score)) %>%
  ggplot(aes(Measure, SIR_score)) + 
  geom_boxplot(fill = "red") +
  labs(title = "SIR Score per Measure", x = "Quality Measure", y = "Score") + 
  theme_bw()
  
hai_reduced_spread %>%
  filter(!is.na(SIR_score)) %>%
  ggplot(mapping = aes(x = Measure)) +
  geom_bar(fill = "red") + 
  theme_bw() +
  labs(x = "Quality Measure", y = "Count of Reporting Providers", title = "HAI Score Distribution of Reporting Providers")
