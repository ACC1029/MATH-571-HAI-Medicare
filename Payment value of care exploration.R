# what is the distribution of value_of_care_categories missing values?
pay_val_care_reduced %>%
  filter(is.na(value_of_care_category)) %>%
  count(payment_measure_id, value_of_care_category)

# what is the distribution of payment_categories missing values?
pay_val_care_reduced %>%
  filter(is.na(payment_category)) %>%
  count(payment_measure_id, payment_category)

# what is the distribution of payment_categories missing values?
pay_val_care_reduced %>%
  filter(is.na(payment_category)) %>%
  count(value_of_care_measure, value_of_care_category)

# what are the measure names?
pay_val_care_reduced %>%
  select(provider_id, payment_measure_name) %>%
  count(payment_measure_name)

# what are the payment categories (comparisons)?
pay_val_care_reduced %>%
  select(provider_id, payment_category) %>%
  count(payment_category)

# what are the value of care display names?
pay_val_care_reduced %>%
  select(provider_id, value_of_care_display_name) %>%
  count(value_of_care_display_name)

# what are the value of care display ids?
pay_val_care_reduced %>%
  select(provider_id, value_of_care_display_id) %>%
  count(value_of_care_display_id)

# what are the value of care categories?
pay_val_care_reduced %>%
  select(provider_id, value_of_care_category) %>%
  count(value_of_care_category)


# Vizualizations and summary stats

# distribution of payment
ggplot(pay_val_care_reduced, aes(payment_measure_id, payment)) +
  labs(title = "Payment per Payment Measure", x = "Payment Measure", y = "Payment Amount") + 
  geom_boxplot(fill = "red") +
  theme_bw()

# summary of SIR
pay_val_care_reduced %>%
  filter(!is.na(payment)) %>%
  group_by(payment_measure_id) %>%
  summarise(mean = mean(as.double(payment)), sd = sd(as.double(payment)),
            IQR = IQR(as.double(payment)),
            min = min(as.double(payment)), max = max(as.double(payment))
  )


