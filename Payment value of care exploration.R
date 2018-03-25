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