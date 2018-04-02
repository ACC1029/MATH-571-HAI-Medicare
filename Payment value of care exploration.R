# makes dollar numeric
pay_val_care_reduced <- pay_val_care_reduced %>% 
  mutate(payment = as.numeric(sub(',', '', sub('\\$', '', pay_val_care_reduced$payment))),
         lower_estimate = as.numeric(sub(',', '', sub('\\$', '', pay_val_care_reduced$lower_estimate))),
         higher_estimate = as.numeric(sub(',', '', sub('\\$', '', pay_val_care_reduced$higher_estimate)))
         )

pay_val_care_reduced %>%
  filter(is.na(location)) %>%
  select(location)

# what are the measure names?
pay_val_care_reduced %>%
  select(provider_id, payment_measure_name) %>%
  count(payment_measure_name)

# what are the payment categories (comparisons)?
pay_val_care_reduced %>%
  select(provider_id, payment_category) %>%
  count(payment_category)

# distribution of payment
ggplot(pay_val_care_reduced, aes(provider_id, payment)) +
  labs(x = "Provider ID", y = "Payment") + 
  geom_boxplot() 

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

# TODO: should we sum the payments per hospital?