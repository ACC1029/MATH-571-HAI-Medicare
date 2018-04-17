# makes dollar numeric and break up measures into measure and their types
pay_val_care_reduced <- pay_val_care_reduced %>% 
  mutate(payment = as.numeric(sub(',', '', sub('\\$', '', pay_val_care_reduced$payment))),
         lower_estimate = as.numeric(sub(',', '', sub('\\$', '', pay_val_care_reduced$lower_estimate))),
         higher_estimate = as.numeric(sub(',', '', sub('\\$', '', pay_val_care_reduced$higher_estimate))),
         "payment_measure" = substring(text = payment_measure_id, first = 1, last = 7),
         "payment_type" = substring(text = payment_measure_id, first = 9, last = length(payment_measure_id)),
         "value_of_care_measure" = substring(text = value_of_care_display_id, first = 1, last = 12),
         "value_of_care_type" = substring(text = value_of_care_display_id, first = 14, last = length(value_of_care_display_id))
         )

# recode ordered categories. I think it's ok to ignore payment level in value of care category because it's 
# captured in the payment category.
pay_val_care_recoded <- pay_val_care_reduced %>%
  mutate(payment_category_code = recode(payment_category,
                                        "Greater than the National Average Payment" = 1,
                                        "No different than the national average payment" = 0,
                                        "Less than the National Average Payment" = -1,
                                        "Number of Cases Too Small" = -2),
         value_of_care_category_code = recode(value_of_care_category,
                                              "Average complications and higher payment" = 0,
                                              "Average complications and average payment" = 0,
                                              "Average complications and lower payment " = 0,
                                              "Average mortality and higher payment" = 0,
                                              "Average mortality and average payment" = 0,
                                              "Average mortality and lower payment" = 0,
                                              "Better complications and higher payment" = 1,
                                              "Better complications and average payment" = 1,
                                              "Better complications and lower payment" = 1,
                                              "Better mortality and higher payment" = 1,
                                              "Better mortality and average payment" = 1,
                                              "Better mortality and lower payment" = 1,
                                              "Worse complications and higher payment" = -1,
                                              "Worse complications and average payment" = -1,
                                              "Worse complications and lower payment" = -1,
                                              "Worse mortality and higher payment" = -1,
                                              "Worse mortality and average payment" = -1,
                                              "Worse mortality and lower payment" = -1
                                              )
         ) %>%
  mutate(payment_category_code = replace(payment_category_code, payment_category_code == -2, NA))

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