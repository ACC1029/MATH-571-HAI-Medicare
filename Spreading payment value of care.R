# These are the steps I took to spread the data out so that we'd get, per provider, a row for each
# pay_val payment_type, which means 6 rows per provider. There's probably a more efficient way of doing this.
# I only made new data frames at each step so that I'd be able to view them better. Only the last is
# what we need.

# Step 1: Make new identifier for provider-pay_val payment_type that we can use to get a row for each payment_type per provider
pay_val_care_reduced %>%
  filter(provider_id == 10005) %>%
  unite(provider_payer, provider_id, payment_payment_measure) 

# Step 2: Gather
pay_val_reduced_gather <- pay_val_care_reduced %>%
  filter(provider_id == 10005) %>%
  unite(provider_payer, provider_id, payment_measure) %>%
  select(provider_payer, payment_type, payment_category) %>%
  gather(variable, value, -(provider_payer:payment_type))

# Step 3: Unite
pay_val_reduced_unite <- pay_val_care_reduced %>%
  filter(provider_id == 10005) %>%
  unite(provider_payer, provider_id, payment_measure) %>%
  select(provider_payer, payment_type, payment_category) %>%
  gather(variable, value, -(provider_payer:payment_type)) %>%
  unite(temp, payment_type, variable)

# Step 4: Spread
pay_val_reduced_spread <- pay_val_care_reduced %>%
  filter(provider_id == 10005) %>%
  unite(provider_payer, provider_id, payment_measure) %>%
  select(provider_payer, payment_type, payment_category) %>%
  gather(variable, value, -(provider_payer:payment_type)) %>%
  unite(temp, payment_type, variable) %>%
  spread(temp, value)

#Step 5: Break out provider-payment_type again and drop provider-payment_type column
pay_val_reduced_spread <- pay_val_reduced_spread %>%
  mutate("provider_id"= as.integer(str_sub(provider_payer, 1, unlist(lapply(strsplit(provider_payer, ''),
                                                                          function(provider_payer) which(provider_payer == '_')))[c(TRUE,FALSE)]-1)),
         "payment_measure" = str_sub(provider_payer, unlist(lapply(strsplit(provider_payer, ''),
                                                         function(provider_payer) which(provider_payer == '_')))[c(TRUE,FALSE)]+1, 
                             str_length(provider_payer))
         ) %>%
  select(provider_id, payment_measure, everything(), -provider_payer) %>%
  arrange(provider_id)

pay_val_reduced_spread
