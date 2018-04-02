#----------Testing example----------#
df <- data.frame(month=rep(1:3,2),
                 student=rep(c("Amy", "Bob"), each=3),
                 A=c(9, 7, 6, 8, 6, 9),
                 B=c(6, 7, 8, 5, 6, 7))

df %>% 
  gather(variable, value, -(month:student))

df %>% 
  gather(variable, value, -(month:student)) %>%
  unite(temp, student, variable)

df %>% 
  gather(variable, value, -(month:student)) %>%
  unite(temp, student, variable) %>%
  spread(temp, value)
#-------------------------------------#

# These are the steps I took to spread the data out so that we'd get, per provider, a row for each
# HAI type, which means 6 rows per provider. There's probably a more efficient way of doing this.
# I only made new data frames at each step so that I'd be able to view them better. Only the last is
# what we need.

# Step 1: Make new identifier for provider-HAI type that we can use to get a row for each type per provider
hai_reduced %>%
  filter(provider_id == 10005) %>%
  unite(provider_hai, provider_id, Measure) 

# Step 2: Gather
hai_reduced_gather <- hai_reduced %>%
  # filter(provider_id == 670112) %>%
  unite(provider_hai, provider_id, Measure) %>%
  select(provider_hai, Type, score, compared_to_national) %>%
  gather(variable, value, -(provider_hai:Type))

# Step 3: Unite
hai_reduced_unite <- hai_reduced %>%
  # filter(provider_id == 670112) %>%
  unite(provider_hai, provider_id, Measure) %>%
  select(provider_hai, Type, score, compared_to_national) %>%
  gather(variable, value, -(provider_hai:Type)) %>%
  unite(temp, Type, variable)

# Step 4: Spread
hai_reduced_spread <- hai_reduced %>%
  # filter(provider_id == 670112) %>%
  unite(provider_hai, provider_id, Measure) %>%
  select(provider_hai, Type, score, compared_to_national) %>%
  gather(variable, value, -(provider_hai:Type)) %>%
  unite(temp, Type, variable) %>%
  spread(temp, value)

#Step 5: Break out provider-type again and drop provider-type column
hai_reduced_spread <- hai_reduced_spread %>%
  mutate("provider_id"= as.integer(str_sub(provider_hai, 1, unlist(lapply(strsplit(provider_hai, ''),
                                                                          function(provider_hai) which(provider_hai == '_')))[c(TRUE,FALSE)]-1)),
         "Measure" = str_sub(provider_hai, unlist(lapply(strsplit(provider_hai, ''),
                                                      function(provider_hai) which(provider_hai == '_')))[c(TRUE,FALSE)]+1, 
                          str_length(provider_hai)),
         CI_LOWER_score = as.double(CI_LOWER_score),
         CI_UPPER_score = as.double(CI_UPPER_score),
         DOPC_DAYS_score = as.integer(DOPC_DAYS_score),
         ELIGCASES_score = as.double(ELIGCASES_score),
         NUMERATOR_score = as.integer(NUMERATOR_score),
         SIR_score = as.double(SIR_score)) %>%
  select(provider_id, Measure, everything(), -provider_hai) %>%
  arrange(provider_id)

hai_reduced_spread
