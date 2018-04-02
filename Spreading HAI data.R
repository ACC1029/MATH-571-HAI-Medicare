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
  filter(provider_id == 10001) %>%
  unite(provider_hai, provider_id, Measure) 

# Step 2: Gather
hai_reduced_gather <- hai_reduced %>%
  filter(provider_id == 10001) %>%
  unite(provider_hai, provider_id, Measure) %>%
  select(provider_hai, Type, score, compared_to_national) %>%
  gather(variable, value, -(provider_hai:Type))

# Step 3: Unite
hai_reduced_unite <- hai_reduced %>%
  filter(provider_id == 10001) %>%
  unite(provider_hai, provider_id, Measure) %>%
  select(provider_hai, Type, score, compared_to_national) %>%
  gather(variable, value, -(provider_hai:Type)) %>%
  unite(temp, Type, variable)

# Step 4: Spread
hai_reduced_spread <- hai_reduced %>%
  filter(provider_id == 10001) %>%
  unite(provider_hai, provider_id, Measure) %>%
  select(provider_hai, Type, score, compared_to_national) %>%
  gather(variable, value, -(provider_hai:Type)) %>%
  unite(temp, Type, variable) %>%
  spread(temp, value)

#Step 5: Break out provider-type again and drop provider-type column
hai_reduced_spread <- hai_reduced_spread %>%
  mutate("provider_id"= substring(text = provider_hai, first = 1, last = 5),
           "Type" = substring(text = provider_hai, first = 7, last = 11)) %>%
  select(provider_id, Type, everything(), -provider_hai)