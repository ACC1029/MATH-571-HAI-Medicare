mspb_reduced 



# there are 3152 providers with scores
mspb_reduced %>%
  select(provider_id, score) %>%
  filter(!is.na(score)) 

# there are 1644 providers without scores
mspb_reduced %>%
  select(provider_id, score) %>%
  filter(is.na(score)) 

# mean score excluding NA rows
mspb_reduced %>%
  filter(!is.na(score)) %>%
  summarise(mean = mean(as.double(score)), sd = sd(as.double(score)),
            median = median(as.double(score)),
            IQR = IQR(as.double(score)),
            min = min(as.double(score)), max = max(as.double(score))
  )

# provider 170201 has the highest score by far
no_outlier <- mspb_raw %>%
  filter(score != 2.63)



ggplot(no_outlier, aes(measure_id, as.double(score)), xlab = "Measure ID") + 
  geom_boxplot() +
  labs(title = "Medicare spending per beneficiary scores", x = "Measure ID", y = "Score")

# TODO: check outlier -- what other characteristics does the provider have?