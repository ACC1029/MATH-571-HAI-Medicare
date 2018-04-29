mspb_reduced %>%
  filter(is.na(score))
  

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
  summarise(mean = mean(score), sd = sd(as.double(score)),
            median = median(as.double(score)),
            IQR = IQR(as.double(score)),
            min = min(as.double(score)), max = max(as.double(score))
  )

# provider 170201 has the highest score by far
no_outlier <- mspb_raw %>%
  filter(spend_score != 2.63)



ggplot(no_outlier, aes(measure_id, as.double(spend_score)), xlab = "Measure ID") + 
  geom_boxplot(fill = "red") +
  labs(title = "Medicare spending per beneficiary scores", x = "Measure ID", y = "Score") + 
  theme_bw()

# summary of spending
no_outlier %>%
  filter(spend_score != "Not Available") %>%
  summarise(mean = mean(as.double(spend_score)), sd = sd(as.double(spend_score)),
            IQR = IQR(as.double(spend_score)),
            min = min(as.double(spend_score)), max = max(as.double(spend_score))
  )

# TODO: check outlier -- what other characteristics does the provider have?
