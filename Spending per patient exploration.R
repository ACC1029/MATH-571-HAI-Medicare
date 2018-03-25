mspb_scores <- mspb_reduced %>%
  select(score, measure_id) %>%
  filter(score != "Not Available") 
mspb_scores

ggplot(mspb_scores, aes(measure_id, as.double(score)), xlab = "Measure ID") + 
  geom_boxplot() +
  labs(title = "Medicare spending per beneficiary scores", x = "Measure ID", y = "Score")