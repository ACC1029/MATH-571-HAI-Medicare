# R script to create reduced datasets, minus all hospitals too small
# Run HAI exploration script and all its dependencies before this one if you're in a fresh session

## Using lack of ANY HAI scores as a easy parameter for too small
## This may be incorrect

hosp_gen_info_reduced_nona <- hosp_gen_info_reduced
hosp_gen_info_reduced_nona[!(hosp_gen_info_reduced$provider_id %in% no_score_providers$provider_id),]

mspb_reduced_nona <- mspb_reduced
mspb_reduced_nona[!(mspb_reduced_nona$provider_id %in% no_score_providers$provider_id),]

pay_val_care_reduced_nona <- pay_val_care_reduced
pay_val_care_reduced_nona[!(pay_val_care_reduced_nona$provider_id %in% no_score_providers$provider_id),]

write_csv(hosp_gen_info_reduced_nona, "adjusted_data/hosp_gen_info.csv", na = "NA", append = FALSE, col_names = TRUE)

write_csv(mspb_reduced_nona, "adjusted_data/mspb.csv", na = "NA", append = FALSE, col_names = TRUE)

write_csv(pay_val_care_reduced_nona, "adjusted_data/pay_val_of_care.csv", na = "NA", append = FALSE, col_names = TRUE)