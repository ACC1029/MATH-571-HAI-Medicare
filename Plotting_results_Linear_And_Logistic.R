library(tidyverse)
library(ggplot2)
library(RColorBrewer)


#Plotting hte division of above or below average HAI 6 SIR scores versus the mortality
p <- ggplot(data=hai_6_nona_recoded, aes(hai_6_nona_recoded$mortality)) + geom_bar(aes(fill = hai_6_nona_recoded$above_avg_score), position = "dodge") + scale_fill_brewer(palette="Set1")
p <- p + xlab("Mortality Compared To Average") 
p <- p + ylab("Count of Reporting Providers") 
p <- p + labs(title = "Distribution of Reporing Providers by HAI 6 SIR Scores and Mortality Scores", caption = "Based on cleaned CMS Hospital Compare data where HAI 6 SIR was reported, excluding providers with Tribal ownership or Children's type",legend = "HAI 6 Score Compared To National Average")
p <- p + guides(fill = guide_legend(title  = "HAI 6 Score Compared To National Average"))

p

# Distribution of reporting providers by HAI Measure
hai_sir_nona <- hai_sir  %>% 
  filter(!is.na(score)) %>%
  select(provider_id, Measure) %>%
  group_by(provider_id)


i <- ggplot(data = hai_sir_nona, aes(Measure)) + geom_bar(fill = "red")
i <- i + xlab("Quality Measure") 
i <- i + ylab("Count of Reporting Providers")
i <- i + labs(title = "Distribution of Reporing Providers With Non-Null Scores Measure", caption = "Based on CMS Hospital Compare data")

i

#j <- ggplot(data = hosp_gen_info_raw, aes(hospital_type)) + geom_bar(aes(fill = hosp_gen_info_raw$hospital_owner), position = "dodge")
j <- ggplot(data = hosp_gen_info_raw, aes(hospital_type)) + geom_bar()
j <- j + scale_fill_brewer(palette="Set1")
#j <- j + theme(axis.text.x=element_text(angle=45,hjust=1))
j <- j + labs(title = "Providers Reporting CMS Measures", caption = "Based on raw CMS Hospital Compare data")
j <- j + guides(fill = guide_legend(title  = "Hospital Type"))
#j <- j + ylim(0,1000)
j <- j + facet_wrap("hosp_gen_info_raw$hospital_owner")
j

j <- ggplot(data = hosp_gen_info_raw, aes(hospital_type)) + geom_bar(aes(fill = hosp_gen_info_raw$hospital_owner), position = "dodge")
#j <- ggplot(data = hosp_gen_info_raw, aes(hospital_type)) + geom_bar()
j <- j + scale_fill_brewer(palette="Set1")
j <- j + theme(axis.text.x=element_text(angle=45,hjust=1))
j <- j + labs(title = "Providers Reporting CMS Measures", caption = "Based on raw CMS Hospital Compare data")
j <- j + guides(fill = guide_legend(title  = "Hospital Type"))
#j <- j + ylim(0,1000)
j <- j + facet_wrap("hosp_gen_info_raw$hospital_owner")
j

