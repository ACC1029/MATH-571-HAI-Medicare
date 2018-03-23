install.packages("DBI")

library(sqldf)
library(RH2)

#-----missing data-----#

# 15 missing county_name
sqldf("
      select count(provider_id)
      from hosp_gen_info_reduced
      where county_name is null
      ")

# 152 missing meets_meaningful_use
sqldf("
      select count(provider_id)
      from hosp_gen_info_reduced
      where meets_meaningful_use is null
      ")

# 546 providers are missing all national comparisons
sqldf("
      select count(provider_id)
      from
      (
      select provider_id
      from hosp_gen_info_reduced
      where hospital_overall_rating like 'Not available'
      intersect
      select provider_id
      from hosp_gen_info_reduced
      where safety_of_care like 'Not available'
      intersect
      select provider_id
      from hosp_gen_info_reduced
      where patient_experience like 'Not available'
      intersect
      select provider_id
      from hosp_gen_info_reduced
      where timeliness_of_care like 'Not available'
      intersect
      select provider_id
      from hosp_gen_info_reduced
      where mortality like 'Not available'
      intersect
      select provider_id
      from hosp_gen_info_reduced
      where readmission like 'Not available'
      intersect
      select provider_id
      from hosp_gen_info_reduced
      where effectiveness_of_care like 'Not available'
      intersect
      select provider_id
      from hosp_gen_info_reduced
      where efficient_use_of_medical_imaging like 'Not available'
      )
      ")

# 1114 missing hospital_overall_rating
sqldf("
      select count(provider_id)
      from hosp_gen_info_reduced
      where hospital_overall_rating like 'Not available'
      ")

# 2162 missing safety_of_care
sqldf("
      select count(provider_id)
      from hosp_gen_info_reduced
      where safety_of_care like 'Not available'
      ")

# 1321 missing patient_experience
sqldf("
      select count(provider_id)
      from hosp_gen_info_reduced
      where patient_experience like 'Not available'
      ")

# 1128 missing patient_experience
sqldf("
      select count(provider_id)
      from hosp_gen_info_reduced
      where timeliness_of_care like 'Not available'
      ")

# 1362 missing mortality
sqldf("
      select count(provider_id)
      from hosp_gen_info_reduced
      where mortality like 'Not available'
      ")

# 914 missing readmission
sqldf("
      select count(provider_id)
      from hosp_gen_info_reduced
      where readmission like 'Not available'
      ")

# 1067 missing effectiveness_of_care
sqldf("
      select count(provider_id)
      from hosp_gen_info_reduced
      where effectiveness_of_care like 'Not available'
      ")

# 1883 missing efficient_use_of_medical_imaging
sqldf("
      select count(provider_id)
      from hosp_gen_info_reduced
      where efficient_use_of_medical_imaging like 'Not available'
      ")



library(DBI)

mydb <- dbConnect(RSQLite::SQLite(), "")

dbWriteTable(mydb, "hosp_gen_info_reduced", hosp_gen_info_reduced)
dbWriteTable(mydb, "hai_reduced", hai_reduced)
dbWriteTable(mydb, "mspb_reduced", mspb_reduced)
dbWriteTable(mydb, "pay_val_care_reduced", pay_val_care_reduced)

dbListTables(mydb)

dbDisconnect(mydb)