setwd("~/Simon/Projects/Current projects/randomly_generated_integrated_data")
source("~/Simon/Projects/Current projects/randomly_generated_integrated_data/Code main/generate_pub_fund_hosp_discharges_event.R")

## historic ------------------------------------------------------

hist(generated_historic$snz_uid)
hist(generated_historic$snz_nzta_uid)
barplot(table(generated_historic$nzta_snz_sex_code))
hist(generated_historic$nzta_hist_birth_month_nbr)
hist(generated_historic$nzta_hist_birth_year_nbr)
barplot(table(generated_historic$nzta_hist_licence_type_text))
barplot(table(generated_historic$nzta_hist_licence_start_date))
barplot(table(generated_historic$nzta_hist_licence_class_text))
barplot(table(generated_historic$nzta_hist_learner_start_date))
barplot(table(generated_historic$nzta_hist_restricted_start_date))
barplot(table(generated_historic$nzta_hist_full_start_date))

hist(as.numeric(generated_historic$nzta_hist_full_start_date - generated_historic$nzta_hist_restricted_start_date))
hist(as.numeric(generated_historic$nzta_hist_restricted_start_date - generated_historic$nzta_hist_learner_start_date))
                                          
barplot(table(is.na(generated_historic$snz_uid)))
barplot(table(is.na(generated_historic$snz_nzta_uid)))
barplot(table(is.na(generated_historic$nzta_snz_sex_code)))
barplot(table(is.na(generated_historic$nzta_hist_birth_month_nbr)))
barplot(table(is.na(generated_historic$nzta_hist_birth_year_nbr)))
barplot(table(is.na(generated_historic$nzta_hist_licence_type_text)))
barplot(table(is.na(generated_historic$nzta_hist_licence_start_date)))
barplot(table(is.na(generated_historic$nzta_hist_licence_class_text)))
barplot(table(is.na(generated_historic$nzta_hist_learner_start_date)))
barplot(table(is.na(generated_historic$nzta_hist_restricted_start_date)))
barplot(table(is.na(generated_historic$nzta_hist_full_start_date)))

qq = generated_historic %>%
  filter(is.na(nzta_hist_licence_start_date))

## current ------------------------------------------------------

# misc demographic
hist(gg$snz_uid)
barplot(table(gg$snz_sex_gender_code))
hist(gg$snz_birth_year_nbr)
hist(gg$snz_birth_month_nbr)
hist(gg$snz_deceased_year_nbr)
hist(gg$snz_nzta_uid)

# licence
barplot(table(gg$licence_type))
barplot(table(year(gg$nzta_dlr_licence_issue_date)))
barplot(table(gg$nzta_dlr_licence_stage_text))
barplot(table(gg$nzta_dlr_licence_status_text))
barplot(table(gg$year_latest_licence_issue))
barplot(table(year(gg$nzta_dlr_licence_from_date)))

# class
barplot(table(gg$nzta_dlr_class_status_text))
barplot(table(year(gg$nzta_dlr_lic_class_grant_date)))
barplot(table(gg$years_latest_class_issue))
barplot(table(year(gg$nzta_dlr_class_from_date)))
table(is.na(gg$nzta_dlr_class_status_text))
table(is.na(gg$nzta_dlr_lic_class_grant_date))
table(is.na(gg$years_latest_class_issue))
table(is.na(gg$nzta_dlr_class_from_date))

# combination
barplot(table(gg$nzta_dlr_licence_class_text))
table(gg$nzta_dlr_licence_class_text)
table(gg$nzta_dlr_licence_class_text, gg$nzta_dlr_licence_status_text)


# randomly generated
barplot(table(gg$nzta_dlr_organ_donor_ind))
barplot(table(gg$nzta_dlr_region_code))
barplot(table(gg$nzta_dlr_endorsement_type_text))
table(is.na(gg$nzta_dlr_endorsement_type_text))




barplot(table(year(gg$nzta_dlr_licence_start_date)))


