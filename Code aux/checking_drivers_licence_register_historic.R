setwd("~/Simon/Projects/Current projects/randomly_generated_integrated_data")
source("~/Simon/Projects/Current projects/randomly_generated_integrated_data/Code main/generate_pub_fund_hosp_discharges_event.R")


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

