setwd("~/Simon/Projects/Current projects/randomly_generated_integrated_data")
source("~/Simon/Projects/Current projects/randomly_generated_integrated_data/Code main/generate_pub_fund_hosp_discharges_event.R")

hist(generated_data$snz_uid)
hist(generated_data$snz_moh_uid)
hist(generated_data$snz_acc_claim_uid)
hist(generated_data$snz_moh_evt_uid)
hist(generated_data$moh_evt_event_id_nbr)
barplot(table(generated_data$moh_evt_adm_src_code))
barplot(table(generated_data$moh_evt_adm_type_code))
barplot(table(generated_data$moh_evt_nz_res_code))
barplot(table(generated_data$moh_evt_birth_month_nbr))
hist(generated_data$moh_evt_birth_year_nbr)
barplot(table(generated_data$moh_evt_sex_snz_code))
barplot(table(generated_data$moh_evt_eth_priority_grp_code))
barplot(table(generated_data$moh_evt_ethnicity_1_code))
barplot(table(generated_data$moh_evt_ethnicity_2_code))
barplot(table(generated_data$moh_evt_dhb_dom_code))
barplot(table(generated_data$moh_evt_event_type_code))
barplot(table(generated_data$moh_evt_end_type_code))
barplot(table(generated_data$moh_evt_evst_date))
barplot(table(generated_data$moh_evt_even_date))
barplot(table(generated_data$moh_evt_local_id_nbr))
barplot(table(generated_data$moh_evt_facility_code))
barplot(table(generated_data$moh_evt_facility_type_code))
barplot(table(generated_data$moh_evt_hours_on_cpap_nbr))
barplot(table(generated_data$moh_evt_tot_icu_hours_nbr))
barplot(table(generated_data$moh_evt_tot_niv_hours_nbr))
barplot(table(generated_data$moh_evt_acc_flag_code))
barplot(table(generated_data$moh_evt_facility_xfer_to_code))
barplot(table(generated_data$moh_evt_facility_xfer_from_code))
barplot(table(generated_data$moh_evt_occupation_code))
barplot(table(generated_data$moh_evt_occupation_text))
barplot(table(generated_data$moh_evt_ethnic_snz_code))
barplot(table(generated_data$moh_evt_ethnic1_snz_code))
barplot(table(generated_data$moh_evt_ethnic2_snz_code))
barplot(table(generated_data$moh_evt_ethnic_grp1_snz_ind))
barplot(table(generated_data$moh_evt_ethnic_grp2_snz_ind))
barplot(table(generated_data$moh_evt_ethnic_grp3_snz_ind))
barplot(table(generated_data$moh_evt_ethnic_grp4_snz_ind))
barplot(table(generated_data$moh_evt_ethnic_grp5_snz_ind))
barplot(table(generated_data$moh_evt_ethnic_grp6_snz_ind))

barplot(table(generated_data$moh_evt_even_date - generated_data$moh_evt_evst_date))

# yes - people can have multiple hospital visits per year
generated_data %>%
  group_by(snz_uid, the_year) %>%
  summarise(num = n()) %>%
  group_by(num) %>%
  summarise(num_people = n())

# transfers
generated_data %>%
  group_by(
    moh_evt_end_type_code, 
    moh_evt_facility_xfer_from_code, 
    moh_evt_facility_code,
    moh_evt_facility_xfer_to_code
  ) %>%
  summarise(num = n())
# yes, same number of from & to
# however, some transfers are from the same facilitiy to itself.
