# Schema: moh_clean
# Table: pub_fund_hosp_discharges_event
#
# Output columns:
#
# snz_uid
# snz_moh_uid
# snz_acc_claim_uid
# snz_moh_evt_uid
# moh_evt_event_id_nbr
# moh_evt_adm_src_code
# moh_evt_adm_type_code
# moh_evt_nz_res_code
# moh_evt_birth_month_nbr
# moh_evt_birth_year_nbr
# moh_evt_sex_snz_code
# moh_evt_eth_priority_grp_code
# moh_evt_ethnicity_1_code
# moh_evt_ethnicity_2_code
# moh_evt_ethnicity_3_code
# moh_evt_dhb_dom_code
# moh_evt_event_type_code
# moh_evt_end_type_code
# moh_evt_evst_date
# moh_evt_even_date
# moh_evt_local_id_nbr
# moh_evt_facility_code
# moh_evt_facility_type_code
# moh_evt_hours_on_cpap_nbr
# moh_evt_tot_icu_hours_nbr
# moh_evt_tot_niv_hours_nbr
# moh_evt_acc_flag_code
# moh_evt_facility_xfer_to_code
# moh_evt_facility_xfer_from_code
# moh_evt_occupation_code
# moh_evt_occupation_text
# moh_evt_ethnic_snz_code
# moh_evt_ethnic1_snz_code
# moh_evt_ethnic2_snz_code
# moh_evt_ethnic3_snz_code
# moh_evt_ethnic_grp1_snz_ind
# moh_evt_ethnic_grp2_snz_ind
# moh_evt_ethnic_grp3_snz_ind
# moh_evt_ethnic_grp4_snz_ind
# moh_evt_ethnic_grp5_snz_ind
# moh_evt_ethnic_grp6_snz_ind
#

# WARNING - SLOW - RUNTIME

## parameters -----------------------------------------------------------------

# controls
SEED = 123

# we recommend num hospital facilities to be 1/1000th - 1/5000th of num people
NUM_FACILITY = 3

# input/output
LOOKUP_FILE = "./Lookups/[moh_clean]_[pub_fund_hosp_discharges_event].csv"
REFERENCE_FILE = "./Reference/residence.csv"
DEMOGRAPHICS_FILE = "./Generated/[data]_[personal_detail].csv"
GENERATED_FILE = "./Generated/[moh_clean]_[pub_fund_hosp_discharges_event].csv"

# attributes
MIN_YEAR = 2002
MAX_YEAR = 2019

PROB_HOSP = 0.01
PROB_HOSP_AGE12 = 0.015
PROB_HOSP_AGE55 = 0.015
PROB_HOSP_AGE65 = 0.02
PROB_HOSP_AGE70 = 0.025
PROB_HOSP_AGE75 = 0.03

IS_ACC_PROB = 0.1

CPAP_PROB = 0.06
CPAP_MIN = 6
CPAP_MAX = 18
ICU_PROB = 0.09
ICU_MIN = 4
ICU_MAX = 24
NIV_PROB = 0.3
NIV_MIN = 1
NIV_MAX = 17

PROB_TRANSFER = 0.05

## setup ----------------------------------------------------------------------

library(dplyr)
library(lubridate)
source("./Code main/support_functions.R")
set.seed(seed = SEED)

lookup = read.csv(LOOKUP_FILE, stringsAsFactors = FALSE) %>%
  filter(schema == "moh_clean", table == "pub_fund_hosp_discharges_event")

base_population = read.csv(REFERENCE_FILE, stringsAsFactors = FALSE) %>%
  filter(MIN_YEAR <= the_year & the_year <= MAX_YEAR & is_resident == 1)

demographics = read.csv(DEMOGRAPHICS_FILE, stringsAsFactors = FALSE)

## generate events ------------------------------------------------------------

base_population = base_population %>%
  mutate(
    age = the_year - snz_birth_year_nbr,
    hosp_prob = case_when(
      age <= 12 ~ PROB_HOSP_AGE12,
      age >= 75 ~ PROB_HOSP_AGE75,
      age >= 70 ~ PROB_HOSP_AGE70,
      age >= 65 ~ PROB_HOSP_AGE65,
      age >= 55 ~ PROB_HOSP_AGE55,
      TRUE ~ PROB_HOSP
    )
  )

generated_data = data.frame(stringsAsFactors = FALSE)

while(nrow(base_population) > 0){
  pop_size = nrow(base_population)
  base_population = base_population %>%
    mutate(r1 = runif(pop_size)) %>%
    filter(r1 <= hosp_prob)
  
  generated_data = rbind(generated_data, base_population)
}

## add demographic information ------------------------------------------------

generated_data = generated_data %>%
  left_join(demographics, by = "snz_uid") %>%
  mutate(
    e1 = ifelse(snz_ethnicity_grp1_nbr == 1, 1, NA),
    e2 = ifelse(snz_ethnicity_grp2_nbr == 1, 2, NA),
    e3 = ifelse(snz_ethnicity_grp3_nbr == 1, 3, NA),
    e4 = ifelse(snz_ethnicity_grp4_nbr == 1, 4, NA),
    e5 = ifelse(snz_ethnicity_grp5_nbr == 1, 5, NA),
    e6 = ifelse(snz_ethnicity_grp6_nbr == 1, 6, NA),
    moh_evt_ethnic1_snz_code = pmin(e1, e2, e3, e4, e5, e6, na.rm = TRUE),
    moh_evt_ethnic2_snz_code = pmax(e1, e2, e3, e4, e5, e6, na.rm = TRUE),
  ) %>%
  rename(
    moh_evt_nz_res_code = is_resident,
    moh_evt_birth_month_nbr = snz_birth_month_nbr,
    moh_evt_birth_year_nbr = snz_birth_year_nbr.x,
    moh_evt_sex_snz_code = snz_sex_gender_code,
    moh_evt_ethnic_grp1_snz_ind = snz_ethnicity_grp1_nbr,
    moh_evt_ethnic_grp2_snz_ind = snz_ethnicity_grp2_nbr,
    moh_evt_ethnic_grp3_snz_ind = snz_ethnicity_grp3_nbr,
    moh_evt_ethnic_grp4_snz_ind = snz_ethnicity_grp4_nbr,
    moh_evt_ethnic_grp5_snz_ind = snz_ethnicity_grp5_nbr,
    moh_evt_ethnic_grp6_snz_ind = snz_ethnicity_grp6_nbr
  ) %>%
  mutate(
    moh_evt_ethnic_snz_code = ifelse(
      moh_evt_ethnic1_snz_code == moh_evt_ethnic2_snz_code,
      moh_evt_ethnic1_snz_code,
      paste0(moh_evt_ethnic1_snz_code, ";", moh_evt_ethnic2_snz_code)
    ),
    moh_evt_ethnic2_snz_code = ifelse(
      moh_evt_ethnic1_snz_code == moh_evt_ethnic2_snz_code,
      NA,
      moh_evt_ethnic2_snz_code
    ),
    moh_evt_ethnicity_1_code = 10 * moh_evt_ethnic1_snz_code + 1,
    moh_evt_ethnicity_2_code = 10 * moh_evt_ethnic2_snz_code + 1,
    moh_evt_eth_priority_grp_code = case_when(
      e2 == 2 ~ 21,
      e3 == 3 ~ 31,
      e4 == 4 ~ 41,
      e5 == 5 ~ 51,
      e1 == 1 ~ 11,
      e6 == 6 ~ 61
    )
  ) %>%
  select(-e1, -e2, -e3, -e4, -e5, -e6, -age,
         -hosp_prob, -r1, -snz_birth_year_nbr.y,
         -snz_parent1_uid, -snz_parent2_uid, -snz_deceased_year_nbr.y)

# complete at this point
#
# snz_uid
# moh_evt_nz_res_code
# moh_evt_birth_month_nbr
# moh_evt_birth_year_nbr
# moh_evt_sex_snz_code
# moh_evt_eth_priority_grp_code
# moh_evt_ethnicity_1_code
# moh_evt_ethnicity_2_code
# moh_evt_ethnic_snz_code
# moh_evt_ethnic1_snz_code
# moh_evt_ethnic2_snz_code
# moh_evt_ethnic_grp1_snz_ind
# moh_evt_ethnic_grp2_snz_ind
# moh_evt_ethnic_grp3_snz_ind
# moh_evt_ethnic_grp4_snz_ind
# moh_evt_ethnic_grp5_snz_ind
# moh_evt_ethnic_grp6_snz_ind

## event timings and changes --------------------------------------------------

table_size = nrow(generated_data)

generated_data = generated_data %>%
  # facility
  mutate(
    moh_evt_facility_code = randbetween(1, NUM_FACILITY, table_size),
    next_facility = randbetween(1, NUM_FACILITY, table_size)
  ) %>%
  # timing
  mutate(
    month = randbetween(1, 12, table_size),
    day = randbetween(1, 28, table_size),
    r1 = randbetween(1, 8, table_size),
    r2 = randbetween(1, 9, table_size),
    days_stay = pmin(r1, r2),
    death = ifelse(
      the_year == snz_deceased_year_nbr.x & snz_deceased_month_nbr >= month,
      1, 0
    )
  ) %>%
  # start and end
  mutate(
    moh_evt_evst_date = ymd(paste0(the_year,"-",month,"-", day)),
    moh_evt_even_date = moh_evt_evst_date + days_stay
  ) %>%
  # exit type
  mutate(
    r1 = runif(table_size),
    moh_evt_end_type_code = case_when(
      death == 1 ~ "M",
      r1 < PROB_TRANSFER ~ "T",
      TRUE ~ "D"
    ),
    moh_evt_facility_xfer_to_code = ifelse(moh_evt_end_type_code == "T", next_facility, NA),
    moh_evt_facility_xfer_from_code = NA
  ) %>%
  select(-r1, -r2, -days_stay, -death, -day, -month, -the_year,
         snz_deceased_year_nbr.x, next_facility)

# transfers
gen_transfer = generated_data %>%
  filter(moh_evt_end_type_code == "T")

transfer_size = nrow(gen_transfer)

gen_transfer = gen_transfer %>%
  # facility
  mutate(
    moh_evt_facility_xfer_from_code = moh_evt_facility_code,
    moh_evt_facility_code = moh_evt_facility_xfer_to_code,
    moh_evt_facility_xfer_to_code = NA
  ) %>%
  # timing
  mutate(
    r1 = randbetween(1, 8, transfer_size),
    r2 = randbetween(1, 9, transfer_size),
    days_stay = pmin(r1, r2)
  ) %>%
  # start and end
  mutate(
    moh_evt_evst_date = moh_evt_even_date,
    moh_evt_even_date = moh_evt_evst_date + days_stay
  ) %>%
  # exit type
  mutate(moh_evt_end_type_code = "D") %>%
  select(-r1, -r2, -days_stay)

generated_data = bind_rows(generated_data, gen_transfer)

# complete at this point
#
# moh_evt_end_type_code
# moh_evt_evst_date
# moh_evt_even_date
# moh_evt_facility_code
# moh_evt_facility_xfer_to_code
# moh_evt_facility_xfer_from_code

## ACC and other uid's --------------------------------------------------------

table_size = nrow(generated_data)

generated_data = generated_data %>%
  mutate(
    event_id = randbetween(table_size, 10 * table_size, table_size),
    r1 = runif(table_size),
    moh_evt_acc_flag_code = ifelse(r1 < IS_ACC_PROB, 1, 0),
    snz_acc_claim_uid = ifelse(r1 < IS_ACC_PROB, event_id, NA)
  )


# moh_uid
rr = 0.1 + 0.8 * runif(1)
max_id = max(generated_data$snz_uid) + 1113
shift = floor(rr * max_id)

generated_data = generated_data %>%
  mutate(snz_moh_uid = (snz_uid + shift) %% max_id)

# event uids
generated_data = generated_data %>%
  mutate(
    snz_moh_evt_uid = floor(row_number() * 1.9) + 1131,
    moh_evt_event_id_nbr = floor(row_number() * 1.6) + 3111
  ) %>%
  group_by(moh_evt_facility_code) %>%
  mutate(
    moh_evt_local_id_nbr = floor(row_number() * 2.3) + 1311
  ) %>%
  ungroup()

# complete at this point
#
# snz_moh_uid
# snz_acc_claim_uid
# moh_evt_acc_flag_code
# snz_moh_evt_uid
# moh_evt_event_id_nbr
# moh_evt_local_id_nbr

## Hours of specific care types -----------------------------------------------

table_size = nrow(generated_data)

generated_data = generated_data %>%
  mutate(
    r1 = runif(table_size),
    on_cpap = ifelse(r1 < CPAP_PROB, 1, 0),
    r2 = runif(table_size),
    in_icu = ifelse(r2 < ICU_PROB, 1, 0),
    r3 = runif(table_size),
    on_niv = ifelse(r3 < NIV_PROB, 1, 0)
  ) %>%
  mutate(
    r1 = randbetween(CPAP_MIN, CPAP_MAX, table_size),
    r2 = randbetween(ICU_MIN, ICU_MAX, table_size),
    r3 = randbetween(NIV_MIN, NIV_MAX, table_size),
  ) %>%
  mutate(
    moh_evt_hours_on_cpap_nbr = ifelse(on_cpap, r1, NA),
    moh_evt_tot_icu_hours_nbr = ifelse(in_icu, r2, NA),
    moh_evt_tot_niv_hours_nbr = ifelse(on_niv, r3, NA)
  ) %>%
  select(-r1, -r2, -r3, -on_cpap, -in_icu, -on_niv)


# complete at this point
#
# moh_evt_hours_on_cpap_nbr
# moh_evt_tot_icu_hours_nbr
# moh_evt_tot_niv_hours_nbr

## everything to generate with sampler lookup ---------------------------------

table_size = nrow(generated_data)

sampler = lookup %>%
  filter(column == "moh_evt_occupation_code") %>%
  make_weighted_options_array()
generated_data = generated_data %>%
  mutate(moh_evt_occupation_code = sample(sampler, table_size, replace = TRUE))

sampler = lookup %>%
  filter(column == "moh_evt_occupation_text") %>%
  make_weighted_options_array()
generated_data = generated_data %>%
  mutate(moh_evt_occupation_text = sample(sampler, table_size, replace = TRUE))

sampler = lookup %>%
  filter(column == "moh_evt_dhb_dom_code") %>%
  make_weighted_options_array()
generated_data = generated_data %>%
  mutate(moh_evt_dhb_dom_code = sample(sampler, table_size, replace = TRUE))

sampler = lookup %>%
  filter(column == "moh_evt_event_type_code") %>%
  make_weighted_options_array()
generated_data = generated_data %>%
  mutate(moh_evt_event_type_code = sample(sampler, table_size, replace = TRUE))

sampler = lookup %>%
  filter(column == "moh_evt_facility_type_code") %>%
  make_weighted_options_array()
generated_data = generated_data %>%
  mutate(moh_evt_facility_type_code = sample(sampler, table_size, replace = TRUE))

sampler = lookup %>%
  filter(column == "moh_evt_adm_src_code") %>%
  make_weighted_options_array()
generated_data = generated_data %>%
  mutate(moh_evt_adm_src_code = sample(sampler, table_size, replace = TRUE))

sampler = lookup %>%
  filter(column == "moh_evt_adm_type_code") %>%
  make_weighted_options_array()
generated_data = generated_data %>%
  mutate(moh_evt_adm_type_code = sample(sampler, table_size, replace = TRUE))

# completed at this point
#
# moh_evt_occupation_code
# moh_evt_occupation_text
# moh_evt_dhb_dom_code
# moh_evt_event_type_code
# moh_evt_facility_type_code
# moh_evt_adm_src_code
# moh_evt_adm_type_code

## write out table ------------------------------------------------------------

generated_data %>%
  select(
    snz_uid,
    snz_moh_uid,
    snz_acc_claim_uid,
    snz_moh_evt_uid,
    moh_evt_event_id_nbr,
    moh_evt_adm_src_code,
    moh_evt_adm_type_code,
    moh_evt_nz_res_code,
    moh_evt_birth_month_nbr,
    moh_evt_birth_year_nbr,
    moh_evt_sex_snz_code,
    moh_evt_eth_priority_grp_code,
    moh_evt_ethnicity_1_code,
    moh_evt_ethnicity_2_code,
    moh_evt_dhb_dom_code,
    moh_evt_event_type_code,
    moh_evt_end_type_code,
    moh_evt_evst_date,
    moh_evt_even_date,
    moh_evt_local_id_nbr,
    moh_evt_facility_code,
    moh_evt_facility_type_code,
    moh_evt_hours_on_cpap_nbr,
    moh_evt_tot_icu_hours_nbr,
    moh_evt_tot_niv_hours_nbr,
    moh_evt_acc_flag_code,
    moh_evt_facility_xfer_to_code,
    moh_evt_facility_xfer_from_code,
    moh_evt_occupation_code,
    moh_evt_occupation_text,
    moh_evt_ethnic_snz_code,
    moh_evt_ethnic1_snz_code,
    moh_evt_ethnic2_snz_code,
    moh_evt_ethnic_grp1_snz_ind,
    moh_evt_ethnic_grp2_snz_ind,
    moh_evt_ethnic_grp3_snz_ind,
    moh_evt_ethnic_grp4_snz_ind,
    moh_evt_ethnic_grp5_snz_ind,
    moh_evt_ethnic_grp6_snz_ind
  ) %>%
  write.csv(GENERATED_FILE, row.names = FALSE)
