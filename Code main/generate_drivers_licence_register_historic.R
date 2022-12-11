# Schema: nzta_clean
# Tables: dlr_historic & drivers_licence_register
#
# Output columns:
#
# [dlr_historic]
# 
# snz_uid
# snz_nzta_uid
# nzta_snz_sex_code
# nzta_hist_birth_month_nbr
# nzta_hist_birth_year_nbr
# nzta_hist_licence_type_text
# nzta_hist_licence_start_date
# nzta_hist_licence_class_text
# nzta_hist_learner_start_date
# nzta_hist_restricted_start_date
# nzta_hist_full_start_date
# 
#
# [drivers_licence_register]
# 
# snz_uid
# snz_nzta_uid
# nzta_snz_sex_code
# nzta_dlr_birth_month_nbr
# nzta_dlr_birth_year_nbr
# nzta_dlr_licence_issue_date
# nzta_dlr_organ_donor_ind
# nzta_dlr_licence_type_text
# nzta_dlr_licence_status_text
# nzta_dlr_lic_class_grant_date
# nzta_dlr_lic_class_start_date
# nzta_dlr_licence_from_date
# nzta_dlr_class_status_text
# nzta_dlr_licence_class_text
# nzta_dlr_licence_stage_text
# nzta_dlr_class_from_date
# nzta_dlr_endorsement_type_text
# nzta_dlr_endorsement_start_date
# nzta_dlr_endorsement_expiry_date
# nzta_dlr_endorsement_status_text
# nzta_dlr_card_required_ind
# nzta_dlr_endorsement_from_date
# nzta_dlr_region_code
#

## parameters -----------------------------------------------------------------

# controls
SEED = 123

# input/output
LOOKUP_FILE = "./Lookups/[nzta_clean]_[drivers_licence_register_historic].csv"
REFERENCE_FILE = "./Reference/residence.csv"
PERSONAL_DETAILS = "./Generated/[data]_[personal_detail].csv"

GENERATED_FILE1 = "./Generated/[nzta_clean]_[dlr_historic].csv"
GENERATED_FILE2 = "./Generated/[nzta_clean]_[drivers_licence_register].csv"

# attributes
MAX_YEAR = 2020
MIN_YEAR = 1977

NO_LICENCE_MIN_DAYS = 0
NO_LICENCE_MAX_DAYS = 365 * 5
LEARNER_MIN_DAYS = 366
LEARNER_MAX_DAYS = 365 * 6
RESTRICTED_MIN_DAYS = 365
RESTRICTED_MAX_DAYS = 365 * 8
FULL_LOST_MIN_DAYS = 50
FULL_LOST_MAX_DAYS = 365 * 50

# class 1
C1_PROB_LEARNER = 0.97
C1_PROB_LEARNER_LOST = 0.01
C1_PROB_RESTRICTED = 0.96
C1_PROB_RESTRICTED_LOST = 0.02
C1_PROB_FULL = 0.94
C1_PROB_FULL_LOST = 0.03

# class 6
C6_PROB_LEARNER = 0.17
C6_PROB_LEARNER_LOST = 0.015
C6_PROB_RESTRICTED = 0.9
C6_PROB_RESTRICTED_LOST = 0.025
C6_PROB_FULL = 0.85
C6_PROB_FULL_LOST = 0.035

# class 2, 3, 4, & 5
C2_PROB_FULL = 0.09
C2_PROB_FULL_LOST = 0.25
C3_PROB_FULL = 0.60
C3_PROB_FULL_LOST = 0.22
C4_PROB_FULL = 0.55
C4_PROB_FULL_LOST = 0.2
C5_PROB_FULL = 0.45
C5_PROB_FULL_LOST = 0.2
C2_MIN_GAP = 90
C2_MAX_GAP = 365 * 5
C345_MIN_GAP = 15
C345_MAX_GAP = 365 * 8

## setup ----------------------------------------------------------------------

library(dplyr)
library(lubridate)
source("./Code main/support_functions.R")
set.seed(seed = SEED)

base_population = read.csv(REFERENCE_FILE, stringsAsFactors = FALSE)
personal_details = read.csv(PERSONAL_DETAILS, stringsAsFactors = FALSE)


## create base population -----------------------------------------------------

base_population = base_population %>%
  group_by(snz_uid) %>%
  summarise(
    num_res = sum(is_resident),
    num_non = sum(1 - is_resident),
    .groups = "drop"
  ) %>%
  mutate(any_res = num_res > 0, any_non = num_non > 0) %>%
  left_join(personal_details, by = "snz_uid") %>%
  select(snz_uid, snz_sex_gender_code, snz_birth_year_nbr,
         snz_birth_month_nbr, snz_deceased_year_nbr, any_res, any_non) %>%
  mutate(
    bday16 = paste0(snz_birth_year_nbr + 16,"-", snz_birth_month_nbr, "-15"),
    bday16 = lubridate::ymd(bday16)
  )

# snz_nzta_uid
rr = 0.1 + 0.8 * runif(1)
max_id = max(base_population$snz_uid) + 4192
shift = floor(rr * max_id)

base_population = base_population %>%
  mutate(snz_nzta_uid = (snz_uid + shift) %% max_id)

## generate historic ----------------------------------------------------------

table_size = nrow(base_population)

generated_base = base_population %>%
  # class 1
  mutate(
    c1_learner = runif(table_size) < C1_PROB_LEARNER,
    c1_learner_lost = runif(table_size) < C1_PROB_LEARNER_LOST,
    c1_restricted = runif(table_size) < C1_PROB_RESTRICTED,
    c1_restricted_lost = runif(table_size) < C1_PROB_RESTRICTED_LOST,
    c1_full = runif(table_size) < C1_PROB_FULL,
    c1_full_lost = runif(table_size) < C1_PROB_FULL_LOST,
    no_licence_days1 = randbetween(NO_LICENCE_MIN_DAYS, NO_LICENCE_MAX_DAYS, table_size),
    no_licence_days2 = randbetween(NO_LICENCE_MIN_DAYS, NO_LICENCE_MAX_DAYS, table_size),
    licence_days1 = randbetween(LEARNER_MIN_DAYS, LEARNER_MAX_DAYS, table_size),
    licence_days2 = randbetween(LEARNER_MIN_DAYS, LEARNER_MAX_DAYS, table_size),
    restricted_days1 = randbetween(RESTRICTED_MIN_DAYS, RESTRICTED_MAX_DAYS, table_size),
    restricted_days2 = randbetween(RESTRICTED_MIN_DAYS, RESTRICTED_MAX_DAYS, table_size),
    full_days1 = randbetween(FULL_LOST_MIN_DAYS, FULL_LOST_MAX_DAYS, table_size),
    full_days2 = randbetween(FULL_LOST_MIN_DAYS, FULL_LOST_MAX_DAYS, table_size)
  ) %>%
  mutate(
    c1_learner_start_date = bday16 + pmin(no_licence_days1, no_licence_days2),
    c1_restricted_start_date = c1_learner_start_date + pmin(licence_days1, licence_days2),
    c1_full_start_date = c1_restricted_start_date + pmin(restricted_days1, restricted_days2),
    c1_end = c1_full_start_date + pmin(full_days1, full_days2)
  ) %>%
  # class 6
  mutate(
    c6_learner = runif(table_size) < C6_PROB_LEARNER,
    c6_learner_lost = runif(table_size) < C6_PROB_LEARNER_LOST,
    c6_restricted = runif(table_size) < C6_PROB_RESTRICTED,
    c6_restricted_lost = runif(table_size) < C6_PROB_RESTRICTED_LOST,
    c6_full = runif(table_size) < C6_PROB_FULL,
    c6_full_lost = runif(table_size) < C6_PROB_FULL_LOST,
    no_licence_days1 = randbetween(NO_LICENCE_MIN_DAYS, NO_LICENCE_MAX_DAYS, table_size),
    no_licence_days2 = randbetween(NO_LICENCE_MIN_DAYS, NO_LICENCE_MAX_DAYS, table_size),
    licence_days1 = randbetween(LEARNER_MIN_DAYS, LEARNER_MAX_DAYS, table_size),
    licence_days2 = randbetween(LEARNER_MIN_DAYS, LEARNER_MAX_DAYS, table_size),
    restricted_days1 = randbetween(RESTRICTED_MIN_DAYS, RESTRICTED_MAX_DAYS, table_size),
    restricted_days2 = randbetween(RESTRICTED_MIN_DAYS, RESTRICTED_MAX_DAYS, table_size),
    full_days1 = randbetween(FULL_LOST_MIN_DAYS, FULL_LOST_MAX_DAYS, table_size),
    full_days2 = randbetween(FULL_LOST_MIN_DAYS, FULL_LOST_MAX_DAYS, table_size)
  ) %>%
  mutate(
    c6_learner_start_date = bday16 + pmin(no_licence_days1, no_licence_days2),
    c6_restricted_start_date = c6_learner_start_date + pmin(licence_days1, licence_days2),
    c6_full_start_date = c6_restricted_start_date + pmin(restricted_days1, restricted_days2),
    c6_end = c6_full_start_date + pmin(full_days1, full_days2)
  ) %>%
  # class 2
  mutate(
    c2_licence = runif(table_size) < C2_PROB_FULL,
    c2_licence_lost = runif(table_size) < C2_PROB_FULL_LOST,
    days_gap1 = randbetween(C2_MIN_GAP, C_MAX_GAP, table_size),
    days_gap2 = randbetween(C2_MIN_GAP, C_MAX_GAP, table_size),
    full_days1 = randbetween(FULL_LOST_MIN_DAYS, FULL_LOST_MAX_DAYS, table_size),
    full_days2 = randbetween(FULL_LOST_MIN_DAYS, FULL_LOST_MAX_DAYS, table_size)
  ) %>%
  mutate(
    c2_start_date = c1_full_start_date + pmin(days_gap1, days_gap2),
    c2_end = c2_start_date + pmin(full_days1, full_days2)
  ) %>%
  # class 3
  mutate(
    c3_licence = runif(table_size) < C3_PROB_FULL,
    c3_licence_lost = runif(table_size) < C3_PROB_FULL_LOST,
    days_gap1 = randbetween(C345_MIN_GAP, C345_MAX_GAP, table_size),
    days_gap2 = randbetween(C345_MIN_GAP, C345_MAX_GAP, table_size),
    full_days1 = randbetween(FULL_LOST_MIN_DAYS, FULL_LOST_MAX_DAYS, table_size),
    full_days2 = randbetween(FULL_LOST_MIN_DAYS, FULL_LOST_MAX_DAYS, table_size)
  ) %>%
  mutate(
    c3_start_date = c2_start_date + pmin(days_gap1, days_gap2),
    c3_end = c3_start_date + pmin(full_days1, full_days2)
  ) %>%
  # class 4
  mutate(
    c4_licence = runif(table_size) < C4_PROB_FULL,
    c4_licence_lost = runif(table_size) < C4_PROB_FULL_LOST,
    days_gap1 = randbetween(C345_MIN_GAP, C345_MAX_GAP, table_size),
    days_gap2 = randbetween(C345_MIN_GAP, C345_MAX_GAP, table_size),
    full_days1 = randbetween(FULL_LOST_MIN_DAYS, FULL_LOST_MAX_DAYS, table_size),
    full_days2 = randbetween(FULL_LOST_MIN_DAYS, FULL_LOST_MAX_DAYS, table_size)
  ) %>%
  mutate(
    c4_start_date = c3_start_date + pmin(days_gap1, days_gap2),
    c4_end = c4_start_date + pmin(full_days1, full_days2)
  ) %>%
  # class 5
  mutate(
    c5_licence = runif(table_size) < C5_PROB_FULL,
    c5_licence_lost = runif(table_size) < C5_PROB_FULL_LOST,
    days_gap1 = randbetween(C345_MIN_GAP, C345_MAX_GAP, table_size),
    days_gap2 = randbetween(C345_MIN_GAP, C345_MAX_GAP, table_size),
    full_days1 = randbetween(FULL_LOST_MIN_DAYS, FULL_LOST_MAX_DAYS, table_size),
    full_days2 = randbetween(FULL_LOST_MIN_DAYS, FULL_LOST_MAX_DAYS, table_size)
  ) %>%
  mutate(
    c5_start_date = c4_start_date + pmin(days_gap1, days_gap2),
    c5_end = c5_start_date + pmin(full_days1, full_days2)
  ) %>%
  # tidy
  select(-bday16, -no_licence_days1, -no_licence_days2, -licence_days1, 
         -licence_days2, -restricted_days1, -restricted_days2, -full_days1, 
         -full_days2, -days_gap1, -days_gap2)

## complete historic generation -----------------------------------------------

# licence type
lookup = read.csv(LOOKUP_FILE, stringsAsFactors = FALSE) %>%
  filter(schema == "nzta_clean", table == "dlr_historic")

sampler = lookup %>%
  filter(column == "nzta_hist_licence_type_text") %>%
  make_weighted_options_array()

generated_historic = generated_base %>%
  mutate(licence_type = sample(sampler, table_size, replace = TRUE)) %>%
  mutate(licence_type = ifelse(!any_non & licence_type == "DIPLOMATIC", "PERMANENT", licence_type))

# no future licences an no licence after death
generated_historic = generated_historic %>%
  mutate(
    c1_learner_start_date = if_else(year(c1_learner_start_date) > MAX_YEAR | year(c1_learner_start_date) < MIN_YEAR | year(c1_learner_start_date) > snz_deceased_year_nbr, as.Date(NA), c1_learner_start_date),
    c1_restricted_start_date = if_else(year(c1_restricted_start_date) > MAX_YEAR | year(c1_restricted_start_date) < MIN_YEAR | year(c1_restricted_start_date) > snz_deceased_year_nbr, as.Date(NA), c1_restricted_start_date),
    c1_full_start_date = if_else(year(c1_full_start_date) > MAX_YEAR | year(c1_full_start_date) < MIN_YEAR | year(c1_full_start_date) > snz_deceased_year_nbr, as.Date(NA), c1_full_start_date),
    c1_end = if_else(year(c1_end) > MAX_YEAR | year(c1_end) < MIN_YEAR | year(c1_end) > snz_deceased_year_nbr, as.Date(NA), c1_end),
    c6_learner_start_date = if_else(year(c6_learner_start_date) > MAX_YEAR | year(c6_learner_start_date) < MIN_YEAR | year(c6_learner_start_date) > snz_deceased_year_nbr, as.Date(NA), c6_learner_start_date),
    c6_restricted_start_date = if_else(year(c6_restricted_start_date) > MAX_YEAR | year(c6_restricted_start_date) < MIN_YEAR | year(c6_restricted_start_date) > snz_deceased_year_nbr, as.Date(NA), c6_restricted_start_date),
    c6_full_start_date = if_else(year(c6_full_start_date) > MAX_YEAR | year(c6_full_start_date) < MIN_YEAR | year(c6_full_start_date) > snz_deceased_year_nbr, as.Date(NA), c6_full_start_date),
    c6_end = if_else(year(c6_end) > MAX_YEAR | year(c6_end) < MIN_YEAR | year(c6_end) > snz_deceased_year_nbr, as.Date(NA), c6_end),
    c2_start_date = if_else(year(c2_start_date) > MAX_YEAR | year(c2_start_date) < MIN_YEAR | year(c2_start_date) > snz_deceased_year_nbr, as.Date(NA), c2_start_date),
    c2_end = if_else(year(c2_end) > MAX_YEAR | year(c2_end) < MIN_YEAR | year(c2_end) > snz_deceased_year_nbr, as.Date(NA), c2_end),
    c3_start_date = if_else(year(c3_start_date) > MAX_YEAR | year(c3_start_date) < MIN_YEAR | year(c3_start_date) > snz_deceased_year_nbr, as.Date(NA), c3_start_date),
    c3_end = if_else(year(c3_end) > MAX_YEAR | year(c3_end) < MIN_YEAR | year(c3_end) > snz_deceased_year_nbr, as.Date(NA), c3_end),
    c4_start_date = if_else(year(c4_start_date) > MAX_YEAR | year(c4_start_date) < MIN_YEAR | year(c4_start_date) > snz_deceased_year_nbr, as.Date(NA), c4_start_date),
    c4_end = if_else(year(c4_end) > MAX_YEAR | year(c4_end) < MIN_YEAR | year(c4_end) > snz_deceased_year_nbr, as.Date(NA), c4_end),
    c5_start_date = if_else(year(c5_start_date) > MAX_YEAR | year(c5_start_date) < MIN_YEAR | year(c5_start_date) > snz_deceased_year_nbr, as.Date(NA), c5_start_date),
    c5_end = if_else(year(c5_end) > MAX_YEAR | year(c5_end) < MIN_YEAR | year(c5_end) > snz_deceased_year_nbr, as.Date(NA), c5_end),
    
  )

## historic to long-thin ------------------------------------------------------

# class 1
c1 = generated_historic %>%
  filter(licence_type != "DIPLOMATIC") %>%
  mutate(
    c1_learner_start_date = if_else(c1_learner, c1_learner_start_date, as.Date(NA)),
    c1_restricted_start_date = if_else(
      c1_learner & !c1_learner_lost & c1_restricted,
      c1_restricted_start_date, as.Date(NA)),
    c1_full_start_date = if_else(
      c1_learner & !c1_learner_lost & c1_restricted & !c1_restricted_lost & c1_full,
      c1_full_start_date, as.Date(NA)),
  ) %>%
  filter(
    !is.na(c1_learner_start_date) |
      !is.na(c1_restricted_start_date) |
      !is.na(c1_full_start_date)
  ) %>%
  mutate(
    licence_class = "1",
    overall_start_date = pmin(c1_learner_start_date, c1_restricted_start_date, c1_full_start_date, na.rm = TRUE)
  ) %>%
  select(
    snz_uid,
    snz_nzta_uid,
    nzta_snz_sex_code = snz_sex_gender_code,
    nzta_hist_birth_month_nbr = snz_birth_month_nbr,
    nzta_hist_birth_year_nbr = snz_birth_year_nbr,
    nzta_hist_licence_type_text = licence_type,
    nzta_hist_licence_start_date = overall_start_date,
    nzta_hist_licence_class_text = licence_class,
    nzta_hist_learner_start_date = c1_learner_start_date,
    nzta_hist_restricted_start_date = c1_restricted_start_date,
    nzta_hist_full_start_date = c1_full_start_date
  )

# class 6
c6 = generated_historic %>%
  filter(licence_type != "DIPLOMATIC") %>%
  mutate(
    c6_learner_start_date = if_else(c6_learner, c6_learner_start_date, as.Date(NA)),
    c6_restricted_start_date = if_else(
      c6_learner & !c6_learner_lost & c6_restricted,
      c6_restricted_start_date, as.Date(NA)),
    c6_full_start_date = if_else(
      c6_learner & !c6_learner_lost & c6_restricted & !c6_restricted_lost & c6_full,
      c6_full_start_date, as.Date(NA)),
  ) %>%
  filter(
    !is.na(c6_learner_start_date) |
      !is.na(c6_restricted_start_date) |
      !is.na(c6_full_start_date)
  ) %>%
  mutate(
    licence_class = "6",
    overall_start_date = pmin(c6_learner_start_date, c6_restricted_start_date, c6_full_start_date, na.rm = TRUE)
  ) %>%
  select(
    snz_uid,
    snz_nzta_uid,
    nzta_snz_sex_code = snz_sex_gender_code,
    nzta_hist_birth_month_nbr = snz_birth_month_nbr,
    nzta_hist_birth_year_nbr = snz_birth_year_nbr,
    nzta_hist_licence_type_text = licence_type,
    nzta_hist_licence_start_date = overall_start_date,
    nzta_hist_licence_class_text = licence_class,
    nzta_hist_learner_start_date = c6_learner_start_date,
    nzta_hist_restricted_start_date = c6_restricted_start_date,
    nzta_hist_full_start_date = c6_full_start_date
  )

# class 2
c2 = generated_historic %>%
  filter(licence_type != "DIPLOMATIC") %>%
  mutate(
    c2_start_date = if_else(
      c1_learner & !c1_learner_lost & c1_restricted & !c1_restricted_lost & c1_full & c2_licence,
      c2_start_date, as.Date(NA)),
  ) %>%
  filter(!is.na(c2_start_date)
  ) %>%
  mutate(licence_class = "2", overall_start_date = c2_start_date) %>%
  select(
    snz_uid,
    snz_nzta_uid,
    nzta_snz_sex_code = snz_sex_gender_code,
    nzta_hist_birth_month_nbr = snz_birth_month_nbr,
    nzta_hist_birth_year_nbr = snz_birth_year_nbr,
    nzta_hist_licence_type_text = licence_type,
    nzta_hist_licence_start_date = overall_start_date,
    nzta_hist_licence_class_text = licence_class,
    nzta_hist_full_start_date = c2_start_date
  )

# class 3
c3 = generated_historic %>%
  filter(licence_type != "DIPLOMATIC") %>%
  mutate(
    c3_start_date = if_else(
      c1_learner & !c1_learner_lost & c1_restricted & !c1_restricted_lost & c1_full & c2_licence & c3_licence,
      c3_start_date, as.Date(NA)),
  ) %>%
  filter(!is.na(c3_start_date)
  ) %>%
  mutate(licence_class = "3", overall_start_date = c3_start_date) %>%
  select(
    snz_uid,
    snz_nzta_uid,
    nzta_snz_sex_code = snz_sex_gender_code,
    nzta_hist_birth_month_nbr = snz_birth_month_nbr,
    nzta_hist_birth_year_nbr = snz_birth_year_nbr,
    nzta_hist_licence_type_text = licence_type,
    nzta_hist_licence_start_date = overall_start_date,
    nzta_hist_licence_class_text = licence_class,
    nzta_hist_full_start_date = c3_start_date
  )

# class 4
c4 = generated_historic %>%
  filter(licence_type != "DIPLOMATIC") %>%
  mutate(
    c4_start_date = if_else(
      c1_learner & !c1_learner_lost & c1_restricted & !c1_restricted_lost & c1_full & c2_licence & c3_licence & c4_licence,
      c4_start_date, as.Date(NA)),
  ) %>%
  filter(!is.na(c4_start_date)
  ) %>%
  mutate(licence_class = "4", overall_start_date = c4_start_date) %>%
  select(
    snz_uid,
    snz_nzta_uid,
    nzta_snz_sex_code = snz_sex_gender_code,
    nzta_hist_birth_month_nbr = snz_birth_month_nbr,
    nzta_hist_birth_year_nbr = snz_birth_year_nbr,
    nzta_hist_licence_type_text = licence_type,
    nzta_hist_licence_start_date = overall_start_date,
    nzta_hist_licence_class_text = licence_class,
    nzta_hist_full_start_date = c4_start_date
  )

# class 5
c5 = generated_historic %>%
  filter(licence_type != "DIPLOMATIC") %>%
  mutate(
    c5_start_date = if_else(
      c1_learner & !c1_learner_lost & c1_restricted & !c1_restricted_lost & c1_full & c2_licence & c3_licence & c4_licence & c5_licence,
      c5_start_date, as.Date(NA)),
  ) %>%
  filter(!is.na(c5_start_date)
  ) %>%
  mutate(licence_class = "5", overall_start_date = c5_start_date) %>%
  select(
    snz_uid,
    snz_nzta_uid,
    nzta_snz_sex_code = snz_sex_gender_code,
    nzta_hist_birth_month_nbr = snz_birth_month_nbr,
    nzta_hist_birth_year_nbr = snz_birth_year_nbr,
    nzta_hist_licence_type_text = licence_type,
    nzta_hist_licence_start_date = overall_start_date,
    nzta_hist_licence_class_text = licence_class,
    nzta_hist_full_start_date = c5_start_date
  )

# diplomatic licences
dip = generated_historic %>%
  filter(licence_type == "DIPLOMATIC") %>%
  mutate(licence_class = "1", overall_start_date = c1_full_start_date) %>%
  select(
    snz_uid,
    snz_nzta_uid,
    nzta_snz_sex_code = snz_sex_gender_code,
    nzta_hist_birth_month_nbr = snz_birth_month_nbr,
    nzta_hist_birth_year_nbr = snz_birth_year_nbr,
    nzta_hist_licence_type_text = licence_type,
    nzta_hist_licence_start_date = overall_start_date,
    nzta_hist_licence_class_text = licence_class,
    nzta_hist_full_start_date = c1_full_start_date
  )

generated_historic = bind_rows(c1, c2, c3, c4, c5, c6, dip) %>%
  filter(!is.na(nzta_hist_licence_start_date))

## output historic ------------------------------------------------------------

generated_historic %>%
  select(
    snz_uid,
    snz_nzta_uid,
    nzta_snz_sex_code,
    nzta_hist_birth_month_nbr,
    nzta_hist_birth_year_nbr,
    nzta_hist_licence_type_text,
    nzta_hist_licence_start_date,
    nzta_hist_licence_class_text,
    nzta_hist_learner_start_date,
    nzta_hist_restricted_start_date,
    nzta_hist_full_start_date
  ) %>%
  write.csv(GENERATED_FILE1, row.names = FALSE)

## HISTORIC TABLE COMPLETE ----------------------------------------------------

