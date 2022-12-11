# Schema: data
# Table: snz_res_pop
#
# Output columns:
#
# snz_uid
# srp_ref_date
# srp_flag_ir_ind
# srp_flag_ed_ind
# srp_flag_health_ind
# srp_flag_acc_ind
# srp_flag_under5_ind
#

## parameters -----------------------------------------------------------------

# controls
SEED = 123

# input/output
LOOKUP_FILE = "./Lookups/[data]_[snz_res_pop].csv"
REFERENCE_FILE = "./Reference/residence.csv"
GENERATED_FILE = "./Generated/[data]_[snz_res_pop].csv"

# attributes
RES_MIN_YEAR = 2000
RES_MAX_YEAR = 2020
INITIAL_PROPORTION_RESIDENT = 0.4
TRANSITION_PROB = 0.01

## setup ----------------------------------------------------------------------

library(dplyr)
source("./Code main/support_functions.R")
set.seed(seed = SEED)

lookup = read.csv(LOOKUP_FILE, stringsAsFactors = FALSE) %>%
  filter(schema == "data", table == "snz_res_pop")

base_population = read.csv("./Generated/[data]_[personal_detail].csv",
                           stringsAsFactors = FALSE) %>%
  select(snz_uid, snz_birth_year_nbr, snz_deceased_year_nbr)

pop_size = nrow(base_population)

## generate -------------------------------------------------------------------

#### base year ----
generated_table = base_population %>%
  mutate(the_year = RES_MIN_YEAR,
         pp = runif(pop_size)) %>%
  mutate(is_resident = ifelse(pp <= INITIAL_PROPORTION_RESIDENT, 1, 0)) %>%
  select(snz_uid, snz_birth_year_nbr, snz_deceased_year_nbr, the_year, is_resident)

#### increment years ----
current_year = generated_table

for(each_year in (RES_MIN_YEAR+1):RES_MAX_YEAR){
  new_year = current_year %>%
    mutate(the_year = each_year,
           pp = runif(pop_size)) %>%
    mutate(is_resident = ifelse(pp < TRANSITION_PROB, 1 - is_resident, is_resident)) %>%
    select(snz_uid, snz_birth_year_nbr, snz_deceased_year_nbr, the_year, is_resident)
  
  generated_table = bind_rows(generated_table, new_year)
  current_year = new_year
}

table_size = nrow(generated_table)

#### srp_flag_ir_ind ----
sampler = lookup %>%
  filter(column == "srp_flag_ir_ind") %>%
  make_weighted_options_array()

generated_table = generated_table %>%
  mutate(srp_flag_ir_ind = sample(sampler, table_size, replace = TRUE))

#### srp_flag_ed_ind ----
sampler = lookup %>%
  filter(column == "srp_flag_ed_ind") %>%
  make_weighted_options_array()

generated_table = generated_table %>%
  mutate(srp_flag_ed_ind = sample(sampler, table_size, replace = TRUE))

#### srp_flag_health_ind ----
sampler = lookup %>%
  filter(column == "srp_flag_health_ind") %>%
  make_weighted_options_array()

generated_table = generated_table %>%
  mutate(srp_flag_health_ind = sample(sampler, table_size, replace = TRUE))

#### srp_flag_acc_ind ----
sampler = lookup %>%
  filter(column == "srp_flag_acc_ind") %>%
  make_weighted_options_array()

generated_table = generated_table %>%
  mutate(srp_flag_acc_ind = sample(sampler, table_size, replace = TRUE))

#### srp_flag_under5_ind ----
# sampler = lookup %>%
#   filter(column == "srp_flag_under5_ind") %>%
#   make_weighted_options_array()
# 
# generated_table = generated_table %>%
#   mutate(srp_flag_under5_ind = sample(sampler, table_size, replace = TRUE))

generated_table = generated_table %>%
  mutate(srp_flag_under5_ind = ifelse(
    snz_birth_year_nbr <= the_year
    & the_year <= snz_birth_year_nbr + 5
    , 1, 0
  ))

#### srp_ref_date ----

generated_table = generated_table %>%
  mutate(srp_ref_date = lubridate::ymd(paste0(the_year,"-06-30")))

## write out table ------------------------------------------------------------

generated_table %>%
  select(snz_uid, snz_birth_year_nbr, snz_deceased_year_nbr, the_year, is_resident) %>%
  write.csv(REFERENCE_FILE, row.names = FALSE)

generated_table %>%
  mutate(
    start_year = pmax(snz_birth_year_nbr, RES_MIN_YEAR),
    end_year = pmin(snz_deceased_year_nbr, RES_MAX_YEAR)
  ) %>%
  filter(is_resident == 1) %>%
  filter(start_year <= the_year,
         the_year <= end_year) %>%
  select(
    snz_uid,
    srp_ref_date,
    srp_flag_ir_ind,
    srp_flag_ed_ind,
    srp_flag_health_ind,
    srp_flag_acc_ind,
    srp_flag_under5_ind
  ) %>%
  write.csv(GENERATED_FILE, row.names = FALSE)


