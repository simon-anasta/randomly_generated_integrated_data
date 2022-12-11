# Schema: ir_clean
# Table: ird_ems (employer monthly summary)
#
# Output columns:
#
# snz_uid
# snz_ird_uid
# snz_employer_ird_uid
# ir_ems_employer_location_nbr
# ir_ems_return_period_date
# ir_ems_line_nbr
# ir_ems_snz_unique_nbr
# ir_ems_version_nbr
# ir_ems_doc_lodge_prefix_nbr
# ir_ems_doc_lodge_nbr
# ir_ems_doc_lodge_suffix_nbr
# ir_ems_gross_earnings_amt
# ir_ems_gross_earnings_imp_code
# ir_ems_paye_deductions_amt
# ir_ems_paye_imp_ind
# ir_ems_earnings_not_liable_amt
# ir_ems_earnings_not_liab_imp_ind
# ir_ems_fstc_amt
# ir_ems_sl_amt
# ir_ems_return_line_item_code
# ir_ems_withholding_type_code
# ir_ems_income_source_code
# ir_ems_lump_sum_ind
# ir_ems_tax_code
# ir_ems_enterprise_nbr
# ir_ems_pbn_nbr
# ir_ems_emp_specific_job_nbr
# ir_ems_pbn_anzsic06_code
# ir_ems_ent_anzsic06_code
#

## parameters -----------------------------------------------------------------

# controls
SEED = 123

# input/output
LOOKUP_FILE = "./Lookups/[ir_clean]_[ird_ems].csv"
REFERENCE_FILE = "./Reference/residence.csv"
GENERATED_FILE = "./Generated/[ir_clean]_[ird_ems].csv"

# attributes
MIN_YEAR = 2000
MAX_YEAR = 2020
START_EARNING_MIN_AGE = 15
START_EARNING_MAX_AGE = 25
PENSION_MIN_AGE = 65
PENSION_MAX_AGE = 70
MONTHS_STABLE_FOR = 4
NON_RESIDENT_EARNING = 0.1

TRANSITION_MATRIX = matrix(
  c(
    #NA, BEN, WAS_b, WAS_g, WHP
    0.90, 0.03, 0.05, 0.01, 0.01, # NONE
    0.05, 0.85, 0.07, 0.01, 0.02, # BEN
    0.02, 0.05, 0.85, 0.06, 0.02, # WAS_b
    0.01, 0.01, 0.01, 0.95, 0.02, # WAS_g
    0.01, 0.01, 0.03, 0.10, 0.85  # WHP
  ),
  nrow = 5,
  ncol = 5,
  byrow = TRUE
)

INITIAL_NONE = 0.15
INITIAL_BEN = 0.12
INITIAL_WAS_b = 0.16
INITIAL_WAS_g = 0.46
INITIAL_WHP = 0.11

## setup ----------------------------------------------------------------------

library(dplyr)
library(lubridate)
source("./Code main/support_functions.R")
set.seed(seed = SEED)

lookup = read.csv(LOOKUP_FILE, stringsAsFactors = FALSE) %>%
  filter(schema == "ir_clean", table == "ird_ems")

base_population = read.csv(REFERENCE_FILE, stringsAsFactors = FALSE)

## helper functions -----------------------------------------------------------

index_to_char = function(index){
  case_when(
    index == 1 ~ "none",
    index == 2 ~ "BEN",
    index == 3 ~ "WAS_b",
    index == 4 ~ "WAS_g",
    index == 5 ~ "WHP"
  )
}

char_to_index = function(char){
  case_when(
    char == "none" ~ 1,
    char == "BEN" ~ 2,
    char == "WAS_b" ~ 3,
    char == "WAS_g" ~ 4,
    char == "WHP" ~ 5
  )
}

make_transition = function(char){
  index = char_to_index(char)
  row = TRANSITION_MATRIX[index,]
  prob = runif(1)
  smallers = sum(prob < cumsum(row))
  index = length(row) - smallers + 1
  index_to_char(index)
}

## generate -------------------------------------------------------------------

pop_size = nrow(base_population)

#### prep ----
earning_by_year = base_population %>%
  # filter to ever earns in NZ
  mutate(
    r1 = runif(pop_size),
    is_earning = ifelse(r1 < NON_RESIDENT_EARNING, 1, is_resident)
  ) %>%
  filter(is_earning == 1)


employ_pop = earning_by_year %>%
  select(-the_year, -is_resident, -r1, -is_earning) %>%
  distinct()

pop_size = nrow(employ_pop)

employ_pop = employ_pop %>%
  mutate(
    start_earn = randbetween(START_EARNING_MIN_AGE, START_EARNING_MAX_AGE, pop_size),
    swap_pension = randbetween(PENSION_MIN_AGE, PENSION_MAX_AGE, pop_size),
    stable_remaining = randbetween(1, MONTHS_STABLE_FOR, pop_size)
  ) %>%
  mutate(
    year_start = snz_birth_year_nbr + start_earn,
    year_swap = snz_birth_year_nbr + swap_pension
  ) %>%
  mutate(
    r2 = runif(pop_size),
    open_job = case_when(
      r2 < INITIAL_NONE ~ 'none',
      r2 < INITIAL_NONE + INITIAL_BEN ~ 'BEN',
      r2 < INITIAL_NONE + INITIAL_BEN + INITIAL_WAS_b ~ 'WAS_b',
      r2 < INITIAL_NONE + INITIAL_BEN + INITIAL_WAS_b + INITIAL_WAS_g ~ 'WAS_g',
      r2 <= 1 ~ 'WHP'
    )
  ) %>%
  select(-start_earn, -swap_pension, -r2, -snz_birth_year_nbr)

#### base year ----

current_year = MIN_YEAR
current_month = 1

current_year_data = employ_pop %>%
  mutate(year = current_year, month = current_month)

#### increment years ----
generated_table = current_year_data

while(TRUE){
  # increment calendar
  current_month = current_month + 1
  if(current_month >= 13){
    current_year = current_year + 1
    current_month = 1
  }
  if(current_year >= MAX_YEAR + 1){
    break
  }
  
  # increment time in data
  new_year_data = current_year_data %>%
    mutate(
      year = current_year,
      month = current_month,
      stable_remaining = stable_remaining - 1
    )
  
  # split
  no_change = new_year_data %>%
    filter(stable_remaining > 0)
  
  yes_change = new_year_data %>%
    filter(stable_remaining <= 0)
  
  # simulate for change
  pop_size = nrow(yes_change)
  yes_change = yes_change %>%
    mutate(
      stable_remaining = randbetween(1, MONTHS_STABLE_FOR, pop_size),
      r2 = runif(pop_size)
    )
  yes_change$new_job = sapply(yes_change$open_job, make_transition)
  
  yes_change = yes_change %>%
    mutate(open_job = new_job) %>%
    select(-r2, -new_job)
  
  new_year_data = bind_rows(no_change, yes_change)
  generated_table = bind_rows(generated_table, new_year_data)
  current_year_data = new_year_data
}


## simulate income type -------------------------------------------------------

snz_uid
ir_ems_return_period_date
snz_employer_ird_uid
ir_ems_gross_earnings_amt
ir_ems_income_source_code
ir_ems_tax_code


## simulate income details ----------------------------------------------------

snz_ird_uid
ir_ems_paye_deductions_amt
ir_ems_earnings_not_liable_amt
ir_ems_fstc_amt
ir_ems_sl_amt


## everything that comes from employer ----------------------------------------

snz_employer_ird_uid
ir_ems_employer_location_nbr
ir_ems_enterprise_nbr
ir_ems_pbn_nbr
ir_ems_emp_specific_job_nbr
ir_ems_pbn_anzsic96_code
ir_ems_pbn_anzsic06_code
ir_ems_ent_anzsic96_code
ir_ems_ent_anzsic06_code




## everything to generate with sampler lookup ---------------------------------
table_size = nrow(generated_table)

#### everything to generate with sampler lookup ----
# sampler = lookup %>%
#   filter(column == "srp_flag_under5_ind") %>%
#   make_weighted_options_array()
# 
# generated_table = generated_table %>%
#   mutate(srp_flag_under5_ind = sample(sampler, table_size, replace = TRUE))

ir_ems_line_nbr			
ir_ems_snz_unique_nbr			
ir_ems_version_nbr			
ir_ems_doc_lodge_prefix_nbr			
ir_ems_doc_lodge_nbr			
ir_ems_doc_lodge_suffix_nbr			
ir_ems_gross_earnings_imp_code			
ir_ems_paye_imp_ind			
ir_ems_earnings_not_liab_imp_ind			
ir_ems_return_line_item_code
ir_ems_withholding_type_code			
ir_ems_lump_sum_ind			




## write out table ------------------------------------------------------------

generated_table %>%
  select(snz_uid, snz_birth_year_nbr, snz_deceased_year_nbr, the_year, is_resident) %>%
  write.csv(REFERENCE_FILE, row.names = FALSE)

generated_table %>%
  filter(is_resident == 1) %>%
  select(
    snz_uid
    snz_ird_uid
    snz_employer_ird_uid
    ir_ems_employer_location_nbr
    ir_ems_return_period_date
    ir_ems_line_nbr
    ir_ems_snz_unique_nbr
    ir_ems_version_nbr
    ir_ems_doc_lodge_prefix_nbr
    ir_ems_doc_lodge_nbr
    ir_ems_doc_lodge_suffix_nbr
    ir_ems_gross_earnings_amt
    ir_ems_gross_earnings_imp_code
    ir_ems_paye_deductions_amt
    ir_ems_paye_imp_ind
    ir_ems_earnings_not_liable_amt
    ir_ems_earnings_not_liab_imp_ind
    ir_ems_fstc_amt
    ir_ems_sl_amt
    ir_ems_return_line_item_code
    ir_ems_withholding_type_code
    ir_ems_income_source_code
    ir_ems_lump_sum_ind
    ir_ems_tax_code
    ir_ems_enterprise_nbr
    ir_ems_pbn_nbr
    ir_ems_emp_specific_job_nbr
    ir_ems_pbn_anzsic06_code
    ir_ems_ent_anzsic06_code
  ) %>%
  write.csv(GENERATED_FILE, row.names = FALSE)


