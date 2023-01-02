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

# WARNING - SLOW - RUNTIME (10k people: 20min, 100k people: 3+hrs)

## parameters -----------------------------------------------------------------

# controls
SEED = 123

# input/output
LOOKUP_FILE = "./Lookups/[ir_clean]_[ird_ems].csv"
REFERENCE_FILE = "./Reference/residence.csv"
BUSINESS_FILE = "./Reference/ref_employer.csv"
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
PROB_CHANGE_EMPLOYER = 0.1

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
PROPORTION_SECOND_JOB = 0.05

MU_WAS_g = 6.5
MU_WAS_b = 6.2
MU_WHP = 6.65
MU_BEN = 5.9
MU_PEN = 6.1
SD_WAS_g = 0.5
SD_WAS_b = 0.5
SD_WHP = 0.5
SD_BEN = 0.1
SD_PEN = 0.1
INCREMENT_WAS_g = 0.02
INCREMENT_WAS_b = 0.018
INCREMENT_WHP = 0.021
INCREMENT_BEN = 0.01
INCREMENT_PEN = 0.015

AMT_MIN = -0.7
AMT_MAX = 0.12



## setup ----------------------------------------------------------------------

library(dplyr)
library(lubridate)
source("./Code main/support_functions.R")
set.seed(seed = SEED)

lookup = read.csv(LOOKUP_FILE, stringsAsFactors = FALSE) %>%
  filter(schema == "ir_clean", table == "ird_ems")

base_population = read.csv(REFERENCE_FILE, stringsAsFactors = FALSE)

businesses = read.csv(BUSINESS_FILE, stringsAsFactors = FALSE)


## helper functions -----------------------------------------------------------

job_to_parameter = function(job, parameter){
  case_when(
    parameter == 'mu' & job == 'WAS_g' ~ MU_WAS_g,
    parameter == 'mu' & job == 'WAS_b' ~ MU_WAS_b,
    parameter == 'mu' & job == 'WHP' ~ MU_WHP,
    parameter == 'mu' & job == 'BEN' ~ MU_BEN,
    parameter == 'mu' & job == 'PEN' ~ MU_PEN,
    parameter == 'sd' & job == 'WAS_g' ~ SD_WAS_g,
    parameter == 'sd' & job == 'WAS_b' ~ SD_WAS_b,
    parameter == 'sd' & job == 'WHP' ~ SD_WHP,
    parameter == 'sd' & job == 'BEN' ~ SD_BEN,
    parameter == 'sd' & job == 'PEN' ~ SD_PEN,
    parameter == 'inc' & job == 'WAS_g' ~ INCREMENT_WAS_g,
    parameter == 'inc' & job == 'WAS_b' ~ INCREMENT_WAS_b,
    parameter == 'inc' & job == 'WHP' ~ INCREMENT_WHP,
    parameter == 'inc' & job == 'BEN' ~ INCREMENT_BEN,
    parameter == 'inc' & job == 'PEN' ~ INCREMENT_PEN
  )
}

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

#### create multiple jobs for some people ----
# by duplicating these people and simulating a duplicate stream

num_second_job = round(PROPORTION_SECOND_JOB * nrow(employ_pop))
index_second_job = sample(1:nrow(employ_pop), num_second_job)

additional_pop = employ_pop[index_second_job,] %>%
  mutate(tax_code = "S")

employ_pop = employ_pop %>% mutate(tax_code = "M")

employ_pop = bind_rows(employ_pop, additional_pop)

#### base year ----

current_year = MIN_YEAR
current_month = 1

current_year_data = employ_pop %>%
  mutate(year = current_year, month = current_month)

# employer code for base year
table_size = nrow(current_year_data)

sampler = businesses %>%
  rename(value = snz_employer_ird_uid) %>%
  make_weighted_options_array()

current_year_data = current_year_data %>%
  mutate(snz_employer_ird_uid = sample(sampler, table_size, replace = TRUE))

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
  
  # employer code for new job
  table_size = nrow(yes_change)
  
  sampler = businesses %>%
    rename(value = snz_employer_ird_uid) %>%
    make_weighted_options_array()
  
  yes_change = yes_change %>%
    mutate(new_employer = sample(sampler, table_size, replace = TRUE))
  
  # ready
  yes_change = yes_change %>%
    mutate(
      snz_employer_ird_uid = ifelse(r2 < PROB_CHANGE_EMPLOYER | open_job != new_job, new_employer, snz_employer_ird_uid),
      open_job = new_job
    ) %>%
    select(-r2, -new_job, -new_employer)
  
  new_year_data = bind_rows(no_change, yes_change)
  generated_table = bind_rows(generated_table, new_year_data)
  current_year_data = new_year_data
}

#### tidy up ----

g2 = generated_table %>%
  # pension if past swap date
  mutate(open_job = ifelse(year > year_swap, "PEN", open_job)) %>%
  # no income before start
  filter(year_start <= year) %>%
  # no income after death
  filter(year <= snz_deceased_year_nbr) %>%
  # exclude no income
  filter(open_job != "none") %>%
  # where tax code = "S" discard non-job
  filter(tax_code != "S" | substr(open_job, 1, 1) == "W") %>%
  # must meet resident or non-resident-earning
  semi_join(earning_by_year, by = c("snz_uid" = "snz_uid", "year" = "the_year")) %>%
  # dates
  mutate(
    return_date = ymd(paste0(year,"-",month,"-01")),
    return_date = rollforward(return_date)
  )



## simulate income by type ----------------------------------------------------

g2$mu = sapply(g2$open_job, job_to_parameter, parameter = "mu")
g2$sd = sapply(g2$open_job, job_to_parameter, parameter = "sd")
g2$inc = sapply(g2$open_job, job_to_parameter, parameter = "inc")

pop_size = nrow(g2)

g2 = g2 %>%
  mutate(
    r3 = rnorm(pop_size),
    ir_ems_gross_earnings_amt = exp(mu + sd * r3 + inc * (year - MIN_YEAR))
  ) %>%
  select(-r3, -mu, -sd, -inc)

# complete at this point
#
# snz_uid
# ir_ems_return_period_date
# snz_employer_ird_uid
# ir_ems_gross_earnings_amt
# ir_ems_income_source_code
# ir_ems_tax_code


## simulate income details ----------------------------------------------------

g2 = g2 %>%
  mutate(
    r1 = runif(pop_size, min = AMT_MIN, max = AMT_MAX),
    r1 = pmax(r1, 0),
    ir_ems_paye_deductions_amt = ir_ems_gross_earnings_amt * r1
  ) %>%
  mutate(
    r1 = runif(pop_size, min = AMT_MIN, max = AMT_MAX),
    r1 = pmax(r1, 0),
    ir_ems_earnings_not_liable_amt = ir_ems_gross_earnings_amt * r1
  ) %>%
  mutate(
    r1 = runif(pop_size, min = AMT_MIN, max = AMT_MAX),
    r1 = pmax(r1, 0),
    ir_ems_fstc_amt = ir_ems_gross_earnings_amt * r1
  ) %>%
  mutate(
    r1 = runif(pop_size, min = AMT_MIN, max = AMT_MAX),
    r1 = pmax(r1, 0),
    ir_ems_sl_amt = ir_ems_gross_earnings_amt * r1
  ) %>%
  select(-r1)

# ird_uid
rr = 0.1 + 0.8 * runif(1)
max_id = max(g2$snz_uid) + 117
shift = floor(rr * max_id)

g2 = g2 %>%
  mutate(snz_ird_uid = (snz_uid + shift) %% max_id)

# complete at this point
#
# snz_ird_uid
# ir_ems_paye_deductions_amt
# ir_ems_earnings_not_liable_amt
# ir_ems_fstc_amt
# ir_ems_sl_amt


## everything that comes from employer ----------------------------------------

# employer codes for goverment payments
max_code = max(businesses$snz_employer_ird_uid)
govt_codes = max_code + 10:20

g2 = g2 %>%
  left_join(businesses, by = "snz_employer_ird_uid") %>%
  # merge WAS_g an WAS_b
  mutate(open_job = ifelse(open_job %in% c("WAS_g", "WAS_b"), "WAS", open_job)) %>%
  # alternatives for PEN and BEN codes
  mutate(
    govt_code = sample(govt_codes, pop_size, replace = TRUE),
    snz_employer_ird_uid = ifelse(open_job %in% c("BEN", "PEN"), govt_code, snz_employer_ird_uid),
    ir_ems_enterprise_nbr = ifelse(open_job %in% c("BEN", "PEN"), NA, ir_ems_enterprise_nbr),
    ir_ems_pbn_nbr = ifelse(open_job %in% c("BEN", "PEN"), NA, ir_ems_pbn_nbr),
    ir_ems_pbn_anzsic06_code = ifelse(open_job %in% c("BEN", "PEN"), NA, ir_ems_pbn_anzsic06_code),
    ir_ems_ent_anzsic06_code = ifelse(open_job %in% c("BEN", "PEN"), NA, ir_ems_ent_anzsic06_code)
  ) %>%
  select(-govt_code)

# complete at this point
#
# snz_employer_ird_uid
# ir_ems_employer_location_nbr
# ir_ems_enterprise_nbr
# ir_ems_pbn_nbr
# ir_ems_pbn_anzsic06_code
# ir_ems_ent_anzsic06_code

## everything to generate with sampler lookup ---------------------------------
table_size = nrow(g2)

sampler = lookup %>%
  filter(column == "ir_ems_line_nbr") %>%
  make_weighted_options_array()
g2 = g2 %>%
  mutate(ir_ems_line_nbr = sample(sampler, table_size, replace = TRUE))

sampler = lookup %>%
  filter(column == "ir_ems_snz_unique_nbr") %>%
  make_weighted_options_array()
g2 = g2 %>%
  mutate(ir_ems_snz_unique_nbr = sample(sampler, table_size, replace = TRUE))

sampler = lookup %>%
  filter(column == "ir_ems_version_nbr") %>%
  make_weighted_options_array()
g2 = g2 %>%
  mutate(ir_ems_version_nbr = sample(sampler, table_size, replace = TRUE))

sampler = lookup %>%
  filter(column == "ir_ems_doc_lodge_prefix_nbr") %>%
  make_weighted_options_array()
g2 = g2 %>%
  mutate(ir_ems_doc_lodge_prefix_nbr = sample(sampler, table_size, replace = TRUE))

sampler = lookup %>%
  filter(column == "ir_ems_doc_lodge_nbr") %>%
  make_weighted_options_array()
g2 = g2 %>%
  mutate(ir_ems_doc_lodge_nbr = sample(sampler, table_size, replace = TRUE))

sampler = lookup %>%
  filter(column == "ir_ems_doc_lodge_suffix_nbr") %>%
  make_weighted_options_array()
g2 = g2 %>%
  mutate(ir_ems_doc_lodge_suffix_nbr = sample(sampler, table_size, replace = TRUE))

sampler = lookup %>%
  filter(column == "ir_ems_gross_earnings_imp_code") %>%
  make_weighted_options_array()
g2 = g2 %>%
  mutate(ir_ems_gross_earnings_imp_code = sample(sampler, table_size, replace = TRUE))

sampler = lookup %>%
  filter(column == "ir_ems_paye_imp_ind") %>%
  make_weighted_options_array()
g2 = g2 %>%
  mutate(ir_ems_paye_imp_ind = sample(sampler, table_size, replace = TRUE))

sampler = lookup %>%
  filter(column == "ir_ems_earnings_not_liab_imp_ind") %>%
  make_weighted_options_array()
g2 = g2 %>%
  mutate(ir_ems_earnings_not_liab_imp_ind = sample(sampler, table_size, replace = TRUE))

sampler = lookup %>%
  filter(column == "ir_ems_return_line_item_code") %>%
  make_weighted_options_array()
g2 = g2 %>%
  mutate(ir_ems_return_line_item_code = sample(sampler, table_size, replace = TRUE))

sampler = lookup %>%
  filter(column == "ir_ems_withholding_type_code") %>%
  make_weighted_options_array()
g2 = g2 %>%
  mutate(ir_ems_withholding_type_code = sample(sampler, table_size, replace = TRUE))

sampler = lookup %>%
  filter(column == "ir_ems_lump_sum_ind") %>%
  make_weighted_options_array()
g2 = g2 %>%
  mutate(ir_ems_lump_sum_ind = sample(sampler, table_size, replace = TRUE))

# complete at this point
#
# ir_ems_line_nbr			
# ir_ems_snz_unique_nbr			
# ir_ems_version_nbr			
# ir_ems_doc_lodge_prefix_nbr			
# ir_ems_doc_lodge_nbr			
# ir_ems_doc_lodge_suffix_nbr			
# ir_ems_gross_earnings_imp_code			
# ir_ems_paye_imp_ind			
# ir_ems_earnings_not_liab_imp_ind			
# ir_ems_return_line_item_code
# ir_ems_withholding_type_code			
# ir_ems_lump_sum_ind			

## write out table ------------------------------------------------------------

g2 %>%
  filter(open_job != 'none') %>%
  select(
    snz_uid,
    snz_ird_uid,
    snz_employer_ird_uid,
    ir_ems_employer_location_nbr,
    ir_ems_return_period_date = return_date,
    ir_ems_line_nbr,
    ir_ems_snz_unique_nbr,
    ir_ems_version_nbr,
    ir_ems_doc_lodge_prefix_nbr,
    ir_ems_doc_lodge_nbr,
    ir_ems_doc_lodge_suffix_nbr,
    ir_ems_gross_earnings_amt,
    ir_ems_gross_earnings_imp_code,
    ir_ems_paye_deductions_amt,
    ir_ems_paye_imp_ind,
    ir_ems_earnings_not_liable_amt,
    ir_ems_earnings_not_liab_imp_ind,
    ir_ems_fstc_amt,
    ir_ems_sl_amt,
    ir_ems_return_line_item_code,
    ir_ems_withholding_type_code,
    ir_ems_income_source_code = open_job,
    ir_ems_lump_sum_ind,
    ir_ems_tax_code = tax_code,
    ir_ems_enterprise_nbr,
    ir_ems_pbn_nbr,
    ir_ems_pbn_anzsic06_code,
    ir_ems_ent_anzsic06_code
  ) %>%
  write.csv(GENERATED_FILE, row.names = FALSE)
