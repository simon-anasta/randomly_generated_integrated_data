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
MIN_YEAR = 1990
MAX_YEAR = 2020



## setup ----------------------------------------------------------------------

library(dplyr)
source("./Code main/support_functions.R")
set.seed(seed = SEED)

lookup = read.csv(LOOKUP_FILE, stringsAsFactors = FALSE) %>%
  filter(schema == "ir_clean", table == "ird_ems")

base_population = read.csv(REFERENCE_FILE,
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


