# Schema: NA
# Table: employers
#
# Output columns:
#
# weight
# snz_employer_ird_uid
# ir_ems_employer_location_nbr
# ir_ems_enterprise_nbr
# ir_ems_pbn_nbr
# ir_ems_pbn_anzsic06_code
# ir_ems_ent_anzsic06_code
# 
#

## parameters -----------------------------------------------------------------

# controls
SEED = 123

# we recommend num businesses to be 1/20th - 1/50th of num people
NUM_BUSINESS = 50
# NUM_BUSINESS = 2500

# input/output
LOOKUP_FILE = "./Lookups/employer.csv"
GENERATED_FILE = "./Reference/ref_employer.csv"

# attributes
MIN_SIZE = 1
MAX_SIZE = 50
MIN_FIRMS = 1
MAX_FIRMS = 20
PROB_OWN_PBN = 0.5



## setup ----------------------------------------------------------------------

library(dplyr)
source("./Code main/support_functions.R")
set.seed(seed = SEED)

lookup = read.csv(LOOKUP_FILE, stringsAsFactors = FALSE) %>%
  filter(schema == "ref", table == "employer")

## generate -------------------------------------------------------------------

industry = lookup %>%
  filter(column == "anzsic06") %>%
  select(value)

n_industry = nrow(industry)

industry = industry %>%
  mutate(firm_size1 = randbetween(MIN_SIZE, MAX_SIZE, n_industry),
         firm_size2 = randbetween(MIN_SIZE, MAX_SIZE, n_industry),
         firm_size = pmin(firm_size1, firm_size2),
         num_firms = randbetween(MIN_FIRMS, MAX_FIRMS, n_industry))

# sample firms
sampler = sample(1:n_industry, size = NUM_BUSINESS, replace = TRUE, prob = industry$num_firms)

generated_table = data.frame(
  anzsic06 = industry$value[sampler],
  size = industry$firm_size[sampler]
) %>%
  mutate(
    size = round(size * (0.75 + runif(NUM_BUSINESS)/2)),
    ird_uid = sample(1:1E7, NUM_BUSINESS),
    ent_uid = sample(1:1E7, NUM_BUSINESS),
    pbn_uid = sample(1:1E7, NUM_BUSINESS)
  ) %>%
  # combine some as PBN
  mutate(
    pbn_anzsic = anzsic06,
    own_pbn = ifelse(runif(NUM_BUSINESS) < PROB_OWN_PBN, 1, 0),
    sorter = runif(NUM_BUSINESS)
  )

for(ii in 1:7){
  generated_table = generated_table %>%
    arrange(sorter) %>%
    mutate(
      prev_pbn = lag(pbn_uid),
      prev_anzsic = lag(pbn_anzsic)) %>%
    mutate(
      pbn_uid = ifelse(own_pbn == 1, pbn_uid, prev_pbn),
      pbn_anzsic = ifelse(own_pbn == 1, pbn_anzsic, prev_anzsic)
    )
}

#### ir_ems_employer_location_nbr ----
sampler = lookup %>%
  filter(column == "ir_ems_employer_location_nbr") %>%
  make_weighted_options_array()

generated_table = generated_table %>%
  mutate(ir_ems_employer_location_nbr = sample(sampler, NUM_BUSINESS, replace = TRUE))

## write out table ------------------------------------------------------------

generated_table %>%
  select(
    weight = size,
    snz_employer_ird_uid = ird_uid,
    ir_ems_employer_location_nbr,
    ir_ems_enterprise_nbr = ent_uid,
    ir_ems_pbn_nbr = pbn_uid,
    ir_ems_pbn_anzsic06_code = pbn_anzsic,
    ir_ems_ent_anzsic06_code = anzsic06
  ) %>%
  write.csv(GENERATED_FILE, row.names = FALSE)
