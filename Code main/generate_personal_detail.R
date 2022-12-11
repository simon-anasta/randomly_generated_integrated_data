# Schema: data
# Table: personal_detail
#
# Output columns:
#
# snz_uid
# snz_sex_gender_code
# snz_birth_year_nbr
# snz_birth_month_nbr
# snz_ethnicity_grp1_nbr
# snz_ethnicity_grp2_nbr
# snz_ethnicity_grp3_nbr
# snz_ethnicity_grp4_nbr
# snz_ethnicity_grp5_nbr
# snz_ethnicity_grp6_nbr
# snz_deceased_year_nbr
# snz_deceased_month_nbr
# snz_parent1_uid
# snz_parent2_uid
#

## parameters -----------------------------------------------------------------

# controls
SEED = 123
POP_SIZE = 1000

# input/output
LOOKUP_FILE = "./Lookups/[data]_[personal_detail].csv"
GENERATED_FILE = "./Generated/[data]_[personal_detail].csv"

# attributes
BIRTH_MIN_YEAR = 1920
BIRTH_MAX_YEAR = 2020
LIFE_EXPECTANCY_AVG = 70
LIFE_EXPECTANCY_DEV = 8
PROB_EARLY_DEATH = 0.1
FERTILE_MIN_AGE = 18
FERTILE_MAX_AGE = 37

## setup ----------------------------------------------------------------------

library(dplyr)
source("./Code main/support_functions.R")
set.seed(seed = SEED)

lookup = read.csv(LOOKUP_FILE, stringsAsFactors = FALSE) %>%
  filter(schema == "data", table == "personal_detail")

## generate -------------------------------------------------------------------

#### snz_uid ----
generated_table = data.frame(
  snz_uid = sample(100:1E7, POP_SIZE, replace = FALSE),
  stringsAsFactors = FALSE
)

#### snz_sex_gender_code ----
sampler = lookup %>%
  filter(column == "snz_sex_gender_code") %>%
  make_weighted_options_array()

generated_table = generated_table %>%
  mutate(snz_sex_gender_code = sample(sampler, POP_SIZE, replace = TRUE))

#### snz_birth_year_nbr ----
generated_table = generated_table %>%
  mutate(
    yr1 = randbetween(BIRTH_MIN_YEAR, BIRTH_MAX_YEAR, POP_SIZE),
    yr2 = randbetween(BIRTH_MIN_YEAR, BIRTH_MAX_YEAR, POP_SIZE)
  ) %>%
  mutate(snz_birth_year_nbr = pmin(yr1, yr2)) %>%
  select(-yr1, -yr2)

#### snz_birth_month_nbr ----
sampler = lookup %>%
  filter(column == "snz_birth_month_nbr") %>%
  make_weighted_options_array()

generated_table = generated_table %>%
  mutate(snz_birth_month_nbr = sample(sampler, POP_SIZE, replace = TRUE))

#### snz_ethnicity ----
# snz_ethnicity_grp1_nbr
# snz_ethnicity_grp2_nbr
# snz_ethnicity_grp3_nbr
# snz_ethnicity_grp4_nbr
# snz_ethnicity_grp5_nbr
# snz_ethnicity_grp6_nbr
sampler = lookup %>%
  filter(column == "snz_ethnicity") %>%
  make_weighted_options_array()

generated_table = generated_table %>%
  mutate(eth1 = sample(sampler, POP_SIZE, replace = TRUE))

sampler = sampler[sampler != 7]

generated_table = generated_table %>%
  mutate(
    eth2 = sample(sampler, POP_SIZE, replace = TRUE),
    eth3 = sample(sampler, POP_SIZE, replace = TRUE)
  ) %>%
  mutate(
    snz_ethnicity_grp1_nbr = ifelse(
      eth1 == 1
      | (eth1 == 7 & eth2 == 1)
      | (eth1 == 7 & eth3 == 1), 1, 0),
    snz_ethnicity_grp2_nbr = ifelse(
      eth1 == 2
      | (eth1 == 7 & eth2 == 2)
      | (eth1 == 7 & eth3 == 2), 1, 0),
    snz_ethnicity_grp3_nbr = ifelse(
      eth1 == 3
      | (eth1 == 7 & eth2 == 3)
      | (eth1 == 7 & eth3 == 3), 1, 0),
    snz_ethnicity_grp4_nbr = ifelse(
      eth1 == 4
      | (eth1 == 7 & eth2 == 4)
      | (eth1 == 7 & eth3 == 4), 1, 0),
    snz_ethnicity_grp5_nbr = ifelse(
      eth1 == 5
      | (eth1 == 7 & eth2 == 5)
      | (eth1 == 7 & eth3 == 5), 1, 0),
    snz_ethnicity_grp6_nbr = ifelse(
      eth1 == 6
      | (eth1 == 7 & eth2 == 6)
      | (eth1 == 7 & eth3 == 6), 1, 0)
  ) %>%
  select(-eth1, -eth2, -eth3)

#### snz_deceased_year_nbr ----
generated_table = generated_table %>%
  mutate(
    dur1 = rnorm(POP_SIZE, LIFE_EXPECTANCY_AVG, LIFE_EXPECTANCY_DEV),
    dur2 = randbetween(1, LIFE_EXPECTANCY_AVG, POP_SIZE),
    dur3 = runif(POP_SIZE)
  ) %>%
  mutate(life_span = ifelse(dur3 < PROB_EARLY_DEATH, dur2, dur1)) %>%
  mutate(snz_deceased_year_nbr = snz_birth_year_nbr + round(life_span)) %>%
  select(-dur1, -dur2, -dur3, -life_span)
  
#### snz_deceased_month_nbr ----
sampler = lookup %>%
  filter(column == "snz_deceased_month_nbr") %>%
  make_weighted_options_array()

generated_table = generated_table %>%
  mutate(snz_deceased_month_nbr = sample(sampler, POP_SIZE, replace = TRUE))

#### snz_parent ----
# snz_parent1_uid
# snz_parent2_uid
generated_table = generated_table %>%
  mutate(
    snz_parent1_uid = NA,
    snz_parent2_uid = NA
  )

for(ii in 1:nrow(generated_table)){
  my_birth_year = generated_table$snz_birth_year_nbr[ii]
  
  LB_parent = my_birth_year - FERTILE_MAX_AGE
  UB_parent = my_birth_year - FERTILE_MIN_AGE
  
  possible_parents = generated_table %>%
    filter(snz_birth_year_nbr >= LB_parent,
           snz_birth_year_nbr <= UB_parent)
  
  possible_p1 = possible_parents %>%
    filter(snz_sex_gender_code != 1) %>%
    select(snz_uid) %>%
    unlist() %>%
    append(NA)
  
  possible_p2 = possible_parents %>%
    filter(snz_sex_gender_code != 2) %>%
    select(snz_uid) %>%
    unlist() %>%
    append(NA)
  
  generated_table$snz_parent1_uid[ii] = sample(possible_p1)
  generated_table$snz_parent2_uid[ii] = sample(possible_p2)
}


FERTILE_MIN_AGE = 18
FERTILE_MAX_AGE = 37

## write out table ------------------------------------------------------------

generated_table %>%
  select(
    snz_uid,
    snz_sex_gender_code,
    snz_birth_year_nbr,
    snz_birth_month_nbr,
    snz_ethnicity_grp1_nbr,
    snz_ethnicity_grp2_nbr,
    snz_ethnicity_grp3_nbr,
    snz_ethnicity_grp4_nbr,
    snz_ethnicity_grp5_nbr,
    snz_ethnicity_grp6_nbr,
    snz_deceased_year_nbr,
    snz_deceased_month_nbr,
    snz_parent1_uid,
    snz_parent2_uid
  ) %>%
  write.csv(GENERATED_FILE, row.names = FALSE)

