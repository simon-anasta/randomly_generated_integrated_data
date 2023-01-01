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

link_bins = round(POP_SIZE / 1000)

generated_table = generated_table %>%
  mutate(linker = randbetween(1, link_bins, POP_SIZE))

# child
possible_child_table = generated_table %>%
  select(snz_uid, snz_birth_year_nbr, linker)

# first parent
possible_parent1_table = generated_table %>%
  filter(snz_sex_gender_code != 1) %>%
  mutate(
    min_birth_year = snz_birth_year_nbr + FERTILE_MIN_AGE,
    max_birth_year = snz_birth_year_nbr + FERTILE_MAX_AGE
  ) %>%
  select(snz_uid, min_birth_year, max_birth_year, snz_deceased_year_nbr, linker)

possible_child_parent1 = possible_child_table %>%
  left_join(possible_parent1_table, by = "linker", suffix = c("_c","_p1")) %>%
  filter(
    !is.na(snz_uid_p1),
    min_birth_year <= snz_birth_year_nbr,
    snz_birth_year_nbr <= max_birth_year,
    snz_birth_year_nbr < snz_deceased_year_nbr
  )

chosen_parent1 = possible_child_parent1 %>%
  mutate(
    rand = runif(nrow(possible_child_parent1))
    ) %>%
  group_by(snz_uid_c) %>%
  mutate(min_rand = min(rand)) %>%
  filter(rand == min_rand) %>%
  select(snz_uid = snz_uid_c, snz_parent1_uid = snz_uid_p1)
           
# second parent
possible_parent2_table = generated_table %>%
  filter(snz_sex_gender_code != 2) %>%
  mutate(
    min_birth_year = snz_birth_year_nbr + FERTILE_MIN_AGE,
    max_birth_year = snz_birth_year_nbr + FERTILE_MAX_AGE
  ) %>%
  select(snz_uid, min_birth_year, max_birth_year, snz_deceased_year_nbr, linker)

possible_child_parent2 = possible_child_table %>%
  left_join(possible_parent2_table, by = "linker", suffix = c("_c","_p2")) %>%
  filter(
    !is.na(snz_uid_p2),
    min_birth_year <= snz_birth_year_nbr,
    snz_birth_year_nbr <= max_birth_year,
    snz_birth_year_nbr < snz_deceased_year_nbr
  )

chosen_parent2 = possible_child_parent2 %>%
  mutate(
    rand = runif(nrow(possible_child_parent2))
  ) %>%
  group_by(snz_uid_c) %>%
  mutate(min_rand = min(rand)) %>%
  filter(rand == min_rand) %>%
  select(snz_uid = snz_uid_c, snz_parent2_uid = snz_uid_p2)

# add to generated data
generated_table = generated_table %>%
  left_join(chosen_parent1, by = "snz_uid") %>%
  left_join(chosen_parent2, by = "snz_uid") %>%
  select(-linker)

rm(
  "possible_child_parent1",
  "possible_child_parent2",
  "possible_child_table",
  "possible_parent1_table",
  "possible_parent2_table",
  "chosen_parent1",
  "chosen_parent2"
)

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

