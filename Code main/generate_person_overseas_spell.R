# Schema: data
# Table: person_overseas_spell
#
# Output columns:
#
# snz_uid
# pos_applied_date
# pos_ceased_date
# pos_day_span_nbr
# pos_first_arrival_ind
# pos_last_departure_ind
# pos_source_code
#

## parameters -----------------------------------------------------------------

# controls
SEED = 123

# input/output
LOOKUP_FILE = "./Lookups/[data]_[person_overseas_spell].csv"
REFERENCE_FILE = "./Reference/residence.csv"
GENERATED_FILE = "./Generated/[data]_[person_overseas_spell].csv"

# attributes
RES_PROB_OVERSEAS = 0.003
RES_MIN_OVERSEAS = 3
RES_MAX_OVERSEAS = 23
VIS_PROB_NZ = 0.05
VIS_MIN_NZ = 3
VIS_MAX_NZ = 162
IMMIGRATE_PRE_PROB = 0.1
IMMIGRATE_MIN_YEAR = 1980
IMMIGRATE_MAX_YEAR = 2000



## setup ----------------------------------------------------------------------

library(dplyr)
source("./Code main/support_functions.R")
set.seed(seed = SEED)

lookup = read.csv(LOOKUP_FILE, stringsAsFactors = FALSE) %>%
  filter(schema == "data", table == "person_overseas_spell")

base_population = read.csv(REFERENCE_FILE, stringsAsFactors = FALSE)

## generate -------------------------------------------------------------------

#### residents going overseas ----

res_overseas = base_population %>%
  filter(is_resident == 1)

table_size = nrow(res_overseas)

res_overseas = res_overseas %>%
  mutate(pp = runif(table_size)) %>%
  filter(pp < RES_PROB_OVERSEAS)

table_size = nrow(res_overseas)

res_overseas = res_overseas %>%
  mutate(
    month = randbetween(1,12,table_size),
    day = randbetween(1,28,table_size),
    pos_day_span_nbr = randbetween(RES_MIN_OVERSEAS, RES_MAX_OVERSEAS, table_size)
  ) %>%
  mutate(
    date_str = paste0(the_year,"-",month,"-",day),
    pos_applied_date = lubridate::ymd(date_str),
    pos_ceased_date = pos_applied_date + pos_day_span_nbr
  ) %>%
  select(snz_uid, pos_applied_date, pos_ceased_date, pos_day_span_nbr)
  
#### non-residents visiting ----

nonres_visit = base_population %>%
  filter(is_resident == 0)

table_size = nrow(nonres_visit)

nonres_visit = nonres_visit %>%
  mutate(pp = runif(table_size)) %>%
  filter(pp < VIS_PROB_NZ)

table_size = nrow(nonres_visit)

nonres_visit = nonres_visit %>%
  mutate(
    month = randbetween(1,12,table_size),
    day = randbetween(1,28,table_size),
    visit_length = randbetween(VIS_MIN_NZ, VIS_MAX_NZ, table_size)
  ) %>%
  mutate(
    date_str = paste0(the_year,"-",month,"-",day),
    visit_start = lubridate::ymd(date_str),
    visit_end = visit_start + visit_length
  ) %>%
  select(snz_uid, visit_start, visit_end, visit_length)

#### visit spells to overseas spells ----

prep_nonres_overseas = nonres_visit %>%
  group_by(snz_uid) %>%
  arrange(visit_start) %>%
  mutate(prev_end = lag(visit_end),
         next_start = lead(visit_start))

nonres_overseas_1 = prep_nonres_overseas %>%
  select(
    snz_uid, 
    pos_applied_date = prev_end,
    pos_ceased_date = visit_start
  ) %>%
  mutate(pos_applied_date = coalesce(pos_applied_date, as.Date("1900-01-01")))
    
nonres_overseas_2 = prep_nonres_overseas %>%
  filter(is.na(next_start)) %>%
  mutate(next_start = as.Date('9999-12-31')) %>%
  select(
    snz_uid,
    pos_applied_date = visit_end,
    pos_ceased_date = next_start
  )

nonres_overseas = bind_rows(
  nonres_overseas_1,
  nonres_overseas_2
) %>%
  mutate(
    pos_day_span_nbr = as.numeric(pos_ceased_date - pos_applied_date)
  )

#### non-res becoming res ----

nonres_to_res = base_population %>%
  filter(is_resident == 1) %>%
  group_by(snz_uid) %>%
  summarise(earliest_res = min(the_year))

table_size = nrow(nonres_to_res)

nonres_to_res = nonres_to_res %>%
  mutate(
    pp = runif(table_size),
    rand_year = randbetween(IMMIGRATE_MIN_YEAR, 
                            IMMIGRATE_MAX_YEAR, 
                            table_size)
  ) %>%
  filter(
    earliest_res > IMMIGRATE_MAX_YEAR
    | pp < IMMIGRATE_PRE_PROB
  ) %>%
  mutate(
    immigrate_year = ifelse(
      earliest_res > IMMIGRATE_MAX_YEAR, earliest_res, rand_year
    )
  )

table_size = nrow(nonres_to_res)

nonres_to_res = nonres_to_res %>%
  mutate(
    month = randbetween(1,12,table_size),
    day = randbetween(1,28,table_size),
    date_str = paste0(immigrate_year,"-",month,"-",day),
    pos_applied_date = lubridate::ymd("1900-01-01"),
    pos_ceased_date = lubridate::ymd(date_str),
    pos_day_span_nbr = as.numeric(pos_ceased_date - pos_applied_date)
  ) %>%
  select(snz_uid, pos_applied_date, pos_ceased_date, pos_day_span_nbr)

#### res becoming non-res ----

res_to_nonres = base_population %>%
  filter(is_resident == 1) %>%
  group_by(snz_uid) %>%
  summarise(migrate_year = max(the_year))

table_size = nrow(res_to_nonres)

res_to_nonres = res_to_nonres %>%
  mutate(
    month = randbetween(1,12,table_size),
    day = randbetween(1,28,table_size),
    date_str = paste0(migrate_year,"-",month,"-",day),
    pos_applied_date = lubridate::ymd(date_str),
    pos_ceased_date = lubridate::ymd("9999-12-31"),
    pos_day_span_nbr = as.numeric(pos_ceased_date - pos_applied_date)
  ) %>%
  select(snz_uid, pos_applied_date, pos_ceased_date, pos_day_span_nbr)

## combine to single table ----------------------------------------------------

generated_table = bind_rows(
  res_overseas,
  res_to_nonres,
  nonres_overseas,
  nonres_to_res
) %>%
  mutate(
    pos_first_arrival_ind = ifelse(lubridate::year(pos_applied_date) <= 1900, 'y', 'n'),
    pos_last_departure_ind = ifelse(lubridate::year(pos_ceased_date) >= 9000, 'y', 'n')
  )

#### pos_source_code ----
table_size = nrow(generated_table)

sampler = lookup %>%
  filter(column == "pos_source_code") %>%
  make_weighted_options_array()

generated_table = generated_table %>%
  mutate(pos_source_code = sample(sampler, table_size, replace = TRUE))


## write out table ------------------------------------------------------------

generated_table %>%
  select(
    snz_uid,
    pos_applied_date,
    pos_ceased_date,
    pos_day_span_nbr,
    pos_first_arrival_ind,
    pos_last_departure_ind,
    pos_source_code
  ) %>%
  write.csv(GENERATED_FILE, row.names = FALSE)
