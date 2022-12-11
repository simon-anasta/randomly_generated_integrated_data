# Schema: moh_clean
# Table: pub_fund_hosp_discharges_event
#
# Output columns:
#
# moh_dia_event_id_nbr
# moh_dia_clinical_sys_code
# moh_dia_submitted_system_code
# moh_dia_diagnosis_type_code
# moh_dia_clinical_code
# moh_dia_op_date
# moh_dia_op_flag_ind
#

## parameters -----------------------------------------------------------------

# controls
SEED = 123

# input/output
LOOKUP_FILE = "./Lookups/[moh_clean]_[pub_fund_hosp_discharges_diag].csv"
REFERENCE_FILE = "./Generated/[moh_clean]_[pub_fund_hosp_discharges_event].csv"
GENERATED_FILE = "./Generated/[moh_clean]_[pub_fund_hosp_discharges_diag].csv"

# attributes
MIN_YEAR = 2002
MAX_YEAR = 2019

ICD_TRANSITION_START = 2003
ICD_TRANITION_LENGTH = 5

PROB_B = 0.1
PROB_O = 0.4
PROB_E = 0.2

OPERATION_RANGE = c(rep(1, 5), rep(2, 3))

## setup ----------------------------------------------------------------------

library(dplyr)
library(lubridate)
source("./Code main/support_functions.R")
set.seed(seed = SEED)

lookup = read.csv(LOOKUP_FILE, stringsAsFactors = FALSE) %>%
  filter(schema == "moh_clean", table == "pub_fund_hosp_discharges_diag")

events = read.csv(REFERENCE_FILE, stringsAsFactors = FALSE) %>%
  select(moh_evt_event_id_nbr, moh_evt_evst_date, moh_evt_even_date)

ICD9_lookup = lookup %>%
  filter(from == "ICD9") %>%
  select(type, value = from_code, weight)

ICD10_lookup = lookup %>%
  filter(from == "ICD10") %>%
  select(type, value = from_code, weight)

## generate type --------------------------------------------------------------

table_size = nrow(events)

events = events %>%
  mutate(
    A_type = 1,
    B_type = ifelse(runif(table_size) < PROB_B, 1, 0),
    E_type = ifelse(runif(table_size) < PROB_E, 1, 0),
    O_type = ifelse(runif(table_size) < PROB_O, 1, 0),
  ) %>%
  mutate(
    PROB_ICD10 = pmax(0,
                      pmin(1,
                           (year(moh_evt_evst_date) - ICD_TRANSITION_START) / ICD_TRANITION_LENGTH
                      )),
    coded_in_ICD10 = ifelse(runif(table_size) < PROB_ICD10, 1, 0)
  )

gen_A = events %>%
  filter(A_type == 1) %>%
  select(moh_evt_event_id_nbr, coded_in_ICD10)

gen_B = events %>%
  filter(B_type == 1) %>%
  select(moh_evt_event_id_nbr, coded_in_ICD10)

gen_E = events %>%
  filter(E_type == 1) %>%
  select(moh_evt_event_id_nbr, coded_in_ICD10)

gen_O = events %>%
  filter(O_type == 1) %>%
  select(moh_evt_event_id_nbr, coded_in_ICD10, moh_evt_evst_date)

## generate A type - main diagnosis -------------------------------------------

table_size = nrow(gen_A)

ICD9_sampler = ICD9_lookup %>%
  filter(type == "A") %>%
  make_weighted_options_array()

ICD10_sampler = ICD10_lookup %>%
  filter(type == "A") %>%
  make_weighted_options_array()

gen_A = gen_A %>%
  mutate(
    ICD9_code = sample(ICD9_sampler, table_size, replace = TRUE),
    ICD10_code = sample(ICD10_sampler, table_size, replace = TRUE)
  ) %>%
  mutate(
    moh_dia_clinical_code = ifelse(coded_in_ICD10 == 1, ICD10_code, ICD9_code),
    moh_dia_diagnosis_type_code = "A"
  )

gen_A = gen_A %>%
  inner_join(lookup, by = c("moh_dia_clinical_code" = "from_code",
                            "moh_dia_diagnosis_type_code" = "type"))

## generate B type - additional diagnosis -------------------------------------

table_size = nrow(gen_B)

ICD9_sampler = ICD9_lookup %>%
  filter(type == "B") %>%
  make_weighted_options_array()

ICD10_sampler = ICD10_lookup %>%
  filter(type == "B") %>%
  make_weighted_options_array()

gen_B = gen_B %>%
  mutate(
    ICD9_code = sample(ICD9_sampler, table_size, replace = TRUE),
    ICD10_code = sample(ICD10_sampler, table_size, replace = TRUE)
  ) %>%
  mutate(
    moh_dia_clinical_code = ifelse(coded_in_ICD10 == 1, ICD10_code, ICD9_code),
    moh_dia_diagnosis_type_code = "B"
  )

gen_B = gen_B %>%
  inner_join(lookup, by = c("moh_dia_clinical_code" = "from_code",
                            "moh_dia_diagnosis_type_code" = "type"))

## generate E type - external cause -------------------------------------------

table_size = nrow(gen_E)

ICD9_sampler = ICD9_lookup %>%
  filter(type == "E") %>%
  make_weighted_options_array()

ICD10_sampler = ICD10_lookup %>%
  filter(type == "E") %>%
  make_weighted_options_array()

gen_E = gen_E %>%
  mutate(
    ICD9_code = sample(ICD9_sampler, table_size, replace = TRUE),
    ICD10_code = sample(ICD10_sampler, table_size, replace = TRUE)
  ) %>%
  mutate(
    moh_dia_clinical_code = ifelse(coded_in_ICD10 == 1, ICD10_code, ICD9_code),
    moh_dia_diagnosis_type_code = "E"
  )

gen_E = gen_E %>%
  inner_join(lookup, by = c("moh_dia_clinical_code" = "from_code",
                            "moh_dia_diagnosis_type_code" = "type"))

## generate O type - operation ------------------------------------------------

table_size = nrow(gen_O)

ICD9_sampler = ICD9_lookup %>%
  filter(type == "O") %>%
  make_weighted_options_array()

ICD10_sampler = ICD10_lookup %>%
  filter(type == "O") %>%
  make_weighted_options_array()

gen_O = gen_O %>%
  mutate(
    ICD9_code = sample(ICD9_sampler, table_size, replace = TRUE),
    ICD10_code = sample(ICD10_sampler, table_size, replace = TRUE)
  ) %>%
  mutate(
    moh_dia_clinical_code = ifelse(coded_in_ICD10 == 1, ICD10_code, ICD9_code),
    moh_dia_diagnosis_type_code = "O"
  )

gen_O = gen_O %>%
  inner_join(lookup, by = c("moh_dia_clinical_code" = "from_code",
                            "moh_dia_diagnosis_type_code" = "type"))

# operation ate
gen_O = gen_O %>%
  mutate(
    days = sample(OPERATION_RANGE, table_size, replace = TRUE),
    moh_dia_op_date = as.Date(moh_evt_evst_date) + days,
    moh_dia_op_flag_ind = 1
  )


## combine generated tables ---------------------------------------------------

generated_data = bind_rows(
  gen_A %>%
    select(
      moh_dia_event_id_nbr = moh_evt_event_id_nbr,
      from = from,
      to = from,
      moh_dia_diagnosis_type_code = moh_dia_diagnosis_type_code,
      moh_dia_clinical_code = moh_dia_clinical_code
    ),
  gen_A %>%
    select(
      moh_dia_event_id_nbr = moh_evt_event_id_nbr,
      from = from,
      to = to,
      moh_dia_diagnosis_type_code = moh_dia_diagnosis_type_code,
      moh_dia_clinical_code = to_code
    ),
  gen_B %>%
    select(
      moh_dia_event_id_nbr = moh_evt_event_id_nbr,
      from = from,
      to = from,
      moh_dia_diagnosis_type_code = moh_dia_diagnosis_type_code,
      moh_dia_clinical_code = moh_dia_clinical_code
    ),
  gen_B %>%
    select(
      moh_dia_event_id_nbr = moh_evt_event_id_nbr,
      from = from,
      to = to,
      moh_dia_diagnosis_type_code = moh_dia_diagnosis_type_code,
      moh_dia_clinical_code = to_code
    ),
  gen_E %>%
    select(
      moh_dia_event_id_nbr = moh_evt_event_id_nbr,
      from = from,
      to = from,
      moh_dia_diagnosis_type_code = moh_dia_diagnosis_type_code,
      moh_dia_clinical_code = moh_dia_clinical_code
    ),
  gen_E %>%
    select(
      moh_dia_event_id_nbr = moh_evt_event_id_nbr,
      from = from,
      to = to,
      moh_dia_diagnosis_type_code = moh_dia_diagnosis_type_code,
      moh_dia_clinical_code = to_code
    ),
  gen_O %>%
    select(
      moh_dia_event_id_nbr = moh_evt_event_id_nbr,
      from = from,
      to = from,
      moh_dia_diagnosis_type_code = moh_dia_diagnosis_type_code,
      moh_dia_clinical_code = moh_dia_clinical_code
    ),
  gen_O %>%
    select(
      moh_dia_event_id_nbr = moh_evt_event_id_nbr,
      from = from,
      to = to,
      moh_dia_diagnosis_type_code = moh_dia_diagnosis_type_code,
      moh_dia_clinical_code = to_code,
      moh_dia_op_date,
      moh_dia_op_flag_ind
    )
)

## systems labels -------------------------------------------------------------

generated_data = generated_data %>%
  mutate(
    moh_dia_submitted_system_code = ifelse(from == "ICD9", 6, 11),
    moh_dia_clinical_sys_code = ifelse(to == "ICD9", 6, 11)
  )

## write out table ------------------------------------------------------------

generated_data %>%
  select(
    moh_dia_event_id_nbr,
    moh_dia_clinical_sys_code,
    moh_dia_submitted_system_code,
    moh_dia_diagnosis_type_code,
    moh_dia_clinical_code,
    moh_dia_op_date,
    moh_dia_op_flag_ind
  ) %>%
  write.csv(GENERATED_FILE, row.names = FALSE)
