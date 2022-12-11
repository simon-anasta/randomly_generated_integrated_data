setwd("~/Simon/Projects/Current projects/randomly_generated_integrated_data")
source("~/Simon/Projects/Current projects/randomly_generated_integrated_data/Code main/generate_pub_fund_hosp_discharges_diag.R")


barplot(table(generated_data$moh_dia_event_id_nbr))
barplot(table(generated_data$from))
barplot(table(generated_data$to))
barplot(table(generated_data$moh_dia_diagnosis_type_code))
barplot(table(generated_data$moh_dia_clinical_code))
barplot(table(generated_data$moh_dia_op_date))
barplot(table(generated_data$moh_dia_op_flag_ind))
barplot(table(generated_data$moh_dia_submitted_system_code))
barplot(table(generated_data$moh_dia_clinical_sys_code))

# number of types per code
generated_data %>%
  group_by(moh_dia_event_id_nbr, moh_dia_clinical_sys_code) %>%
  summarise(num = n()) %>%
  group_by(num) %>%
  summarise(num_events = n())

# duplicate types = none
generated_data %>%
  group_by(moh_dia_event_id_nbr, moh_dia_clinical_sys_code, moh_dia_diagnosis_type_code) %>%
  summarise(num = n()) %>%
  group_by(num) %>%
  summarise(num_events = n())

# same code, different type = yes, can occur
generated_data %>%
  group_by(moh_dia_clinical_code) %>%
  summarise(num = n_distinct(moh_dia_diagnosis_type_code)) %>%
  group_by(num) %>%
  summarise(num_events = n())



