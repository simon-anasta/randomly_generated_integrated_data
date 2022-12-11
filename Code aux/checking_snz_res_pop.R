setwd("~/Simon/Projects/Current projects/simulated_integrated_data")
source("~/Simon/Projects/Current projects/simulated_integrated_data/Code main/support_functions.R")
source("~/Simon/Projects/Current projects/simulated_integrated_data/Code main/generate_snz_res_pop.R")

# check histograms
hist(generated_table$start_year)
hist(generated_table$end_year)
hist(generated_table$the_year)
hist(generated_table$is_resident)
hist(generated_table$srp_flag_ir_ind)
hist(generated_table$srp_flag_ed_ind)
hist(generated_table$srp_flag_health_ind)
hist(generated_table$srp_flag_acc_ind)
hist(generated_table$srp_flag_under5_ind)

table(generated_table$srp_ref_date)
table(generated_table$the_year)

# check pattern over time
generated_table %>%
  group_by(snz_uid) %>%
  arrange(the_year) %>%
  mutate(
    lead1 = lead(is_resident, 1),
    lead2 = lead(is_resident, 2),
    lead3 = lead(is_resident, 3),
    lead4 = lead(is_resident, 4)
  ) %>%
  filter(!is.na(lead1),!is.na(lead2),!is.na(lead3),!is.na(lead4)) %>%
  group_by(lead1, lead2, lead3, lead4) %>%
  summarise(num = n())

# total number of residents
generated_table %>%
  group_by(the_year) %>%
  summarise(num = sum(is_resident))

# under5
generated_table %>%
  group_by(snz_birth_year_nbr, the_year, srp_flag_under5_ind) %>%
  summarise(num = n()) %>%
  View()

  