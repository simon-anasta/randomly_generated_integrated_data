setwd("~/Simon/Projects/Current projects/simulated_integrated_data")
source("~/Simon/Projects/Current projects/simulated_integrated_data/Code main/support_functions.R")
source("~/Simon/Projects/Current projects/simulated_integrated_data/Code main/generate_ird_ems.R")

colnames(generated_table)
colnames(g2)
library(lubridate)

# check histograms
hist(generated_table$snz_uid)
hist(generated_table$snz_deceased_year_nbr)
hist(generated_table$stable_remaining)
hist(generated_table$year_start)
hist(generated_table$year_swap)
barplot(table(generated_table$open_job))
barplot(table(generated_table$tax_code))
hist(generated_table$year)
barplot(table(generated_table$month))
hist(generated_table$snz_employer_ird_uid)

hist(g2$snz_uid)
hist(g2$snz_deceased_year_nbr)
hist(g2$stable_remaining)
hist(g2$year_start)
hist(g2$year_swap)
barplot(table(g2$open_job))
barplot(table(g2$tax_code))
barplot(table(g2$year))
barplot(table(g2$month))
hist(g2$snz_employer_ird_uid)
barplot(table(g2$return_date))
hist(g2$ir_ems_gross_earnings_amt)
hist(g2$ir_ems_paye_deductions_amt)
hist(g2$ir_ems_earnings_not_liable_amt)
hist(g2$ir_ems_fstc_amt)
hist(g2$ir_ems_sl_amt)
hist(g2$snz_ird_uid)
hist(g2$weight)
hist(g2$ir_ems_employer_location_nbr)
hist(g2$ir_ems_enterprise_nbr)
hist(g2$ir_ems_pbn_nbr)
barplot(table(g2$ir_ems_pbn_anzsic06_code))
barplot(table(g2$ir_ems_ent_anzsic06_code))
barplot(table(g2$ir_ems_line_nbr))
barplot(table(g2$ir_ems_snz_unique_nbr))
barplot(table(g2$ir_ems_version_nbr))
barplot(table(g2$ir_ems_doc_lodge_prefix_nbr))
barplot(table(g2$ir_ems_doc_lodge_nbr))
barplot(table(g2$ir_ems_doc_lodge_suffix_nbr))
barplot(table(g2$ir_ems_gross_earnings_imp_code))
barplot(table(g2$ir_ems_paye_imp_ind))
barplot(table(g2$ir_ems_earnings_not_liab_imp_ind))
barplot(table(g2$ir_ems_return_line_item_code))
barplot(table(g2$ir_ems_withholding_type_code))
barplot(table(g2$ir_ems_lump_sum_ind))

# uniqueness of records person-month
g2 %>%
  group_by(snz_uid, return_date) %>%
  summarise(num = n(), .groups = 'drop') %>%
  group_by(num) %>%
  summarise(num_people = n(), .groups = 'drop') %>%
  arrange(num) %>%
  print()

# uniqueness of records person-month-employer (not quite unique)
g2 %>%
  group_by(snz_uid, return_date, snz_employer_ird_uid) %>%
  summarise(num = n(), .groups = 'drop') %>%
  group_by(num) %>%
  summarise(num_people = n(), .groups = 'drop') %>%
  arrange(num) %>%
  print()

# earn while dead (noone)
g2 %>%
  mutate(earn_while_dead = ifelse(snz_deceased_year_nbr < year, 1, 0)) %>%
  select(earn_while_dead) %>%
  unlist() %>%
  table() %>%
  barplot()

# annual income distribution
df = g2 %>%
  filter(open_job != 'none') %>%
  group_by(snz_uid, year) %>%
  summarise(total_income = sum(ir_ems_gross_earnings_amt), .groups = 'drop')

df %>%
  select(total_income) %>%
  filter(total_income < 1E5) %>%
  unlist() %>%
  hist()

# annual income distribution by year
library(ggplot2)
df_2000 = df[df$year == 2000,]
df_2005 = df[df$year == 2005,]
df_2010 = df[df$year == 2010,]
df_2015 = df[df$year == 2015,]
df_2020 = df[df$year == 2020,]

ggplot() +
  geom_histogram(aes(x = total_income), data = df_2000, alpha = 0.1, color = 'grey') +
  geom_histogram(aes(x = total_income), data = df_2005, alpha = 0.1, color = 'green') +
  geom_histogram(aes(x = total_income), data = df_2010, alpha = 0.1, color = 'red') +
  geom_histogram(aes(x = total_income), data = df_2015, alpha = 0.1, color = 'blue') +
  geom_histogram(aes(x = total_income), data = df_2020, alpha = 0.1, color = 'black')

ggplot() +
  geom_density(aes(x = total_income), data = df_2000, alpha = 0.1, color = 'grey') +
  geom_density(aes(x = total_income), data = df_2005, alpha = 0.1, color = 'green') +
  geom_density(aes(x = total_income), data = df_2010, alpha = 0.1, color = 'red') +
  geom_density(aes(x = total_income), data = df_2015, alpha = 0.1, color = 'blue') +
  geom_density(aes(x = total_income), data = df_2020, alpha = 0.1, color = 'black')

# monthly income distribution by type
df = g2 %>%
  select(open_job, ir_ems_gross_earnings_amt)

ggplot() +
  geom_density(aes(x = ir_ems_gross_earnings_amt, color = open_job), data = df)

# uid comparison
plot(g2$snz_uid, g2$snz_ird_uid)
plot(g2$snz_uid, g2$snz_employer_ird_uid)

# monthly change
df = g2 %>%
  filter(tax_code == "M") %>%
  group_by(snz_uid) %>%
  arrange(return_date) %>%
  mutate(
    lag_job = lag(open_job),
    lag_month = lag(month),
    lag_employer = lag(snz_employer_ird_uid)
  ) %>%
  filter(
    !is.na(lag_job),
    !is.na(lag_month),
    !is.na(lag_employer)
  ) %>%
  mutate(
    same_job = lag_job == open_job,
    same_month = (lag_month + 1) %% 12 == month %% 12,
    same_employer = lag_employer == snz_employer_ird_uid
  )

df %>%
  group_by(same_job, same_month, same_employer) %>%
  summarise(num = n(), .groups = "drop") %>%
  arrange(num) %>%
  print()
# this distribution is reasonable
#
# same_job same_month same_employer   num
# <lgl>    <lgl>      <lgl>         <int>
#   1 FALSE    FALSE      TRUE              2
# 2 TRUE     FALSE      TRUE              5
# 3 FALSE    TRUE       TRUE             15
# 4 FALSE    FALSE      FALSE           122
# 5 TRUE     FALSE      FALSE           128
# 6 FALSE    TRUE       FALSE          1191
# 7 TRUE     TRUE       FALSE         13308
# 8 TRUE     TRUE       TRUE          39358



