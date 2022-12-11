setwd("~/Simon/Projects/Current projects/simulated_integrated_data")
source("~/Simon/Projects/Current projects/simulated_integrated_data/Code main/support_functions.R")
source("~/Simon/Projects/Current projects/simulated_integrated_data/Code main/generate_person_overseas_spell.R")

colnames(generated_table)
library(lubridate)

# check histograms
hist(generated_table$snz_uid)
# hist(generated_table$pos_applied_date)
hist(year(generated_table$pos_applied_date))
hist(month(generated_table$pos_applied_date))
hist(day(generated_table$pos_applied_date))
# hist(generated_table$pos_ceased_date)
hist(year(generated_table$pos_ceased_date))
hist(month(generated_table$pos_ceased_date))
hist(day(generated_table$pos_ceased_date))
hist(generated_table$pos_day_span_nbr)

table(month(generated_table$pos_applied_date))
table(day(generated_table$pos_applied_date))
table(generated_table$pos_first_arrival_ind)
table(generated_table$pos_last_departure_ind)
table(generated_table$pos_source_code)

# final year of residence in NZ
base_population %>%
  filter(is_resident == 1) %>%
  group_by(snz_uid) %>%
  summarise(max_year = max(the_year)) %>%
  group_by(max_year) %>% 
  summarise(num = n()) %>% 
  View()

# how many people solely in NZ
generated_table %>%
  mutate(has_first = ifelse(pos_first_arrival_ind == 'y', 1, 0),
         has_last = ifelse(pos_last_departure_ind == 'y', 1, 0)) %>%
  group_by(snz_uid) %>%
  summarise(has_first = max(has_first), has_last = max(has_last)) %>%
  group_by(has_first, has_last) %>%
  summarise(num = n())


# anyone with concurrent events
generated_table %>%
  inner_join(generated_table, by = 'snz_uid', suffix = c("_x","_y")) %>%
  filter(pos_applied_date_x <= pos_ceased_date_y,
         pos_applied_date_y <= pos_ceased_date_x,
         pos_applied_date_x != pos_applied_date_y,
         pos_ceased_date_x != pos_ceased_date_y) %>%
  select(snz_uid) %>%
  distinct() %>%
  nrow()
# 2130204, 2915759
# one remains unresolved, for known reasons

# first date matches
lhs = generated_table %>%
  filter(pos_first_arrival_ind == 'y') %>%
  mutate(ceased_year = year(pos_ceased_date)) %>%
  select(snz_uid, ceased_year)

rhs = base_population %>%
  filter(is_resident == 1) %>%
  group_by(snz_uid) %>%
  summarise(first_year = min(the_year))

full_join(lhs, rhs, by = "snz_uid") %>% View()

full_join(lhs, rhs, by = "snz_uid") %>% 
  group_by(ceased_year, first_year) %>% 
  summarise(num = n()) %>%
  write.csv("tmp.csv")

full_join(lhs, rhs, by = "snz_uid") %>% 
  filter(abs(ceased_year - first_year) > 2) %>%
  View()
# 7062928, 6895986, 2130204

generated_table %>%
  filter(snz_uid %in% c(7062928, 6895986, 2130204)) %>%
  View()

# find resident with the most events
pop = base_population %>%
  filter(is_resident == 1) %>%
  select(snz_uid) %>%
  distinct()

generated_table %>%
  semi_join(pop, by = "snz_uid") %>%
  group_by(snz_uid) %>%
  summarise(num = n()) %>%
  arrange(-num) %>%
  head(1)

base_population %>%
  filter(snz_uid %in% c(1343304, 3246965)) %>%
  arrange(snz_uid, the_year) %>%
  View()

generated_table %>%
  filter(snz_uid %in% c(1343304, 3246965)) %>%
  arrange(snz_uid, pos_applied_date) %>%
  View()

# find non-resident with the most events
pop = base_population %>%
  group_by(snz_uid) %>%
  summarise(num = sum(is_resident)) %>%
  filter(num == 0)

generated_table %>%
  semi_join(pop, by = "snz_uid") %>%
  group_by(snz_uid) %>%
  summarise(num = n()) %>%
  arrange(-num) %>%
  head(1)

generated_table %>%
  filter(snz_uid %in% c(5044356, 8577026, 780420)) %>%
  arrange(snz_uid, pos_applied_date) %>%
  View()

# find full resident with the most events
pop = base_population %>%
  group_by(snz_uid) %>%
  summarise(num = sum(is_resident)) %>%
  filter(num == 21)

generated_table %>%
  semi_join(pop, by = "snz_uid") %>%
  group_by(snz_uid) %>%
  summarise(num = n()) %>%
  arrange(-num) %>%
  head(1)

generated_table %>%
  filter(snz_uid %in% c(1919228, 679467)) %>%
  arrange(snz_uid, pos_applied_date) %>%
  View()
