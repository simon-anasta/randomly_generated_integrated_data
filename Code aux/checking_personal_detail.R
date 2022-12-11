setwd("~/Simon/Projects/Current projects/simulated_integrated_data")
source("~/Simon/Projects/Current projects/simulated_integrated_data/Code main/support_functions.R")

# test weighted generation fuction
lookup %>%
  filter(column = snz_sex_gender_code) %>%
  make_weighted_options_array()

# test randbetween
randbetween(1,5)
randbetween(4,8,3000) %>% table()
randbetween(104,108,3000) %>% table()
randbetween(104,108,30000) %>% table()

source("~/Simon/Projects/Current projects/simulated_integrated_data/Code main/generate_personal_detail.R")

# check ethnic distribution
qq = generated_table %>%
  group_by(
    eth1, eth2, eth3, 
    snz_ethnicity_grp1_nbr, 
    snz_ethnicity_grp2_nbr, 
    snz_ethnicity_grp3_nbr, 
    snz_ethnicity_grp4_nbr, 
    snz_ethnicity_grp5_nbr, 
    snz_ethnicity_grp6_nbr
    ) %>% 
  summarise(num = n())

write.csv(qq, "tmp.csv")

qq %>%
  filter(eth1 != 7) %>% 
  group_by(
    snz_ethnicity_grp1_nbr, 
    snz_ethnicity_grp2_nbr, 
    snz_ethnicity_grp3_nbr, 
    snz_ethnicity_grp4_nbr, 
    snz_ethnicity_grp5_nbr, 
    snz_ethnicity_grp6_nbr
    ) %>% 
 summarise(num = sum(num))

# trial life expectancy distributions
q1 = rnorm(10000, 70, 6) %>% round()
q2 = randbetween(1, 80, 10000)
q3 = q2;q3[runif(10000) < 0.9] = 100
qq = pmin(q1,q3)
hist(qq)

source("~/Simon/Projects/Current projects/simulated_integrated_data/Code main/generate_personal_detail.R")

# examine simulated life expectancy
qq = generated_table$snz_deceased_year_nbr - generated_table$snz_birth_year_nbr
hist(qq)

# check gender for parent 1
generated_table %>%
  semi_join(generated_table, by = c("snz_uid" = "snz_parent1_uid")) %>%
  group_by(snz_sex_gender_code) %>%
  summarise(num = n())

# check gender for parent 2
generated_table %>%
  semi_join(generated_table, by = c("snz_uid" = "snz_parent2_uid")) %>%
  group_by(snz_sex_gender_code) %>% 
  summarise(num = n())

# only parent 2
pp = generated_table %>%
  semi_join(generated_table, by = c("snz_uid" = "snz_parent2_uid"))
# parent 2 with children
qq = pp %>%
  left_join(generated_table, by = c("snz_uid" = "snz_parent2_uid"), suffix = c("_p","_c"))

# number of kids per person
qq %>%
  group_by(snz_uid) %>% 
  summarise(num_kids = n()) %>%
  group_by(num_kids) %>%
  summarise(num_people = n())

# parents age at birth of child
qq %>%
  mutate(age_at_birth = snz_birth_year_nbr_c - snz_birth_year_nbr_p) %>% 
  group_by(age_at_birth) %>%
  summarise(num = n())

# parent alive at time of birth
qq %>%
  mutate(prob = ifelse(snz_birth_year_nbr_c < snz_deceased_year_nbr_p, 1, 0)) %>%
  group_by(prob) %>%
  summarise(num = n())
