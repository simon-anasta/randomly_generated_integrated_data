setwd("~/Simon/Projects/Current projects/simulated_integrated_data")
source("~/Simon/Projects/Current projects/simulated_integrated_data/Code main/support_functions.R")
source("~/Simon/Projects/Current projects/simulated_integrated_data/Code main/generate_employers.R")

# check histograms
hist(generated_table$size)
hist(generated_table$ird_uid)
hist(generated_table$ent_uid)
hist(generated_table$pbn_uid)
hist(generated_table$own_pbn)
hist(as.numeric(generated_table$ir_ems_employer_location_nbr))

# check pattern by industry
qq = generated_table %>%
  group_by(anzsic06) %>%
  summarise(num = n(),
            range = max(size) - min(size),
            dev = sd(size),
            min = min(size),
            max = max(size))

hist(qq$num)
hist(qq$range)
hist(qq$dev)
hist(qq$min)
hist(qq$max)
