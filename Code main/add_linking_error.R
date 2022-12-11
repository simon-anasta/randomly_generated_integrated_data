#
# files and IDs to mix:
#
# [data]_[person_overseas_spell].csv
# snz_uid
# 
# [data]_[personal_detail].csv
# snz_uid
# 
# [data]_[snz_res_pop].csv
# snz_uid
# 
# [ir_clean]_[ird_ems].csv
# snz_uid
# snz_ird_uid
# 
# [moh_clean]_[pub_fund_hosp_discharges_event].csv
# snz_uid
# snz_moh_uid
# 
# [nzta_clean]_[dlr_historic].csv
# [nzta_clean]_[drivers_licence_register].csv
# snz_uid
# snz_nzta_uid
#

## parameters -----------------------------------------------------------------

# controls
SEED = 123

# input/output
GENERATED_FOLDER = "./Generated"
FINAL_FOLDER = "./Final datasets"

# attributes
BREAK_PROB = 0.008 # 0.8%
MIX_PROB = 0.011 # 1.1% --> doubles because of swapping IDs
BREAK_MIN = 10
BREAK_MAX = 210


## setup ----------------------------------------------------------------------

library(dplyr)
source("./Code main/support_functions.R")
set.seed(seed = SEED)

## shuffle and break links function --------------------------------------------
# input: df_in = the data.frame to modify
# input: these_columns = the names of the columns that contain IDs to shuffle.
#        shuffling is consistent for IDs - if to columns are provided then
#        shuffling takes place with pairs of IDs
#
# output: a data.frame with old IDs and new IDs
#         shuffling can be done by joining on the old IDs and keeping just the
#         new IDs.
#

produce_shuffle_ids <- function(df_in, these_columns) {
  
  # get data.frame of unique ids
  id_df = df_in %>%
    select(all_of(these_columns)) %>%
    distinct()
  
  num_ids = nrow(id_df)
  
  # mix
  id_df = id_df %>%
    mutate(
      row_num_in = 1:num_ids,
      row_num_out = 1:num_ids,
      to_mix = runif(num_ids) < MIX_PROB
    )
  
  mixer_in = id_df$row_num_in[id_df$to_mix]
  mixer_out = sample(1:num_ids, length(mixer_in))
  
  id_df$row_num_out[mixer_in] = mixer_out
  id_df$row_num_out[mixer_out] = mixer_in
  
  id_from = id_df %>% select(all_of(these_columns), row_num_in)
  id_to = id_df %>% select(all_of(these_columns), row_num_out)
  
  id_df2 = inner_join(
    id_from, id_to, 
    by = c("row_num_in" = "row_num_out"), 
    suffix = c("","_new")
  )
  
  # break
  id_df2 = id_df2 %>%
    mutate(
      to_break = runif(num_ids) < BREAK_PROB,
      break_addition = randbetween(BREAK_MIN, BREAK_MAX, num_ids)
    )
  
  for(cc in these_columns){
    cc = paste0(cc, "_new")
    id_df2 = id_df2 %>%
      mutate(!!sym(cc) := ifelse(to_break, !!sym(cc) + break_addition, !!sym(cc)))
  }
  
  # prep to output
  id_df2 = id_df2 %>%
    select(-to_break, -break_addition, -row_num_in)
  
  return(id_df2)
}

# example join back
# 
# df_out = df_in %>%
#   left_join(id_df2, by = these_columns) %>%
#   select(-all_of(these_columns))
# 
# for(cc in these_columns){
#   new = paste0(cc, "_new")
#   df_out = df_out %>%
#     rename(!!sym(cc) := !!sym(new))
# }
# 
# df_out = df_out %>%
#   select(all_of(colnames(df_in)))

## [data]_[person_overseas_spell] ---------------------------------------------

this_file = "[data]_[person_overseas_spell].csv"
these_columns = "snz_uid"
df_in = read.csv(file.path(GENERATED_FOLDER, this_file))

id_shuffle = produce_shuffle_ids(df_in, these_columns)

# join back
df_out =  left_join(df_in, id_shuffle, by = these_columns) %>%
  select(-all_of(these_columns))
# rename
for(cc in these_columns){
  new = paste0(cc, "_new")
  df_out = df_out %>%
    rename(!!sym(cc) := !!sym(new))
}
# write
df_out %>%
  select(all_of(colnames(df_in))) %>% 
  write.csv(file.path(FINAL_FOLDER, this_file), row.names = FALSE)

## [data]_[personal_detail] ---------------------------------------------------

this_file = "[data]_[personal_detail].csv"
these_columns = "snz_uid"
df_in = read.csv(file.path(GENERATED_FOLDER, this_file))

id_shuffle = produce_shuffle_ids(df_in, these_columns)

# join back
df_out =  left_join(df_in, id_shuffle, by = these_columns) %>%
  select(-all_of(these_columns))
# rename
for(cc in these_columns){
  new = paste0(cc, "_new")
  df_out = df_out %>%
    rename(!!sym(cc) := !!sym(new))
}
# write
df_out %>%
  select(all_of(colnames(df_in))) %>% 
  write.csv(file.path(FINAL_FOLDER, this_file), row.names = FALSE)

## [data]_[snz_res_pop] -------------------------------------------------------

this_file = "[data]_[snz_res_pop].csv"
these_columns = "snz_uid"
df_in = read.csv(file.path(GENERATED_FOLDER, this_file))

id_shuffle = produce_shuffle_ids(df_in, these_columns)

# join back
df_out =  left_join(df_in, id_shuffle, by = these_columns) %>%
  select(-all_of(these_columns))
# rename
for(cc in these_columns){
  new = paste0(cc, "_new")
  df_out = df_out %>%
    rename(!!sym(cc) := !!sym(new))
}
# write
df_out %>%
  select(all_of(colnames(df_in))) %>% 
  write.csv(file.path(FINAL_FOLDER, this_file), row.names = FALSE)

## [ir_clean]_[ird_ems] -------------------------------------------------------

this_file = "[ir_clean]_[ird_ems].csv"
these_columns = c("snz_uid", "snz_ird_uid")
df_in = read.csv(file.path(GENERATED_FOLDER, this_file))

id_shuffle = produce_shuffle_ids(df_in, these_columns)

# join back
df_out =  left_join(df_in, id_shuffle, by = these_columns) %>%
  select(-all_of(these_columns))
# rename
for(cc in these_columns){
  new = paste0(cc, "_new")
  df_out = df_out %>%
    rename(!!sym(cc) := !!sym(new))
}
# write
df_out %>%
  select(all_of(colnames(df_in))) %>% 
  write.csv(file.path(FINAL_FOLDER, this_file), row.names = FALSE)

## [moh_clean]_[pub_fund_hosp_discharges_event] -------------------------------

this_file = "[moh_clean]_[pub_fund_hosp_discharges_event].csv"
these_columns = c("snz_uid", "snz_moh_uid")
df_in = read.csv(file.path(GENERATED_FOLDER, this_file))

id_shuffle = produce_shuffle_ids(df_in, these_columns)

# join back
df_out =  left_join(df_in, id_shuffle, by = these_columns) %>%
  select(-all_of(these_columns))
# rename
for(cc in these_columns){
  new = paste0(cc, "_new")
  df_out = df_out %>%
    rename(!!sym(cc) := !!sym(new))
}
# write
df_out %>%
  select(all_of(colnames(df_in))) %>% 
  write.csv(file.path(FINAL_FOLDER, this_file), row.names = FALSE)

## [nzta_clean]_[dlr_historic] and [drivers_licence_register] -----------------

file_a = "[nzta_clean]_[dlr_historic].csv"
file_b = "[nzta_clean]_[drivers_licence_register].csv"

these_columns = c("snz_uid", "snz_nzta_uid")
df_in_a = read.csv(file.path(GENERATED_FOLDER, file_a))
df_in_b = read.csv(file.path(GENERATED_FOLDER, file_b))

# combine columns
df_in = bind_rows(
  select(df_in_a, all_of(these_columns)),
  select(df_in_b, all_of(these_columns))
)

id_shuffle = produce_shuffle_ids(df_in, these_columns)

# join back A
df_out =  left_join(df_in_a, id_shuffle, by = these_columns) %>%
  select(-all_of(these_columns))
# rename
for(cc in these_columns){
  new = paste0(cc, "_new")
  df_out = df_out %>%
    rename(!!sym(cc) := !!sym(new))
}
# write
df_out %>%
  select(all_of(colnames(df_in_a))) %>% 
  write.csv(file.path(FINAL_FOLDER, file_a), row.names = FALSE)

# join back B
df_out =  left_join(df_in_b, id_shuffle, by = these_columns) %>%
  select(-all_of(these_columns))
# rename
for(cc in these_columns){
  new = paste0(cc, "_new")
  df_out = df_out %>%
    rename(!!sym(cc) := !!sym(new))
}
# write
df_out %>%
  select(all_of(colnames(df_in_b))) %>% 
  write.csv(file.path(FINAL_FOLDER, file_b), row.names = FALSE)

## conclusion -----------------------------------------------------------------

# copy [moh_clean]_[pub_fund_hosp_discharges_diag].csv unchanged
this_file = "[moh_clean]_[pub_fund_hosp_discharges_diag].csv"
file.copy(file.path(GENERATED_FOLDER, this_file), file.path(FINAL_FOLDER, this_file))

# report
for(ff in dir(GENERATED_FOLDER)){
  tmp = read.csv(file.path(GENERATED_FOLDER, ff), stringsAsFactors = FALSE)
  msg = sprintf("Generated file %s : %d rows, %d cols\n", ff, nrow(tmp), ncol(tmp))
  cat(msg)
  
  tmp = read.csv(file.path(GENERATED_FOLDER, ff), stringsAsFactors = FALSE)
  msg = sprintf("Final     file %s : %d rows, %d cols\n", ff, nrow(tmp), ncol(tmp))
  cat(msg)
}
 


