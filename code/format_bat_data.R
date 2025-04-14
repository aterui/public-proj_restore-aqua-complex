#' DESCRIPTION
#' Format bat activity data
#' `df_bat.rds` for GAM analysis
#' `df_bat_reg.reg` disabled - used for old analysis

# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")

## update if needed
# googledrive::drive_download("data_bat_v_0_1_0",
#                             type = "csv",
#                             path = "data_raw/data_bat.csv",
#                             overwrite = TRUE)

# data --------------------------------------------------------------------

# define the julian_to_date function
julian_to_date <- function(julian_date, reference_year = 2021) {
  reference_date <- as.Date(paste0(reference_year, "-01-01"))
  target_date <- reference_date + julian_date - 1
  return(target_date)
}

# read data
df_temp_prec <- read_csv(here::here("data_raw/data_temp_prec.csv")) %>% 
  rename_with(.fn = str_to_lower, .cols = everything()) %>% 
  rename(julian = 'julian_date') %>% 
  mutate(date = julian_to_date(julian)) %>% 
  mutate(month = months(date))

# "data_bat.csv" - source data is "Meta_Data_Bats2021" in googledrive
df0_bat <- read_csv(here::here("data_raw/data_bat.csv")) %>% 
  rename_with(.fn = str_to_lower, .cols = everything()) %>% 
  rename(species_id = 'auto id*',
         site_id = site) %>% 
  mutate(site = case_when(site_id == "RECCON" ~ "st2",
                          site_id == "RECWET" ~ "st2",
                          site_id == "WOODCON" ~ "st1",
                          site_id == "WOODWET" ~ "st1"),
         habitat = case_when(site_id == "RECCON" ~ "stream",
                             site_id == "RECWET" ~ "wetland_stream",
                             site_id == "WOODCON" ~ "stream",
                             site_id == "WOODWET" ~ "wetland_stream")) %>% 
  drop_na(species_id) %>%
  mutate(year = format(as.Date(date, format = "%m/%d/%Y"), "%Y"),
         date = as.Date(date, format = "%m/%d/%Y"),
         first_date = as.Date(paste0(year, "/1/1"), format = "%Y/%m/%d"),
         julian = julian(date) - julian(first_date) + 1)

# # proportion of species identified
# # - yes = 0.735
# df0_bat %>% 
#   filter(year == 2021) %>% 
#   mutate(identified = ifelse(species_id == "NoID", 0, 1)) %>% 
#   group_by(identified) %>% 
#   tally() %>% 
#   mutate(total = sum(n),
#          p = n / total)

## manipulate;
## - format data for n detects per day
df_bat <- df0_bat %>% 
  select(-time) %>%
  filter(year == 2021) %>% 
  group_by(site,
           habitat,
           date) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  right_join(expand(.,
                    date = seq(as.Date("2021-01-01"),
                               as.Date("2021-12-31"),
                               by = 1),
                    site = unique(site),
                    habitat = unique(habitat)),
             by = c("date", "site", "habitat")) %>%
  arrange(date) %>%
  mutate(n = replace_na(n, 0),
         month = months(date))

# # merge with temperature and precipitation data
# df_merge_weather <- dplyr::left_join(df_bat,
#                                      df_temp_prec,
#                                      by = c("date", "month"))
# 
# # summarize by month
# df_reg <- df_merge_weather %>% 
#   group_by(month, site, habitat) %>% 
#   summarize(total_n = sum(n),
#             n_date = n_distinct(date),
#             avg_temp = mean(temp_avg),
#             avg_prec = mean(prec_avg)) %>% 
#   ungroup() %>% 
#   mutate(avg_temp = (avg_temp - 32) * (5/9)) # from [F] to [C]

# export ------------------------------------------------------------------

saveRDS(df_bat,
        file = "data_fmt/data_bat.rds")

# saveRDS(df_reg,
#         file = "data_fmt/data_bat_reg.rds")
