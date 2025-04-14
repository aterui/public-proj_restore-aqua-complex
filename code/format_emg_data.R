#' DESCRIPTION
#' Format insect emergence data
#' `data_emerge.rds` for CV analysis

# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")

# data format -------------------------------------------------------------

# ## update as needed
# googledrive::drive_download(file = "data_emg_v_0_2_0",
#                             path = "data_raw/data_emg_v_0_2_0.csv")

## read data
df_raw <- read_csv("data_raw/data_emg_v_0_2_0.csv")

## initial screening
## we removed "sites" (regardless of habitat types) if any traps did not function correctly
df0 <- df_raw %>%
  filter(!(taxon %in% c("other", "damselfly")), # use only ETD
         site != "st3") %>% # remove site3, not used
  select(-c(date_processed,
            who_processed,
            note,
            correction)) %>% # remove unnecessary columns
  mutate(dt_set = as.POSIXct(paste(date_set, time_set),
                             format = "%m/%d/%Y %H:%M:%S",
                             tz = Sys.timezone()), # convert <chr> to <date>
         dt_col = as.POSIXct(paste(date_collected, time_collected),
                             format = "%m/%d/%Y %H:%M:%S",
                             tz = Sys.timezone()),
         interval = as.numeric(dt_col - dt_set), # time interval
         date = as.Date(date_collected,
                        format = "%m/%d/%Y"))

## calculate flux
## - flux per unit time (abundance / (day * trap) or wet_weight / (day * trap))
df_flux <- df0 %>%
  mutate(flux_abund = abundance / interval,
         flux_mass = wet_weight / interval,
         trap_id = case_when(microhabitat %in% c("for_side", "open", "riffle") ~ 1, # trap id = 1 if microhabitat is either for_side, open or riffle
                             microhabitat %in% c("road_side", "closed", "pool") ~ 2)  # trap id = 2 if microhabitat is either road_side, closed or pool
  )

# data check --------------------------------------------------------------

## all should have two microhabitats => ok
## except one datum with 1 - this is because one trap had only "other" category
## 1/30/2022, st1, wetland roadside
df0 %>% 
  group_by(site, habitat, date) %>% 
  reframe(n = n_distinct(microhabitat)) %>% 
  pull(n) %>% 
  table()

## all should have one row => ok
## - except:
## 2021/4/8, st1, stream riffle, this datum has two Diptera rows 
## due to Terui's correction to taxon entry
## thus, not an error, should be summed when summarizing
df0 %>% 
  group_by(date, site, habitat, microhabitat, taxon) %>% 
  tally() %>% 
  arrange(desc(n))

# data re-format ----------------------------------------------------------

## total flux
## summarize by month (`date`) x site x habitat x trap => `msht`
df_msht <- df_flux %>% 
  group_by(date,
           site,
           habitat,
           trap_id) %>% # grouping by habitat, month  
  summarize(flux_abund = sum(flux_abund), # summed over taxa
            flux_mass = sum(flux_mass),
            n_taxa = n_distinct(taxon),
            n_trap = n_distinct(trap_id)) %>%
  ungroup()

## summarize by month (`date`) x habitat x site => `msh`
## per trap flux (0.25 m^2) times 4.0 returns flux per unit m^2
df_msh <- df_msht %>% 
  group_by(date, site, habitat) %>% 
  summarize(mean_flux_mass = mean(flux_mass) * 4, # area per trap = 0.25 m^2 times four = unit m^2
            mean_flux_abund = mean(flux_abund) * 4) %>% 
  ungroup()

## combine
df_wet_str <- df_msh %>% 
  group_by(site, date) %>% 
  summarize(mean_flux_mass = sum(mean_flux_mass),
            mean_flux_abund = sum(mean_flux_abund)) %>% 
  ungroup() %>% 
  mutate(habitat = "wetland_stream")

df_str <- df_msh %>% 
  filter(habitat == "stream") %>% # no wetland
  group_by(site, date) %>% 
  summarize(habitat = unique(habitat),
            mean_flux_mass = mean(mean_flux_mass),
            mean_flux_abund = mean(mean_flux_abund)) %>% 
  ungroup()

df_wet <- df_msh %>% 
  filter(habitat == "wetland") %>% # no wetland
  group_by(site, date) %>% 
  summarize(habitat = unique(habitat),
            mean_flux_mass = mean(mean_flux_mass),
            mean_flux_abund = mean(mean_flux_abund)) %>% 
  ungroup()

df_flux_total <- bind_rows(df_wet_str, df_str, df_wet) %>% 
  mutate(abud_unit = "ind / (m^2 day)",
         mass_unit = "wet mg / (m^2 day)")

# taxonomic summary -------------------------------------------------------
## flux by taxon (df_flux_btx)
## - ._btx0 omit taxa that were not present
## - ._btx include zero data for taxa that were not present in the sample
## - flux was summed across traps, the resultant flux has a unit of 0.5 m^2 (since ZERO emergence not included in the dataframe)
## - thus, times two yields a unit of m^2
df_flux_btx0 <- df_flux %>% 
  group_by(date,
           site,
           habitat,
           taxon) %>%
  summarize(mean_flux_abund = sum(flux_abund) * 2,
            mean_flux_mass = sum(flux_mass) * 2) %>% 
  ungroup()

df_flux_btx <- df_flux %>% 
  filter(taxon != "no_aquatic_insect") %>% 
  expand(date, site, habitat, taxon) %>% 
  left_join(df_flux_btx0) %>% 
  mutate(mean_flux_abund = replace_na(mean_flux_abund, 0),
         mean_flux_mass = replace_na(mean_flux_mass, 0),
         abud_unit = "ind / (m^2 day)",
         mass_unit = "wet mg / (m^2 day)") %>% 
  ungroup()

## proportional contribution by taxa
df_flux_btx %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(year == 2021,
         between(month, 3, 10)) %>% 
  group_by(year, habitat, taxon) %>% 
  summarize(mass = sum(mean_flux_mass)) %>% 
  group_by(habitat, year) %>% 
  mutate(total = sum(mass),
         p = round(mass / total, 2)) %>% 
  ungroup() %>% 
  mutate(mass_unit = "wet mg / (m^2 day)")

# # same outcome - for check
# df_flux %>%
#   filter(year == 2021,
#          between(month, 3, 10)) %>%
#   group_by(year, habitat, taxon) %>%
#   summarize(mass = sum(flux_mass)) %>%
#   group_by(habitat, year) %>%
#   mutate(total = sum(mass),
#          p = round(mass / total, 2))


# export ------------------------------------------------------------------

df_emerge <- df_flux_total %>% 
  mutate(taxon = "total") %>% 
  bind_rows(df_flux_btx)

## save as RDS file
saveRDS(df_emerge, file = "data_fmt/data_emerge.rds")
