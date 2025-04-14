#' DESCRIPTION
#' Create tables in LaTeX format

# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")
options(xtable.comment = FALSE)

# table for emergence flux ------------------------------------------------

## data
df_emerge <- readRDS("data_fmt/data_emerge.rds") %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(taxon == "total")

df_table_emg <- df_emerge %>% 
  filter(year == 2021,
         between(month, 3, 10)) %>% 
  group_by(site, habitat) %>% 
  summarize(average = mean(mean_flux_mass),
            sd = sd(mean_flux_mass)) %>%
  ungroup() %>% 
  filter(habitat != "wetland_stream") %>% 
  mutate(average = sprintf("%.1f", average),
         sd = sprintf("%.1f", sd),
         site = case_when(site == "st1" ~ "Closed",
                          site == "st2" ~ "Open"),
         site = ifelse(duplicated(site), NA, site),
         habitat = str_to_sentence(habitat)) %>% 
  rename(Area = site,
         Habitat = habitat,
         Average = average,
         SD = sd)

## export 
print(xtable(df_table_emg,
             caption = "Temporal average and standard deviation (SD) of aquatic insect emergence flux (wet mass [mg m$^{-2}$ day$^{-1}$]) from March to October in 2021.",
             align = "lllcc",
             label = "tab:emg"),
      sanitize.text.function = function(x) x, # for math mode
      include.rownames = FALSE,
      caption.placement = "top",
      size = "\\small",
      file = "tex/table_emg.tex")


# table for gam results ---------------------------------------------------

source("code/analysis_bat_gam.R")

bnm <- names(coef(m))
bid <- which(bnm == "habitatwetland_stream:sitest2")
b <- coef(m)[1:bid]
se <- sqrt(diag(vcov(m)))[1:bid]
p <- summary(m)$p.pv

df_gam <- tibble(Variable = names(b[1:bid]),
                 Estimate = paste0("$", sprintf("%.2f", b[1:bid]), "$"),
                 SE = se,
                 `P value` = paste0("$", ifelse(p < 1E-3, "<0.001", p), "$")) %>% 
  mutate(Variable = case_when(Variable == "(Intercept)" ~
                                "Intercept",
                              Variable == "habitatwetland_stream" ~
                                "Restoration (wetland + stream vs. stream only)",
                              Variable == "sitest2" ~
                                "Openness (open vs. closed)",
                              Variable == "habitatwetland_stream:sitest2" ~
                                "Restoration $\\times$ Openness"))

print(xtable(df_gam,
             caption = "Estimated effects of linear predictors in the generalized additive model (GAM).
             Wald p-values were calculated using z-scores derived from the slope estimates and their standard errors (SE).",
             align = "llccc",
             label = "tab:gam-est"),
      tabular.environment = "tabular", # use \begin{tabular}
      sanitize.text.function = function(x) x, # for math mode
      include.rownames = FALSE,
      caption.placement = "top",
      size = "\\small",
      file = "tex/table_gam.tex")


# table for bat composition -----------------------------------------------

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

df_tbl_bat <- df0_bat %>% 
  filter(year == 2021) %>% 
  group_by(species_id, site) %>% 
  tally() %>% 
  group_by(site) %>% 
  mutate(total = sum(n),
         p = n / total) %>% 
  ungroup() %>% 
  arrange(site, desc(n)) %>% 
  transmute(Area = ifelse(site == "st1", "Closed", "Open"),
            Species = case_when(species_id == "NoID" ~ "No ID",
                                species_id == "EPTFUS" ~ "Eptesicus fuscus",
                                species_id == "LASBOR" ~ "Lasiurus borealis",
                                species_id == "LASCIN" ~ "Lasiurus cinereus",
                                species_id == "LASNOC" ~ "Lasionycteris noctivagans",
                                species_id == "NYCHUM" ~ "Nycticeius humeralis",
                                species_id == "PERSUB" ~ "Perimyotis subflavus",
                                species_id == "TADBRA" ~ "Tadarida brasiliensis"),
            Species = ifelse(Species == "No ID", 
                             Species, 
                             paste0("\\textit{", Species, "}")),
            Count = n,
            Proportion = sprintf(fmt = "%.2f", p)) %>% 
  mutate(Area = ifelse(duplicated(Area), NA, Area))

print(xtable(df_tbl_bat,
             caption = "Summary of bat activity in the study area.
             Total number of bat passes recorded from January 1 to December 31, 2021,
             aggregated by closed and open areas.",
             align = "lllcc", 
             label = "tab:bat-tbl"),
      tabular.environment = "tabular", # use \begin{tabular}
      sanitize.text.function = function(x) x, # for math mode
      include.rownames = FALSE,
      caption.placement = "top",
      size = "\\small",
      file = "tex/table_bat.tex")

