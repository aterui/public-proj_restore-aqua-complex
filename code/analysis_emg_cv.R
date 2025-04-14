#' DESCRIPTION
#' Comparison of observed and predicted CVs/rho for insect emergence
#' "predicted" CV or rho is a weighted mean of stream and wetland

# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")


# CV analysis -------------------------------------------------------------

## data
df_emerge <- readRDS("data_fmt/data_emerge.rds") %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(taxon == "total")

## cv for individual and aggregate emergence
## analysis for March 2021 to October 2021
df_cv <- df_emerge %>% 
  filter(year == 2021,
         between(month, 3, 10)) %>% 
  group_by(year, site, habitat) %>% 
  summarize(mu = mean(mean_flux_mass),
            sigma = sd(mean_flux_mass),
            cv = sigma / mu) %>% 
  mutate(habitat_label = case_when(habitat == "stream" ~ "Stream",
                                   habitat == "wetland" ~ "Wetland",
                                   habitat == "wetland_stream" ~ "Wetland & Stream")) %>% 
  ungroup()

df_ws <- df_emerge %>% 
  filter(year == 2021,
         between(month, 3, 10)) %>% 
  group_by(site, habitat, year) %>% 
  summarize(mu = mean(mean_flux_mass)) %>% 
  filter(habitat == "wetland_stream") %>% 
  ungroup() %>% 
  dplyr::select(-habitat) %>% 
  rename(mu_sum = mu)

## cv, predicted (weighted mean of stream & wetland) vs. observed
df_cv_pred <- df_cv %>% 
  left_join(df_ws,
            by = c("year", "site")) %>% 
  mutate(w = mu / mu_sum,
         w_cv = w * cv) %>% 
  filter(habitat != "wetland_stream") %>% 
  group_by(site, year) %>% 
  summarize(cv = sum(w_cv)) %>% 
  ungroup() %>% 
  mutate(habitat = "predicted") %>% 
  bind_rows(df_cv %>% 
              filter(habitat == "wetland_stream") %>% 
              dplyr::select(-c(mu, sigma))) %>% 
  pivot_wider(id_cols = c(year, site),
              names_from = "habitat",
              values_from = "cv") %>% 
  rename(observed = wetland_stream) %>% 
  mutate(habitat_label = "Wetland & Stream")

## export
saveRDS(list(df_cv, df_cv_pred),
        "data_fmt/data_emerge_cv.rds")

# rho analysis ------------------------------------------------------------

## seasonality "rho" (circular metric of seasonality)
## - convert month to angles (January - August, 0 - 315 degrees by 45dg interval)
angle <- seq(0, 315, by = 45)

## calculate rho
## create month vector based on relative flux of emergence in each month
## size = 10000 in sample() is arbitrary - made it large to reduce uncertainty
## set.seed for reproducibility
set.seed(123)

df_rho <- df_emerge %>% 
  filter(year == 2021,
         between(month, 3, 10)) %>% 
  arrange(month) %>% 
  group_by(site, habitat, year) %>% 
  summarize(mu = mean(mean_flux_mass),
            rho = sample(angle, 
                         size = 10000,
                         replace = TRUE, 
                         prob = mean_flux_mass) %>% 
              circular::circular(type = "angles",
                                 units = "degrees") %>% 
              circular::rho.circular(),
            .groups = "drop") %>% 
  mutate(habitat_label = case_when(habitat == "stream" ~ "Stream",
                                   habitat == "wetland" ~ "Wetland",
                                   habitat == "wetland_stream" ~ "Wetland & Stream"))

## rho, predicted (weighted mean of stream & wetland) vs. observed
df_rho_pred <- df_rho %>% 
  left_join(df_ws,
            by = c("site", "year")) %>% 
  mutate(w = mu / mu_sum,
         w_rho = w * rho) %>% 
  filter(habitat != "wetland_stream") %>% 
  group_by(site, year) %>% 
  summarize(rho = sum(w_rho)) %>% 
  ungroup() %>% 
  mutate(habitat = "predicted") %>% 
  bind_rows(df_rho %>% 
              filter(habitat == "wetland_stream") %>% 
              dplyr::select(-c(mu))) %>% 
  pivot_wider(id_cols = c(year, site),
              names_from = "habitat",
              values_from = "rho") %>% 
  rename(observed = wetland_stream) %>% 
  mutate(habitat_label = "Wetland & Stream")

## export
saveRDS(list(df_rho, df_rho_pred),
        "data_fmt/data_emerge_rho.rds")