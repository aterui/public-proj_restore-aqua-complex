#' DESCRIPTION
#' Generalized Additive Model for bat activity

# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")

# data --------------------------------------------------------------------

## duration for analysis
dur <- c(as.Date("2021-03-01"), as.Date("2021-10-31"))

## read data source
df_bat0 <- readRDS("data_fmt/data_bat.rds") 

df_bat <- df_bat0 %>% 
  filter(between(date, dur[1], dur[2])) %>% 
  mutate(julian = julian(date, origin = as.Date("2021-01-01")) + 1) %>% 
  mutate(site = as.factor(site),
         habitat = as.factor(habitat),
         sh = interaction(site, habitat))

# analysis ----------------------------------------------------------------

## GAM with a negative-binomial family
m <- gam(n ~ 
           s(julian, by = sh) +
           habitat * site,
         data = df_bat,
         family = "nb")


# export ------------------------------------------------------------------

## prediction data frame
y0 <- predict(m, se.fit = TRUE)

df_bat <- df_bat %>% 
  mutate(y_pred = exp(y0$fit),
         y_low = with(y0, exp(fit - se.fit)),
         y_high = with(y0, exp(fit + se.fit)))

df_fit <- left_join(df_bat0, df_bat)

saveRDS(df_fit, "data_fmt/data_bat_fit.rds")

