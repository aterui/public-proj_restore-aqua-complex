
# setup -------------------------------------------------------------------

library(tidyverse)

# tables ------------------------------------------------------------------

## file description
fnm <- list.files("data_fmt")

df_fnm <- tibble(file = fnm) %>% 
  mutate(description = case_when(file == "data_bat.rds" ~ 
                                   "Daily bat activity count.",
                                 file == "data_bat_fit.rds" ~ 
                                   "Daily bat activity count with GAM fitted values.",
                                 file == "data_emerge.rds" ~
                                   "Insect emergence data.",
                                 file == "data_emerge_cv.rds" ~
                                   "Temporal CV values of insect emergence (class `list`).",
                                 file == "data_emerge_rho.rds" ~
                                   "Rho values of insect emergence (class `list`).",
                                 file == "data_w_temp.rds" ~
                                   "Water temperature data.")) %>% 
  mutate(file = paste0("`", file, "`"))

## column specification
## - column names
cnm <- paste0("data_fmt/", fnm) %>% 
  lapply(readRDS) %>% 
  sapply(function(x) {
    if (is.data.frame(x)) colnames(x) else lapply(x, colnames)
  })

## - duplicated file names
x <- sapply(1:length(fnm),
            function(i) rep(fnm[i], each = length(unlist(cnm[[i]]))))

## - column data frame
df_cnm <- tibble(file = unlist(x),
                 column = unlist(cnm)) %>% 
  mutate(description = case_when(column == "site" ~
                                   "Site ID. `st1` = Closed Area, `st2` = Open Area",
                                 column == "habitat" & str_detect(file, "data_bat") ~
                                   "Type of aquatic habitat at the site. `stream` = control site, `wetland_stream` = restored site",
                                 column == "habitat" & str_detect(file, "data_emerge") ~
                                   "Aquatic habitat type",
                                 column == "habitat" & file == "data_w_temp.rds" ~
                                   "Aquatic habitat type",
                                 column == "date" & str_detect(file, "data_bat") ~ 
                                   "Date of bat pass detected",
                                 column == "date" & file == "data_emerge.rds" ~ 
                                   "Date of emergence traps retrieved",
                                 column == "n" ~
                                   "Count of bat passes",
                                 column == "month" ~
                                   "Month of observation",
                                 column == "julian" ~
                                   "Julian date",
                                 column == "sh" ~ 
                                   "Area (named as `site`, closed vs. open) x Restoration (named as `habitat`, stream vs. wetland + stream) unique combination index. Used for GAM fitting",
                                 column == "y_pred" ~
                                   "Median prediction from the GAM model",
                                 column == "y_low" ~
                                   "Median - SE prediction from the GAM model",
                                 column == "y_high" ~
                                   "Median + SE prediction from the GAM model",
                                 column == "mean_flux_mass" ~
                                   "Mass flux of emerging aquatic insects",
                                 column == "mean_flux_abund" ~
                                   "Abundance flux of emerging aquatic insects",
                                 column == "mass_unit" ~
                                   "Unit of mass flux",
                                 column == "abud_unit" ~
                                   "Unit of abundance flux",
                                 column == "taxon" ~
                                   "Taxonomic group",
                                 column == "year" ~
                                   "Year of observation",
                                 column == "mu" ~
                                   "Temporal mean of insect emergence flux (wet mass)",
                                 column == "sigma" ~
                                   "Temporal standard deviation of  insect emergence flux (wet mass)",
                                 column == "cv" ~
                                   "Coefficient of variation",
                                 column == "rho" ~
                                   "Circular metric of seasonality",
                                 column == "habitat_label" ~
                                   "Aquatic habitat type",
                                 column == "predicted" ~
                                   "Weighted mean of CV across stream and wetland",
                                 column == "observed" ~
                                   "Observed CV when emergence summed across stream and wetland",
                                 column == "id" ~
                                   "Row ID",
                                 column == "dttm" ~
                                   "Date of observation",
                                 column == "temperature" ~
                                   "Water temperature (degree C)")) %>% 
  mutate(file = paste0("`", file, "`"),
         file = ifelse(duplicated(file), NA, file))
  
