#' DESCRIPTION
#' Format water temperature data
#' `data_w_temp.rds` for visualization

# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")


# water temp --------------------------------------------------------------

df_temp0 <- list.files("data_raw/data_src_w_temp",
                       full.names = TRUE) %>% 
  lapply(function(x){
    read_csv(x) %>% 
      dplyr::select(1:3) %>% 
      setNames(c("id", "dttm", "temperature")) %>% 
      mutate(dttm = as.POSIXct(dttm,
                               format = "%m/%d/%Y %H:%M",
                               tz = Sys.timezone()),
             habitat = str_extract(x, "stream|wetland"),
             site = str_extract(x, "site1|site2"))
  }) %>% 
  bind_rows()

df_temp <- df_temp0 %>% 
  drop_na(dttm, temperature)

## export
saveRDS(df_temp, "data_fmt/data_w_temp.rds")


# # sample plot -------------------------------------------------------------
# 
# df_temp0 %>%
#   drop_na(temperature, dttm) %>% 
#   ggplot(aes(x = dttm,
#              y = temperature,
#              color = habitat)) +
#   facet_wrap(facets =~ site) +
#   geom_point(alpha = 0.05,
#              size = 0.01)# +
#   #geom_smooth()
