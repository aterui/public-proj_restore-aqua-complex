#' DESCRIPTION
#' Create figures

# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")

label <- c(`stream` = "Stream",
           `wetland` = "Wetland",
           `wetland_stream` = "Wetland & Stream",
           `st1` = "Closed area",
           `st2` = "Open area")

col <- MetBrewer::met.brewer("Hiroshige", 5)[c(1, 5, 4)]
lty <- c("solid", "dotted", "dashed")
names(lty) <- names(col) <- c("Wetland & Stream", "Wetland", "Stream")
dur <- c(as.Date("2021-03-01"),
         as.Date("2021-10-31"))

# figure of emerging insects ----------------------------------------------

## data
## - note: `date` refers to the date of trap retrieval
df_emerge <- readRDS("data_fmt/data_emerge.rds") %>% 
  mutate(month = month(date),
         year = year(date),
         taxon = case_when(taxon == "mayfly" ~ "Ephemeroptera",
                           taxon == "caddisfly" ~ "Trichoptera",
                           taxon == "diptera" ~ "Diptera",
                           taxon == "total" ~ "Total"),
         taxon = fct_relevel(taxon, "Total", "Ephemeroptera", "Trichoptera"))

df_emerge_ws <- df_emerge %>% 
  filter(habitat != "wetland_stream") %>% 
  mutate(habitat = str_to_sentence(habitat))
  
(figure_emerge <- df_emerge_ws %>% 
    ggplot(aes(x = date,
               y = mean_flux_mass,
               color = habitat,
               linetype = habitat)) +
    geom_line(alpha = 0.75) +
    geom_point(size = 1.5,
               alpha = 0.75) +
    facet_grid(rows = vars(taxon),
               cols = vars(site),
               scales = "free",
               labeller = labeller(site = label)) +
    annotate("rect",
             xmin = dur[2],
             xmax = as.Date("2022/3/22"),
             ymin = 0,
             ymax = max(df_emerge_ws$mean_flux_mass),
             alpha = 0.25,
             fill = "grey") +
    scale_color_manual(values = col) +
    scale_linetype_manual(values = lty) +
    labs(y = "Emergence flux ("~wet~mg~m^{-2}~day^{-1}*")",
         size = 20,
         color = "Aquatic habitat",
         linetype = "Aquatic habitat") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x.bottom = element_text(angle = 90,
                                            hjust = 1)) +
    scale_x_date(date_breaks = "2 month",
                 date_labels = "%b-%Y"))

ggsave(figure_emerge,
       filename = "output/figure_emg.png",
       height = 7,
       width = 6.5)

# figure cv emerge --------------------------------------------------------

## cv, variability
list_cv <- readRDS("data_fmt/data_emerge_cv.rds")
df_cv <- list_cv[[1]]
df_cv_pred <- list_cv[[2]]

(figure_cv <- df_cv %>% 
    ggplot(aes(x = habitat_label,
               y = cv,
               color = habitat_label)) +
    geom_point(size = 4) +
    geom_point(data = df_cv_pred,
               aes(y = predicted),
               size = 4,
               pch = 21) +
    geom_segment(data = df_cv_pred,
                 aes(y = predicted,
                     yend = observed),
                 arrow = arrow(length = unit(0.15, "cm")),
                 color = grey(0.4)) +
    facet_wrap(facets = ~ site,
               labeller = labeller(site = label)) +
    scale_color_manual(values = col) +
    labs(y = "CV",
         color = "Aquatic habitat") +
    theme_bw() +
    theme(strip.background = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    guides(color = "none"))

## rho, seasonality
list_rho <- readRDS("data_fmt/data_emerge_rho.rds")
df_rho <- list_rho[[1]]
df_rho_pred <- list_rho[[2]]

(figure_rho <- df_rho %>% 
    ggplot(aes(x = habitat_label,
               y = rho,
               color = habitat_label)) +
    geom_point(size = 4) +
    geom_point(data = df_rho_pred,
               aes(y = predicted),
               size = 4,
               pch = 21) +
    geom_segment(data = df_rho_pred,
                 aes(y = predicted,
                     yend = observed),
                 arrow = arrow(length = unit(0.15, "cm")),
                 color = grey(0.4)) +
    facet_wrap(facets = ~ site,
               labeller = labeller(site = label)) +
    scale_color_manual(values = col) +
    labs(y = expression("Seasonality"~rho),
         color = "Aquatic habitat") +
    theme_bw() +
    theme(strip.background = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x.bottom = element_text(angle = 45,
                                            hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_blank()) +
    guides(color = "none"))

figure_cv_rho <- figure_cv / figure_rho

ggsave(figure_cv_rho,
       filename = "output/figure_cv_rho.png",
       height = 6,
       width = 6)


# figure of mean bat activity ---------------------------------------------

df_bat <- readRDS("data_fmt/data_bat_fit.rds")

## regular plot example
(figure_bat_habitat <- df_bat %>% 
    mutate(habitat = case_when(habitat == "stream" ~ "Stream",
                               habitat == "wetland_stream" ~ "Wetland & Stream")) %>% 
    ggplot(aes(x = date,
               y = n,
               color = habitat,
               fill = habitat)) +
    geom_point(alpha = .35,
               size = 0.5) +
    geom_line(aes(y = y_pred)) +
    geom_ribbon(aes(ymin = y_low,
                    ymax = y_high),
                alpha = 0.25,
                color = NA) +
    facet_wrap(facets =~ site,
               labeller = labeller(site = label)) +
    annotate("rect",
             xmin = as.Date("2021/1/1"),
             xmax = dur[1],
             ymin = 0,
             ymax = max(df_bat$n),
             alpha = 0.25,
             fill = "grey") +
    annotate("rect",
             xmin = dur[2],
             xmax = as.Date("2021/12/31"),
             ymin = 0,
             ymax = max(df_bat$n),
             alpha = 0.25,
             fill = "grey") +
    scale_x_date(date_breaks = "2 month",
                 date_labels = "%b-%Y") +
    scale_color_manual(values = col) +
    scale_fill_manual(values = col) +
    labs(y = expression("Bat activity ("*count~night^{-1}*")"),
         x = "Date",
         size = 20,
         color = "Aquatic habitat",
         fill = "Aquatic habitat") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x.bottom = element_text(angle = 90,
                                            hjust = 1),
          strip.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()))

ggsave(figure_bat_habitat,
       filename = "output/figure_bat_habitat.png",
       height = 3.5,
       width = 8)

# water temperature -------------------------------------------------------

df_temp <- readRDS("data_fmt/data_w_temp.rds") %>% 
  mutate(site = case_when(site == "site1" ~ "st1",
                          site == "site2" ~ "st2"),
         habitat = str_to_sentence(habitat))

df_temp_daily <- df_temp %>% 
  mutate(dttm = as.POSIXct.Date(date(dttm))) %>% 
  group_by(dttm, site, habitat) %>% 
  summarize(temperature = mean(temperature))

figure_w_temp <- df_temp %>% 
  ggplot(aes(x = dttm,
             y = temperature,
             color = habitat,
             fill = habitat)) +
  facet_wrap(facets =~ site) +
  geom_smooth(linewidth = 0.4) +
  geom_point(data = df_temp_daily,
             alpha = 0.25,
             size = 0.5) +
  facet_wrap(facets =~ site,
             labeller = labeller(site = label)) +
  scale_color_manual(values = col) +
  scale_fill_manual(values = col) +
  scale_x_datetime(date_breaks = "4 month",
                   date_labels = "%b-%Y") +
  labs(y = "Water temperature (degree C)",
       color = "Aquatic habitat",
       fill = "Aquatic habitat") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x.bottom = element_text(angle = 45,
                                          hjust = 1),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(figure_w_temp,
       filename = "output/figure_w_temp.pdf",
       height = 3.5,
       width = 8)
