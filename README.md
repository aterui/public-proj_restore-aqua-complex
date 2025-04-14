README
================

# Article Information

**Tentative title**: Restoration of aquatic habitat complex extends the
foraging window of terrestrial consumers

**Authors**: Ibrahim M, Petric R, Wahl C, Terui A

# Analysis Flow

## Aquatic Insect Emergence

Script `analysis_emg_cv.R` runs analysis to calculate temporal CVs. Data
is sourced from `data_raw/data_emg_v_0_2_0.csv`, which was formatted in
`format_emg_data.R`.

## Bat Activity Analysis

Script `analysis_bat_gam.R` runs the Generalized Additive Model. Data is
sourced from `data_raw/data_bat.csv`, which was formatted in
`format_bat_data.R`.

# Data Sources

## File Description

| file | description |
|:---|:---|
| `data_bat.rds` | Daily bat activity count. |
| `data_bat_fit.rds` | Daily bat activity count with GAM fitted values. |
| `data_emerge.rds` | Insect emergence data. |
| `data_emerge_cv.rds` | Temporal CV values of insect emergence (class `list`). |
| `data_emerge_rho.rds` | Rho values of insect emergence (class `list`). |
| `data_w_temp.rds` | Water temperature data. |

## Column Description

| file | column | description |
|:---|:---|:---|
| `data_bat.rds` | site | Site ID. `st1` = Closed Area, `st2` = Open Area |
|  | habitat | Type of aquatic habitat at the site. `stream` = control site, `wetland_stream` = restored site |
|  | date | Date of bat pass detected |
|  | n | Count of bat passes |
|  | month | Month of observation |
| `data_bat_fit.rds` | site | Site ID. `st1` = Closed Area, `st2` = Open Area |
|  | habitat | Type of aquatic habitat at the site. `stream` = control site, `wetland_stream` = restored site |
|  | date | Date of bat pass detected |
|  | n | Count of bat passes |
|  | month | Month of observation |
|  | julian | Julian date |
|  | sh | Area (named as `site`, closed vs. open) x Restoration (named as `habitat`, stream vs. wetland + stream) unique combination index. Used for GAM fitting |
|  | y_pred | Median prediction from the GAM model |
|  | y_low | Median - SE prediction from the GAM model |
|  | y_high | Median + SE prediction from the GAM model |
| `data_emerge.rds` | site | Site ID. `st1` = Closed Area, `st2` = Open Area |
|  | date | Date of emergence traps retrieved |
|  | mean_flux_mass | Mass flux of emerging aquatic insects |
|  | mean_flux_abund | Abundance flux of emerging aquatic insects |
|  | habitat | Aquatic habitat type |
|  | abud_unit | Unit of abundance flux |
|  | mass_unit | Unit of mass flux |
|  | taxon | Taxonomic group |
| `data_emerge_cv.rds` | year | Year of observation |
|  | site | Site ID. `st1` = Closed Area, `st2` = Open Area |
|  | habitat | Aquatic habitat type |
|  | mu | Temporal mean of insect emergence flux (wet mass) |
|  | sigma | Temporal standard deviation of insect emergence flux (wet mass) |
|  | cv | Coefficient of variation |
|  | habitat_label | Aquatic habitat type |
|  | year | Year of observation |
|  | site | Site ID. `st1` = Closed Area, `st2` = Open Area |
|  | predicted | Weighted mean of CV across stream and wetland |
|  | observed | Observed CV when emergence summed across stream and wetland |
|  | habitat_label | Aquatic habitat type |
| `data_emerge_rho.rds` | site | Site ID. `st1` = Closed Area, `st2` = Open Area |
|  | habitat | Aquatic habitat type |
|  | year | Year of observation |
|  | mu | Temporal mean of insect emergence flux (wet mass) |
|  | rho | Circular metric of seasonality |
|  | habitat_label | Aquatic habitat type |
|  | year | Year of observation |
|  | site | Site ID. `st1` = Closed Area, `st2` = Open Area |
|  | predicted | Weighted mean of CV across stream and wetland |
|  | observed | Observed CV when emergence summed across stream and wetland |
|  | habitat_label | Aquatic habitat type |
| `data_w_temp.rds` | id | Row ID |
|  | dttm | Date of observation |
|  | temperature | Water temperature (degree C) |
|  | habitat | Aquatic habitat type |
|  | site | Site ID. `st1` = Closed Area, `st2` = Open Area |

# Session Information

    ## R version 4.4.2 (2024-10-31 ucrt)
    ## Platform: x86_64-w64-mingw32/x64
    ## Running under: Windows 11 x64 (build 26100)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.utf8 
    ## [2] LC_CTYPE=English_United States.utf8   
    ## [3] LC_MONETARY=English_United States.utf8
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.utf8    
    ## 
    ## time zone: America/New_York
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] circular_0.5-1  patchwork_1.2.0 xtable_1.8-4    MuMIn_1.47.5   
    ##  [5] ggthemes_5.1.0  cowplot_1.1.1   ggpubr_0.4.0    foreach_1.5.2  
    ##  [9] mgcv_1.8-41     nlme_3.1-160    sf_1.0-19       lubridate_1.9.3
    ## [13] forcats_1.0.0   stringr_1.5.1   dplyr_1.1.4     purrr_1.0.2    
    ## [17] readr_2.1.5     tidyr_1.3.1     tibble_3.2.1    ggplot2_3.5.1  
    ## [21] tidyverse_2.0.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gtable_0.3.6       xfun_0.51          rstatix_0.7.0      lattice_0.22-6    
    ##  [5] tzdb_0.5.0         vctrs_0.6.5        tools_4.4.2        generics_0.1.3    
    ##  [9] stats4_4.4.2       proxy_0.4-27       pacman_0.5.1       pkgconfig_2.0.3   
    ## [13] Matrix_1.7-1       KernSmooth_2.23-20 lifecycle_1.0.4    compiler_4.4.2    
    ## [17] munsell_0.5.1      codetools_0.2-18   carData_3.0-5      htmltools_0.5.8.1 
    ## [21] class_7.3-20       yaml_2.3.10        pillar_1.10.1      car_3.1-1         
    ## [25] classInt_0.4-11    iterators_1.0.14   boot_1.3-28        abind_1.4-8       
    ## [29] tidyselect_1.2.1   digest_0.6.33      mvtnorm_1.3-2      stringi_1.8.4     
    ## [33] splines_4.4.2      rprojroot_2.0.3    fastmap_1.2.0      grid_4.4.2        
    ## [37] here_1.0.1         colorspace_2.1-1   cli_3.6.4          magrittr_2.0.3    
    ## [41] broom_1.0.5        e1071_1.7-16       withr_3.0.2        scales_1.3.0      
    ## [45] backports_1.4.1    timechange_0.3.0   rmarkdown_2.28     ggsignif_0.6.4    
    ## [49] hms_1.1.3          evaluate_0.24.0    knitr_1.49         rlang_1.1.5       
    ## [53] Rcpp_1.0.14        glue_1.8.0         DBI_1.2.3          rstudioapi_0.14   
    ## [57] R6_2.6.1           units_0.8-7
