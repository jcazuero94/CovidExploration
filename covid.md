covid\_exploration
================
Juan Camilo Azuero
2/9/2022

In this project we are going to analyze ths distribution of shooting
incidents over time in New York City.

## Import libraries

``` r
library("tidyverse")
library("lubridate")
library("leaflet")
library("forecast")
library("prophet")
library("zoo")
```

## Import data

Data is imported from a public repository of the Jhon Hopkins university

``` r
tests_data <- read_csv("https://github.com/govex/COVID-19/raw/master/data_tables/testing_data/time_series_covid19_US.csv?accessType=DOWNLOAD")
```

    ## Parsed with column specification:
    ## cols(
    ##   date = col_character(),
    ##   state = col_character(),
    ##   cases_conf_probable = col_double(),
    ##   cases_confirmed = col_double(),
    ##   cases_probable = col_double(),
    ##   tests_viral_positive = col_double(),
    ##   tests_viral_negative = col_double(),
    ##   tests_viral_total = col_double(),
    ##   tests_antigen_positive = col_logical(),
    ##   tests_antigen_total = col_logical(),
    ##   people_viral_positive = col_double(),
    ##   people_viral_total = col_logical(),
    ##   people_antigen_positive = col_logical(),
    ##   people_antigen_total = col_logical(),
    ##   encounters_viral_total = col_logical(),
    ##   tests_combined_total = col_double()
    ## )

    ## Warning: 50336 parsing failures.
    ##  row                    col           expected actual                                                                                                                   file
    ## 1347 tests_antigen_positive 1/0/T/F/TRUE/FALSE   1085 'https://github.com/govex/COVID-19/raw/master/data_tables/testing_data/time_series_covid19_US.csv?accessType=DOWNLOAD'
    ## 1347 tests_antigen_total    1/0/T/F/TRUE/FALSE   7090 'https://github.com/govex/COVID-19/raw/master/data_tables/testing_data/time_series_covid19_US.csv?accessType=DOWNLOAD'
    ## 1348 tests_antigen_positive 1/0/T/F/TRUE/FALSE   1085 'https://github.com/govex/COVID-19/raw/master/data_tables/testing_data/time_series_covid19_US.csv?accessType=DOWNLOAD'
    ## 1348 tests_antigen_total    1/0/T/F/TRUE/FALSE   7090 'https://github.com/govex/COVID-19/raw/master/data_tables/testing_data/time_series_covid19_US.csv?accessType=DOWNLOAD'
    ## 1349 tests_antigen_positive 1/0/T/F/TRUE/FALSE   1085 'https://github.com/govex/COVID-19/raw/master/data_tables/testing_data/time_series_covid19_US.csv?accessType=DOWNLOAD'
    ## .... ...................... .................. ...... ......................................................................................................................
    ## See problems(...) for more details.

``` r
vaccine_global_data <- read_csv("https://github.com/govex/COVID-19/raw/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_global.csv?accessType=DOWNLOAD")
```

    ## Parsed with column specification:
    ## cols(
    ##   Country_Region = col_character(),
    ##   Date = col_date(format = ""),
    ##   Doses_admin = col_double(),
    ##   People_partially_vaccinated = col_double(),
    ##   People_fully_vaccinated = col_double(),
    ##   Report_Date_String = col_date(format = ""),
    ##   UID = col_double(),
    ##   Province_State = col_logical()
    ## )

    ## Warning: 107618 parsing failures.
    ##   row            col           expected                    actual                                                                                                                                           file
    ## 13972 Province_State 1/0/T/F/TRUE/FALSE Alberta                   'https://github.com/govex/COVID-19/raw/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_global.csv?accessType=DOWNLOAD'
    ## 13973 Province_State 1/0/T/F/TRUE/FALSE British Columbia          'https://github.com/govex/COVID-19/raw/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_global.csv?accessType=DOWNLOAD'
    ## 13974 Province_State 1/0/T/F/TRUE/FALSE Manitoba                  'https://github.com/govex/COVID-19/raw/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_global.csv?accessType=DOWNLOAD'
    ## 13975 Province_State 1/0/T/F/TRUE/FALSE New Brunswick             'https://github.com/govex/COVID-19/raw/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_global.csv?accessType=DOWNLOAD'
    ## 13976 Province_State 1/0/T/F/TRUE/FALSE Newfoundland and Labrador 'https://github.com/govex/COVID-19/raw/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_global.csv?accessType=DOWNLOAD'
    ## ..... .............. .................. ......................... ..............................................................................................................................................
    ## See problems(...) for more details.

``` r
vaccine_us_data <- read_csv("https://github.com/govex/COVID-19/raw/master/data_tables/vaccine_data/us_data/time_series/vaccine_data_us_timeline.csv?accessType=DOWNLOAD")
```

    ## Parsed with column specification:
    ## cols(
    ##   Province_State = col_character(),
    ##   Date = col_date(format = ""),
    ##   Vaccine_Type = col_character(),
    ##   FIPS = col_double(),
    ##   Country_Region = col_character(),
    ##   Lat = col_double(),
    ##   Long_ = col_double(),
    ##   Doses_alloc = col_double(),
    ##   Doses_shipped = col_double(),
    ##   Doses_admin = col_double(),
    ##   Stage_One_Doses = col_double(),
    ##   Stage_Two_Doses = col_double(),
    ##   Combined_Key = col_character()
    ## )

## TEST

### Session info

``` r
sessionInfo()
```

    ## R version 3.6.1 (2019-07-05)
    ## Platform: x86_64-apple-darwin13.4.0 (64-bit)
    ## Running under: macOS  10.16
    ## 
    ## Matrix products: default
    ## BLAS/LAPACK: /opt/anaconda3/envs/R_maestria/lib/R/lib/libRblas.dylib
    ## 
    ## locale:
    ## [1] C
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] zoo_1.8-6       prophet_1.0     rlang_0.3.4     Rcpp_1.0.1     
    ##  [5] forecast_8.6    leaflet_2.0.2   lubridate_1.7.4 forcats_0.4.0  
    ##  [9] stringr_1.4.0   dplyr_0.8.0.1   purrr_0.3.2     readr_1.3.1    
    ## [13] tidyr_0.8.3     tibble_2.1.1    ggplot2_3.1.1   tidyverse_1.2.1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] lattice_0.20-38    assertthat_0.2.1   digest_0.6.18     
    ##  [4] lmtest_0.9-36      mime_0.6           R6_2.4.0          
    ##  [7] cellranger_1.1.0   plyr_1.8.4         backports_1.1.4   
    ## [10] evaluate_0.13      httr_1.4.0         pillar_1.3.1      
    ## [13] curl_3.3           lazyeval_0.2.2     readxl_1.3.1      
    ## [16] rstudioapi_0.10    TTR_0.23-4         fracdiff_1.4-2    
    ## [19] rmarkdown_1.12     htmlwidgets_1.3    munsell_0.5.0     
    ## [22] shiny_1.3.2        broom_0.5.2        compiler_3.6.1    
    ## [25] httpuv_1.5.1       modelr_0.1.4       xfun_0.6          
    ## [28] pkgconfig_2.0.2    urca_1.3-0         htmltools_0.3.6   
    ## [31] nnet_7.3-12        tidyselect_0.2.5   codetools_0.2-16  
    ## [34] quadprog_1.5-5     crayon_1.3.4       withr_2.1.2       
    ## [37] later_0.8.0        grid_3.6.1         nlme_3.1-139      
    ## [40] jsonlite_1.6       xtable_1.8-4       gtable_0.3.0      
    ## [43] magrittr_1.5       scales_1.0.0       RcppParallel_5.1.4
    ## [46] quantmod_0.4-14    cli_1.1.0          stringi_1.4.3     
    ## [49] promises_1.0.1     tseries_0.10-46    timeDate_3043.102 
    ## [52] xml2_1.2.0         xts_0.11-2         generics_0.0.2    
    ## [55] tools_3.6.1        glue_1.3.1         hms_0.4.2         
    ## [58] crosstalk_1.0.0    parallel_3.6.1     yaml_2.2.0        
    ## [61] colorspace_1.4-1   rvest_0.3.3        knitr_1.22        
    ## [64] haven_2.1.0
