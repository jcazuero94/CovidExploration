NYC shooting incidents
================
Juan Camilo Azuero
1/25/2022

In this project we are going to analyze ths distribution of shooting
incidents over time in New York City.

## Import libraries

``` r
library("tidyverse")
```

    ## Registered S3 methods overwritten by 'ggplot2':
    ##   method         from 
    ##   [.quosures     rlang
    ##   c.quosures     rlang
    ##   print.quosures rlang

    ## Registered S3 method overwritten by 'rvest':
    ##   method            from
    ##   read_xml.response xml2

    ## -- Attaching packages ------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.1       v purrr   0.3.2  
    ## v tibble  2.1.1       v dplyr   0.8.0.1
    ## v tidyr   0.8.3       v stringr 1.4.0  
    ## v readr   1.3.1       v forcats 0.4.0

    ## -- Conflicts ---------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library("lubridate")
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library("leaflet")
library("forecast")
```

    ## Registered S3 method overwritten by 'xts':
    ##   method     from
    ##   as.zoo.xts zoo

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ## Registered S3 methods overwritten by 'forecast':
    ##   method             from    
    ##   fitted.fracdiff    fracdiff
    ##   residuals.fracdiff fracdiff

``` r
library("prophet")
```

    ## Warning: package 'prophet' was built under R version 3.6.3

    ## Loading required package: Rcpp

    ## Loading required package: rlang

    ## 
    ## Attaching package: 'rlang'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     %@%, as_function, flatten, flatten_chr, flatten_dbl,
    ##     flatten_int, flatten_lgl, flatten_raw, invoke, list_along,
    ##     modify, prepend, splice

``` r
library("zoo")
```

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

## Import data

Data is imported from a public api of NYC

``` r
nypd_shooting_data <- read_csv("https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD")
```

    ## Parsed with column specification:
    ## cols(
    ##   INCIDENT_KEY = col_double(),
    ##   OCCUR_DATE = col_character(),
    ##   OCCUR_TIME = col_time(format = ""),
    ##   BORO = col_character(),
    ##   PRECINCT = col_double(),
    ##   JURISDICTION_CODE = col_double(),
    ##   LOCATION_DESC = col_character(),
    ##   STATISTICAL_MURDER_FLAG = col_logical(),
    ##   PERP_AGE_GROUP = col_character(),
    ##   PERP_SEX = col_character(),
    ##   PERP_RACE = col_character(),
    ##   VIC_AGE_GROUP = col_character(),
    ##   VIC_SEX = col_character(),
    ##   VIC_RACE = col_character(),
    ##   X_COORD_CD = col_double(),
    ##   Y_COORD_CD = col_double(),
    ##   Latitude = col_double(),
    ##   Longitude = col_double(),
    ##   Lon_Lat = col_character()
    ## )

## Format variables

``` r
nypd_shooting_data <-nypd_shooting_data %>% mutate(OCCUR_DATE=mdy(OCCUR_DATE))
```

## Select columns

``` r
cols_to_keep <- c("OCCUR_DATE",
                  "OCCUR_TIME",
                  "BORO",
                  "PRECINCT",
                  "LOCATION_DESC",
                  "STATISTICAL_MURDER_FLAG",
                  "PERP_AGE_GROUP",
                  "PERP_SEX",
                  "PERP_RACE",
                  "VIC_AGE_GROUP",
                  "VIC_SEX",
                  "VIC_RACE",
                  "Latitude",
                  "Longitude"
                 )
nypd_shooting_data <- nypd_shooting_data %>% select(cols_to_keep)
```

## Clean column values

``` r
# Location
clasify_location <- function(location) {
    if((location =="NONE") | is.na(location)){
        return("NA")
    } 
    if(location %in% c("BANK","CHECK CASH","LOAN COMPANY","ATM","")){
        return("Financial_institution")
    }
    if(location %in% c("MULTI DWELL - PUBLIC HOUS","MULTI DWELL - APT BUILD","PVT HOUSE")){
        return("Housing")
    }
    if(location %in% c("DOCTOR/DENTIST","GYM/FITNESS FACILITY","HOSPITAL",
                       "SCHOOL","HOTEL/MOTEL","SOCIAL CLUB/POLICY LOCATI",
                       "BAR/NIGHT CLUB","FACTORY/WAREHOUSE","STORAGE FACILITY")){
        return("Other")
    }
    if(location %in% c("GROCERY/BODEGA","FAST FOOD","COMMERCIAL BLDG","RESTAURANT/DINER",
                       "CLOTHING BOUTIQUE","SMALL MERCHANT","LIQUOR STORE","SUPERMARKET",
                       "BEAUTY/NAIL SALON","SHOE STORE","DRY CLEANER/LAUNDRY","DEPT STORE",
                       "GAS STATION","PHOTO/COPY STORE","VIDEO STORE","STORE UNCLASSIFIED",
                       "CHAIN STORE","DRUG STORE","TELECOMM. STORE","JEWELRY STORE",
                       "CANDY STORE","VARIETY STORE")){
        return("Commercial")
    }
    return("Otro no encontrado")
    
}
nypd_shooting_data["LOCATION_DESC"] <-nypd_shooting_data["LOCATION_DESC"] %>% apply(1,clasify_location)
distinct(nypd_shooting_data['LOCATION_DESC'])
```

    ## # A tibble: 5 x 1
    ##   LOCATION_DESC        
    ##   <chr>                
    ## 1 NA                   
    ## 2 Housing              
    ## 3 Commercial           
    ## 4 Other                
    ## 5 Financial_institution

``` r
# Age
clasify_age_group <- function(age_group) {
    if((age_group =="UNKNOWN") | is.na(age_group)){
        return("UNKNOWN")
    } 
    if(age_group %in% c("<18","18-24","25-44","45-64","65+")){
        return(age_group)
    }
    return("UNKNOWN")
    
}
nypd_shooting_data["PERP_AGE_GROUP"] <-nypd_shooting_data["PERP_AGE_GROUP"] %>% apply(1,clasify_age_group)
distinct(nypd_shooting_data['PERP_AGE_GROUP'])
```

    ## # A tibble: 6 x 1
    ##   PERP_AGE_GROUP
    ##   <chr>         
    ## 1 UNKNOWN       
    ## 2 18-24         
    ## 3 25-44         
    ## 4 <18           
    ## 5 45-64         
    ## 6 65+

``` r
nypd_shooting_data["VIC_AGE_GROUP"] <-nypd_shooting_data["VIC_AGE_GROUP"] %>% apply(1,clasify_age_group)
distinct(nypd_shooting_data['VIC_AGE_GROUP'])
```

    ## # A tibble: 6 x 1
    ##   VIC_AGE_GROUP
    ##   <chr>        
    ## 1 25-44        
    ## 2 65+          
    ## 3 18-24        
    ## 4 <18          
    ## 5 45-64        
    ## 6 UNKNOWN

``` r
# Sex
clean_sex <- function(sex) {
    if(sex %in% c("M","F")){
        return(sex)
    }
    return("U")
    
}
nypd_shooting_data["VIC_SEX"] <-nypd_shooting_data["VIC_SEX"] %>% apply(1,clean_sex)
distinct(nypd_shooting_data['VIC_SEX'])
```

    ## # A tibble: 3 x 1
    ##   VIC_SEX
    ##   <chr>  
    ## 1 F      
    ## 2 M      
    ## 3 U

``` r
nypd_shooting_data["PERP_SEX"] <-nypd_shooting_data["PERP_SEX"] %>% apply(1,clean_sex)
distinct(nypd_shooting_data['PERP_SEX'])
```

    ## # A tibble: 3 x 1
    ##   PERP_SEX
    ##   <chr>   
    ## 1 U       
    ## 2 M       
    ## 3 F

``` r
# Race
clean_race <- function(race) {
    if(is.na(race)){
        return("UNKNOWN")
    }
    return(race)
    
}
nypd_shooting_data["PERP_RACE"] <-nypd_shooting_data["PERP_RACE"] %>% apply(1,clean_race)
distinct(nypd_shooting_data['PERP_RACE'])
```

    ## # A tibble: 7 x 1
    ##   PERP_RACE                     
    ##   <chr>                         
    ## 1 UNKNOWN                       
    ## 2 BLACK                         
    ## 3 WHITE HISPANIC                
    ## 4 WHITE                         
    ## 5 ASIAN / PACIFIC ISLANDER      
    ## 6 BLACK HISPANIC                
    ## 7 AMERICAN INDIAN/ALASKAN NATIVE

``` r
distinct(nypd_shooting_data['VIC_RACE'])
```

    ## # A tibble: 7 x 1
    ##   VIC_RACE                      
    ##   <chr>                         
    ## 1 BLACK HISPANIC                
    ## 2 WHITE                         
    ## 3 BLACK                         
    ## 4 WHITE HISPANIC                
    ## 5 AMERICAN INDIAN/ALASKAN NATIVE
    ## 6 UNKNOWN                       
    ## 7 ASIAN / PACIFIC ISLANDER

``` r
# Extract hour of the day when the incident took place
get_hour <- function(time) {
    return(hour(hms(time))) 
}
nypd_shooting_data['OCCUR_HOUR'] <- nypd_shooting_data["OCCUR_TIME"] %>% apply(1,get_hour)
```

## Maps

Lets visualize the spatial distribution of cases. (commented as it does
not work in github README.md)

``` r
#nypd_shooting_data %>%
#  select("Latitude","Longitude") %>%
#  rename("lat" = "Latitude") %>%
#  rename("long"="Longitude") %>%
#  leaflet( width = 900) %>%
#  addTiles() %>%
#  #addHeatmap(group="heat", max=.6, blur = 60)
#  addMarkers(clusterOptions = markerClusterOptions())
```

As shown in the plot above Staten Island has a small amount of incidents
while The Bronx and Brooklin are very violent.

## Average cases

Now lets visualize the evolution of shooting incidents through time

``` r
cases_by_day <- nypd_shooting_data %>%  count(OCCUR_DATE)
# Moving averages
cases_by_day['month_ma'] <- rollmean(cases_by_day['n'],30,c(NA,NULL,NA))
cases_by_day['year_ma'] <- rollmean(cases_by_day['n'],365,c(NA,NULL,NA))
ggplot(aes(x=OCCUR_DATE,y=n),data=cases_by_day) +
  geom_col(color="grey")+
  geom_line(aes(y = month_ma), color = "red", size = 0.1)+ 
  geom_line(aes(y = year_ma), color = "blue", size = 0.1)+ 
  labs(y='Cases by day',title="Shootings reported by day")
```

    ## Warning: Removed 29 rows containing missing values (geom_path).

    ## Warning: Removed 364 rows containing missing values (geom_path).

![](nypd_shooting_files/figure-gfm/incidents_time-1.png)<!-- --> <br> As
the yearly mooving average shows, the cases have a descending trend
since 2006. The monthly mooving average also hints of a yearly
seasonality that will be explored in more detail next. <br> It can also
be seen a spike in shooting incidents in 2020 correlating with the covid
outbreak. Although the correlation seems evident, a causal relation
cannot be infered with the information available. <br> To explore in
more detail the descending trend observed and the yearly stationality,
incidents from 2020 are dropped because of the unexplained peak.

``` r
cases_by_day['OCCUR_YEAR'] <- cases_by_day['OCCUR_DATE'] %>% apply(1,year)
average_cases_by_year <- cases_by_day %>% filter(OCCUR_YEAR != 2020) %>% 
    group_by(OCCUR_YEAR) %>% summarise(cases=mean(n))
ggplot(aes(x=OCCUR_YEAR,y=cases),data=average_cases_by_year) +
  geom_col(fill="blue")
```

![](nypd_shooting_files/figure-gfm/average_cases_year-1.png)<!-- -->

The number of average cases per day have almost halved in 13 years going
from 5.7 in 2006 to 3.1 in 2019.

``` r
cases_by_day['OCCUR_MONTH'] <- cases_by_day['OCCUR_DATE'] %>% apply(1,month)
average_cases_by_month <- cases_by_day %>% filter(OCCUR_YEAR != 2020) %>% 
    group_by(OCCUR_MONTH) %>% summarise(cases=mean(n))
ggplot(aes(x=OCCUR_MONTH,y=cases),data=average_cases_by_month) +
  geom_col(fill="blue")
```

![](nypd_shooting_files/figure-gfm/average_cases_month-1.png)<!-- -->

The months with less shooting incidents are february and march averaging
3.4 cases per day, while july and august are the most violent months
with a mean of 5.9 cases per day.

Looking to explore in more detail the evolution of cases through time,
lets estimate a model using the library prophet. Prophet is an open
source library devoloped by Meta to model time series. It has a curve
fitting approach that allows to estimate additive or multiplicative
models with a lot of flexibilty which allows to include domain
knowladge. One of the key advantages is how robust it is to outliers and
missing data compared with more traditional approaches like ARIMA.

## Prophet

``` r
cases_by_day_ph <- cases_by_day %>% select(c("OCCUR_DATE","n"))
colnames(cases_by_day_ph) <- c("ds","y")
prophet_model <- prophet(changepoint.prior.scale=50)
prophet_model <- add_country_holidays(prophet_model, country_name = 'US')
prophet_model <- fit.prophet(prophet_model,cases_by_day_ph)
```

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

``` r
future <- make_future_dataframe(prophet_model, periods = 365)
forecast <- predict(prophet_model, future)
prophet_plot_components(prophet_model,forecast)
```

![](nypd_shooting_files/figure-gfm/prophet_model-1.png)<!-- -->

``` r
plot(prophet_model, forecast)
```

![](nypd_shooting_files/figure-gfm/prophet_model_forecast-1.png)<!-- -->

From the component graph of the prophet model it can also be concluded
that holidays increase the amount of gun violence in the city, and that
the days with more incidents are sunday and saturday followed by friday
and monday.

The sudden increase in cases that happened in 2020 makes the trend to
turn upward. This upward trend is automaticaly continued by prophet when
doing a forecast into the future. In order to explore what would be the
situation without the outlier data of 2020 lets estimate another model
dropping the last year of information.

``` r
cases_by_day_ph <- cases_by_day  %>% filter(OCCUR_YEAR < 2020) %>%    select(c("OCCUR_DATE","n"))
colnames(cases_by_day_ph) <- c("ds","y")
prophet_model <- prophet(changepoint.prior.scale=5)
prophet_model <- add_country_holidays(prophet_model, country_name = 'US')
prophet_model <- fit.prophet(prophet_model,cases_by_day_ph)
```

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

``` r
future <- make_future_dataframe(prophet_model, periods = 365)
forecast <- predict(prophet_model, future)
prophet_plot_components(prophet_model,forecast)
```

![](nypd_shooting_files/figure-gfm/prophet_model_2-1.png)<!-- -->

``` r
plot(prophet_model, forecast)
```

![](nypd_shooting_files/figure-gfm/prophet_model_2_forecast-1.png)<!-- -->

As shown in the components plot of this model without 2020, the trend
does not have an upward turn. The desicion of not including 2020
produces minor changes in the time series components of the model, and a
big change in the trend. Personally I think that the better approach is
to drop this data while doing the estimation of the model and to
forecast into the future using the monthly moving average adjusted by
the yearly seasonality as the first plot shows that the situation is
returnning to normal. This desicion induces bias and is dependent on the
analyst.

In order to make a more robust analysis one can manually specify the
checkpoints to include more flexibility during covid and capture the
volatility of the trend during 2020. Another option could be to include
an external regresor that has information about the quarantine.

It would also be interesting to segment the analysis for different
locations in NY and to include information like the severity of the
incident and demographic characteristics of the people involved.

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
    ##  [1] nlme_3.1-139       matrixStats_0.58.0 xts_0.11-2        
    ##  [4] httr_1.4.0         rstan_2.19.3       tools_3.6.1       
    ##  [7] backports_1.1.4    utf8_1.1.4         R6_2.4.0          
    ## [10] lazyeval_0.2.2     colorspace_1.4-1   nnet_7.3-12       
    ## [13] withr_2.1.2        tidyselect_0.2.5   gridExtra_2.3     
    ## [16] prettyunits_1.0.2  processx_3.3.0     curl_3.3          
    ## [19] compiler_3.6.1     cli_1.1.0          rvest_0.3.3       
    ## [22] xml2_1.2.0         labeling_0.3       tseries_0.10-46   
    ## [25] scales_1.0.0       lmtest_0.9-36      fracdiff_1.4-2    
    ## [28] quadprog_1.5-5     callr_3.2.0        digest_0.6.18     
    ## [31] StanHeaders_2.19.0 rmarkdown_1.12     extraDistr_1.9.1  
    ## [34] pkgconfig_2.0.2    htmltools_0.3.6    htmlwidgets_1.3   
    ## [37] readxl_1.3.1       TTR_0.23-4         rstudioapi_0.10   
    ## [40] quantmod_0.4-14    shiny_1.3.2        generics_0.0.2    
    ## [43] jsonlite_1.6       crosstalk_1.0.0    inline_0.3.18     
    ## [46] magrittr_1.5       loo_2.4.1          munsell_0.5.0     
    ## [49] fansi_0.4.0        stringi_1.4.3      yaml_2.2.0        
    ## [52] pkgbuild_1.2.0     plyr_1.8.4         grid_3.6.1        
    ## [55] parallel_3.6.1     promises_1.0.1     crayon_1.3.4      
    ## [58] lattice_0.20-38    haven_2.1.0        hms_0.4.2         
    ## [61] knitr_1.22         ps_1.3.0           pillar_1.3.1      
    ## [64] codetools_0.2-16   stats4_3.6.1       urca_1.3-0        
    ## [67] glue_1.3.1         evaluate_0.13      RcppParallel_5.1.4
    ## [70] modelr_0.1.4       httpuv_1.5.1       cellranger_1.1.0  
    ## [73] gtable_0.3.0       assertthat_0.2.1   xfun_0.6          
    ## [76] mime_0.6           xtable_1.8-4       broom_0.5.2       
    ## [79] later_0.8.0        timeDate_3043.102
