COVID 19
================
Juan Camilo Azuero
2/9/2022

The purpose of this project is to explore how COVID has developed
through time how it has affected different regions.

## Import libraries

``` r
library("tidyverse")
library("zoo")
library("tidyr")
library("dplyr")
library("lemon")
```

## Import data

Data is imported from a public repository of the Jhon Hopkins university

``` r
base_url <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/"
access_type <- "?accessType=DOWNLOAD"
confirmed_filename <- "csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
deaths_filename <- "csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
recovered_filename <- "csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
regions_filename <- "UID_ISO_FIPS_LookUp_Table.csv"

cases <- read_csv(paste(base_url,confirmed_filename,access_type,sep=""))
deaths <- read_csv(paste(base_url,deaths_filename,access_type,sep=""))
regions <- read_csv(paste(base_url,regions_filename,access_type,sep=""))
```

## Initial transformations

``` r
colnames(cases)[colnames(cases) == "Country/Region"] <- "Country"
colnames(deaths)[colnames(deaths) == "Country/Region"] <- "Country"
colnames(regions)[colnames(regions) == "Country_Region"] <- "Country"
# Delete info from places that are not countries, delete unnecesary columns
# and transform data into long format
cases <- cases %>% 
    filter(is.na(cases["Province/State"])) %>% 
    select(-c("Province/State","Lat","Long")) %>%
    gather("day","cases",-c("Country"))
deaths <- deaths %>% 
    filter(is.na(deaths["Province/State"])) %>% 
    select(-c("Province/State","Lat","Long")) %>%
    gather("day","deaths",-c("Country"))
regions <- regions %>% filter(is.na(regions["Province_State"])) %>% select("Country","Population")
# Merge datasets into summary
summary <- merge(cases,deaths,by=c("Country","day"))
summary <- merge(summary,regions,by="Country", all.y = FALSE)
summary <- summary %>% filter(!is.na(summary['Population']))

summary$day <- as.Date(summary$day, format = "%m/%d/%y")
summary <- summary[order(summary["Country"], summary["day"]),]
# Obtains daily info from cumulative columns
summary_aux <- summary %>% select("Country","day","cases","deaths") %>% mutate(day = day+1)
colnames(summary_aux)[colnames(summary_aux) == "cases"] <- "cases_lag"
colnames(summary_aux)[colnames(summary_aux) == "deaths"] <- "deaths_lag"
summary <- merge(summary,summary_aux,by=c("Country","day"), all.y=FALSE)
summary['cases_day'] = summary['cases']-summary['cases_lag']
summary['deaths_day'] = summary['deaths']-summary['deaths_lag']
summary <- summary %>% select(-c("cases_lag","deaths_lag"))
# Normalize per million
summary['cases_per_million'] = 1000000*summary['cases']/summary['Population']
summary['deaths_per_million'] = 1000000*summary['deaths']/summary['Population']
summary['cases_day_per_million'] = 1000000*summary['cases_day']/summary['Population']
summary['deaths_day_per_million'] = 1000000*summary['deaths_day']/summary['Population']
# Global summary
global_summary <- summary %>% group_by(day) %>% summarize(cases=sum(cases), deaths=sum(deaths),
                                                         cases_day=sum(cases_day), deaths_day=sum(deaths_day))
global_summary["cases_day_wa"] = rollmean(global_summary["cases_day"],7,c(NA,NULL,NA))
global_summary["deaths_day_wa"] = rollmean(global_summary["deaths_day"],7,c(NA,NULL,NA))
global_summary["mortality_rate"] <- global_summary["deaths"]/global_summary["cases"]
```

## Global plots

First let’s plot the evolution over time of the total number cases and
deaths

``` r
coeff <- 100
deaths_color <- rgb(0.9, 0.2, 0.1, 1)
cases_color <- rgb(0.2, 0.6, 0.9, 1)
global_summary %>% select("day","cases","deaths") %>% 
  ggplot(aes(x=day)) + 
  geom_line(aes(y=cases),color=cases_color) + 
  geom_line(aes(y=deaths*coeff),color=deaths_color) + 
  xlab("Date") + 
  scale_y_continuous(
    name = "Cases",
    sec.axis = sec_axis(~./coeff, name="Deaths")
  ) +
  theme(
    axis.title.y = element_text(color = cases_color),
    axis.title.y.right = element_text(color = deaths_color),
    plot.title = element_text(size=14, face="bold.italic")
  ) + 
  ggtitle("Cases and deaths")
```

![](covid_files/figure-gfm/plots_1-1.png)<!-- -->

Now let’s repeat the previous plot, but using a logarithmic axis for the
amount of cases and deaths.

``` r
global_summary %>% select("day","cases","deaths") %>% 
  ggplot(aes(x=day)) + 
  geom_line(aes(y=cases,colour="Cases")) + 
  geom_line(aes(y=deaths,colour="Deaths")) +
  xlab("Date") + 
  ylab("Number of people") + 
  scale_y_continuous(trans='log10') +
  scale_colour_manual("", breaks = c("Cases", "Deaths"), values = c(cases_color,deaths_color)) + 
  ggtitle("Cases and deaths")+
  theme(plot.title = element_text(size=14, face="bold.italic"))
```

![](covid_files/figure-gfm/plots_2-1.png)<!-- -->

The first plot shows how cases and deaths has risen consistently since
the start of 2020. The plot also shows how since 2022 the number of
cases per day increased considerably due to the omicron variant.

As the nature of contagion is exponential, the second graph is very
useful to understand the evolution of the pandemic. The logarithmic
transformation serves to see how high exponential growth in the first 3
months decayed until today. The wave because of omicron at the beginning
of 2022 produces an upward turn of the cases, but deaths stayed flat.

In order to further explore the evolution of cases and deaths through
time, lets plot also the rate of change for these 2 variables.

``` r
global_summary %>% select("day","cases","deaths","cases_day_wa","deaths_day_wa") %>% 
  ggplot(aes(x=day),yaxp = c(0, 1, 10)) + 
  geom_line(aes(y=100*cases_day_wa/cases,colour="Cases")) + 
  geom_line(aes(y=100*deaths_day_wa/deaths,colour="Deaths")) +
  xlab("Date") + 
  ylab("Percentage increase") + 
  scale_y_continuous(trans='log10',breaks=c(0,0.1,0.2,0.5,1,2,5,10,20,50,100)) +
  scale_colour_manual("", breaks = c("Cases", "Deaths"), values = c(cases_color,deaths_color)) + 
  ggtitle("Rate of change for deaths and confirmed cases") +
  theme(plot.title = element_text(size=14, face="bold.italic"))
```

![](covid_files/figure-gfm/plots_3-1.png)<!-- -->

This plot displays how cases and deaths increased daily by approximate
30% at the beginning of the pandemic, and has decayed until reaching
levels of daily change of 0.2% and 0.1% for cases and deaths
respectively.

Now let’s plot the number of daily cases and deaths.

``` r
global_summary %>% select("day","cases_day_wa") %>% 
  ggplot(aes(x=day, y=cases_day_wa)) + 
  geom_line() + 
  xlab("day")+ 
  ylab("Daily infections") +
  ggtitle("Weekly average of cases per day") +
  theme(plot.title = element_text(size=14, face="bold.italic"))
```

![](covid_files/figure-gfm/plots_4-1.png)<!-- -->

``` r
global_summary %>% select("day","deaths_day_wa") %>% 
  ggplot(aes(x=day, y=deaths_day_wa)) + 
  geom_line() + 
  xlab("day") +
  ylab("Daily deaths") +
  ggtitle("Weekly average of deaths per day") +
  theme(plot.title = element_text(size=14, face="bold.italic"))
```

![](covid_files/figure-gfm/plots_4-2.png)<!-- -->

In the plot of daily cases a steady increase in all 2020 is shown ending
in a peak of cases at the end of the year. During 2021 there were
another 2 similar peaks, and then in December the number of cases
skyrocketed because of the omicron variant. The plot of daily deaths has
some considerable differences including an early peak in April and the
absence of immense growth in the end of 2021. The early peak is probably
not seen in the cases plot because detection was not as good at the
beginning of the pandemic, and there is no mortality increase in the end
of 2021 thanks to the rollout of the vaccine and the lower severity of
omicron infections. The following plot displays how the measured
mortality peaks in April (probably because of lack of diagnostic tools)
stabilizes at 2% in 2021 and turn downward in 2022 for the reasons
mentioned.

``` r
global_summary %>% select("day","mortality_rate") %>% 
  ggplot(aes(x=day, y=mortality_rate)) + 
  geom_line() + 
  xlab("day") +
  ylab("Mortality rate (deaths/cases)") +
  ggtitle("Acumulated mortality") +
  theme(plot.title = element_text(size=14, face="bold.italic"))
```

![](covid_files/figure-gfm/plots_5-1.png)<!-- -->

## Analysis by country

Now let’s analyze how the outbreak of COVID affected the different
countries. Seeking to avoid outliers produced by small countries it was
decided to filter all countries with less than 1 million habitants.

``` r
summary_total <- summary %>% group_by(Country) %>% summarize(
  cases=sum(cases_day),
  deaths=sum(deaths_day),
  population=mean(Population)
)
summary_total["mortality_rate"] <- summary_total["deaths"]/summary_total["cases"]
summary_total["deaths_per_million"] <- 1000000*summary_total["deaths"]/summary_total["population"]
summary_total["cases_per_million"] <- 1000000*summary_total["cases"]/summary_total["population"]
summary_total_big <- summary_total %>% filter(population > 1000000)
```

Let’s organize countries by the normalized amount of deaths, cases and
by the mortality rate to see what we can find.

### Least deaths per million people

``` r
head(summary_total_big[order(summary_total_big$deaths_per_million),],15)
```

| Country                  |  cases | deaths | population | mortality\_rate | deaths\_per\_million | cases\_per\_million |
|:-------------------------|-------:|-------:|-----------:|----------------:|---------------------:|--------------------:|
| Burundi                  |  37902 |     38 |   11890781 |       0.0010026 |             3.195753 |           3187.5114 |
| New Zealand              |  22328 |     53 |    4822233 |       0.0023737 |            10.990759 |           4630.2201 |
| Chad                     |   7216 |    190 |   16425859 |       0.0263304 |            11.567127 |            439.3073 |
| South Sudan              |  16897 |    137 |   11193729 |       0.0081079 |            12.238996 |           1509.5059 |
| Niger                    |   8712 |    303 |   24206636 |       0.0347796 |            12.517229 |            359.9013 |
| Tajikistan               |  17773 |    125 |    9537642 |       0.0070331 |            13.105965 |           1863.4585 |
| Tanzania                 |  33436 |    792 |   59734213 |       0.0236870 |            13.258733 |            559.7462 |
| Benin                    |  26552 |    163 |   12123198 |       0.0061389 |            13.445297 |           2190.1812 |
| Congo (Kinshasa)         |  85793 |   1316 |   89561404 |       0.0153392 |            14.693829 |            957.9238 |
| Nigeria                  | 254016 |   3141 |  206139587 |       0.0123654 |            15.237248 |           1232.2524 |
| Sierra Leone             |   7652 |    125 |    7976985 |       0.0163356 |            15.670081 |            959.2597 |
| Burkina Faso             |  20721 |    375 |   20903278 |       0.0180976 |            17.939770 |            991.2799 |
| Central African Republic |  14187 |    113 |    4829764 |       0.0079650 |            23.396588 |           2937.4106 |
| Eritrea                  |   9675 |    103 |    3546427 |       0.0106460 |            29.043316 |           2728.0979 |
| Cote d’Ivoire            |  81213 |    791 |   26378275 |       0.0097398 |            29.986798 |           3078.7836 |

### Most deaths per million people

``` r
tail(summary_total_big[order(summary_total_big$deaths_per_million),],15)
```

| Country                |    cases | deaths | population | mortality\_rate | deaths\_per\_million | cases\_per\_million |
|:-----------------------|---------:|-------:|-----------:|----------------:|---------------------:|--------------------:|
| US                     | 77925898 | 922840 |  329466283 |       0.0118425 |             2801.015 |            236521.6 |
| Poland                 |  5388907 | 108137 |   37846605 |       0.0200666 |             2857.244 |            142388.1 |
| Slovenia               |   851416 |   6099 |    2078932 |       0.0071634 |             2933.718 |            409544.9 |
| Lithuania              |   820131 |   8134 |    2722291 |       0.0099179 |             2987.925 |            301265.0 |
| Brazil                 | 27552267 | 639151 |  212559409 |       0.0231978 |             3006.929 |            129621.5 |
| Romania                |  2574384 |  61676 |   19237682 |       0.0239576 |             3206.000 |            133819.9 |
| Slovakia               |  1870119 |  18105 |    5459643 |       0.0096812 |             3316.151 |            342535.0 |
| Czechia                |  3403469 |  37873 |   10708982 |       0.0111278 |             3536.564 |            317814.4 |
| Croatia                |  1018367 |  14537 |    4105268 |       0.0142748 |             3541.060 |            248063.5 |
| Georgia                |  1458093 |  15577 |    3989175 |       0.0106831 |             3904.817 |            365512.4 |
| North Macedonia        |   287481 |   8768 |    2083380 |       0.0304994 |             4208.546 |            137987.8 |
| Hungary                |  1717072 |  42631 |    9660350 |       0.0248277 |             4412.987 |            177744.3 |
| Bosnia and Herzegovina |   364610 |  15096 |    3280815 |       0.0414031 |             4601.296 |            111134.0 |
| Bulgaria               |  1042954 |  34591 |    6948445 |       0.0331664 |             4978.236 |            150098.9 |
| Peru                   |  3449712 | 208466 |   32971846 |       0.0604300 |             6322.546 |            104626.0 |

The countries with higher mortality rates are principally countries from
eastern Europe where omicron has been particularly severe, Peru where
the health system collapsed catastrophically in the first wave, and
countries like Brazil and the USA where distancing and other measures
were famously dismissed by their presidents.

On the other hand, very surprisingly the countries with the least number
of deaths per million people are African countries with limited
exceptions like New Zeland and Tanzania. In
[this](https://www.bbc.com/news/world-africa-54418613) BBC report Anne
Soy points to the early adoption of distancing and other measures, the
support from the public, the relatively young population, the warmer
climate and the good health systems which had previous experience
dealing with viruses like Ebola.

Now let’s explore the number of reported infections

### Least reported infections per million people

``` r
head(summary_total_big[order(summary_total_big$cases_per_million),],15)
```

| Country          |  cases | deaths | population | mortality\_rate | deaths\_per\_million | cases\_per\_million |
|:-----------------|-------:|-------:|-----------:|----------------:|---------------------:|--------------------:|
| Niger            |   8712 |    303 |   24206636 |       0.0347796 |             12.51723 |            359.9013 |
| Yemen            |  11679 |   2107 |   29825968 |       0.1804093 |             70.64314 |            391.5715 |
| Chad             |   7216 |    190 |   16425859 |       0.0263304 |             11.56713 |            439.3073 |
| Tanzania         |  33436 |    792 |   59734213 |       0.0236870 |             13.25873 |            559.7462 |
| Taiwan\*         |  19620 |    851 |   23816775 |       0.0433741 |             35.73112 |            823.7891 |
| Congo (Kinshasa) |  85793 |   1316 |   89561404 |       0.0153392 |             14.69383 |            957.9238 |
| Sierra Leone     |   7652 |    125 |    7976985 |       0.0163356 |             15.67008 |            959.2597 |
| Burkina Faso     |  20721 |    375 |   20903278 |       0.0180976 |             17.93977 |            991.2799 |
| Nigeria          | 254016 |   3141 |  206139587 |       0.0123654 |             15.23725 |           1232.2524 |
| Sudan            |  59903 |   3831 |   43849269 |       0.0639534 |             87.36748 |           1366.1117 |
| Liberia          |   7359 |    290 |    5057677 |       0.0394075 |             57.33858 |           1455.0158 |
| Mali             |  30266 |    716 |   20250834 |       0.0236569 |             35.35657 |           1494.5557 |
| South Sudan      |  16897 |    137 |   11193729 |       0.0081079 |             12.23900 |           1509.5059 |
| Somalia          |  26203 |   1340 |   15893219 |       0.0511392 |             84.31269 |           1648.6906 |
| Tajikistan       |  17773 |    125 |    9537642 |       0.0070331 |             13.10596 |           1863.4585 |

### Most reported infections per million people

``` r
tail(summary_total_big[order(summary_total_big$cases_per_million),],15)
```

| Country     |    cases | deaths | population | mortality\_rate | deaths\_per\_million | cases\_per\_million |
|:------------|---------:|-------:|-----------:|----------------:|---------------------:|--------------------:|
| Bahrain     |   468623 |   1427 |    1701583 |       0.0030451 |             838.6309 |            275404.1 |
| Latvia      |   522766 |   5029 |    1886202 |       0.0096200 |            2666.2044 |            277152.7 |
| Switzerland |  2586724 |  12963 |    8654618 |       0.0050114 |            1497.8131 |            298883.7 |
| Belgium     |  3447539 |  29724 |   11492641 |       0.0086218 |            2586.3507 |            299978.0 |
| Lithuania   |   820131 |   8134 |    2722291 |       0.0099179 |            2987.9245 |            301265.0 |
| Portugal    |  3093723 |  20565 |   10196707 |       0.0066473 |            2016.8276 |            303404.1 |
| Czechia     |  3403469 |  37873 |   10708982 |       0.0111278 |            3536.5640 |            317814.4 |
| Estonia     |   423499 |   2115 |    1326539 |       0.0049941 |            1594.3745 |            319251.1 |
| France      | 21186090 | 132323 |   65249843 |       0.0062457 |            2027.9436 |            324691.8 |
| Netherlands |  5789483 |  21402 |   17134873 |       0.0036967 |            1249.0317 |            337877.2 |
| Slovakia    |  1870119 |  18105 |    5459643 |       0.0096812 |            3316.1509 |            342535.0 |
| Georgia     |  1458093 |  15577 |    3989175 |       0.0106831 |            3904.8174 |            365512.4 |
| Israel      |  3451526 |   9624 |    8655541 |       0.0027883 |            1111.8889 |            398764.9 |
| Denmark     |  2356873 |   4109 |    5837213 |       0.0017434 |             703.9318 |            403766.8 |
| Slovenia    |   851416 |   6099 |    2078932 |       0.0071634 |            2933.7179 |            409544.9 |

The countries with the least number of reported infections per million
are African countries for the reasons commented before and also probably
influenced by under reporting. The countries with most number of cases
per million are countries from eastern Europe that also have relatively
large amounts of deaths, and countries with low deaths per million that
did a very good job in testing and isolating infected people.

The two tables below display the countries with higher and lower
mortality rates.

### Lower mortality rates

``` r
head(summary_total_big[order(summary_total_big$mortality_rate),],15)
```

| Country              |   cases | deaths | population | mortality\_rate | deaths\_per\_million | cases\_per\_million |
|:---------------------|--------:|-------:|-----------:|----------------:|---------------------:|--------------------:|
| Burundi              |   37902 |     38 |   11890781 |       0.0010026 |             3.195753 |            3187.511 |
| Norway               | 1049811 |   1513 |    5421242 |       0.0014412 |           279.087338 |          193647.692 |
| Denmark              | 2356873 |   4109 |    5837213 |       0.0017434 |           703.931825 |          403766.832 |
| Qatar                |  351402 |    658 |    2881060 |       0.0018725 |           228.388163 |          121969.692 |
| Singapore            |  478577 |    906 |    5850343 |       0.0018931 |           154.862715 |           81803.238 |
| New Zealand          |   22328 |     53 |    4822233 |       0.0023737 |            10.990759 |            4630.220 |
| Mongolia             |  885554 |   2148 |    3278292 |       0.0024256 |           655.219242 |          270126.639 |
| United Arab Emirates |  869428 |   2287 |    9890400 |       0.0026305 |           231.234328 |           87906.253 |
| Cyprus               |  293040 |    788 |    1207361 |       0.0026891 |           652.663122 |          242711.169 |
| Israel               | 3451526 |   9624 |    8655541 |       0.0027883 |          1111.888905 |          398764.907 |
| Bahrain              |  468623 |   1427 |    1701583 |       0.0030451 |           838.630851 |          275404.138 |
| Netherlands          | 5789483 |  21402 |   17134873 |       0.0036967 |          1249.031726 |          337877.205 |
| Finland              |  582383 |   2214 |    5540718 |       0.0038016 |           399.587201 |          105109.663 |
| Kuwait               |  603869 |   2521 |    4270563 |       0.0041747 |           590.320293 |          141402.668 |
| Laos                 |  139244 |    597 |    7275556 |       0.0042874 |            82.055584 |           19138.606 |

### Higher mortality rates

``` r
tail(summary_total_big[order(summary_total_big$mortality_rate),],15)
```

| Country                |   cases | deaths | population | mortality\_rate | deaths\_per\_million | cases\_per\_million |
|:-----------------------|--------:|-------:|-----------:|----------------:|---------------------:|--------------------:|
| Bulgaria               | 1042954 |  34591 |    6948445 |       0.0331664 |           4978.23614 |         150098.9070 |
| Niger                  |    8712 |    303 |   24206636 |       0.0347796 |             12.51723 |            359.9013 |
| Burma                  |  548357 |  19311 |   54409794 |       0.0352161 |            354.91772 |          10078.2775 |
| Liberia                |    7359 |    290 |    5057677 |       0.0394075 |             57.33858 |           1455.0158 |
| Bosnia and Herzegovina |  364610 |  15096 |    3280815 |       0.0414031 |           4601.29571 |         111133.9713 |
| Taiwan\*               |   19620 |    851 |   23816775 |       0.0433741 |             35.73112 |            823.7891 |
| Afghanistan            |  171246 |   7501 |   38928341 |       0.0438025 |            192.68738 |           4399.0059 |
| Ecuador                |  799140 |  35036 |   17643060 |       0.0438421 |           1985.82332 |          45294.8638 |
| Somalia                |   26203 |   1340 |   15893219 |       0.0511392 |             84.31269 |           1648.6906 |
| Egypt                  |  457081 |  23409 |  102334403 |       0.0512141 |            228.75005 |           4466.5429 |
| Syria                  |   52881 |   3032 |   17500657 |       0.0573363 |            173.25064 |           3021.6580 |
| Mexico                 | 5292706 | 312819 |  127792286 |       0.0591038 |           2447.87076 |          41416.4749 |
| Peru                   | 3449712 | 208466 |   32971846 |       0.0604300 |           6322.54560 |         104625.9891 |
| Sudan                  |   59903 |   3831 |   43849269 |       0.0639534 |             87.36748 |           1366.1117 |
| Yemen                  |   11679 |   2107 |   29825968 |       0.1804093 |             70.64314 |            391.5715 |

Assuming the almost certainly incorrect, but not too far from reality
claim, that the mortality rate of the virus is homogeneous trough the
world, it can be concluded that the reported mortality rates correlate
negatively with the effectiveness of the countries testing campaigns,
and that the lower mortality rates are close to the virus real mean. The
tables above then show how developed Eruopean countries excelled
regarding their testing campaigns while countries from Africa, Latin
America and the Middle East were not able to diagnosticate their
citizens effectively. It can also be concluded that the real mortality
of the virus is close to 0.15%.

Finally let’s visualize a scatter plot of the deaths vs number of
infections per million.

``` r
summary_total_big %>% select("deaths_per_million","mortality_rate","Country","population") %>% 
  ggplot(aes(x=mortality_rate, y=deaths_per_million,label=Country)) + 
  geom_text(
    aes(label=ifelse(deaths_per_million>3000|mortality_rate>0.035,as.character(Country),'')),
    size=3,
    hjust=-0.1, 
    vjust=-0.1
  ) +
  geom_point(aes(size=population)) + 
  xlab("Mortality rate (deaths/cases)") +
  ylab("Deaths per million") +
  xlim(0, 0.1) +
  ggtitle("Cases vs Deaths per million") +
  theme(plot.title = element_text(size=14, face="bold.italic"))
```

![](covid_files/figure-gfm/plots_6-1.png)<!-- -->

The countries that appear lower on the y axis have the lowest number of
deaths and represent places where the pandemic had small impact. while
the x axis correlates with the inversely with the effectivity of the
diagnostic effort of the country. This plot shows a cluster of African
countries with high mortality rates and low number of deaths per million
and a cluster of eastern European countries with high death toll as
discussed before.

This analysis can be deepened by including more variables like economic
output, mobility trends, region and vaccination timeline, by using maps.

#### Session info

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
    ##  [1] lemon_0.4.3     zoo_1.8-6       forcats_0.4.0   stringr_1.4.0  
    ##  [5] dplyr_0.8.0.1   purrr_0.3.2     readr_1.3.1     tidyr_0.8.3    
    ##  [9] tibble_2.1.1    ggplot2_3.1.1   tidyverse_1.2.1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_0.2.5 xfun_0.6         haven_2.1.0      lattice_0.20-38 
    ##  [5] colorspace_1.4-1 generics_0.0.2   htmltools_0.3.6  yaml_2.2.0      
    ##  [9] rlang_0.3.4      pillar_1.3.1     glue_1.3.1       withr_2.1.2     
    ## [13] modelr_0.1.4     readxl_1.3.1     plyr_1.8.4       munsell_0.5.0   
    ## [17] gtable_0.3.0     cellranger_1.1.0 rvest_0.3.3      evaluate_0.13   
    ## [21] labeling_0.3     knitr_1.22       curl_3.3         highr_0.8       
    ## [25] broom_0.5.2      Rcpp_1.0.1       scales_1.0.0     backports_1.1.4 
    ## [29] jsonlite_1.6     gridExtra_2.3    hms_0.4.2        digest_0.6.18   
    ## [33] stringi_1.4.3    grid_3.6.1       cli_1.1.0        tools_3.6.1     
    ## [37] magrittr_1.5     lazyeval_0.2.2   crayon_1.3.4     pkgconfig_2.0.2 
    ## [41] xml2_1.2.0       lubridate_1.7.4  assertthat_0.2.1 rmarkdown_1.12  
    ## [45] httr_1.4.0       rstudioapi_0.10  R6_2.4.0         nlme_3.1-139    
    ## [49] compiler_3.6.1
