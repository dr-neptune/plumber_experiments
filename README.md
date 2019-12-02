``` r
data(ausNLHYClaimByState)

aus <- ausNLHYClaimByState %>%
    as_tibble()

aus %>%
    head()
```

    ## # A tibble: 6 x 89
    ##   Class NSWACT200506 NSWACT200512 NSWACT200606 NSWACT200612 NSWACT200706
    ##   <chr>        <dbl>        <dbl>        <dbl>        <dbl>        <dbl>
    ## 1 Hous…          659          680          632          635         1100
    ## 2 Comm…          386          361          382          413          429
    ## 3 Dome…         1572         1685         1591         1707         1821
    ## 4 Trav…          110          110          112          140          143
    ## 5 Fire…          515          398          460          590          603
    ## 6 Mari…          108          136          125          132          143
    ## # … with 83 more variables: NSWACT200712 <dbl>, NSWACT200806 <dbl>,
    ## #   NSWACT200812 <dbl>, NSWACT200906 <dbl>, NSWACT200912 <dbl>,
    ## #   NSWACT201006 <dbl>, VIC200506 <dbl>, VIC200512 <dbl>, VIC200606 <dbl>,
    ## #   VIC200612 <dbl>, VIC200706 <dbl>, VIC200712 <dbl>, VIC200806 <dbl>,
    ## #   VIC200812 <dbl>, VIC200906 <dbl>, VIC200912 <dbl>, VIC201006 <dbl>,
    ## #   QLD200506 <dbl>, QLD200512 <dbl>, QLD200606 <dbl>, QLD200612 <dbl>,
    ## #   QLD200706 <dbl>, QLD200712 <dbl>, QLD200806 <dbl>, QLD200812 <dbl>,
    ## #   QLD200906 <dbl>, QLD200912 <dbl>, QLD201006 <dbl>, SA200506 <dbl>,
    ## #   SA200512 <dbl>, SA200606 <dbl>, SA200612 <dbl>, SA200706 <dbl>,
    ## #   SA200712 <dbl>, SA200806 <dbl>, SA200812 <dbl>, SA200906 <dbl>,
    ## #   SA200912 <dbl>, SA201006 <dbl>, WA200506 <dbl>, WA200512 <dbl>,
    ## #   WA200606 <dbl>, WA200612 <dbl>, WA200706 <dbl>, WA200712 <dbl>,
    ## #   WA200806 <dbl>, WA200812 <dbl>, WA200906 <dbl>, WA200912 <dbl>,
    ## #   WA201006 <dbl>, TAS200506 <dbl>, TAS200512 <dbl>, TAS200606 <dbl>,
    ## #   TAS200612 <dbl>, TAS200706 <dbl>, TAS200712 <dbl>, TAS200806 <dbl>,
    ## #   TAS200812 <dbl>, TAS200906 <dbl>, TAS200912 <dbl>, TAS201006 <dbl>,
    ## #   NT200506 <dbl>, NT200512 <dbl>, NT200606 <dbl>, NT200612 <dbl>,
    ## #   NT200706 <dbl>, NT200712 <dbl>, NT200806 <dbl>, NT200812 <dbl>,
    ## #   NT200906 <dbl>, NT200912 <dbl>, NT201006 <dbl>, Total200506 <dbl>,
    ## #   Total200512 <dbl>, Total200606 <dbl>, Total200612 <dbl>,
    ## #   Total200706 <dbl>, Total200712 <dbl>, Total200806 <dbl>,
    ## #   Total200812 <dbl>, Total200906 <dbl>, Total200912 <dbl>,
    ## #   Total201006 <dbl>

``` r
data(nzcathist)

nz <- nzcathist %>%
    as_tibble()

nz %>% head()
```

    ## # A tibble: 6 x 10
    ##    Year Quarter Date  FirstDay   Event Type  Location OriginalCost
    ##   <dbl>   <dbl> <chr> <date>     <fct> <fct> <fct>           <dbl>
    ## 1  2014       4 5Oct  2014-10-05 Auck… Powe… Auckland          2  
    ## 2  2014       2 25Jun 2014-06-25 Nels… Flood Nelson,…          2.7
    ## 3  2014       2 9-11… 2014-06-09 Seve… Weat… North a…         37.6
    ## 4  2014       2 17Apr 2014-04-17 East… Floo… <NA>             55.3
    ## 5  2014       1 15-1… 2014-03-15 Cycl… Cycl… <NA>              3.6
    ## 6  2014       1 23-F… 2014-02-23 Cant… Storm Canterb…          4.8
    ## # … with 2 more variables: NormCost2011 <dbl>, NormCost2014 <dbl>

For AUS data
------------

-   **Class** : Class of business.
-   **NSWACTYYYYMM** : New South Wales / Australian Capital Territory
    for year YYYY.
-   **VICYYYYMM** : Victoria in year YYYY reported onDate YYYYMM.
-   **QLDYYYMM** : Queensland in year YYYY reported onDate YYYYMM.
-   **SAYYYYMM** : South Australia in year YYYY reported onDate YYYYMM.
-   **WAYYYYMM** : Western Australia in year YYYY reported onDate
    YYYYMM.
-   **TAYYYYMM** : Tasmania in year YYYY reported onDate YYYYMM.
-   **NTYYYYMM** : Northern Territory in year YYYY reported onDate
    YYYYMM.
-   **TotalYYYYMM** : Total in year YYYY reported onDate YYYYMM.

Data is in values of millions of Australian dollars (AUD)

For NZ data
-----------

nzcathist is a data frame of 9 columns:

-   **Year** : numeric for the Year.
-   **Quarter** : numeric for the quarter of the year.
-   **Date** : character string for the date.
-   **FirstDayaDateobject** : for the first day of natural catastrophe.
-   **Event** : character string describing the event.
-   **Type** : factor describing the event type among the list:
    -   “Cyclone”
    -   “Earthquake”
    -   “Flood”
    -   “Flood, Storm”
    -   “Hailstorm”
    -   “Other”
    -   “Power outage”
    -   “Storm”
    -   “Tornado”
    -   “Weather”
-   **Location** character string describing the location.
-   **OriginalCostOriginal** cost in million of Australian dollars
    (NZD).
-   **NormCost2011Normed** cost in million of 2011 New Zealand dollars
    (NZD).
-   **NormCost2014Normed** cost in million of 2014 New Zealand dollars
    (NZD)
