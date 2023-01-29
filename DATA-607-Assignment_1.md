Assignment 1
================
Mohamed Hassan-El Serafi
January 29, 2023

## Overview

The Coronavirus pandemic has upended the way of life for billions of
people around the world. This dataset explores the views of Americans
towards the response to COVID-19 from President Biden and President
Trump.

## Load Libraries

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   1.0.1 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.5.0 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(devtools)
```

    ## Loading required package: usethis

``` r
library(RCurl)
```

    ## 
    ## Attaching package: 'RCurl'
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     complete

## Importing CSV Dataset

``` r
df <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/covid-19-polls/master/covid_approval_toplines.csv")
```

    ## Rows: 5641 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): subject, modeldate, party, timestamp
    ## dbl (2): approve_estimate, disapprove_estimate
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(df)
```

    ## # A tibble: 6 × 6
    ##   subject modeldate  party approve_estimate disapprove_estimate timestamp       
    ##   <chr>   <chr>      <chr>            <dbl>               <dbl> <chr>           
    ## 1 Biden   11/27/2022 R                 18.6                74.3 02:31:21 27 Nov…
    ## 2 Biden   11/27/2022 D                 80.9                12.8 02:31:11 27 Nov…
    ## 3 Biden   11/27/2022 I                 37.5                43.8 02:31:16 27 Nov…
    ## 4 Biden   11/27/2022 all               47.8                41.8 02:31:28 27 Nov…
    ## 5 Biden   11/26/2022 D                 80.9                12.8 02:31:13 27 Nov…
    ## 6 Biden   11/26/2022 I                 37.5                43.8 02:31:18 27 Nov…

## Snapshot of Data

``` r
glimpse(df)
```

    ## Rows: 5,641
    ## Columns: 6
    ## $ subject             <chr> "Biden", "Biden", "Biden", "Biden", "Biden", "Bide…
    ## $ modeldate           <chr> "11/27/2022", "11/27/2022", "11/27/2022", "11/27/2…
    ## $ party               <chr> "R", "D", "I", "all", "D", "I", "R", "all", "I", "…
    ## $ approve_estimate    <dbl> 18.60035, 80.87721, 37.50505, 47.82518, 80.87721, …
    ## $ disapprove_estimate <dbl> 74.28683, 12.76845, 43.82998, 41.82306, 12.76845, …
    ## $ timestamp           <chr> "02:31:21 27 Nov 2022", "02:31:11 27 Nov 2022", "0…

``` r
sum(is.na(df))
```

    ## [1] 0

Renaming Columns

``` r
df <- as_tibble(df)
new_df <- rename(df, President = subject, Date = modeldate, Party = party, Approve = approve_estimate, Disapprove = disapprove_estimate, Timestamp = timestamp)
```

Replacing Text Values in Party Column

``` r
new_df <- new_df %>% 
  mutate(Party = str_replace(Party, "R", "Republican"))
new_df
```

    ## # A tibble: 5,641 × 6
    ##    President Date       Party      Approve Disapprove Timestamp           
    ##    <chr>     <chr>      <chr>        <dbl>      <dbl> <chr>               
    ##  1 Biden     11/27/2022 Republican    18.6       74.3 02:31:21 27 Nov 2022
    ##  2 Biden     11/27/2022 D             80.9       12.8 02:31:11 27 Nov 2022
    ##  3 Biden     11/27/2022 I             37.5       43.8 02:31:16 27 Nov 2022
    ##  4 Biden     11/27/2022 all           47.8       41.8 02:31:28 27 Nov 2022
    ##  5 Biden     11/26/2022 D             80.9       12.8 02:31:13 27 Nov 2022
    ##  6 Biden     11/26/2022 I             37.5       43.8 02:31:18 27 Nov 2022
    ##  7 Biden     11/26/2022 Republican    18.6       74.3 02:31:23 27 Nov 2022
    ##  8 Biden     11/26/2022 all           47.8       41.8 02:31:30 27 Nov 2022
    ##  9 Biden     11/25/2022 I             37.5       43.8 02:31:20 27 Nov 2022
    ## 10 Biden     11/25/2022 Republican    18.6       74.3 02:31:25 27 Nov 2022
    ## # … with 5,631 more rows

``` r
new_df <- new_df %>% 
  mutate(Party = str_replace(Party, "D", "Democrat"))
new_df
```

    ## # A tibble: 5,641 × 6
    ##    President Date       Party      Approve Disapprove Timestamp           
    ##    <chr>     <chr>      <chr>        <dbl>      <dbl> <chr>               
    ##  1 Biden     11/27/2022 Republican    18.6       74.3 02:31:21 27 Nov 2022
    ##  2 Biden     11/27/2022 Democrat      80.9       12.8 02:31:11 27 Nov 2022
    ##  3 Biden     11/27/2022 I             37.5       43.8 02:31:16 27 Nov 2022
    ##  4 Biden     11/27/2022 all           47.8       41.8 02:31:28 27 Nov 2022
    ##  5 Biden     11/26/2022 Democrat      80.9       12.8 02:31:13 27 Nov 2022
    ##  6 Biden     11/26/2022 I             37.5       43.8 02:31:18 27 Nov 2022
    ##  7 Biden     11/26/2022 Republican    18.6       74.3 02:31:23 27 Nov 2022
    ##  8 Biden     11/26/2022 all           47.8       41.8 02:31:30 27 Nov 2022
    ##  9 Biden     11/25/2022 I             37.5       43.8 02:31:20 27 Nov 2022
    ## 10 Biden     11/25/2022 Republican    18.6       74.3 02:31:25 27 Nov 2022
    ## # … with 5,631 more rows

``` r
new_df <- new_df %>% 
  mutate(Party = str_replace(Party, "I", "Independent"))
new_df
```

    ## # A tibble: 5,641 × 6
    ##    President Date       Party       Approve Disapprove Timestamp           
    ##    <chr>     <chr>      <chr>         <dbl>      <dbl> <chr>               
    ##  1 Biden     11/27/2022 Republican     18.6       74.3 02:31:21 27 Nov 2022
    ##  2 Biden     11/27/2022 Democrat       80.9       12.8 02:31:11 27 Nov 2022
    ##  3 Biden     11/27/2022 Independent    37.5       43.8 02:31:16 27 Nov 2022
    ##  4 Biden     11/27/2022 all            47.8       41.8 02:31:28 27 Nov 2022
    ##  5 Biden     11/26/2022 Democrat       80.9       12.8 02:31:13 27 Nov 2022
    ##  6 Biden     11/26/2022 Independent    37.5       43.8 02:31:18 27 Nov 2022
    ##  7 Biden     11/26/2022 Republican     18.6       74.3 02:31:23 27 Nov 2022
    ##  8 Biden     11/26/2022 all            47.8       41.8 02:31:30 27 Nov 2022
    ##  9 Biden     11/25/2022 Independent    37.5       43.8 02:31:20 27 Nov 2022
    ## 10 Biden     11/25/2022 Republican     18.6       74.3 02:31:25 27 Nov 2022
    ## # … with 5,631 more rows

``` r
new_df <- new_df %>% 
  mutate(Party = str_replace(Party, "all", "Overall"))
new_df
```

    ## # A tibble: 5,641 × 6
    ##    President Date       Party       Approve Disapprove Timestamp           
    ##    <chr>     <chr>      <chr>         <dbl>      <dbl> <chr>               
    ##  1 Biden     11/27/2022 Republican     18.6       74.3 02:31:21 27 Nov 2022
    ##  2 Biden     11/27/2022 Democrat       80.9       12.8 02:31:11 27 Nov 2022
    ##  3 Biden     11/27/2022 Independent    37.5       43.8 02:31:16 27 Nov 2022
    ##  4 Biden     11/27/2022 Overall        47.8       41.8 02:31:28 27 Nov 2022
    ##  5 Biden     11/26/2022 Democrat       80.9       12.8 02:31:13 27 Nov 2022
    ##  6 Biden     11/26/2022 Independent    37.5       43.8 02:31:18 27 Nov 2022
    ##  7 Biden     11/26/2022 Republican     18.6       74.3 02:31:23 27 Nov 2022
    ##  8 Biden     11/26/2022 Overall        47.8       41.8 02:31:30 27 Nov 2022
    ##  9 Biden     11/25/2022 Independent    37.5       43.8 02:31:20 27 Nov 2022
    ## 10 Biden     11/25/2022 Republican     18.6       74.3 02:31:25 27 Nov 2022
    ## # … with 5,631 more rows

## Approval Ratings of President Biden’s Handling of COVID-19 Response

The following subsets of data focuses on American views of President
Biden’s response to COVID-19, parsed down by political party affiliation
and sorted in descending order by highest job approval rating:

``` r
filter(new_df, President == 'Biden' & Party == 'Republican') %>%
  arrange(desc(Approve))
```

    ## # A tibble: 667 × 6
    ##    President Date      Party      Approve Disapprove Timestamp           
    ##    <chr>     <chr>     <chr>        <dbl>      <dbl> <chr>               
    ##  1 Biden     5/11/2021 Republican    34.0       61.5 08:40:22 12 May 2021
    ##  2 Biden     5/10/2021 Republican    34.0       61.5 09:10:29 10 May 2021
    ##  3 Biden     5/9/2021  Republican    34.0       61.5 09:10:30 10 May 2021
    ##  4 Biden     5/8/2021  Republican    34.0       61.5 09:10:30 10 May 2021
    ##  5 Biden     5/12/2021 Republican    33.8       61.5 09:05:15 12 May 2021
    ##  6 Biden     5/13/2021 Republican    33.6       61.5 09:00:16 13 May 2021
    ##  7 Biden     5/16/2021 Republican    33.5       61.7 08:55:37 17 May 2021
    ##  8 Biden     5/15/2021 Republican    33.5       61.7 08:55:37 17 May 2021
    ##  9 Biden     5/14/2021 Republican    33.5       61.7 08:55:38 17 May 2021
    ## 10 Biden     6/19/2021 Republican    33.1       62.4 09:25:41 21 Jun 2021
    ## # … with 657 more rows

``` r
filter(new_df, President == 'Biden' & Party == 'Democrat') %>%
  arrange(desc(Approve))
```

    ## # A tibble: 662 × 6
    ##    President Date      Party    Approve Disapprove Timestamp           
    ##    <chr>     <chr>     <chr>      <dbl>      <dbl> <chr>               
    ##  1 Biden     3/22/2021 Democrat    92.7       5.00 09:45:07 22 Mar 2021
    ##  2 Biden     3/21/2021 Democrat    92.7       5.00 09:45:08 22 Mar 2021
    ##  3 Biden     3/20/2021 Democrat    92.7       5.00 09:45:08 22 Mar 2021
    ##  4 Biden     3/24/2021 Democrat    92.6       4.75 13:20:06 24 Mar 2021
    ##  5 Biden     3/23/2021 Democrat    92.6       4.75 10:45:07 24 Mar 2021
    ##  6 Biden     6/29/2021 Democrat    92.5       5.69 21:05:07 29 Jun 2021
    ##  7 Biden     6/28/2021 Democrat    92.5       5.69 21:05:08 29 Jun 2021
    ##  8 Biden     6/27/2021 Democrat    92.5       5.69 21:05:08 29 Jun 2021
    ##  9 Biden     6/26/2021 Democrat    92.5       5.69 21:05:08 29 Jun 2021
    ## 10 Biden     6/25/2021 Democrat    92.5       5.69 21:05:09 29 Jun 2021
    ## # … with 652 more rows

``` r
filter(new_df, President == 'Biden' & Party == 'Independent') %>%
  arrange(desc(Approve))
```

    ## # A tibble: 665 × 6
    ##    President Date      Party       Approve Disapprove Timestamp           
    ##    <chr>     <chr>     <chr>         <dbl>      <dbl> <chr>               
    ##  1 Biden     3/31/2021 Independent    60.9       32.0 19:35:12 31 Mar 2021
    ##  2 Biden     5/27/2021 Independent    60.8       32.7 07:40:17 28 May 2021
    ##  3 Biden     5/26/2021 Independent    60.8       32.7 21:20:12 26 May 2021
    ##  4 Biden     4/13/2021 Independent    60.7       31.6 09:10:11 13 Apr 2021
    ##  5 Biden     4/12/2021 Independent    60.7       31.6 10:15:13 12 Apr 2021
    ##  6 Biden     5/3/2021  Independent    60.6       32.6 10:15:24  5 May 2021
    ##  7 Biden     5/2/2021  Independent    60.6       32.6 10:15:25  5 May 2021
    ##  8 Biden     5/1/2021  Independent    60.6       32.6 20:20:10  1 May 2021
    ##  9 Biden     4/30/2021 Independent    60.6       32.6 16:55:14 30 Apr 2021
    ## 10 Biden     4/11/2021 Independent    60.5       31.7 16:35:26 11 Apr 2021
    ## # … with 655 more rows

``` r
filter(new_df, President == 'Biden' & Party == 'Overall') %>%
  arrange(desc(Approve))
```

    ## # A tibble: 671 × 6
    ##    President Date      Party   Approve Disapprove Timestamp           
    ##    <chr>     <chr>     <chr>     <dbl>      <dbl> <chr>               
    ##  1 Biden     1/25/2021 Overall    69         30.1 11:11:16  8 Feb 2021
    ##  2 Biden     1/24/2021 Overall    69         30.1 11:11:17  8 Feb 2021
    ##  3 Biden     1/23/2021 Overall    69         30.1 11:11:17  8 Feb 2021
    ##  4 Biden     1/22/2021 Overall    69         31   11:11:18  8 Feb 2021
    ##  5 Biden     5/27/2021 Overall    63.4       32.1 07:40:33 28 May 2021
    ##  6 Biden     5/26/2021 Overall    63.4       32.1 21:20:20 26 May 2021
    ##  7 Biden     5/11/2021 Overall    63.3       31.1 08:40:30 12 May 2021
    ##  8 Biden     5/16/2021 Overall    63.3       31.3 08:55:52 17 May 2021
    ##  9 Biden     5/15/2021 Overall    63.3       31.3 08:55:53 17 May 2021
    ## 10 Biden     5/14/2021 Overall    63.3       31.3 08:55:53 17 May 2021
    ## # … with 661 more rows

The favorable ratings of President Biden varied based on political party
affiliation. The highest approval ratings the President received from
Democrats was under 93%, and 34% from people who identified as
Republicans. Among Americans who identified as Independents, the highest
approval rating was just under 61%, and overall Americans gave a high of
69% approval rating. All of the highest approval ratings occurred in
2021, which may indicate that President Biden’s lowest approval ratings
occurred in 2022.

## Comparing Approval/Disapproval Ratings of Each President

How does President Biden and President Trump’s approval ratings of
responding to the Coronavirus compare with each other?

![](DATA-607-Assignment_1_files/figure-gfm/pressure-1.png)<!-- --> As
the boxplot shows, President Biden’s median approval ratings are almost
10 percentage points better than President Trump. Trump’s lowest
approval ratings are lower than Biden’s lowest approval ratings, and
Biden’s highest approval ratings are higher than Trump’s highest
approval ratings. While Biden’s approval ratings are higher than Trump,
how does each President’s disapproval ratings compare with each other?

``` r
ggplot(new_df, aes(x = President, y=Disapprove,color=President)) +
  geom_boxplot() + 
  labs(x='President', y='Disapproval Rating', title='COVID-19 Response Job Disapproval Ratings')
```

![](DATA-607-Assignment_1_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
When comparing each President’s disapproval ratings, the median
disapproval rating of Trump is about 10 percentage points higher than
Biden. The highest and lowest disapproval rating of Trump is higher than
that of Biden, respectively.

## Conclusion/Next Steps

The initial response by President Biden to the Coronavirus pandemic was
positive. He received his highest approval ratings in his first year as
President. By comparison, while President Trump did receive high
approval ratings for handling the COVID-19 response, they were not as
high as President Biden. The high favorable ratings President Biden
received broke along party lines, with the President receiving a high of
almost 93% approval from Democrats and only a high of 34% from
Republicans. However, Independents, who do not have a party affiliation,
had a high of 60% approval of President Biden’s response. This indicates
that while there is certain polarization when examining the how
respondents approved/disapproved of President Biden, those who
identified as Independents may not hold the same inclinations of bias.
Further exploration can be done comparing the approval and disapproval
ratings of each President, analyzing how party affiliation dictated
whether respondents were more likely to approve or disapprove each
President’s handling of the pandemic.
