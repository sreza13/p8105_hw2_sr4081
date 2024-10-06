HW2_Markdown
================
Samiha Reza
2024-10-05

## Question 1

The cleaned dataset has 19 variables: line, station_name,
station_latitude, station_longitude, route1, route2, route3, route4,
route5, route6, route7, route8, route9, route10, route11, entry,
vending, entrance_type, ada. I have formatted variables using
“janitor::clean_names”, and used “select()” to specify which variables
to keep from the original dataset. Then I used “mutate()” to change
“entry” from a character to a logic variable. The dimensions are 1868 x
19. The data is not tidy because there are some repeat observations
instead of having one row per station observation.

Below are the code chunks used to answer questions:

``` r
distinct_stations <- nyc_subway |>
  distinct(station_name, line)

ada_number <- nyc_subway |>
  filter(ada == TRUE) |>
  distinct(station_name, line) |>
  nrow()

no_vending <- nyc_subway |>
  filter(vending == "NO") |>
  distinct(station_name, line) |>
  nrow()

no_vending_entry <- nyc_subway |> 
  filter(vending == "NO", entry == TRUE) |>
  distinct(station_name, line) |>
  nrow()

vending_entry_proportion <- no_vending_entry / no_vending
```

There are 465 distinct stations. 84 of these distinct stations are ADA
compliant. 0.4343434 of station entrances/exits without vending allow
entry, looking at specifically distinct stations.

``` r
a_stations = filter(nyc_subway, route1 == "A")
a_distinct_stations = distinct(a_stations)

a_ada_stations = filter(nyc_subway, route1 == "A", ada == TRUE)
a_ada_distinct_stations = distinct(a_ada_stations)
```

There are 91 distinct stations that service the A train. Of those, 34
are ADA compliant.

## Problem 2

Below is the code to load trash_wheel.

``` r
trash_wheel <- read_excel('./Trash_Wheel_Data.xlsx', 
                sheet = 1 , 
                na = c("NA", ".", "")) |> 
                janitor::clean_names() |> 
                select(dumpster:homes_powered) |> 
                drop_na(dumpster) |>
                mutate(sports_balls = as.integer(round(sports_balls)),
                trash_sheet = "Mr. Trash Wheel", 
                year = as.integer(year))
```

    ## New names:
    ## • `` -> `...15`
    ## • `` -> `...16`

``` r
professor_trash <- read_excel('./Trash_Wheel_Data.xlsx', 
                    sheet = 2,
                    na = c("NA", ".", "")) |> 
                    janitor::clean_names() |> 
                    drop_na(dumpster) |>
                    select(dumpster:homes_powered) |>
                    mutate(
                      trash_sheet = "Professor",
                      year = as.integer(year))
      
                      
gwynnda <- read_excel('./Trash_Wheel_Data.xlsx', 
                    sheet = 3,
                    skip = 1,
                    na = c("NA", ".", "")) |> 
                    janitor::clean_names() |>
                    select(dumpster:homes_powered) |>
                    drop_na(dumpster) |>
                    mutate(
                      trash_sheet = "Gwynnda",
                      year = as.integer(year))
                    
trash <-bind_rows(trash_wheel, professor_trash, gwynnda)                   
```

There are 15 variables and 802 rows in the merged trash dataset.

``` r
       sum <- sum(professor_trash$weight_tons, na.rm = TRUE)
```

The professor collected a total of 246.74 tons of trash.

``` r
cigs <- gwynnda |>
  filter(month == "June", year == "2022")
```

Gwynnda collected 2800 cigarette butts in June 2022.

## Problem 3

``` r
bakers <- read.csv("./gbb_datasets/bakers.csv", 
                   na = c("NA", ".", "")) |>
                   janitor::clean_names() |>
                   separate(baker_name, into = c("baker", "last_name"), sep = ' ')
                   
bakes <- read.csv("./gbb_datasets/bakes.csv",
                  na = c("NA", ".", "")) |>
                  janitor::clean_names()
  
results <- read.csv("./gbb_datasets/results.csv",
                    na = c("NA", ".", ""),
                    skip = 2) |>
                    janitor::clean_names() |>
                    drop_na(, result)

bakers <- bakers %>%
    mutate( baker = ifelse(baker == "Jo","Joanne", baker))
    bakes$baker <- gsub("\"|'", "", bakes$baker)
bakes <- bakes %>%
      mutate( baker = ifelse(baker == "Jo","Joanne", baker))

                          
anti_join(bakers, bakes, by = "baker")
```

    ##       baker         last_name series baker_age
    ## 1     Alice          Fevronia     10        28
    ## 2    Amelia           LeBruin     10        24
    ## 3    Antony         Amourdoux      9        30
    ## 4    Briony          Williams      9        33
    ## 5       Dan   Beasley-Harling      9        36
    ## 6       Dan          Chambers     10        32
    ## 7    Helena            Garcia     10        40
    ## 8     Henry              Bird     10        20
    ## 9    Imelda          McCarron      9        33
    ## 10    Jamie              Finn     10        20
    ## 11      Jon           Jenkins      9        47
    ## 12    Karen            Wright      9        60
    ## 13  Kim-Joy           Hewlett      9        27
    ## 14     Luke          Thompson      9        30
    ## 15    Manon           Lagrave      9        26
    ## 16 Michelle       Evans-Fecci     10        35
    ## 17     Phil            Thorne     10        56
    ## 18    Priya            O'Shea     10        34
    ## 19    Rahul            Mandal      9        30
    ## 20    Rosie Brandreth-Poynter     10        28
    ## 21    Steph         Blackwell     10        28
    ## 22    Terry           Hartill      9        56
    ##                     baker_occupation      hometown
    ## 1                  Geography teacher         Essex
    ## 2                   Fashion designer       Halifax
    ## 3                             Banker        London
    ## 4                   Full-time parent       Bristol
    ## 5                   Full-time parent        London
    ## 6                     Support worker     Rotherham
    ## 7             Online project manager         Leeds
    ## 8                            Student        Durham
    ## 9     Countryside recreation officer County Tyrone
    ## 10                  Part-time waiter        Surrey
    ## 11                     Blood courier       Newport
    ## 12       In-store sampling assistant     Wakefield
    ## 13          Mental health specialist         Leeds
    ## 14 Civil servant/house and techno DJ     Sheffield
    ## 15          Software project manager        London
    ## 16          Print shop administrator  Tenby, Wales
    ## 17                        HGV driver       Rainham
    ## 18              Marketing consultant     Leicester
    ## 19                Research scientist     Rotherham
    ## 20                Veterinary surgeon      Somerset
    ## 21                    Shop assistant       Chester
    ## 22               Retired air steward West Midlands

``` r
anti_join(bakers, results, by = "baker")
```

    ## [1] baker            last_name        series           baker_age       
    ## [5] baker_occupation hometown        
    ## <0 rows> (or 0-length row.names)

``` r
anti_join(bakes, results, by = c("series", "episode"))
```

    ## [1] series         episode        baker          signature_bake show_stopper  
    ## <0 rows> (or 0-length row.names)

``` r
gbb = 
  left_join(bakers, results, by = c("baker","series")) |> 
  left_join(bakes, by = c("baker", "series", "episode")) |> 
  relocate(series,episode,baker,last_name, baker_age, baker_occupation, hometown, signature_bake,show_stopper,technical,result) |> 
  arrange(series,baker, episode)

write_csv(gbb, './gbb_datasets/gbb.csv')
```

To clean this data, I first separated the full names in the bakers
dataset into “baker” and “last_name” to match the other data sets. I
used janitor for all three data sets, and for results, removed non-data
lines and any rows of datas of contestants that had already been
eliminated. I then had to fix Joanne Wheatley’s name by converting Jo
and “Jo” to Joanne. I had to drop the double-quotes and then mutate the
baker variable for both data sets, baker and bakes. I then used
anti_join on all three combinations, specifically to ensure that
Joanne’s name was correctly changed. Bakers and bakes had non-matching
rows, but I chose to keep them, because they were still contestants on
the show. Finally, I used left_join for all three datasets, rearranged
the columns and arranged the data to sort by series, then by baker name
alphabetically, and then episode. I exported the csv to my directory.
The final data set has 710 rows and `r ncol(gbb)` columns.

``` r
winners_data <- gbb %>%
  select(series, episode, baker, last_name, result) %>%
  filter(series %in% c(5, 6, 7, 8, 9, 10) ) %>%
  filter(result == "STAR BAKER" | result == "WINNER") %>%
  arrange(series,episode)
```

Predictable winners were those who had been the Star Baker either
directly before the last episode, or several times before. Surprises
were winners who weren’t Star Bakers in the most recent episodes, which
include Seasons 5, 9 and 10.

``` r
viewers <- read.csv("./gbb_datasets/viewers.csv", 
                   na = c("NA", ".", "")) |>
                   janitor::clean_names()
print(viewers)
```

    ##    episode series_1 series_2 series_3 series_4 series_5 series_6 series_7
    ## 1        1     2.24     3.10     3.85     6.60    8.510    11.62    13.58
    ## 2        2     3.00     3.53     4.60     6.65    8.790    11.59    13.45
    ## 3        3     3.00     3.82     4.53     7.17    9.280    12.01    13.01
    ## 4        4     2.60     3.60     4.71     6.82   10.250    12.36    13.29
    ## 5        5     3.03     3.83     4.61     6.95    9.950    12.39    13.12
    ## 6        6     2.75     4.25     4.82     7.32   10.130    12.00    13.13
    ## 7        7       NA     4.42     5.10     7.76   10.280    12.35    13.45
    ## 8        8       NA     5.06     5.35     7.41    9.023    11.09    13.26
    ## 9        9       NA       NA     5.70     7.41   10.670    12.65    13.44
    ## 10      10       NA       NA     6.74     9.45   13.510    15.05    15.90
    ##    series_8 series_9 series_10
    ## 1      9.46     9.55      9.62
    ## 2      9.23     9.31      9.38
    ## 3      8.68     8.91      8.94
    ## 4      8.55     8.88      8.96
    ## 5      8.61     8.67      9.26
    ## 6      8.61     8.91      8.70
    ## 7      9.01     9.22      8.98
    ## 8      8.95     9.69      9.19
    ## 9      9.03     9.50      9.34
    ## 10    10.04    10.34     10.05

``` r
season_5 <- mean(viewers$series_5, na.rm = TRUE)
season_1 <- mean(viewers$series_1, na.rm = TRUE)
```

There was an average of 2.77 million viewers in Season 1 and an average
of 10.0393 million viewers in Season 5.
