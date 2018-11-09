p8105\_hw5\_jih2119
================

### Setup

``` r
library(tidyverse)
library(rvest)
```

Problem 1
---------

``` r
id = list.files("./data") 

file_base = "./data/"
file_names = str_c(file_base, id)

read_data = function(file_names) {
  read_csv(file_names)
}

observations = map(file_names, read_data)

results_df = tibble(
  id,
  observations
) %>% 
  mutate(id = str_replace(id, "con", "Control"), 
  id = str_replace(id, "exp", "Experimental"), 
  id = str_replace(id, ".csv", "")) %>% 
  mutate(arm = str_replace(id, "[1-9]$", ""), 
         arm = str_replace(arm, "[0-1]$", ""),
         arm = str_replace(arm, "1$", ""),
         arm = str_replace(arm, "_", "")) %>% 
  unnest() %>% 
  gather(key = week, value = observations, week_1:week_8) %>% 
  mutate(week = str_replace(week, "week_", ""))
```

``` r
ggplot(
  results_df, aes(x = week, y = observations, group = id, color = arm)) +
  geom_line() +
  labs(
    title = "Week vs Observations Scatterplot",
    x = "Week",
    y = "Observations"
  ) +
  theme_bw()
```

![](hw5_files/figure-markdown_github/long_plot-1.png)

From the data and subsequent plot it looks like the experimental arm overall had an increase in observation values over time compared to the control group.

Problem 2
---------

The dataset contains variables describing the victim's first and last name, race, age, and sex. It also contains when the crime was reported, which city and state, and disposition of the case.

``` r
homicide = read_csv("homicide-data.csv") %>% 
  unite(city_state, city:state, sep = ", ")
```

    ## Parsed with column specification:
    ## cols(
    ##   uid = col_character(),
    ##   reported_date = col_integer(),
    ##   victim_last = col_character(),
    ##   victim_first = col_character(),
    ##   victim_race = col_character(),
    ##   victim_age = col_character(),
    ##   victim_sex = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   disposition = col_character()
    ## )

``` r
sum_hom = homicide %>% 
  group_by(city_state, disposition) %>%
  summarize(n_hom = n()) %>% 
  spread(key = disposition, value = n_hom)
sum_hom[is.na(sum_hom)] = 0
sum_hom = janitor::clean_names(sum_hom) %>% 
  mutate(unsolved = closed_without_arrest + open_no_arrest) %>% 
  mutate(total = 
           closed_by_arrest + closed_without_arrest + open_no_arrest)
```

``` r
#prop = prop.test(unsolved_hom, total_hom)
```
