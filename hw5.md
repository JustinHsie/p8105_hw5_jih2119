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


results = map(file_names, read_data)

results_df = tibble(
  id,
  results
) %>% 
  separate(id, into = c("arm", "id"), sep = "_") %>% 
  mutate(arm = str_replace(arm, "con", "Control")) %>% 
  mutate(arm = str_replace(arm, "exp", "Experimental")) %>% 
  mutate(id = str_replace(id, ".csv", "")) %>% 
  unnest()
```
