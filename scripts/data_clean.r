# Data Clean

library(rio)
library(tidyverse)
library(glue)
library(tidycensus)

clean_walkability_data <- function() {
  d <- import("../data/walkability_index.csv")
  s <- import("../data/states_and_counties.csv")
  
  d <- d %>% 
    drop_na(NatWalkInd) %>% 
    mutate(tract = TRACTCE / 100) %>% 
    rename(state_fp = STATEFP) %>% 
    rename(county_fp = COUNTYFP) %>% 
    rename(block_group = BLKGRPCE) %>% 
    select(OBJECTID, NatWalkInd, state_fp, county_fp,
           tract, block_group, TotPop, CountHU, HH,
           Pct_AO0, Pct_AO1, Pct_AO2p, D3A, D3B, D4A) %>% 
    filter(state_fp == 11)
    
  s <- s %>% 
    mutate(state_fp = as.numeric(state_fp),
           county_fp = as.numeric(county_fp))
  
  d <- left_join(d, s, by = c("state_fp", "county_fp"))
  
  return(d)
}

clean_income_data <- function() {
  census_api_key("8548ab2d78a355ce0c3903c03a0c613d68dff396")
  
  x <- get_acs(state = "DC", year = 2018, geography = "block group",
               variables = "B19013_001", geometry = FALSE,
               survey = "acs5", show_call = FALSE, output = "wide")
  
  y <- x %>% 
    separate(NAME, 
             c("block_group", "tract", "county_name", "state_name"),
             sep = ", ")
  
  y$block_group <- str_replace_all(y$block_group, "[A-Za-z ]", "") %>% 
    as.numeric()
  y$tract <- str_replace_all(y$tract, "[A-Za-z ]", "") %>% 
    as.numeric()
  y$county_name <- str_replace_all(y$county_name, " County", "")
  

  return(y)
}

clean_education_data <- function() {
  census_api_key("8548ab2d78a355ce0c3903c03a0c613d68dff396")
  
  v <- load_variables(2018, "acs5")
  v <- v$name[str_detect(v$name, "B15003")]
  
  x <- get_acs(state = "DC", year = 2018, geography = "block group",
               variables = v, geometry = FALSE,
               survey = "acs5", show_call = FALSE, output = "wide")
  
  y <- x %>% 
    separate(NAME, 
             c("block_group", "tract", "county_name", "state_name"),
             sep = ", ")
  
  y$block_group <- str_replace_all(y$block_group, "[A-Za-z ]", "") %>% 
    as.numeric()
  y$tract <- str_replace_all(y$tract, "[A-Za-z ]", "") %>% 
    as.numeric()
  y$county_name <- str_replace_all(y$county_name, " County", "")
  
  log_hs <- seq(8, 44, 2)
  log_col <- c(46, 48)
  log_grad <- c(50, 52, 54)
  
  y <- y %>% 
    mutate(pct_hs = rowSums(across(log_hs)) / B15003_001E,
           pct_col = rowSums(across(log_col)) / B15003_001E,
           pct_grad = rowSums(across(log_grad)) / B15003_001E) %>% 
    select(block_group, tract, county_name, state_name,
           pct_hs, pct_col, pct_grad)

  return(y)
}

clean_race_data <- function() {
  census_api_key("8548ab2d78a355ce0c3903c03a0c613d68dff396")
  
  x <- get_decennial(state = "DC", year = 2010, geography = "block group",
               variables = c("P003001", "P003002", "P003003"),
               geometry = FALSE, show_call = FALSE, output = "wide")
  
  y <- x %>% 
    separate(NAME, 
             c("block_group", "tract", "county_name", "state_name"),
             sep = ", ") %>% 
    mutate(pct_white = P003002 / P003001) %>% 
    mutate(pct_black = P003003 / P003001)
  
  y$block_group <- str_replace_all(y$block_group, "[A-Za-z ]", "") %>% 
    as.numeric()
  y$tract <- str_replace_all(y$tract, "[A-Za-z ]", "") %>% 
    as.numeric()
  y$county_name <- str_replace_all(y$county_name, " County", "")
  
  return(y)
}

join_clean_data <- function(w, y, r, e) {
  d <- left_join(w, y, by = c("tract", "block_group"))
  d <- left_join(d, r, by = c("tract", "block_group"))
  d <- left_join(d, e, by = c("tract", "block_group"))
  return(d)
}

clean_data <- function() {
  w <- clean_walkability_data()
  y <- clean_income_data()
  r <- clean_race_data()
  e <- clean_education_data()
  d <- join_clean_data(w, y, r, e)
  
  d <- d %>% 
    select(NatWalkInd, state_fp, county_fp, state, tract, block_group,
           P003001, P003002, P003003,
           B19013_001E, B19013_001M, pct_white, pct_black,
           pct_hs, pct_col, pct_grad,
           Pct_AO0, Pct_AO1, Pct_AO2p, D3A, D3B, D4A)
  d <- d %>% 
    rename(income_est = B19013_001E,
           income_moe = B19013_001M,
           pct_car0 = Pct_AO0,
           pct_car1 = Pct_AO1,
           pct_car2p = Pct_AO2p,
           total_pop = P003001,
           white_pop = P003002,
           black_pop = P003003,
           walkability = NatWalkInd)
  
  export(d, "../data_clean/wyre.csv")
  
  return(d)
}
