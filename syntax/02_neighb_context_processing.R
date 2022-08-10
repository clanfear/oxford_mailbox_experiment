library(tidyverse)
library(janitor)
library(googlesheets4)

if(!gs4_has_token()){
  gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets.readonly") # This will bring up a browser to authenticate
}

neighb_context <- read_sheet("https://docs.google.com/spreadsheets/d/160mZ3stobTbusj87wgxR86ZPw14VAklsIyi06oYaGcw/") %>%
  clean_names() 

prior_disorder <- neighb_context %>%
  mutate(prior_disorder = case_when(
    is.na(visible_signs_of_disorder) ~ "No Disorder",
    visible_signs_of_disorder == "none" ~ "No Disorder",
    TRUE ~ "Prior Disorder"
  ),
  lsoa_number = str_replace(lsoa_number, "EO", "E0")) %>%
  select(experiment_date, lsoa_number, mailbox_id_number, prior_disorder) %>%
  mutate(experiment_date = as.Date(experiment_date)) %>%
  distinct()
save(prior_disorder, file = "../data/derived/prior_disorder.RData")
