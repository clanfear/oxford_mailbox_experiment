# This is not presently in use

library(tidyverse)
library(janitor)


if(!gs4_has_token()){
  gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets.readonly") # This will bring up a browser to authenticate
}

cleanup <- read_sheet("https://docs.google.com/spreadsheets/d/1QmSZTUtuX0h3WCSzHWo-UQ0hooQzL5uYdl75oyHISRg/") %>%
  clean_names()
  
cleanup %>% distinct(treatment_description)
