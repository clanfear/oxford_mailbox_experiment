#---------------------------------------------#
# Mailbox Experiment Participants Data Processing
# Chuck Lanfear (clanfear@uw.edu; cclanfear@gmail.com)
# This script processes raw spreadsheet data on participants
#---------------------------------------------#

library(tidyverse)
library(janitor)
library(googlesheets4)

if(!gs4_has_token()){
  gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets.readonly") # This will bring up a browser to authenticate
}
participants_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1DmchoP0kffIM7X2z-xIE1F0R_X9JxHVC1sXui_etLs4/", 
                               sheet = 1,
                               col_types = paste0("TcDccccc", paste0(rep("i", 39), collapse = ""), "ciccccccccc"))
#---------------------------------------------#
# Oxford 2022 Data
# These are in wide format: One row per letter
# Inactive pedestrians are cell counts, active ped info in columns
#---------------------------------------------#



participants <- participants_raw %>%
  # Make all names snake_case
  clean_names() %>%
  # Drop blank rows and omitted actions
  filter(!is.na(letter_id_number) & action_type %in% c("Mailed", "Stolen")) %>%
  # Generate letter id, clean up times and group column
  mutate(unique_letter_id = paste0("ox_", str_pad(row_number(), pad = "0", side = "left", width = 3)),
         start_time = lubridate::ymd_hms(paste(as.character(experiment_date), start_time, sep = " ")),
         action_time = lubridate::ymd_hms(paste(as.character(experiment_date), action_time, sep = " ")),
         walking_in_a_group = as.integer(str_extract(walking_in_a_group, "^[0-9]")),
         lsoa_number = str_replace(lsoa_number, "EO", "E0"))

# Extract trial-level information, calculate time letter was on the ground
trials <- participants %>%
  select(unique_letter_id, experiment_type, start_time, action_time, letter_id_number, experiment_date, lsoa_number, mailbox_id_number) %>%
  mutate(drop_duration = difftime(action_time, start_time, units = "mins"))

# Get inactive participants. This involves converting the cell counts into rows.
inactives <- 
  participants %>%
    select(matches("group_of_|male_|female_"), unique_letter_id) %>%
    pivot_longer(matches("group_of_|male_|female_"), names_to = "walkby_type", values_to = "walkby_count") %>%
    filter(walkby_count > 0) %>%
    uncount(walkby_count) %>%
    mutate(in_group = ifelse(str_detect(walkby_type, "group"), "In Group", "Alone")) %>%
    mutate(group_size = case_when(
    in_group == "Alone" ~ 1,
    in_group == "In Group" ~ as.numeric(str_extract(walkby_type, "[2-5]"))
    )) %>%
    mutate(group_id = paste0(unique_letter_id,"-",row_number())) %>% # group_id just for verification purposes
    uncount(group_size, .remove=FALSE) %>%
    mutate(gender = case_when(
      str_detect(walkby_type, "^male|_male") ~ "Male",
      str_detect(walkby_type, "^female|_female") ~ "Female",
      str_detect(walkby_type, "[1-5]_mixed_") & row_number() %% 2 == 0 ~ "Male", # flipping changes 5 cases
      str_detect(walkby_type, "[1-5]_mixed_") & row_number() %% 2 == 1 ~ "Female",
      str_detect(walkby_type, "group_of_[1-5]$") ~ "Unknown",
      TRUE ~ "ERROR: Check gender"
      )) %>%
    mutate(age = case_when(
      str_detect(walkby_type, "group_of_") ~ "Unknown",
      str_detect(walkby_type, "_young") ~ "Young",
      str_detect(walkby_type, "_middle_age") ~ "Middle-Age",
      str_detect(walkby_type, "_older") ~ "Older",
      TRUE ~ "ERROR: Check age"
      )) %>% 
    mutate(race = case_when(
      str_detect(walkby_type, "non_white") ~ "Non-White",
      str_detect(walkby_type, "_white") & !str_detect(walkby_type, "non") ~ "White",
      str_detect(walkby_type, "mixed$") & row_number() %% 2 == 0 ~ "White", # flipping changes 7 cases
      str_detect(walkby_type, "mixed$") & row_number() %% 2 == 1 ~ "Non-White", 
      str_detect(walkby_type, "group_of_[1-5]$") ~ "Unknown",
      TRUE ~ "ERROR: Check race"
      )) %>% 
    mutate(action = "Walkby") %>% 
    select(-walkby_type, -group_id)

# Get active participants
actives <- 
  participants %>%
    select(!matches("group_of_|male_|female_")) %>%
    mutate(in_group = case_when(
      walking %in% c("Alone", "Yes") ~ "Alone",
      walking %in% c("In a Group", "No") ~ "In Group",
      walking == "Unknown" ~ "Unknown",
      TRUE ~ "ERROR: Check in_group"
    )) %>%
    mutate(group_size = ifelse(!is.na(walking_in_a_group), walking_in_a_group, 1)) %>%
    mutate(action = case_when(
      action_type == "Mailed" ~ "Mailed",
      action_type == "Stolen" ~ "Stolen",
      TRUE ~ "Other"
    )) %>%
    select(unique_letter_id, gender, age, race, in_group, action, group_size)

# Combine trial data with active and inactive participants to get full individual-level data
individuals <- trials %>%
  left_join(bind_rows(actives, inactives), 
            by = "unique_letter_id") %>%
  group_by(unique_letter_id) %>%
  mutate(walkby_sum = sum(action == "Walkby")) %>%
  mutate(letter_walkby_rate = walkby_sum / as.numeric(drop_duration))

# Calculate supertrial level values and add to the individual data
individuals <- individuals %>%
  distinct(experiment_date, experiment_type, lsoa_number, unique_letter_id, walkby_sum, drop_duration) %>%
  group_by(experiment_date, experiment_type, lsoa_number) %>%
  summarize(trial_walkby_rate = sum(walkby_sum) / sum(as.numeric(drop_duration)), .groups = "drop") %>%
  right_join(individuals, by = c("experiment_date", "experiment_type", "lsoa_number"))

save(individuals, file = "./data/derived/individuals.RData")
