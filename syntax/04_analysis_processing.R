library(tidyverse)

load("../data/derived/individuals.RData")
load("../data/derived/prior_disorder.RData")

analytical_data <- individuals %>%
  left_join(prior_disorder, by = c("experiment_date", "lsoa_number", "mailbox_id_number")) %>%
  mutate(action = fct_relevel(factor(action), "Walkby")) %>%
  select(
    # TRIAL LEVEL
    experiment_date,
    lsoa_number,
    condition              = experiment_type,
    experiment_walkby_rate = trial_walkby_rate,
    prior_disorder,
    # LETTER LEVEL
    unique_envelope_id     = unique_letter_id,
    envelope_id_number     =  letter_id_number,
    start_time,
    end_time               = action_time,
    trial_duration         = drop_duration,
    action,
    sex                    = gender,
    age,
    race,
    in_group,
    group_size,
    walkby_sum,
    trial_walkby_rate      = letter_walkby_rate)

# cleanup <- analytical_data %>%
#   select(experiment_date, lsoa_number, condition, unique_envelope_id, start_time, prior_disorder) %>%
#   group_by(experiment_date, lsoa_number, condition) %>%
#   arrange(start_time) %>%
#   slice(1L) %>%
#   ungroup() %>%
#   group_by(experiment_date, lsoa_number) %>%
#   mutate(exp_order = row_number()) %>%
#   mutate(control_was_cleanup = fct_relevel(
#     ifelse(any(exp_order == 1 & condition == "Control" & prior_disorder == "Prior Disorder"), 
#            "Control was cleanup", 
#            "Control was not cleanup"),
#     "Control was not cleanup")) %>%
#   ungroup() %>%
#   distinct(experiment_date, lsoa_number, condition, control_was_cleanup)
# analytical_data <- analytical_data %>% left_join(cleanup)

save(analytical_data, file = "../data/derived/analytical_data.RData")
