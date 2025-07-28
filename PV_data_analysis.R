# Name: Plum Village Project 
# Last Modified: July 27 2025


# LOAD LIBRARIES -----------------------------------------------------------------------------------------------------------------

# List of required packages
required_packages <- c("fuzzyjoin", "ggplot2", "ggtext", "knitr", "tidyverse", 
             "tidyr", "dplyr", "here")

if (!require(pacman)) install.packages("pacman")
pacman::p_load(fuzzyjoin, ggplot2, ggtext, knitr, tidyverse, here)

# load_or_install <- function(pkg) {
#   if (!require(pkg, character.only = TRUE)) {
#     install.packages(pkg)
#     library(pkg, character.only = TRUE)
#   }
# }
# 
# # Install missing packages and load all packages
# invisible(lapply(required_packages, load_or_install))

# LOAD & CLEAN the data -----------------------------------------------------------------------------------------------------------------


# Function to clean names to match with their IDs
clean_names <- function(x) {
  x |>
    tolower() |>
    stringr::str_trim() |>
    str_replace_all("\\s+", " ")
}

# Load your datasets

pre_clean <- read_csv(here("data", "raw", "PV_pre_raw-data.csv")) |>
  select(2:last_col()) |>
  rename(
    name = 1,
    matching_id = 2,
    mindful_experi = 3,
    mindful_style = 4,
    plum_village_experi = 5,
    training_experi = 6,
    training_name_experi = 7,
    retreat_binary = 8,
    retreat_type = 9,
    plum_village_practice = 10,
    plum_village_sangha = 11,
    work_sector = 12,
    area_of_exploration = 13,
    affiliation = 14,
    anxiety = 15,
    bordeom = 16,
    calm = 17,
    connection = 18,
    curiosity = 19,
    despair = 20,
    frustrations = 21,
    joy = 22,
    collaboration = 23,
    competetition = 24,
    colleague_wellness_support = 25,
    community = 26,
    wellness_work = 27,
    body_aware = 28,
    feelings_aware = 29,
    mind_aware = 30,
    perception_aware = 31,
    collaboration_qual = 32,
    competition_qual = 33,
    happy = 34,
    life_interest = 35,
    life_satisfication = 36,
    society_contribution = 37,
    belonging = 38,
    society_good = 39,
    people_good = 40,
    society_makes_sense = 41,
    personality_satisfication = 42,
    life_responsibility_management = 43,
    warm_trusting_relationship = 44,
    growth_opportunities = 45,
    confidence_ideas = 46,
    life_direction = 47,
    work_life = 48,
    age = 49,
    gender = 50,
    country = 51,
    ethnicity = 52,
    nature_binary = 53,
    nature = 54,
    work_challenges = 55,
    anything_else = 56) |>
  relocate(age:ethnicity, .after = matching_id) |>
  relocate(work_sector:affiliation, .after = ethnicity) |>
  relocate(nature_binary:nature, .after = plum_village_practice) |>
  relocate(happy:life_direction, .after = perception_aware) |>
  mutate(cleaned_id = clean_names(matching_id)) |>
  relocate(cleaned_id, .before = name)

matching_log <- read_csv(here("data", "raw", "master_log.csv")) |>
  rename(
    id = 1,
    name = 2,
    matching_id = 3) |>
  mutate(cleaned_id = clean_names(matching_id))


# Match data to add participant ID to
matched_data <- pre_clean |>
  left_join(
    matching_log |> 
      select(id, cleaned_id),
    by = c("cleaned_id") 
  ) |>
  relocate(id, .before = age)

matched_data |>
  distinct(cleaned_id, .keep_all = TRUE) |>
  count(cleaned_id) |>
  filter(n > 1)
# 
  # rename(training_name_experi = 5) |>
  
# Remove Timestamp
# Change Name

# post_data <- here("data", "raw", "PV_post_raw-data.csv")
# matching_data <- here("data", "raw", "Master_Log.csv")
# 
# pre_post_data <- c(pre_data, post_data, matching_data)
# read_csv(pre_post_data, id = "Tine_Point")


# # Clean names by removing extra spaces and standardizing case
# clean_names <- function(x) {
#   x |>  
#     tolower() |> 
#     str_trim() |>
#     str_replace_all("\\s+", " ")  # collapse multiple spaces
# }

# # Create fuzzy matched dataset
# matched_data <- stringdist_full_join(
#   pre_data |>
#    mutate(clean_name = clean_names(participant_name)),
#   post_data |> mutate(clean_name = clean_names(participant_name)),
#   by = "clean_name",
#   max_dist = 2,  # allows for small differences
#   distance_col = "name_distance"
# ) |> 
#   arrange(name_distance)

# # Identify exact matches from master log
# master_matches <- master_log %>%
#   mutate(clean_name = clean_names(name)) %>%
#   right_join(bind_rows(
#     pre_data %>% mutate(clean_name = clean_names(participant_name), source = "pre"),
#     post_data %>% mutate(clean_name = clean_names(participant_name), source = "post")
#   ), by = "clean_name")

# # Combine automatic and manual matches
# final_data <- matched_data %>%
#   left_join(master_matches %>% select(-source), by = c("clean_name.x" = "clean_name")) %>%
#   mutate(
#     participant_id = coalesce(participant_number, 
#                              paste0("NEW_", row_number()))  # create IDs for new participants
#   )

# # Save results
# write_csv(final_data, "merged_participant_data.csv")