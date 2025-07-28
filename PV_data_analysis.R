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

# Load your datasets

pre_clean <- read_csv(here("data", "raw", "PV_pre_raw-data.csv")) |>
  select(2:last_col()) |>
  rename(
    name = 1,
    partipant_id = 2,
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
    affiliation = 14)
    
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