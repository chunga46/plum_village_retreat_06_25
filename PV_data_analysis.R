# Name: Plum Village Project 
# Last Modified: July 27 2025


# LOAD LIBRARIES -----------------------------------------------------------------------------------------------------------------

# List of required packages
required_packages <- c("fuzzyjoin", "ggplot2", "ggtext", "knitr", "tidyverse", 
             "tidyr", "dplyr", "here", "stringr")

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
    # Remove accents
    iconv(to = "ASCII//TRANSLIT") %>%
    # Remove punctuation and symbols
    str_replace_all("[^[:alnum:] ]", "") %>%
    # Optional: convert to lowercase
    str_to_lower() %>%
    # Optional: trim whitespace
    str_trim()
}

# Loading Datasets and Cleaning Column Names

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
    wellness_enhance_work = 27,
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

# # Match data to add participant ID to
# matched_data <- pre_clean |>
#   left_join(
#     matching_log |> 
#       select(id, cleaned_id),
#     by = c("cleaned_id") 
#   ) |>
#   relocate(id, .before = age) |> 
#   distinct(cleaned_id, .keep_all = TRUE) |>
#   mutate(time_point = "pre") |>
#   relocate(time_point, .after = id)

post_data <- read_csv(here("data", "raw", "PV_post_raw-data.csv")) |>
  select(2:last_col()) |>
  rename(
    presurvey_check = 1,
    attendance_criteria = 2,
    matching_id = 3,
    name = 4,
    mindful_experi = 5,
    mindful_style = 6,
    plum_village_experi = 7,
    training_experi = 8,
    training_name_experi = 9,
    retreat_binary = 10,
    retreat_type = 11,
    plum_village_practice = 12,
    plum_village_sangha = 13,
    work_sector = 14,
    area_of_exploration = 15,
    affiliation = 16,
    age = 17,
    gender = 18,
    country = 19,
    ethnicity = 20,
    nature_binary = 21,
    nature = 22,
    takeaway = 23,
    post_collaboration = 24,
    compassion_insight = 25,
    global_challenge_insight = 26,
    collaborative_atmosphere_scientist = 27,
    collaborative_atmosphere_attendees = 28,
    colleague_wellness_support = 29,
    community = 30,
    body_aware = 31,
    feelings_aware = 32,
    mind_aware = 33,
    perception_aware = 34,
    happy = 35,
    life_interest = 36,
    life_satisfication = 37,
    society_contribution = 38,
    belonging = 39,
    society_good = 40,
    people_good = 41,
    society_makes_sense = 42,
    personality_satisfication = 43,
    life_responsibility_management = 44,
    warm_trusting_relationship = 45,
    growth_opportunities = 46,
    confidence_ideas = 47,
    life_direction = 48,
    work_challenges = 49,
    anything_else = 50) |>
  filter(attendance_criteria == "Yes") |>
  
  #this participant did not sign the consent form with their name 
  # and had to be removed. They entered "yes". 
  slice(-57) |>
  mutate(cleaned_id = clean_names(matching_id)) |>
  mutate(cleaned_name = clean_names(name)) |>
  select(-c(attendance_criteria, name, matching_id)) |>
  relocate(cleaned_id:cleaned_name, .after = presurvey_check) |>
  mutate(time_point = "post") |>
  relocate(time_point, .after = presurvey_check)
 

# Matching data with participant 
matching_log <- read_csv(here("data", "raw", "master_log.csv")) |>
  rename(
    id = 1,
    name = 2,
    matching_id = 3) |>
  mutate(cleaned_id = clean_names(matching_id),
         cleaned_name = clean_names(name),
         time_point = "pre") |>
  distinct(cleaned_id, .keep_all = TRUE)|>
  select(-c(name, matching_id))

# step 1: Add participants that completed both post and follow up
exact_matches <- matching_log |>
  left_join(post_data |> 
              select(cleaned_id, cleaned_name, time_point), 
            by = "cleaned_id")

# identify unmatched records
unmatched <- exact_matches |>
  filter(is.na(cleaned_name.y) & is.na(time_point.y))

# Find name containment matches for unmatched records
name_matches <- unmatched |>
  select(id, cleaned_id, cleaned_name.x, time_point.x) |>
  cross_join(post_data |> 
               rename(post_id = cleaned_id,
                      post_time = time_point) |>
               filter(presurvey_check == "Yes") |>
               select(post_id, post_time)) |>
  filter(
    str_detect(post_id, fixed(cleaned_name.x, ignore_case = TRUE)) |
    str_detect(cleaned_name.x, fixed(post_id, ignore_case = TRUE)) |
    str_detect(post_id, fixed(cleaned_id, ignore_case = TRUE)) |
    str_detect(cleaned_name.x, fixed(post_id, ignore_case = TRUE)) 
  )|> 
  slice(-c(6, 9, 10, 12, 13))

# # Combine results
# final_log <- updated_log %>%
#   # Remove original unmatched records that we're replacing
#   filter(!(id %in% unmatched$id)) %>%
#   # Add the name-matched records
#   bind_rows(name_matches) %>%
#   # Add completely new participants from post_data
#   bind_rows(
#     post_data %>%
#       anti_join(matching_log, by = "cleaned_id") %>%
#       anti_join(name_matches, by = c("cleaned_name" = "cleaned_name.y")) %>%
#       mutate(
#         id = paste0("Participant_", 
#                     max(as.numeric(str_extract(matching_log$id, "\\d+")), na.rm = TRUE) + row_number()),
#         cleaned_name.x = NA_character_,
#         time_point.x = NA_character_
#       )
#   ) %>%
#   # Standardize column order
#   select(id, cleaned_id, cleaned_name.x, time_point.x, cleaned_name.y, time_point.y)


  # inner_join(
  #   post_data |>
  #     mutate(contained = TRUE),  # Flag for joining
  #   by = character()  # Cartesian product
  # ) %>%
  # filter(
  #   str_detect(name.y, fixed(name.x, ignore_case = TRUE)) |
  #     str_detect(name.x, fixed(name.y, ignore_case = TRUE))
  # ) %>%
  # select(-contained) %>%
  # distinct()  # Avoid duplicates
  # # Categorize each record
  # mutate(
  #   status = ifelse(!is.na(cleaned_id.y) | !is.na(name.y), "confirmed", "new"),
  #   time_point = ifelse(status == "confirmed", "both", "post")
  # )


# # Step 1: Identify new participants in post_data
# updated_log <- post_data |>
#   # anti_join(new_matching_log, by = "cleaned_name") |>
#   anti_join(updated_log, by = "cleaned_id") |>
#   select(cleaned_id) |>
#   distinct() |>
#   mutate(
#     # Extract highest existing ID number
#     last_id_num = ifelse(
#       nrow(matching_log) > 0,
#       max(
#         as.numeric(str_extract(matching_log$id, "\\d+")),
#         na.rm = TRUE
#       ),
#       0  # Default if no IDs exist
#     ),
#     # Create new IDs
#     id = paste0("Participant ", last_id_num + row_number())
#   )

# # Step 2: Update matching log
# updated_matching_log <- matching_log |>
#   # Update existing participants (bring in new names if changed)
#   rows_update(
#     post_data |>
#       select(cleaned_id),
#     by = "cleaned_id",
#     unmatched = "ignore"
#   ) |>
#   # Add new participants
#   bind_rows(new_participants) |>
#   # Optional: Add time_point from post_data
#   left_join(
#     post_data |>
#       select(cleaned_id, time_point),
#       by = "cleaned_id") |>
#   # For existing participants with multiple time_points, collapse them
#   group_by(id, cleaned_id) |>
#   summarize(
#     time_points = paste(unique(time_point), collapse = ", "),
#     .groups = "drop"
#   )

# # Step 3: Verify
# list(
#   existing_updated = nrow(new_matching_log),
#   new_added = nrow(new_participants),
#   final_count = nrow(updated_matching_log))
#   

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