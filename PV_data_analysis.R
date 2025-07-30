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
    iconv(to = "ASCII//TRANSLIT") |>
    # Remove punctuation and symbols
    str_replace_all("[^[:alnum:] ]", "") |>
    # Optional: convert to lowercase
    str_to_lower() |>
    # Optional: trim whitespace
    str_trim()
}

# Loading Datasets and Cleaning Column Names

pre_data <- read_csv(here("data", "raw", "PV_pre_raw-data.csv")) |>
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
  mutate(cleaned_id = clean_names(matching_id),
         time_point = "pre") |>
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
    colleague_wellness_support_post = 29,
    community_post = 30,
    body_aware_post = 31,
    feelings_aware_post = 32,
    mind_aware_post = 33,
    perception_aware_post = 34,
    happy_post = 35,
    life_interest_post = 36,
    life_satisfication_post = 37,
    society_contribution_post = 38,
    belonging_post = 39,
    society_good_post = 40,
    people_good_post = 41,
    society_makes_sense_post = 42,
    personality_satisfication_post = 43,
    life_responsibility_management_post = 44,
    warm_trusting_relationship_post = 45,
    growth_opportunities_post = 46,
    confidence_ideas_post = 47,
    life_direction_post = 48,
    work_challenges_post = 49,
    anything_else_post = 50) |>
  filter(attendance_criteria == "Yes") |>
  
  #this participant did not sign the consent form with their name 
  # and had to be removed. They entered "yes". 
  slice(-57) |>
  mutate(cleaned_id = clean_names(matching_id)) |>
  mutate(name = clean_names(name)) |>
  select(-c(attendance_criteria, matching_id)) |>
  relocate(cleaned_id,name, .after = presurvey_check) |>
  mutate(time_point = "post") |>
  relocate(time_point, .after = presurvey_check)
 

# Matching data with participant
matching_log <- read_csv(here("data", "raw", "master_log.csv")) |>
  rename(
    id = 1,
    name = 2,
    matching_id = 3) |>
  mutate(cleaned_id = clean_names(matching_id),
         name = clean_names(name),
         time_point = "pre") |>
  distinct(cleaned_id, .keep_all = TRUE) |>
  select(-matching_id)

# step 1 includes folks who have filled post data
exact_matches <- matching_log |>
  left_join(post_data |>
              select(-name), by = "cleaned_id") |>
  unite("time_point", time_point.x, time_point.y, sep = ", ", na.rm = TRUE)  

# step 2:  Add participants with names either in name or id

# identify unmatched records
unmatched <- exact_matches |>
  filter(!str_detect(time_point, fixed("post")))

# Find name containment matches for unmatched records
matched <- unmatched |>
  cross_join(post_data |>
               select(-name) |>
               rename(post_id = cleaned_id,
                      post_time = time_point) |>
               filter(presurvey_check == "Yes")) |>
  relocate(post_id, .after = time_point) |>
  filter(
    str_detect(post_id, fixed(name, ignore_case = TRUE)) |
    str_detect(name, fixed(post_id, ignore_case = TRUE)) |
    str_detect(post_id, fixed(cleaned_id, ignore_case = TRUE)) |
    str_detect(name, fixed(post_id, ignore_case = TRUE))
  ) |>
  # Manually Remove specific matches that 
  # are duplicates or no available matches.
  slice(-c(5, 9, 10, 12, 13)) |>
  unite("time_point", time_point, post_time, sep = ", ", na.rm = TRUE)

weird_matches <- matched |>
  # 1. For every .x column, replace with .y if available
  mutate(across(
    ends_with(".x"),
    ~ coalesce(
      get(str_replace(cur_column(), "\\.x$", ".y")),  # Get matching .y column
      .x                                             # Fall back to .x
    )
  )) |>
  # 2. Drop all .y columns (we've merged their values into .x)
  select(-ends_with(".y")) |>
  # 3. Remove .x suffixes from the remaining columns
  rename_with(~ str_remove(., "\\.x$"), ends_with(".x"))
  
# step 3: find new post participants and add them to the list
new_participants <- post_data |>
  filter(presurvey_check == "No") |>
  mutate(# Extract highest existing ID number
        last_id_num = ifelse(
          nrow(matching_log) > 0,
          max(
            as.numeric(str_extract(matching_log$id, "\\d+")),
            na.rm = TRUE
          ),
          0  # Default if no IDs exist
        ),
    id = paste0("Participant ", last_id_num + row_number())) |>
  select(-c(last_id_num, presurvey_check)) |>
  relocate(id, .before= time_point)
  
# NEW UPDATED MATCHING LOG
pre_post_matching_log <- all_data |>
  select(id:presurvey_check)

write_csv(pre_post_matching_log, here("data", "processed", 
                                      "pre_post_matching_log.csv"))

# MERGING Pre & Post Data

# Find intersecting column names automatically
# Create new matching log
cleaned_post_data <- bind_rows(exact_matches, weird_matches, new_participants) |>
  relocate(post_id, .after = id) |>
  select(-c(post_id, name, cleaned_id)) |>
  relocate(age:nature, .after = time_point) |>
  distinct(id, .keep_all = TRUE)

cleaned_pre_data <- matching_log |>
  left_join(pre_data |> select(-name), by = "cleaned_id") |>
  mutate(
    time_point = ifelse(
      !is.na(time_point.x) & !is.na(time_point.y),
      paste(time_point.x),
      coalesce(time_point.x, time_point.y)
    )
  ) |>
  select(-c(time_point.x, time_point.y, name, cleaned_id, matching_id)) |>
  relocate(time_point, .after = id) |>
  distinct(id, .keep_all = TRUE)

common_cols <- intersect(names(cleaned_pre_data), names(cleaned_post_data))

# Use in join
pre_post_data <- full_join(cleaned_pre_data, cleaned_post_data, by = common_cols)

pre_post_data <- pre_post_data |>
  group_by(id) |>
  summarize(
    time_point = paste(
      unique(unlist(strsplit(time_point, ", "))) %>% 
        factor(levels = c("pre", "post")) %>% 
        sort() %>% 
        as.character(),
      collapse = ", "
    ),
    across(everything(), ~ coalesce(.[!is.na(.)][1])),  # Take first non-NA value
    .groups = "drop"
  )

  # # Handle duplicates (if needed)
  # mutate(across(any_of(common_cols), ~ coalesce(.x, .y))) |>
  # select(-ends_with(c(".x", ".y")))

# # Combine results
# pre_post_log <- matching_log |>
#   # Remove original unmatched records that we're replacing
#   filter(!(id %in% unmatched$id)) |>
#   # Add the name-matched records
#   bind_rows(name_matches) |>
#   # Add completely new participants from post_data
#   bind_rows(
#     post_data |>
#       anti_join(matching_log, by = "cleaned_id") |> 
#       anti_join(name_matches, by = c("cleaned_name" = "cleaned_name.y")) |> 
#       mutate(
#         id = paste0("Participant ", last_id_num + row_number()))
#   ) |>
#   mutate(time_point = time_point.x + ", " + time_point.y)
#   # Standardize column order
#   select(id, cleaned_id, cleaned_name.x, time_point, cleaned_name.y)


  # inner_join(
  #   post_data |>
  #     mutate(contained = TRUE),  # Flag for joining
  #   by = character()  # Cartesian product
  # ) |>
  # filter(
  #   str_detect(name.y, fixed(name.x, ignore_case = TRUE)) |
  #     str_detect(name.x, fixed(name.y, ignore_case = TRUE))
  # ) |>
  # select(-contained) |>
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
# master_matches <- master_log |>
#   mutate(clean_name = clean_names(name)) |>
#   right_join(bind_rows(
#     pre_data |> mutate(clean_name = clean_names(participant_name), source = "pre"),
#     post_data |> mutate(clean_name = clean_names(participant_name), source = "post")
#   ), by = "clean_name")

# # Combine automatic and manual matches
# final_data <- matched_data |>
#   left_join(master_matches |> select(-source), by = c("clean_name.x" = "clean_name")) |>
#   mutate(
#     participant_id = coalesce(participant_number, 
#                              paste0("NEW_", row_number()))  # create IDs for new participants
#   )

# # Save results
# write_csv(final_data, "merged_participant_data.csv")