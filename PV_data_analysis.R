# Name: Plum Village Project 
# Last Modified: July 27 2025


# LOAD LIBRARIES -----------------------------------------------------------------------------------------------------------------

# List of required packages
required_packages <- c("fuzzyjoin", "ggplot2", "ggtext", "knitr", "tidyverse", 
             "tidyr", "dplyr", "here", "stringr", "readr")

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


# function to clean names to match with their IDs
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

#pre data 
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

# post data
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
 
# follow up data

follow_up_data <- read_csv(here("data", "raw", "PV_follow_up_raw-data.csv")) |>
  select(2:last_col()) |>
  rename(
    presurvey_post_check = 1,
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
    follow_up_meditation_binary = 23,
    follow_up_meditation_style_Q = 24,
    follow_up_changes_Q = 25,
    anxiety_follow_up = 26,
    bordeom_follow_up = 27,
    calm_follow_up = 28,
    connection_follow_up = 29,
    curiosity_follow_up = 30,
    despair_follow_up = 31,
    frustrations_follow_up = 32,
    joy_follow_up = 33,
    collaboration_follow_up = 34,
    competetition_follow_up = 35,
    colleague_wellness_support_follow_up = 36,
    community_support_follow_up = 37,
    wellness_enhance_work_follow_up = 38,
    body_aware_follow_up = 39,
    feelings_aware_follow_up = 40,
    mind_aware_follow_up = 41,
    perception_aware_follow_up = 42,
    collaboration_qual_follow_up = 43,
    competition_qual_follow_up = 44,
    happy_follow_up = 45,
    life_interest_follow_up = 46,
    life_satisfication_follow_up = 47,
    society_contribution_follow_up = 48,
    belonging_follow_up = 49,
    society_good_follow_up = 50,
    people_good_follow_up = 51,
    society_makes_sense_follow_up = 52,
    personality_satisfication_follow_up = 53,
    life_responsibility_management_follow_up = 54,
    warm_trusting_relationship_follow_up = 55,
    growth_opportunities_follow_up = 56,
    confidence_ideas_follow_up = 57,
    life_direction_follow_up = 58,
    work_challenges_follow_up = 59,
    anything_else_follow_up = 60) |>

  mutate(cleaned_id = clean_names(matching_id)) |>
  mutate(name = clean_names(name)) |>
  select(-c(attendance_criteria, matching_id)) |>
  relocate(cleaned_id,name, .after = presurvey_post_check) |>
  mutate(time_point = "follow_up") |>
  relocate(time_point, .after = presurvey_post_check)


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

# MATCHING PRE POST FOLLOW UP Participants  -----------------------------------------------------------------------------------------------------------------

# step 1 includes folks who have filled post data
# Merge together names from matching log and the post data.
# Merge time_points if there is direct match based on cleaned_id
exact_matches <- matching_log |>
  left_join(post_data |>
              select(-name), by = "cleaned_id") |>
  unite("time_point", time_point.x, time_point.y, sep = ", ", na.rm = TRUE)

# step 2:  Add participants with ids that don't match but are contained in name or id

#filter out the participants that don't have a match based on cleaned_id
unmatched <- exact_matches |>
  filter(!str_detect(time_point, fixed("post"))) |> 
  select(id:time_point)

# Cross join compares every row combination including itself.
# Use cross join to compare 
# Find name matches for unmatched records
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
    str_detect(cleaned_id, fixed(post_id, ignore_case = TRUE)) 
  ) |>
  # Manually Remove specific matches that
  # All 5 removed matches are instances where post_id is incorrectly matched 
  # to either cleaned_id or to name
  slice(-c(5, 9, 10, 12, 13)) |>
  mutate(time_point = "pre, post") |>
  unite("cleaned_id", cleaned_id, post_id, sep = "; ", na.rm = TRUE)
  #Merge timepoints and id's together
 
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

#merge pre and pre post participants
all_data <- bind_rows(exact_matches, matched, new_participants)

#group to remove redundancies
merged <- all_data |>
  #group to remove outdated duplicates
  group_by(id) |>
  slice_tail(n=1) |>
  ungroup() |>
  mutate(id_num = parse_number(id)) |>
  arrange(id_num) |>
  select(-id_num)

# NEW UPDATED MATCHING LOG
pre_post_matching_log <- merged |>
  select(id:presurvey_check)

# write_csv(pre_post_matching_log, here("data", "processed",
#                                       "pre_post_matching_log.csv"))
# MERGING Pre & Post Data

# Find intersecting column names automatically
# Create new matching log

# first cleaning pre and post
cleaned_post_data <- merged |>
  select(-c(name, cleaned_id)) |>
  relocate(age:nature, .after = time_point) 

cleaned_pre_data <- matching_log |>
  left_join(pre_data |> select(-c(name:matching_id, time_point)), 
            by = "cleaned_id") 

# MERGED PRE POST DATA

common_cols <- intersect(names(cleaned_pre_data), names(cleaned_post_data))
pre_post_data <- full_join(cleaned_pre_data, cleaned_post_data, by = common_cols) |>
  group_by(id) |>
  summarize(
    across(age:anything_else, 
           ~ if_else(n() >= 2, first(.x), first(.x))),
    across(c(time_point, presurvey_check:anything_else_post), 
           ~ if_else(n() >= 2, nth(.x, 2), first(.x)))
  )|>
  ungroup() |>
  mutate(id_num = parse_number(id)) |>
  arrange(id_num) |>
  select(-id_num) |>
  mutate(
    age = parse_number(age)
  ) |>
  relocate(time_point, .after = id)

#separating qual and quant for initial independent analysis
qual_pre_post <- pre_post_data |>
  select(-c(anxiety:life_direction,
            collaborative_atmosphere_scientist:life_direction_post))

quant_pre_post <- pre_post_data |>
  select(-c(collaboration_qual: anything_else,
            takeaway:global_challenge_insight,
            work_challenges_post:anything_else_post))

# Writing data
write_csv(pre_post_data, here("data", "processed", "pre_post_data.csv"))
write_csv(qual_pre_post, here("data", "processed", "qual_pre_post_data.csv"))

# MATCHING POST FOLLOW UP Participants  -----------------------------------------------------------------------------------------------------------------

# Matching data with participant
matching_log_follow <- read_csv(here("data", "processed", "pre_post_matching_log.csv"))

# step 1 includes folks who have filled post data
exact_matches_follow <- matching_log_follow |>
  left_join(follow_up_data |>
              select(-name), by = "cleaned_id") |>
  unite("time_point", time_point.x, time_point.y, sep = ", ", na.rm = TRUE)
# multiple columns have the same name and are being duplicated.
# I removed the repeated columns and added follow up to the new columns

# step 2:  Add participants with ids that don't match but are contained in name or id

unmatched_follow <- exact_matches_follow |>
  filter(!str_detect(time_point, fixed("follow_up"))) |>
  select(id:time_point)

matched_follow <- unmatched_follow |> 
  cross_join(follow_up_data |>
                             select(-name) |>
                             rename(follow_id = cleaned_id,
                                    follow_time = time_point) |>
                             filter(presurvey_post_check == "Yes")) |>
  filter(
      str_detect(cleaned_id, fixed(follow_id, ignore_case = TRUE)) |
      str_detect(name,       fixed(follow_id, ignore_case = TRUE)) |
      str_detect(follow_id,  fixed(cleaned_id, ignore_case = TRUE)) |
      str_detect(follow_id,  fixed(name, ignore_case = TRUE)) 
  ) |>
  relocate(follow_id, .after = cleaned_id) |>
  #remove specific matches that are invalid. 
  slice(-c(4, 8, 9, 11, 12))
  #E.x 1 case of same name id, but for wrong participant, 
  # follow_id incorrectly matched

# step 3: find new post participants and add them to the list
new_participants_follow <- follow_up_data |>
  filter(presurvey_post_check == "No") |>
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
  select(-c(last_id_num, presurvey_post_check)) |>
  relocate(id, .before= time_point)

# NEW UPDATED MATCHING LOG
all_data_follow <- bind_rows(exact_matches_follow, matched_follow
                             , new_participants_follow) |>
  relocate(follow_id, .after = cleaned_id)

merged_follow <- all_data_follow |>
  #group to remove outdated duplicates
  group_by(id) |>
  slice_tail(n=1) |>
  ungroup() |>
  mutate(id_num = parse_number(id)) |>
  arrange(id_num) |>
  select(-id_num)

final_matching_log <- merged_follow |>
  select(id:presurvey_check)

write_csv(final_matching_log, here("data", "processed",
                                      "pre_post_follow_matching_log.csv"))
# # MERGING Pre & Post Data
# 
# # Find intersecting column names automatically
# # Create new matching log
# 
# # first cleaning pre and post
# cleaned_post_data <- bind_rows(exact_matches, weird_matches, new_participants) |>
#   relocate(post_id, .after = id) |>
#   select(-c(post_id, name, cleaned_id)) |>
#   relocate(age:nature, .after = time_point) |>
#   distinct(id, .keep_all = TRUE)
# 
# cleaned_pre_data <- matching_log |>
#   left_join(pre_data |> select(-name), by = "cleaned_id") |>
#   mutate(
#     time_point = ifelse(
#       !is.na(time_point.x) & !is.na(time_point.y),
#       paste(time_point.x),
#       coalesce(time_point.x, time_point.y)
#     )
#   ) |>
#   select(-c(time_point.x, time_point.y, name, cleaned_id, matching_id)) |>
#   relocate(time_point, .after = id) |>
#   distinct(id, .keep_all = TRUE)
# 
# # MERGED PRE POST DATA 
# 
# common_cols <- intersect(names(cleaned_pre_data), names(cleaned_post_data))
# pre_post_data <- full_join(cleaned_pre_data, cleaned_post_data, by = common_cols)
#   #person did not enter name and has not consented
# 
# # removes duplicates and merges them together while also fixing pre post
# pre_post_data <- pre_post_data |>
#   group_by(id) |>
#   summarize(
#     time_point = paste(
#       unique(unlist(strsplit(time_point, ", "))) |> 
#         factor(levels = c("pre", "post")) |> 
#         sort() |> 
#         as.character(),
#       collapse = ", "
#     ),
#     across(everything(), ~ coalesce(.[!is.na(.)][1])),  # Take first non-NA value
#     .groups = "drop"
#   ) |>
#   slice(-c(11)) 
# 
# #separating qual and quant for initial independent analysis
# qual_pre_post <- pre_post_data |>
#   select(-c(anxiety:life_direction, 
#             collaborative_atmosphere_scientist:life_direction_post))
# 
# quant_pre_post <- pre_post_data |> 
#   select(-c(collaboration_qual: anything_else, 
#             takeaway:global_challenge_insight, 
#             work_challenges_post:anything_else_post))

# Writing data
# write_csv(pre_post_data, here("data", "processed", "pre_post_data.csv"))
# write_csv(qual_pre_post, here("data", "processed", "qual_pre_post_data.csv"))
# write_csv(quant_pre_post, here("data", "processed", "quant_pre_post_data.csv"))