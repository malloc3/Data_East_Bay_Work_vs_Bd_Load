library(tidyverse)  # For data manipulation and visualization
library(lubridate)  # For date handling
library(ggplot2)    # For creating plots
library(patchwork)  # For combining multiple plots
library(viridis)    # For color palettes
library(readr)      # For reading CSV files

data_folder <- "/Users/canonmallory/Library/Mobile Documents/com~apple~CloudDocs/Documents/School/UCSB/Briggs Lab/Climate Grant/Parks_DATA"

# Read in the CSV file
pond_species <- read_csv(file.path(data_folder, "PondSpeciesSurvey_10252024.csv"), show_col_types = FALSE)

# Clean up column names in pond_species dataframe
pond_species <- pond_species %>%
  rename_with(
    ~ tolower(gsub("-", "_", gsub(":", "", gsub(" ", "_", .x)))),
    everything()
  )
  

#NOTE I have no idea why.  But "<Null>" is not the same as "<Null>".
# I swear I am not lying.   If you need to move the nulls copy this one ( "<Null>")
# Or better yet just copy straight from the file.   IDK how this works but id does.
pond_species[pond_species == "<Null>"] <- NA

#We gotta changes all the column titles to something that will play nice with R
# Rename "Survey Date:" column to "date" in pond_species dataframe
pond_species <- pond_species %>%
  rename(date = "survey_date") %>%
  rename(pondid = "pond_id") %>%
  rename(end_time = "survey_end_time") %>%
  rename(survey_method = "select_which_survey_observation_methods_were_used") %>%
  rename(inverts_observed = "list_invertebrates_observed") %>%
  rename(birds_observed = "list_birds_observed") %>%
  

# Convert date columns to proper date format using lubridate
pond_species <- pond_species %>%
  mutate(date = mdy(date))

unique(pond_species$number_of_ca_red_legged_frog_juveniles)


# Clean up AM/PM times in start_time column
pond_species <- pond_species %>%
  mutate(start_time = case_when(
    grepl("AM|PM", start_time, ignore.case = TRUE) ~ format(parse_date_time(start_time, '%I:%M:%S %p')),
    TRUE ~ start_time
  )) %>%
  mutate(end_time = case_when(
    grepl("AM|PM", end_time, ignore.case = TRUE) ~ format(parse_date_time(end_time, '%I:%M:%S %p')),
    TRUE ~ end_time
  ))

# Convert start_time and end_time for entries that are 5 characters or less
pond_species <- pond_species %>%
  mutate(
    start_time = as_datetime(case_when(
      nchar(start_time) <= 5 ~ format(parse_date_time(start_time, '%I:%M')),
      TRUE ~ start_time
    ))) %>%
  mutate(
    end_time = as_datetime(case_when(
      nchar(end_time) <= 5 ~ format(parse_date_time(end_time, '%I:%M')),
      TRUE ~ end_time
    )))







# Calculate elapsed time between start and end times
pond_species <- pond_species %>%
  mutate(survey_elapsed_time_min = as.numeric(difftime(end_time, start_time, units = "mins")))

#Gotta flop the negative times to positive times and account for the fact that AM/PM is a thing
# If you end time is PM and start time is AM.   Then 12-(end-start) = elapsed time when we forget to include
# the am/pm in the time!   Ive lost my mind so bear with me.
# Fix negative elapsed times by adding 12 hours (720 minutes) when time crosses AM/PM boundary
pond_species <- pond_species %>%
  mutate(survey_elapsed_time_min = ifelse(survey_elapsed_time_min < 0,
                                        720 + survey_elapsed_time_min,
                                        survey_elapsed_time_min))



# Lets get an effor metric for each survey.   Effort = Nunber of people hunting * elapsed time.

# Count number of surveyors by counting commas in observers column and adding 1.  WE do this since they
# put a comma between the observers names but not at the end (classic fence post problem).
pond_species <- pond_species %>%
  mutate(num_surveyors = str_count(observers, ",") + 1)

# Calculate survey effort as number of surveyors multiplied by elapsed time
pond_species <- pond_species %>%
  mutate(survey_effort = num_surveyors * survey_elapsed_time_min)



# Get column names for pond_species dataframe
print("Pond Species Survey Columns:")
print(colnames(pond_species))

