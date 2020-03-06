# Create dataset to display as bookdown
# To run manually to generate the clean dataset to display

# Dependencies ----
library(dplyr)
library(tidyr)
library(readxl)

# Expected files ----
#Generate filan output
output_file <- file.path("inst", "erum2020_confirmed_program.rds")
if (!file.exists(output_file)) {
  file.create(output_file)
}

#sessionize dump
sessionize_full_dump <- file.path("inst", "data_dump", "erum2020 sessions - exported 2020-03-05.xlsx")
gform_confirmation <- file.path("inst", "data_dump", "eRum 2020 - Contribution Acceptance Form (Responses).xlsx")

# Read files
all_sessions <- read_excel(sessionize_full_dump, sheet = "All Submitted Sessions")
all_speakers <- read_excel(sessionize_full_dump, sheet = "All Speakers")
confirmations <- read_excel(gform_confirmation)

# Utilities ----
remove_spaces_df <- function(df) {
  gsub(" ","",  names(df))
}

# Clean up data ----
# Remove unecessary columns
all_sessions_reduced <- all_sessions %>%
  setNames(remove_spaces_df(all_sessions)) %>%
  select(Id, Title, Description, Speakers, Sessionformat, Track) %>%
  mutate(NameSurname = gsub(" ", ",", .$Speakers))

all_speakers_reduced <- all_speakers %>%
  setNames(remove_spaces_df(all_speakers)) %>%
  select(Id, FirstName, LastName, TagLine) %>%
  unite(NameSurname, FirstName, LastName, sep = ",")

confirmations_reduced <- confirmations %>%
  unite(NameSurname, Name, Surname, sep = ",") %>%
  rename(confirm = starts_with("Do you confirm")) %>%
  select(NameSurname, confirm)

# Join data
all_speakers_confirmed <- all_speakers_reduced %>%
  left_join(confirmations_reduced) %>%
  mutate(NameSurname = gsub(" ", ",", .$NameSurname))

session_speakers_confirmed <- full_join(all_sessions_reduced, all_speakers_confirmed, by = "NameSurname") %>%
  filter(tolower(confirm) == "yes") %>%
  select(Title, Speakers, TagLine, Track, Sessionformat, Description) %>%
  setNames(c("title", "author", "affiliation", "track", "session_type", "description"))

# Save output ----
saveRDS( session_speakers_confirmed, output_file)
