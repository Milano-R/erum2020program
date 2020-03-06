# Create dataset to display as bookdown
# To run manually to generate the clean dataset to display

# Dependencies ----
library(dplyr)
library(tidyr)
library(readxl)
library(googledrive)

# Expected files ----
#Generate filan output

output_file <- file.path("data", "erum2020_confirmed_program.json")

#contributions dump

update_dump <- FALSE
dump_dir <- file.path("tools", "data_dump")

if (update_dump) {
  googledrive::drive_auth()
  gdrive_program <- drive_find("Program", n_max = 1)
}

program_downolad <- function(file) {
  if (update_dump) {
    no_ext <- sub("[.][^.]*$", "", file)
    gdrive_file <- drive_ls(as_id(gdrive_program$id), recursive = TRUE) %>%
      subset(., grepl(no_ext, name, fixed = TRUE))
    stopifnot(nrow(gdrive_file) == 1L)
    result <- drive_download(as_id(gdrive_file$id), path = file.path(dump_dir, file), overwrite = TRUE)
    result$local_path
  } else {
    file.path(dump_dir, file)
  }
}

sessionize_full_dump <- program_downolad("erum2020 sessions - exported 2020-03-05.xlsx")
gform_confirmation <- program_downolad("eRum 2020 - Contribution Acceptance Form (Responses).xlsx")
accepted_full <- program_downolad("finaltable_homework_contributedsessions.xlsx")

if (update_dump) {
  googledrive::drive_deauth()
}

# Read files
all_sessions <- read_excel(sessionize_full_dump, sheet = "All Submitted Sessions")
all_speakers <- read_excel(sessionize_full_dump, sheet = "All Speakers")
confirmations <- read_excel(gform_confirmation)
accepted <- read_excel(accepted_full)

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

accepted_reduced <- accepted %>%
  mutate(choice = `choice\n`) %>%
  select(Id, `choice`, Title)

# Join data
all_speakers_confirmed <- all_speakers_reduced %>%
  left_join(confirmations_reduced) %>%
  mutate(NameSurname = gsub(" ", ",", .$NameSurname))

all_sessions_accepted <- all_sessions_reduced %>%
  left_join(accepted_reduced, by = "Title") %>%
  filter(choice > 0)

session_speakers_confirmed <- full_join(all_sessions_accepted, all_speakers_confirmed, by = "NameSurname") %>%
  filter(tolower(confirm) == "yes") %>%
  transmute(
    title = Title,
    author = Speakers,
    affiliation = TagLine,
    track = Track,
    session_type = Sessionformat,
    description = Description
    ) %>%
  filter(!is.na(title)) %>%
  arrange(session_type, author, title)

# Save output ----
jsonlite::write_json(session_speakers_confirmed, output_file, pretty = TRUE)
