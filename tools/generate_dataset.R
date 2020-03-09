# Create dataset to display as bookdown
# To run manually to generate the clean dataset to display

# Dependencies ----
library(dplyr)
library(tidyr)
library(readxl)
library(purrr)

# Expected files ----
#Generate filan output

output_file <- file.path("data", "erum2020_confirmed_program.json")
dump_dir <- file.path("tools", "data_dump")

#sessionize dump
sessionize_full_dump <- file.path(dump_dir, "erum2020 sessions - exported 2020-03-05.xlsx")
gform_confirmation <- file.path(dump_dir, "eRum 2020 - Contribution Acceptance Form (Responses).xlsx")
accepted_full <- file.path(dump_dir, "finaltable_homework_contributedsessions.xlsx")

# Read files
all_sessions <- read_excel(sessionize_full_dump, sheet = "All Submitted Sessions")
all_speakers <- read_excel(sessionize_full_dump, sheet = "All Speakers")
confirmations <- read_excel(gform_confirmation)
accepted <- read_excel(accepted_full)

# Utilities ----
remove_spaces_df <- function(df) {
  gsub(" ","",  names(df))
}

clean_up_NameSurname <-  function(df){
  if ("NameSurname" %in% names(df)) {
    df <- df  %>%
      mutate(NameSurname = toupper(NameSurname)) %>%
      mutate(NameSurname = gsub(",N/A", "", .$NameSurname)) %>%
      mutate(NameSurname = gsub(" ", ",", .$NameSurname))
  }
  df
}

remove_double_spaces <- function(x){
  gsub("  "," ",  x)
}

clean_df <- function(df){
  df %>%
    setNames(remove_spaces_df(df)) %>%
    purrr::map_df(trimws) %>%
    purrr::map_df(remove_double_spaces)
}


# Clean up data ----
# Remove unecessary columns
all_sessions_reduced <- all_sessions %>%
  mutate(Id = as.character(Id)) %>%
  select(-`Date Submitted`) %>%
  #Manual fix add RICCARDO,CORRADIN and Thomas Maier contribution that does not seem to be part of the all_session but is part of the accepted. Notice that this does not fix the all_speakers tab, so we are still missing his affiliation
  bind_rows(accepted %>% filter(Speakers == "Riccardo Corradin")) %>%
  bind_rows(accepted %>% filter(Speakers == "Thomas Maier", )) %>%
  clean_df() %>%
  select(Id, Title, Description, Speakers, Sessionformat, Track) %>%
  mutate(Speakers = gsub(" N/A", "", .$Speakers)) %>%
  #Manual fix case of Shodipo Ayomide and Mieke Deschepper that switched Name and Surname
  #Manual fix for Dana Jomar/Jomer that mispelled her name
  mutate(Speakers = case_when(
    Speakers == "Shodipo Ayomide" ~ "Ayomide Shodipo",
    Speakers == "Mieke Deschepper" ~ "Deschepper Mieke",
    Speakers == "Dana Jomar" ~ "Dana Jomer",
    TRUE ~ Speakers
  )) %>%
  mutate(NameSurname = gsub(" ", ",", .$Speakers)) %>%
  clean_up_NameSurname()

all_speakers_reduced <- all_speakers %>%
  clean_df() %>%
  select(Id, FirstName, LastName, TagLine) %>%
  unite(NameSurname, FirstName, LastName, sep = ",") %>%
  clean_up_NameSurname() %>%
  #Manual fix case of Shodipo Ayomide and Mieke Deschepper that switched Name and Surname
  #Manual fix for Dana Jomar/Jomer that mispelled her name
  mutate(NameSurname = case_when(
    NameSurname == toupper("Shodipo,Ayomide") ~ toupper("Ayomide,Shodipo"),
    NameSurname == toupper("Mieke,Deschepper") ~ toupper("Deschepper,Mieke"),
    NameSurname == toupper("Dana,Jomar") ~ toupper("Dana,Jomer"),
    TRUE ~ NameSurname
  ))

confirmations_reduced <- confirmations %>%
  rename(confirm = starts_with("Do you confirm")) %>%
  clean_df() %>%
  unite(NameSurname, Name, Surname, sep = ",") %>%
  select(NameSurname, confirm) %>%
  clean_up_NameSurname() %>%
  #Manual fix case of Shodipo Ayomide and Mieke Deschepper that switched Name and Surname
  #Manual fix for Dana Jomar/Jomer that mispelled her name
  #Manual fix for Giulio,Ferrero,Ferrero repeated surname
  #Manual fix for Olalekan Joseph Akintande	 that did not add middle name
  #Manual fix for name with special character inconsitently entered
  mutate(NameSurname = case_when(
    NameSurname == toupper("Shodipo,Ayomide") ~ toupper("Ayomide,Shodipo"),
    NameSurname == toupper("Mieke,Deschepper") ~ toupper("Deschepper,Mieke"),
    NameSurname == toupper("Dana,Jomar") ~ toupper("Dana,Jomer"),
    NameSurname == toupper("Zbynek,Slajchrt") ~ toupper("Zbyněk,Šlajchrt"),
    NameSurname == toupper("Giulio,Ferrero,Ferrero") ~ toupper("Giulio,Ferrero"),
    NameSurname == toupper("Olalekan,Akintande") ~ toupper("Olalekan,Joseph,Akintande"),
    TRUE ~ NameSurname
  ))

accepted_reduced <- accepted %>%
  purrr::map_df(trimws) %>%
  mutate(choice = `choice\n`) %>%
  #Manual fix case of Shodipo Ayomide and Mieke Deschepper that switched Name and Surname
  #Manual fix for Dana Jomar/Jomer that mispelled her name
  mutate(Speakers = case_when(
    Speakers == "Shodipo Ayomide" ~ "Ayomide Shodipo",
    Speakers == "Mieke Deschepper" ~ "Deschepper Mieke",
    Speakers == "Dana Jomar" ~ "Dana Jomer",
    TRUE ~ Speakers
  )) %>%
  mutate(NameSurname = gsub(" ", ",", .$Speakers)) %>%
  clean_up_NameSurname() %>%
  select(Id, `choice`, Title, NameSurname)

# Join data
all_speakers_confirmed <- all_speakers_reduced %>%
  full_join(confirmations_reduced, by = "NameSurname")

all_sessions_accepted <- all_sessions_reduced %>%
  full_join(accepted_reduced, by = c("NameSurname", "Title") ) %>%
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
  distinct() %>%
  filter(!is.na(title)) %>%
  arrange(session_type, author, title)

# Save output ----
jsonlite::write_json(session_speakers_confirmed, output_file, pretty = TRUE)
