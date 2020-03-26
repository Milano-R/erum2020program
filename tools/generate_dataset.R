# Create dataset to display as bookdown
# To run manually to generate the clean dataset to display

# Dependencies ----
library(dplyr)
library(tidyr)
library(readxl)
library(purrr)
library(googledrive)

# Expected files ----
#Generate filan output

output_file <- file.path("data", "erum2020_confirmed_program.json")

#contributions dump

update_dump <- TRUE
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
  select(Id, Title, Description, Speakers, Track) %>%
  mutate(Speakers = gsub(" N/A", "", .$Speakers)) %>%
  #Manual fix case of Shodipo Ayomide and Mieke Deschepper that switched Name and Surname
  #Manual fix for Dana Jomar/Jomer that mispelled her name
  #Manual fix for Parvaneh Shafiei that mispelled her name
  mutate(Speakers = case_when(
    Speakers == "Shodipo Ayomide" ~ "Ayomide Shodipo",
    Speakers == "Mieke Deschepper" ~ "Deschepper Mieke",
    Speakers == "Dana Jomar" ~ "Dana Jomer",
    Speakers == "parvane shafiei" ~ "Parvaneh Shafiei",
    Speakers == "mustapha Larbaoui" ~ "Mustapha Larbaoui",
    TRUE ~ Speakers
  )) %>%
  mutate(NameSurname = gsub(" ", ",", .$Speakers)) %>%
  clean_up_NameSurname()

all_speakers_reduced <- all_speakers %>%
  clean_df() %>%
  select(Id, FirstName, LastName, TagLine) %>%
  #Manual fix add speaker NameSurname RICCARDO,CORRADIN
  #Manual fix add speaker NameSurname THOMAS,MAIER
  add_row(FirstName = "Riccardo", LastName = "Corradin", TagLine = "Università degli Studi Milano Bicocca") %>%
  add_row(FirstName = "Thomas", LastName = "Maier", TagLine = "Datahouse AG") %>%
  unite(NameSurname, FirstName, LastName, sep = ",") %>%
  clean_up_NameSurname() %>%
  #Manual fix case of Shodipo Ayomide and Mieke Deschepper that switched Name and Surname
  #Manual fix for Dana Jomar/Jomer that mispelled her name
  #Manual fix for Parvaneh Shafiei that mispelled her name
  mutate(NameSurname = case_when(
    NameSurname == toupper("Shodipo,Ayomide") ~ toupper("Ayomide,Shodipo"),
    NameSurname == toupper("Mieke,Deschepper") ~ toupper("Deschepper,Mieke"),
    NameSurname == toupper("Dana,Jomar") ~ toupper("Dana,Jomer"),
    NameSurname == toupper("Parvane,Shafiei") ~ toupper("Parvaneh,Shafiei"),
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
  #Manual fix for Desjeux Yann that switched Name Surname
  #Manual fix for Izhar Asael Alonzo Matamoros Asael that did not separate correctly Name and Surname
  #Manual fix for Mustafa Larbaoui that switched Name and Surname
  #Manual fix for Luís Gustavo Silva e Silva che in sessionize era Luís G. Silva e Silva
  #Manual fix for Paula González Avalos che in sessionize era Paula Gonzalez Avalos
  mutate(NameSurname = case_when(
    NameSurname == toupper("Shodipo,Ayomide") ~ toupper("Ayomide,Shodipo"),
    NameSurname == toupper("Mieke,Deschepper") ~ toupper("Deschepper,Mieke"),
    NameSurname == toupper("Dana,Jomar") ~ toupper("Dana,Jomer"),
    NameSurname == toupper("Zbynek,Slajchrt") ~ toupper("Zbyněk,Šlajchrt"),
    NameSurname == toupper("Giulio,Ferrero,Ferrero") ~ toupper("Giulio,Ferrero"),
    NameSurname == toupper("Olalekan,Akintande") ~ toupper("Olalekan,Joseph,Akintande"),
    NameSurname == toupper("Desjeux,Yann") ~ toupper("Yann,Desjeux"),
    NameSurname == toupper("Izhar,Asael,Alonzo,Matamoros,Asael") ~ toupper("Izhar,Asael,Alonzo,Matamoros"),
    NameSurname == toupper("Larbaoui,Mustapha") ~ toupper("Mustapha,Larbaoui"),
    NameSurname == toupper("Luís,Gustavo,Silva,e,Silva") ~ toupper("LUÍS,G.,SILVA,E,SILVA"),
    NameSurname == toupper("PAULA,GONZÁLEZ,AVALOS") ~ toupper("PAULA,GONZALEZ,AVALOS"),
    TRUE ~ NameSurname
  ))

accepted_reduced <- accepted %>%
  purrr::map_df(trimws) %>%
  mutate(choice = `choice\n`) %>%
  #Manual fix case of Shodipo Ayomide and Mieke Deschepper that switched Name and Surname
  #Manual fix for Dana Jomar/Jomer that mispelled her name
  #Manual fix for mustapha Larbaoui to Mustapha
  mutate(Speakers = case_when(
    Speakers == "Shodipo Ayomide" ~ "Ayomide Shodipo",
    Speakers == "Mieke Deschepper" ~ "Deschepper Mieke",
    Speakers == "Dana Jomar" ~ "Dana Jomer",
    Speakers == "parvane shafiei" ~ "Parvaneh Shafiei",
    Speakers == "mustapha Larbaoui" ~ "Mustapha Larbaoui",
    TRUE ~ Speakers
  )) %>%
  mutate(NameSurname = gsub(" ", ",", .$Speakers)) %>%
  clean_up_NameSurname() %>%
  select(Id, `choice`, Title, AssignedFormat ,NameSurname)

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
    session_type = AssignedFormat,
    description = Description
  ) %>%
  distinct() %>%
  filter(!is.na(title)) %>%
  arrange(session_type, author, title)

#Manual fix remove broken link
idx_broken_link <- grepl("Hydrological Modelling and R",session_speakers_confirmed$title)
session_speakers_confirmed$description[idx_broken_link] <- gsub('(https://)+(www\\.)+geotop', 'www\\.geotop', session_speakers_confirmed$description[idx_broken_link])

# Save output ----
jsonlite::write_json(session_speakers_confirmed, output_file, pretty = TRUE)
