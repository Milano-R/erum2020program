# Create dataset to display as bookdown
# To run manually to generate the clean dataset to display
rm(list=ls())
# Dependencies ----
library(dplyr)
library(tidyr)
library(readxl)
library(purrr)
library(googledrive)
library(stringr)

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
eventbrite_full <- program_downolad("report-2020-05-31T1707.xlsx")

if (update_dump) {
  googledrive::drive_deauth()
}

# save.image("dataDumpMay31.RData")
load("dataDumpMay31.RData")
# Read files
all_sessions <- read_excel(sessionize_full_dump, sheet = "All Submitted Sessions")
all_speakers <- read_excel(sessionize_full_dump, sheet = "All Speakers")
confirmations <- read_excel(gform_confirmation)
accepted <- read_excel(accepted_full)
eventbrite <- read_excel(eventbrite_full)


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
  bind_rows(accepted %>% filter(Speakers == "Thomas Maier" )) %>%
  clean_df() %>%
  select(Id, Title, Description, Speakers, Track, `Co-authors` ) %>%
  mutate(Speakers = gsub(" N/A", "", .$Speakers)) %>%
  #Manual fix case of Shodipo Ayomide that switched Name and Surname
  #Manual fix for Dana Jomar/Jomer that mispelled her name
  #Manual fix for Parvaneh Shafiei that mispelled her name
  mutate(Speakers = case_when(
    Speakers == "Shodipo Ayomide" ~ "Ayomide Shodipo",
    Speakers == "Dana Jomar" ~ "Dana Jomer",
    Speakers == "parvane shafiei" ~ "Parvaneh Shafiei",
    Speakers == "mustapha Larbaoui" ~ "Mustapha Larbaoui",
    Speakers == "Thomas Maier" ~ "Daniel Meister",
    TRUE ~ Speakers
  )) %>%
  mutate(NameSurname = gsub(" ", ",", .$Speakers)) %>%
  clean_up_NameSurname() %>%
  mutate(`Co-authors` = case_when(NameSurname == "ANDRÉ,RIVENÆS" ~ NA_character_,
                             NameSurname == "LUCA,TORRIANI" ~ "Alessandra Menafoglio, Piercesare Secchi",
                             TRUE ~ `Co-authors`
  ))

# check
# all_sessions_reduced[all_sessions_reduced$NameSurname == "ANDRÉ,RIVENÆS",]
# all_sessions_reduced[all_sessions_reduced$NameSurname == "LUCA,TORRIANI",]

all_speakers_reduced <- all_speakers %>%
  clean_df() %>%
  select(Id, FirstName, LastName, TagLine) %>%
  #Manual fix add speaker NameSurname RICCARDO,CORRADIN
  #Manual fix add speaker NameSurname THOMAS,MAIER
  add_row(FirstName = "Riccardo", LastName = "Corradin", TagLine = "Università degli Studi Milano Bicocca") %>%
  add_row(FirstName = "Daniel", LastName = "Meister", TagLine = "Datahouse AG") %>%
  add_row(FirstName = "Vincent", LastName = "Guyader", TagLine = "ThinkR") %>%
  add_row(FirstName = "Ursula", LastName = "Gasser", TagLine = "PartnerRe") %>%
  unite(NameSurname, FirstName, LastName, sep = ",") %>%
  clean_up_NameSurname() %>%
  #Manual fix case of Shodipo Ayomide and  that switched Name and Surname
  #Manual fix for Dana Jomar/Jomer that mispelled her name
  #Manual fix for Parvaneh Shafiei that mispelled her name
  mutate(NameSurname = case_when(
    NameSurname == toupper("Shodipo,Ayomide") ~ toupper("Ayomide,Shodipo"),
    NameSurname == toupper("Dana,Jomar") ~ toupper("Dana,Jomer"),
    NameSurname == toupper("Parvane,Shafiei") ~ toupper("Parvaneh,Shafiei"),
    TRUE ~ NameSurname
  ))

# all_speakers_reduced$NameSurname[str_detect(all_speakers_reduced$NameSurname,"KIRILL")]

# confirmations_reduced <- confirmations %>%
#   rename(confirm = starts_with("Do you confirm")) %>%
#   clean_df() %>%
#   unite(NameSurname, Name, Surname, sep = ",") %>%
#   select(NameSurname, confirm) %>%
#   clean_up_NameSurname() %>%
#   #Manual fix case of Shodipo Ayomide and  that switched Name and Surname
#   #Manual fix for Dana Jomar/Jomer that mispelled her name
#   #Manual fix for Giulio,Ferrero,Ferrero repeated surname
#   #Manual fix for Olalekan Joseph Akintande	 that did not add middle name
#   #Manual fix for name with special character inconsitently entered
#   #Manual fix for Desjeux Yann that switched Name Surname
#   #Manual fix for Izhar Asael Alonzo Matamoros Asael that did not separate correctly Name and Surname
#   #Manual fix for Mustafa Larbaoui that switched Name and Surname
#   #Manual fix for Luís Gustavo Silva e Silva che in sessionize era Luís G. Silva e Silva
#   #Manual fix for Paula González Avalos che in sessionize era Paula Gonzalez Avalos
#   mutate(NameSurname = case_when(
#     NameSurname == toupper("Shodipo,Ayomide") ~ toupper("Ayomide,Shodipo"),
#     NameSurname == toupper("Dana,Jomar") ~ toupper("Dana,Jomer"),
#     NameSurname == toupper("Zbynek,Slajchrt") ~ toupper("Zbyněk,Šlajchrt"),
#     NameSurname == toupper("Giulio,Ferrero,Ferrero") ~ toupper("Giulio,Ferrero"),
#     NameSurname == toupper("Olalekan,Akintande") ~ toupper("Olalekan,Joseph,Akintande"),
#     NameSurname == toupper("Desjeux,Yann") ~ toupper("Yann,Desjeux"),
#     NameSurname == toupper("Izhar,Asael,Alonzo,Matamoros,Asael") ~ toupper("Izhar,Asael,Alonzo,Matamoros"),
#     NameSurname == toupper("Larbaoui,Mustapha") ~ toupper("Mustapha,Larbaoui"),
#     NameSurname == toupper("Luís,Gustavo,Silva,e,Silva") ~ toupper("LUÍS,G.,SILVA,E,SILVA"),
#     NameSurname == toupper("PAULA,GONZÁLEZ,AVALOS") ~ toupper("PAULA,GONZALEZ,AVALOS"),
#     TRUE ~ NameSurname
#   ))

accepted_reduced <- accepted %>%
  purrr::map_df(trimws) %>%
  mutate(choice = `choice\n`) %>%
  #Manual fix case of Shodipo Ayomide and  that switched Name and Surname
  #Manual fix for Dana Jomar/Jomer that mispelled her name
  #Manual fix for mustapha Larbaoui to Mustapha
  mutate(Speakers = case_when(
    Speakers == "Shodipo Ayomide" ~ "Ayomide Shodipo",
    Speakers == "Dana Jomar" ~ "Dana Jomer",
    Speakers == "parvane shafiei" ~ "Parvaneh Shafiei",
    Speakers == "mustapha Larbaoui" ~ "Mustapha Larbaoui",
    Speakers == "Thomas Maier" ~ "Daniel Meister",
    TRUE ~ Speakers
  )) %>%
  mutate(NameSurname = gsub(" ", ",", .$Speakers)) %>%
  clean_up_NameSurname() %>%
  select(Id, `choice`, Title, AssignedFormat ,NameSurname)


eventbrite_reduced <- eventbrite %>% 
  mutate(TipologiaBiglietto = `Tipologia biglietto`) %>%
  add_row(Nome = "Andrie", Cognome = "De Vries", `E-mail` = "andrie@rstudio.com") %>%
  filter(`Tipologia biglietto` == "Conference ticket - Speaker" 
         | Cognome == "Crippa" 
         | Cognome == "Ryser-Welch"
         | Cognome == "Marini"
         | Cognome == "Melloncelli"
         | Cognome == "Sax"
         | Cognome == "De Vries") %>%
  unite(NameSurname,Nome,Cognome, sep = ",") %>%
  select(NameSurname, `E-mail`, TipologiaBiglietto) %>%
  clean_up_NameSurname() %>%
  distinct(NameSurname,TipologiaBiglietto,.keep_all = TRUE) %>%
  mutate(author2 = NA_character_, affiliation2 = NA_character_) %>%
  mutate(NameSurname = case_when(
    NameSurname == toupper("ottavia,epifania") ~ toupper("ottavia,m.,epifania"),
    NameSurname == toupper("shodipo,ayomide") ~ toupper("ayomide,shodipo"),
    NameSurname == toupper("marco,franco") ~ toupper("marco,cavaliere"),
    NameSurname == toupper("ANDRÉ,WAAGE,RIVENÆS") ~ toupper("ANDRÉ,RIVENÆS"),
    NameSurname == toupper("MATT,BANNERT") ~ toupper("MATTHIAS,BANNERT"),
    NameSurname == toupper("CLAUS,EKSTROM") ~ toupper("CLAUS,EKSTRØM"),
    NameSurname == toupper("OLALEKAN,AKINTANDE") ~ toupper("OLALEKAN,JOSEPH,AKINTANDE"),
    NameSurname == toupper("LUÍS,SILVA,E,SILVA") ~ toupper("LUÍS,G.,SILVA,E,SILVA"), 
  #  NameSurname == toupper("KIRILL,MÜLLER") ~ toupper("KIRILL,MÜLLER"), 
    TRUE ~ NameSurname
  )) %>%
  mutate(author2 = case_when(NameSurname == "ANDRÉ,RIVENÆS" ~ "Markus Mortensen",
         NameSurname == "LUCA,TORRIANI" ~ "Ilaria Sartori",
         TRUE ~ author2
         )) %>%
  mutate(affiliation2 = case_when(NameSurname == "ANDRÉ,RIVENÆS" ~ "PwC",
                             NameSurname == "LUCA,TORRIANI" ~ "Politecnico di Milano",
                             TRUE ~ affiliation2))




# Join data
all_speakers_confirmed <- all_speakers_reduced %>%
  full_join(eventbrite_reduced, by = "NameSurname")

all_sessions_accepted <- all_sessions_reduced %>%
  full_join(accepted_reduced, by = c("NameSurname", "Title") ) %>%
  filter(choice > 0)

session_speakers_confirmed <- full_join(all_sessions_accepted, all_speakers_confirmed, by = "NameSurname") %>%
  # by hand Kirill Muller I don't know why full join doesn't work
  mutate(`E-mail` = case_when(
    Id == "905a186f-f5cc-4253-b449-455c45dea8c4" ~ "kirill@cynkra.com",
    TRUE ~ `E-mail`)) %>%
  mutate(TipologiaBiglietto = case_when(
    Id == "905a186f-f5cc-4253-b449-455c45dea8c4" ~ "Conference ticket - Speaker",
    TRUE ~ TipologiaBiglietto)) %>% 
  filter(!is.na(TipologiaBiglietto)) %>%
  transmute(
    title = Title,
    author = Speakers,
    affiliation = TagLine,
    namesurname = NameSurname,
    coauthor = `Co-authors`,
    author2 = author2,
    affiliation2 = affiliation2,
    track = Track,
    session_type = AssignedFormat,
    description = Description,
    email = `E-mail`
  ) %>%
  distinct() %>%
  filter(!is.na(title)) %>%
  # manual
  filter(title != "Transparent presentation of uncertain lotteries using {deals}") %>%
  arrange(session_type, author, title)

session_speakers_confirmed <- session_speakers_confirmed %>% 
  add_row(title = "What's New in ShinyProxy", author = "Tobias Verbeke", 
          affiliation = "Managing Director, Open Analytics", session_type = "Regular talk",
          description = "Shiny is nice technology to write interactive R-based applications. It is broadly adopted and the R community has collaborated on many 
interesting extensions. Until recently, though, deployments in larger organizations and companies required proprietary solutions. ShinyProxy fills this gap and 
offers a fully open source alternative to run and manage shiny applications at large. In this talk we detail the 
ShinyProxy architecture and demonstrate how it meets the needs of organizations. 
We will discuss how it scales to thousands of concurrent users and how it offers authentication and 
authorization functionality using standard technologies (LDAP, ActiveDirectory, OpenID Connect, SAML 2.0 and Kerberos). 
Also, we will discuss the management interface and how it allows to monitor application usage to collect 
usage statistics in event logging databases. Finally, we will demonstrate that Shiny applications 
can now be easily embedded in broader applications and (responsive) web sites using the ShinyProxy API. 
Learn how academic institutions, governmental organizations and industry roll out Shiny apps with 
          ShinyProxy and how you can do this too. See https://shinyproxy.io.",
          namesurname = "TOBIAS,VERBEKE", track = "R Dataviz & Shiny")

session_speakers_confirmed <- session_speakers_confirmed %>% 
  add_row(title = "The R Consortium 2020: adapting to rapid change and global crisis", author = "Joseph Rickert", 
          affiliation = "RStudio: R Community Ambassador, R Consortium's Board of Directors", session_type = "Regular talk",
          description = "The COVID-19 pandemic has turned the world upside down, and like everyone else the R Community is learning how to adapt to rapid change in order to carry on important work while looking for ways to contribute to the fight against the pandemic. In this talk, I will report on continuing R Community work being organized through the R Consortium such as the R Hub, R User Group Support Program and Diversity and Inclusion Projects; and through the various working groups including the Validation Hub, R / Pharma, R / Medicine and R Business. Additionally, I will describe some of the recently funded ISC projects and report on the COVID-19 Data Forum, a new project that the R Consortium is organizing in partnership with Stanford’s Data Science Institute.",
          namesurname = "JOSEPH,RICKERT", track = "R World")

session_speakers_confirmed <- session_speakers_confirmed %>%
  arrange(session_type, author, title)


# check: comment line filter and check manually

# filter(all_speakers_reduced,str_detect(NameSurname,"AKINTANDE"))
# filter(eventbrite_reduced,str_detect(NameSurname,"AKINTANDE"))
# eventbrite_reduced[str_detect(eventbrite_reduced$NameSurname,"KIRILL"),1]
# all_speakers_reduced[str_detect(all_speakers_reduced$NameSurname,"KIRILL"),2]
# session_speakers_confirmed[str_detect(session_speakers_confirmed$namesurname,"CORRADIN"),2]
filter(all_speakers_confirmed,TipologiaBiglietto != "Conference ticket - Speaker") -> ToAddEventbrite


#Manual fix remove broken link
idx_broken_link <- grepl("Hydrological Modelling and R",session_speakers_confirmed$title)
session_speakers_confirmed$description[idx_broken_link] <- gsub('(https://)+(www\\.)+geotop', 'www\\.geotop', session_speakers_confirmed$description[idx_broken_link])

# Save output ----
jsonlite::write_json(session_speakers_confirmed, output_file, pretty = TRUE)
