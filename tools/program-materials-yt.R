`%>%` <- dplyr::`%>%`
source("tools/program-materials-parse-readme.R")

clean_parentheses <- function(x) {
  pattern <- "\\s*[(][^()]*[)]"
  while (any(grepl(pattern, x))) {
    x <- gsub(pattern, "", x)
  }
  x <- gsub("\\[|\\]", "", x)
  x
}


track_pattern <- "^(.*)\\s+\\-\\s+([^\\-]*)$"
strip_track <- function(title) {
  sub(track_pattern, "\\1", title)
}
get_track <- function(title) {
  sub(track_pattern, "\\2", title)
}
is_keynote <- function(title) {
  grepl("keynote", title, ignore.case = TRUE)
}
yt_title <- function(session) {
  title <- glue::glue("e-Rum2020 :: {session$title}")
  if (is_keynote(session$title)) {
    stopifnot(length(session$children) == 1L)
    title <- glue::glue("{strip_track(title)}: {session$children[[1]]$title}")
  }
  title
}

# create_yt_description(materials$`Invited Sessions`$children$`Invited Session 1 - Life Sciences / CovidR / R World`)
create_yt_description <- function(session) {
  c(
    yt_title(session), "\n",
    if (is_keynote(session$title)) {
      glue::glue('Keynote talk for the "{get_track(session$title)}" session\n\n')
    },
    paste0(
      "Speaker information and materials for this session are available at ",
      session$materials_url,
      "\n"
    ),
    if (!is_keynote(session$title)) {
      vapply(session$children, FUN.VALUE = "", USE.NAMES = FALSE, function(talk) {
        speaker <- talk$content %>%
          .[[intersect(names(.), c("Speaker", "Instructor", "Chairs"))]] %>%
          clean_parentheses()
        glue::glue(
          "- {speaker}: \"{talk$title}\""
        )
      })
    }
  )
}

materials <- parse_readme_materials("README.md")
yt_descriptions <- materials %>%
  lapply(`[[`, "children") %>% unname() %>%
  unlist(recursive = FALSE) %>%
  lapply(create_yt_description)

Map(
  function(title, desc) {
    glue::glue("## {title}\n\n{paste(desc, collapse='\n')}")
  },
  names(yt_descriptions), yt_descriptions
) %>% unlist() %>%
  cat(file = "tools/yt-descriptions.md", sep = "\n\n")
