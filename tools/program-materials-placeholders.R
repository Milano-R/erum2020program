prog <- readODS::read_ods("tools/eRum2020-program-shared.ods")

speaker_field <- function(session) {
  .type <- function(pattern) grepl(pattern, sub("-.*", "", session), ignore.case = TRUE)
  dplyr::case_when(
    .type("workshop") ~ "Instructor",
    .type("thematic") ~ "Participants",
    TRUE ~ "Speaker"
  )
}

md_entry <- function(title, session_type, speaker) {
  glue::glue(
    "#### {title}",
    "",
    "- {speaker_field(session_type)}: {ifelse(is.na(speaker), 'TBD', speaker)}",
    "- Materials: TBD",
    "",
    .sep = "\n"
  )
}

combine_topics <- function(topics) {
  topics <- unique(topics[!is.na(topics)])
  if (length(topics) > 0L) paste(topics, collapse = " / ")
}

sort_sessions_by_type <- function(sessions) {
  .by <- function(pattern) !grepl(pattern, sub("-.*", "", sessions), ignore.case = TRUE)
  sessions[order(
    .by("keynote"),
    .by("invited"),
    .by("parallel"),
    .by("lightning"),
    .by("shiny"),
    .by("poster"),
    .by("thematic"),
    .by("workshop")
  )]
}

group_factor <- function(sessions) {
  factor(sessions, unique(sort_sessions_by_type(sessions)))
}

gfm_heading_slugify <- function(heading) {
  slug <- tolower(heading)
  slug <- gsub("[^a-z0-9' -]+", "", slug)
  slug <- gsub("'", "", slug)
  slug <- gsub("\\s", "-", slug)
  slug
}
# test git-flavored-markdown internal hash slugs:
# > rmarkdown::render("README.md", "github_document", output_file = "/tmp/README.md")
# $ htmlproofer /tmp/README.html

heading_link <- function(session) {
  glue::glue(
    "[{session}](#{gfm_heading_slugify(session)})"
  )
}

session_type <- function(session) {
  type <- sub("\\s+[0-9]+$", "", session)
  type <- ifelse(grepl("workshop", type, ignore.case = TRUE), "Workshop", type)
  type <- sub("^(.*[^s])$", "\\1s", type)
  type
}

session_title <- function(session, topics, speaker) {
  stopifnot(length(session) == 1L)
  if (grepl("keynote", session, ignore.case = TRUE)) {
    session <- c(session, speaker)
  }
  title <- paste(c(unique(session), combine_topics(topics)), collapse = " - ")
}

`%>%` <- dplyr::`%>%`

prog_sessions_md <- prog %>%
  dplyr::rename_with(tolower) %>%
  dplyr::group_by(session) %>%
  dplyr::mutate(
    session_type = session_type(session),
    session = session_title(unique(session), topic, speaker),
    md = md_entry(title, session_type, speaker)
  ) %>%
  dplyr::ungroup()
toc_md <- prog_sessions_md %>%
  dplyr::group_by(group_factor(session_type)) %>%
  dplyr::summarize(
    md = paste0(
      glue::glue("- {heading_link(unique(session_type))}"), "\n",
      paste(glue::glue("  - {heading_link(unique(session))}"), collapse = "\n")
    )
  ) %>%
  .[["md"]] %>%
  c("## Index", .) %>%
  paste(collapse = "\n\n")
sessions_md <- prog_sessions_md %>%
  split(group_factor(.$session_type)) %>%
  vapply(FUN.VALUE = "", function(x) {
    x %>% dplyr::group_by(group_factor(session)) %>%
      dplyr::summarize(
        md = paste(
          glue::glue("### {unique(session)}"),
          paste(md, collapse = "\n\n"),
          sep = "\n\n"
        )
      ) %>%
      .[["md"]] %>%
      c(glue::glue("## {unique(x$session_type)}"), .) %>%
      paste(collapse = "\n\n")
  })

cat(
  toc_md,
  sessions_md,
  sep = "\n\n",
  file = "README.md", append = TRUE
)
