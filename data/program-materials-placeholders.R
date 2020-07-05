prog <- readODS::read_ods("data/eRum2020-program-shared.ods")

by_type <- TRUE

md_entry <- function(speaker, title) {
  glue::glue(
    "#### {title}",
    "",
    "- Speaker: {speaker}",
    "- Materials: ",
    "",
    .sep = "\n"
  )
}

combine_topics <- function(topics) {
  topics <- unique(topics[!is.na(topics)])
  if (length(topics) > 0L) paste(topics, collapse = " / ")
}

sort_sessions_by_type <- function(sessions) {
  .by <- function(pattern) !grepl(pattern, sessions, ignore.case = TRUE)
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


sort_sessions <- if (by_type) sort_sessions_by_type else identity

`%>%` <- dplyr::`%>%`
prog_md <- prog %>%
  dplyr::rename_with(tolower) %>%
  dplyr::group_by(factor(session, unique(sort_sessions(session)))) %>%
  dplyr::mutate(
    session = paste(c(unique(session), combine_topics(topic)), collapse = " - "),
    md = md_entry(speaker, title)
  ) %>%
  dplyr::summarize(
    md = paste(
      glue::glue("### {unique(session)}"),
      paste(md, collapse = "\n\n"),
      sep = "\n\n"
    )
  ) %>%
  .[["md"]] %>%
  paste(collapse = "\n\n")
cat(prog_md, file = "README.md", append = TRUE)

