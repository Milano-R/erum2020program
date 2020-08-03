`%>%` <- dplyr::`%>%`

output_file <- tempfile(fileext = ".html")

# get_readme_html_with_toc("README.md")
get_readme_html_with_toc <- function(file) {
  readme_content <- rmarkdown::render(
    file,
    output_file = output_file,
    output_format = rmarkdown::github_document(
      # we use toc to iner correct anchor links
      toc = TRUE, toc_depth = 4,
      # don't use smart quotes so we can match the raw text in the README
      md_extensions = "-smart"
    )
  ) %>%
    xml2::read_html()
}

# get_readme_anchors("README.md")
get_readme_anchors <- function(file) {
  internal_links <-
    get_readme_html_with_toc(file) %>%
    rvest::xml_nodes("a[href^='#']")
  anchors <- stats::setNames(
    rvest::html_attr(internal_links, "href"),
    rvest::html_text(internal_links)
  )
  anchors <- anchors[!duplicated(names(anchors))]
  anchors
}


find_headings <- function(x, lev) {
  grep(sprintf("^#{%d,%d}\\s+", lev, lev), x)
}

split_heading_content <- function(x, lev) {
  heading_start <- find_headings(x, lev + 1)
  heading_end <- dplyr::lead(heading_start - 1, 1, default = length(x))
  content_end <- c(head(heading_start, 1) - 1, length(x))[[1]]
  list(
    content = x[seq_len(content_end)],
    children = Map(function(start, end) x[start:end], heading_start, heading_end)
  )
}

parse_bullet <- function(x) {
  x <- paste(x, collapse = " ")
  x <- sub("^\\s*-\\s+", "", x)
  pattern <- "^([^\\:]+):\\s+(.*)$"
  if (grepl(pattern, x)) {
    x <- setNames(
      sub(pattern, "\\2", x),
      sub(pattern, "\\1", x)
    )
  }
  as.list(x)
}
parse_bullet("  - Field: value")
parse_bullet("  - value")

parse_content <- function(x) {
  bullet_start <- grep("^\\s*-", x)
  if (length(bullet_start) == 0L) {
    x
  } else {
    bullet_end <- dplyr::lead(bullet_start - 1, 1, default = length(x))
    do.call(
      c,
      Map(
        function(start, end) parse_bullet(x[start:end]),
        bullet_start, bullet_end
      )
    )
  }
}

set_headings_name <- function(x) {
  setNames(x, vapply(x, `[[`, "title", FUN.VALUE = ""))
}

parse_heading <- function(x, anchors) {
  x <- unlist(strsplit(x, "\\n"))
  stopifnot(substr(x[[1]], 1, 1) == "#")
  heading_lev <- nchar(sub("^(#+)\\s+.*$", "\\1", x[[1]]))
  title <- sub("^#+\\s+", "", x[[1]])
  content <- split_heading_content(x[-1], heading_lev)
  if (is.na(anchors[title])) {
    stop("No materials link found for ", sQuote(title))
  }
  materials_repo <- "https://github.com/Milano-R/erum2020program"
  materials_url <- paste0(materials_repo, anchors[title])

  list(
    title = title,
    materials_url = materials_url,
    content = parse_content(content$content),
    children = lapply(
      content$children,
      parse_heading,
      anchors = anchors
    ) %>%
      set_headings_name()
  )
}

# materials <- parse_readme_materials("README.md")
# str(materials, max.level = 7)
parse_readme_materials <- function(file) {
  anchors <- get_readme_anchors(file)
  materials <- readLines("README.md") %>%
    split_heading_content(1) %>%
    .$children %>%
    lapply(parse_heading, anchors) %>%
    set_headings_name() %>%
    tail(-which(names(.) == "Index"))
}
