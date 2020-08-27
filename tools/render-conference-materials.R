output_dir <- "_site"

# create .md file for the conference materials page, extracted from README.md
readme <- readLines("README.md")
materials_md <- c(
  # include heading
  "## e-Rum2020 Conference Materials",
  # content from the Index section (w/o heading)
  tail(readme, -(grep("# Index", readme)))
)
md_file <- file.path(output_dir, "conference-materials.md")
writeLines(materials_md, md_file)

# render to HTML as GitHub document
rmarkdown::render(md_file, "github_document")
file.remove(md_file)

# make external links opening in a new tab, to also avoid browser security
# issues when embedding the page
html_file <- sub(".md", ".html", md_file, fixed = TRUE)
materials_html <- xml2::read_html(html_file)
links <- rvest::html_nodes(materials_html, css = "a")
internal <- grepl("^[#]", xml2::xml_attr(links, "href"))
xml2::xml_attr(links[!internal], "target") <- "_blank"
xml2::write_html(materials_html, html_file)
