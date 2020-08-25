output_dir <- "_site"
readme <- readLines("README.md")
materials_md <- c(
  # include heading
  "## e-Rum2020 Conference Materials",
  # content from the Index section (w/o heading)
  tail(readme, -(grep("# Index", readme)))
)
md_file <- file.path(output_dir, "conference-materials.md")
writeLines(materials_md, md_file)
rmarkdown::render(md_file, "github_document")
file.remove(md_file)
