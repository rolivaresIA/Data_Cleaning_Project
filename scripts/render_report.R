# Runs the cleaning script and renders the report (Markdown + figures).
# Usage (from the repository root):  Rscript scripts/render_report.R
options(cli.unicode = FALSE, width = 110)
knitr::opts_knit$set(root.dir = getwd())
knitr::opts_chunk$set(fig.path = "figures/", dev = "png", dpi = 110, comment = "##", error = FALSE)
rmd <- knitr::spin("scripts/01_clean_shein_data.R", knit = FALSE)
knitr::knit(rmd, output = "01_cleaning_report.md", quiet = TRUE)
file.remove(rmd)
message("Rendered 01_cleaning_report.md")
