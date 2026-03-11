suppressPackageStartupMessages({
  library(rtables)
  library(htmltools)
})

render_table_html <- function(tbl) {
  HTML(as_html(tbl))
}
