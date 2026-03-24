suppressPackageStartupMessages({
  library(htmltools)
})

render_table_html <- function(tbl) {
  txt <- paste(capture.output(print(tbl)), collapse = "\n")

  tags$pre(
    style = paste(
      "white-space: pre-wrap;",
      "font-family: Menlo, Monaco, Consolas, monospace;",
      "font-size: 12px;",
      "line-height: 1.35;",
      "margin: 0;"
    ),
    txt
  )
}
