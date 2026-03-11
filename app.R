suppressPackageStartupMessages({
  library(shiny)
  library(shinyAce)
  library(yaml)
  library(htmltools)
})

source("util/config.R")
source("util/exercise_io.R")
source("util/runner.R")
source("util/grading_rtables.R")
source("util/render_rtables.R")
source("util/save_submission.R")

ex_index <- load_ex_index(CONFIG$APP_ROOT)
choices <- make_exercise_choices(ex_index)

ui <- fluidPage(
  tags$head(tags$style(HTML("\n    .ref-grey .ace_content, .ref-grey .ace_scroller { color: #6b7280 !important; }\n    .muted { color:#6b7280; }\n    .box { border:1px solid #e5e7eb; border-radius:10px; padding:12px; margin-bottom:12px; }\n    .btnrow { display:flex; gap:8px; flex-wrap:wrap; align-items:center; }\n    .small { font-size:12px; color:#6b7280; }\n  "))),

  titlePanel("CSRTRAIN — R Training Playground"),

  fluidRow(
    column(
      3,
      div(class = "box",
          h4("Exercise"),
          selectInput("ex_id", NULL, choices = choices),
          uiOutput("ex_desc"),
          tags$hr(),
          div(class = "btnrow",
              actionButton("run", "Run (Preview)"),
              actionButton("submit", "Submit (Grade)", class = "btn-primary"),
              actionButton("save", "Save code")
          ),
          tags$hr(),
          checkboxInput("show_ref", "Show reference code (read-only)", value = FALSE),
          div(class = "small",
              "Reference/starter files include setup header for copy-paste run.",
              tags$br(),
              "Saved to: ", tags$code(CONFIG$SAVE_ROOT))
      )
    ),

    column(
      5,
      div(class = "box",
          h4("Your code"),
          aceEditor("editor", mode = "r", theme = "chrome", height = "520px", fontSize = 14, value = "")
      ),
      conditionalPanel(
        condition = "input.show_ref == true",
        div(class = "box ref-grey",
            h4("Reference code (read-only)"),
            aceEditor("ref", mode = "r", theme = "chrome", height = "280px", fontSize = 13, readOnly = TRUE, value = "")
        )
      )
    ),

    column(
      4,
      div(class = "box",
          h4("Table preview"),
          uiOutput("tbl_preview")
      ),
      div(class = "box",
          h4("Result"),
          verbatimTextOutput("result")
      )
    )
  )
)

server <- function(input, output, session) {
  user_id <- reactive({
    u <- session$user
    if (is.null(u) || !nzchar(u)) u <- Sys.info()[["user"]]
    if (is.null(u) || !nzchar(u)) u <- "unknown_user"
    u
  })

  current_ex <- reactive({
    req(input$ex_id)
    load_exercise(CONFIG$APP_ROOT, ex_index, input$ex_id)
  })

  observeEvent(current_ex(), {
    ex <- current_ex()

    starter_txt <- readLines(ex$starter_file, warn = FALSE)
    ref_txt <- readLines(ex$reference_file, warn = FALSE)

    updateAceEditor(session, "editor", value = paste(starter_txt, collapse = "\n"))
    updateAceEditor(session, "ref", value = paste(ref_txt, collapse = "\n"))

    output$ex_desc <- renderUI({
      div(
        tags$h5(ex$title),
        div(class = "muted", HTML(gsub("\n", "<br/>", ex$description)))
      )
    })

    output$result <- renderText("")
    output$tbl_preview <- renderUI(tags$em("Run to preview output."))
  }, ignoreInit = FALSE)

  observeEvent(input$run, {
    ex <- current_ex()
    out <- run_user_code(input$editor, timeout_sec = ex$timeout_sec)

    if (!out$ok) {
      output$result <- renderText(out$msg)
      output$tbl_preview <- renderUI(tags$pre("No table (execution error)."))
      return()
    }

    output$result <- renderText("Preview generated. Submit to grade.")
    output$tbl_preview <- renderUI(render_table_html(out$tbl))
  })

  observeEvent(input$submit, {
    ex <- current_ex()
    out <- run_user_code(input$editor, timeout_sec = ex$timeout_sec)

    if (!out$ok) {
      output$result <- renderText(out$msg)
      output$tbl_preview <- renderUI(tags$pre("No table (execution error)."))
      return()
    }

    ref_out <- run_reference_code(ex$reference_file, timeout_sec = ex$timeout_sec)
    if (!ref_out$ok) {
      output$result <- renderText(paste0("Reference failed:\n", ref_out$msg))
      return()
    }

    g <- grade_rtables_matrix(out$tbl, ref_out$tbl)
    output$result <- renderText(g$msg)
    output$tbl_preview <- renderUI(render_table_html(out$tbl))
  })

  observeEvent(input$save, {
    ex <- current_ex()
    saved <- save_submission(CONFIG$SAVE_ROOT, user_id(), ex$id, input$editor)
    output$result <- renderText(paste0("Saved to:\n", saved))
  })
}

shinyApp(ui, server)
