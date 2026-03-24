suppressPackageStartupMessages({
  library(shiny)
  library(yaml)
  library(htmltools)
})

HAS_SHINYACE <- requireNamespace("shinyAce", quietly = TRUE)
if (HAS_SHINYACE) {
  suppressPackageStartupMessages(library(shinyAce))
}

source("util/config.R")
source("util/exercise_io.R")
source("util/runner.R")
source("util/grading_rtables.R")
source("util/render_rtables.R")
source("util/save_submission.R")

ex_index <- load_ex_index(CONFIG$APP_ROOT)
choices <- make_exercise_choices(ex_index)

`%||%` <- function(x, y) if (!is.null(x)) x else y

code_input_ui <- function(id, value = "") {
  if (HAS_SHINYACE) {
    return(aceEditor(
      id,
      mode = "r",
      theme = "chrome",
      height = "520px",
      fontSize = 14,
      value = value
    ))
  }

  textAreaInput(
    id,
    label = NULL,
    value = value,
    width = "100%",
    height = "520px",
    resize = "vertical"
  )
}

update_code_input <- function(session, id, value) {
  if (HAS_SHINYACE) {
    updateAceEditor(session, id, value = value)
  } else {
    updateTextAreaInput(session, id, value = value)
  }
}

get_code_input <- function(input, id) {
  input[[id]] %||% ""
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .muted { color:#6b7280; }
      .box { border:1px solid #e5e7eb; border-radius:10px; padding:12px; margin-bottom:12px; }
      .btnrow { display:flex; gap:8px; flex-wrap:wrap; align-items:center; }
      .small { font-size:12px; color:#6b7280; }
      pre {
        white-space: pre-wrap;
        word-break: break-word;
      }
    "))
  ),

  titlePanel("CSRTRAIN — R Training Playground"),

  fluidRow(
    column(
      3,
      div(
        class = "box",
        h4("Exercise"),
        selectInput("ex_id", NULL, choices = choices),
        uiOutput("ex_desc"),
        tags$hr(),
        div(
          class = "btnrow",
          actionButton("run", "Run (Preview)"),
          actionButton("submit", "Submit (Grade)", class = "btn-primary"),
          actionButton("save", "Save code")
        ),
        tags$hr(),
        div(
          class = "small",
          if (!HAS_SHINYACE) "shinyAce not found: using base textarea fallback.",
          tags$br(),
          "Starter code includes setup header for copy-paste run.",
          tags$br(),
          "Reference code is used only for grading.",
          tags$br(),
          "Saved to: ", tags$code(CONFIG$SAVE_ROOT)
        )
      )
    ),

    column(
      5,
      div(
        class = "box",
        h4("Your code"),
        code_input_ui("editor", value = "")
      )
    ),

    column(
      4,
      div(
        class = "box",
        h4("Table preview"),
        uiOutput("tbl_preview")
      ),
      div(
        class = "box",
        h4("Result"),
        verbatimTextOutput("result")
      )
    )
  )
)

server <- function(input, output, session) {
  user_id <- reactive({
    u <- session$user
    if (is.null(u) || !nzchar(u)) u <- Sys.getenv("USER")
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
    starter_code <- paste(starter_txt, collapse = "\n")

    update_code_input(session, "editor", starter_code)

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
    code <- get_code_input(input, "editor")

    out <- tryCatch(
      run_user_code(code, timeout_sec = ex$timeout_sec),
      error = function(e) {
        list(ok = FALSE, tbl = NULL, msg = paste0("Run failed:\n", conditionMessage(e)))
      }
    )

    if (!isTRUE(out$ok)) {
      output$result <- renderText(out$msg %||% "Unknown execution error.")
      output$tbl_preview <- renderUI(tags$pre("No table (execution error)."))
      return()
    }

    preview_ui <- tryCatch(
      render_table_html(out$tbl),
      error = function(e) {
        tags$pre(paste0("Preview failed:\n", conditionMessage(e)))
      }
    )

    output$result <- renderText("Preview generated. Submit to grade.")
    output$tbl_preview <- renderUI(preview_ui)
  })

  observeEvent(input$submit, {
    ex <- current_ex()
    code <- get_code_input(input, "editor")

    out <- tryCatch(
      run_user_code(code, timeout_sec = ex$timeout_sec),
      error = function(e) {
        list(ok = FALSE, tbl = NULL, msg = paste0("Run failed:\n", conditionMessage(e)))
      }
    )

    if (!isTRUE(out$ok)) {
      output$result <- renderText(out$msg %||% "Unknown execution error.")
      output$tbl_preview <- renderUI(tags$pre("No table (execution error)."))
      return()
    }

    ref_out <- tryCatch(
      run_reference_code(ex$reference_file, timeout_sec = ex$timeout_sec),
      error = function(e) {
        list(ok = FALSE, tbl = NULL, msg = paste0("Reference failed:\n", conditionMessage(e)))
      }
    )

    if (!isTRUE(ref_out$ok)) {
      output$result <- renderText(ref_out$msg %||% "Unknown reference execution error.")
      output$tbl_preview <- renderUI(tags$pre("Reference execution failed."))
      return()
    }

    g <- tryCatch(
      grade_rtables_matrix(out$tbl, ref_out$tbl),
      error = function(e) {
        list(pass = FALSE, msg = paste0("Grading failed:\n", conditionMessage(e)))
      }
    )

    preview_ui <- tryCatch(
      render_table_html(out$tbl),
      error = function(e) {
        tags$pre(paste0("Preview failed:\n", conditionMessage(e)))
      }
    )

    output$result <- renderText(g$msg %||% "Unknown grading result.")
    output$tbl_preview <- renderUI(preview_ui)
  })

  observeEvent(input$save, {
    ex <- current_ex()
    code <- get_code_input(input, "editor")

    saved <- tryCatch(
      save_submission(CONFIG$SAVE_ROOT, user_id(), ex$id, code),
      error = function(e) paste0("Save failed:\n", conditionMessage(e))
    )

    output$result <- renderText(paste0("Saved to:\n", saved))
  })
}

shinyApp(ui, server)
