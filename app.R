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
source("util/helper_io.R")
source("util/runner.R")
source("util/grading_rtables.R")
source("util/render_rtables.R")
source("util/save_submission.R")

`%||%` <- function(x, y) if (!is.null(x)) x else y

ex_index <- load_ex_index(CONFIG$APP_ROOT)
choices <- make_exercise_choices(ex_index)
helper_catalog <- load_helper_catalog(CONFIG$APP_ROOT)

code_input_ui <- function(id, value = "") {
  if (HAS_SHINYACE) {
    return(shinyAce::aceEditor(
      id,
      mode = "r",
      theme = "chrome",
      height = "560px",
      fontSize = 14,
      value = value
    ))
  }

  textAreaInput(
    id,
    label = NULL,
    value = value,
    width = "100%",
    height = "560px",
    resize = "vertical"
  )
}

update_code_input <- function(session, id, value) {
  if (HAS_SHINYACE) {
    shinyAce::updateAceEditor(session, id, value = value)
  } else {
    updateTextAreaInput(session, id, value = value)
  }
}

get_code_input <- function(input, id) {
  input[[id]] %||% ""
}

normalize_key <- function(x) {
  tolower(gsub("[^0-9a-z]+", "", x %||% ""))
}

resolve_shell_file <- function(ex, docs_dir) {
  if (!dir.exists(docs_dir)) return(NULL)

  files <- list.files(docs_dir, full.names = FALSE)
  if (length(files) == 0) return(NULL)

  shell_file <- ex$shell_file %||% ""
  if (nzchar(shell_file) && shell_file %in% files) {
    return(shell_file)
  }

  shell_table <- ex$shell_table %||% ""
  if (!nzchar(shell_table)) return(NULL)

  key <- normalize_key(shell_table)
  key <- sub("^table", "", key)

  norm_files <- vapply(files, normalize_key, character(1))
  idx <- which(grepl(key, norm_files, fixed = TRUE))

  if (length(idx) > 0) files[idx[1]] else NULL
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background:#f8fafc; }
      .muted { color:#6b7280; }
      .box {
        border:1px solid #e5e7eb;
        border-radius:12px;
        padding:14px;
        margin-bottom:14px;
        background:white;
        box-shadow: 0 1px 2px rgba(0,0,0,0.03);
      }
      .btnrow {
        display:flex;
        gap:8px;
        flex-wrap:wrap;
        align-items:center;
      }
      .small { font-size:12px; color:#6b7280; }
      .meta-label {
        font-size: 12px;
        color: #6b7280;
        margin-top: 8px;
        margin-bottom: 2px;
      }
      .meta-value {
        font-size: 13px;
        color: #111827;
        word-break: break-word;
      }
      pre {
        white-space: pre-wrap;
        word-break: break-word;
        margin: 0;
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
        uiOutput("exercise_meta"),
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
          "Reference code is hidden and used only for grading."
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
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Preview",
          div(class = "box", uiOutput("tbl_preview"))
        ),
        tabPanel(
          "Grade",
          div(class = "box", verbatimTextOutput("result"))
        ),
        tabPanel(
          "Helpers",
          div(
            class = "box",
            textInput("helper_search", "Search helpers", value = ""),
            uiOutput("helper_select_ui"),
            tags$hr(),
            uiOutput("helper_preview")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  user_id <- reactive({
    resolve_user_id(session$user)
  })

  user_paths <- reactive({
    build_user_paths(user_id())
  })

  current_ex <- reactive({
    req(input$ex_id)
    load_exercise(CONFIG$APP_ROOT, ex_index, input$ex_id)
  })

  docs_prefix <- reactiveVal(NULL)

  # Filter helpers; empty search should show ALL helpers
  helper_filtered <- reactive({
    filter_helper_catalog(helper_catalog, input$helper_search %||% "")
  })

  observeEvent(user_paths(), {
    up <- user_paths()

    if (dir.exists(up$docs_dir)) {
      prefix <- paste0("docs_", gsub("[^A-Za-z0-9_]", "_", up$user_id))
      addResourcePath(prefix, up$docs_dir)
      docs_prefix(prefix)
    } else {
      docs_prefix(NULL)
    }
  }, ignoreInit = FALSE)

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

  output$helper_select_ui <- renderUI({
    filtered <- helper_filtered()

    if (is.null(filtered) || nrow(filtered) == 0) {
      return(tags$div(class = "muted", "No helpers found."))
    }

    selected <- input$helper_key
    if (is.null(selected) || !selected %in% filtered$key) {
      ex <- current_ex()
      recommended <- ex$helper_keys %||% character()
      hit <- recommended[recommended %in% filtered$key]

      if (length(hit) > 0) {
        selected <- hit[1]
      } else {
        selected <- filtered$key[1]
      }
    }

    selectInput(
      "helper_key",
      "Helper",
      choices = helper_choices(filtered),
      selected = selected
    )
  })

  output$helper_preview <- renderUI({
    filtered <- helper_filtered()

    if (is.null(filtered) || nrow(filtered) == 0) {
      return(tags$pre("No helper documentation available."))
    }

    selected_key <- input$helper_key
    if (is.null(selected_key) || !selected_key %in% filtered$key) {
      ex <- current_ex()
      recommended <- ex$helper_keys %||% character()
      hit <- recommended[recommended %in% filtered$key]

      if (length(hit) > 0) {
        selected_key <- hit[1]
      } else {
        selected_key <- filtered$key[1]
      }
    }

    txt <- read_helper_doc(CONFIG$APP_ROOT, helper_catalog, selected_key)

    tags$pre(
      style = paste(
        "white-space: pre-wrap;",
        "font-family: Menlo, Monaco, Consolas, monospace;",
        "font-size: 12px;",
        "line-height: 1.35;"
      ),
      txt
    )
  })

  output$exercise_meta <- renderUI({
    ex <- current_ex()
    up <- user_paths()
    pfx <- docs_prefix()

    shell_table <- ex$shell_table %||% "Not specified"
    grading_mode <- ex$grading_mode %||% "table_text"
    helper_keys <- ex$helper_keys %||% character()

    shell_doc <- resolve_shell_file(ex, up$docs_dir)
    shell_link <- NULL
    if (!is.null(pfx) && !is.null(shell_doc)) {
      shell_href <- paste0("/", pfx, "/", utils::URLencode(shell_doc))
      shell_link <- tags$a(href = shell_href, target = "_blank", "Open shell / documentation")
    }

    tagList(
      div(class = "meta-label", "Current user"),
      div(class = "meta-value", up$user_id),

      div(class = "meta-label", "Training RA root"),
      div(class = "meta-value", up$ra_root),

      div(class = "meta-label", "Save location"),
      div(class = "meta-value", up$save_dir),

      div(class = "meta-label", "Shell table"),
      div(class = "meta-value", shell_table),

      div(class = "meta-label", "Grading mode"),
      div(class = "meta-value", grading_mode),

      if (length(helper_keys) > 0) tagList(
        div(class = "meta-label", "Recommended helpers"),
        div(class = "meta-value", paste(helper_keys, collapse = ", "))
      ),

      if (!is.null(shell_link)) tagList(
        div(class = "meta-label", "Documentation"),
        div(class = "meta-value", shell_link)
      )
    )
  })

  observeEvent(input$run, {
    ex <- current_ex()
    up <- user_paths()
    code <- get_code_input(input, "editor")

    out <- tryCatch(
      run_user_code(
        code = code,
        training_ra_root = up$ra_root,
        timeout_sec = ex$timeout_sec
      ),
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
    up <- user_paths()
    code <- get_code_input(input, "editor")
    grading_mode <- ex$grading_mode %||% "table_text"

    out <- tryCatch(
      run_user_code(
        code = code,
        training_ra_root = up$ra_root,
        timeout_sec = ex$timeout_sec
      ),
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
      run_reference_code(
        reference_file = ex$reference_file,
        training_ra_root = up$ra_root,
        timeout_sec = ex$timeout_sec
      ),
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
      {
        if (identical(grading_mode, "table_text")) {
          grade_rtables_matrix(out$tbl, ref_out$tbl)
        } else {
          list(pass = FALSE, msg = paste0("Unsupported grading mode: ", grading_mode))
        }
      },
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
    up <- user_paths()
    code <- get_code_input(input, "editor")

    saved <- tryCatch(
      save_submission(
        save_dir = up$save_dir,
        exercise_id = ex$id,
        code = code
      ),
      error = function(e) paste0("Save failed:\n", conditionMessage(e))
    )

    output$result <- renderText(paste0("Saved to:\n", saved))
  })
}

shinyApp(ui, server)
