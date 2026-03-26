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

read_optional_text <- function(path) {
  if (is.null(path) || length(path) == 0 || !nzchar(path)) return(NULL)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

save_exercise_code <- function(save_dir, exercise_id, code) {
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

  existing <- list.files(
    save_dir,
    pattern = paste0("^", exercise_id, "_submission[0-9]+\\.R$"),
    full.names = FALSE
  )

  next_n <- 1L
  if (length(existing) > 0) {
    nums <- suppressWarnings(
      as.integer(sub(paste0("^", exercise_id, "_submission([0-9]+)\\.R$"), "\\1", existing))
    )
    nums <- nums[!is.na(nums)]
    if (length(nums) > 0) next_n <- max(nums) + 1L
  }

  out <- file.path(save_dir, paste0(exercise_id, "_submission", next_n, ".R"))
  writeLines(code, out, useBytes = TRUE)
  normalizePath(out, winslash = "/", mustWork = FALSE)
}

normalize_key <- function(x) {
  tolower(gsub("[^0-9a-z]+", "", x %||% ""))
}

find_shell_doc_path <- function(ex, docs_dir) {
  if (is.null(docs_dir) || !dir.exists(docs_dir)) return(NULL)

  files <- list.files(docs_dir, full.names = FALSE)
  if (length(files) == 0) return(NULL)

  shell_file <- ex$shell_file %||% ""
  if (nzchar(shell_file) && shell_file %in% files) {
    return(file.path(docs_dir, shell_file))
  }

  shell_table <- ex$shell_table %||% ""
  if (!nzchar(shell_table)) return(NULL)

  key <- normalize_key(shell_table)
  key <- sub("^table", "", key)

  norm_files <- vapply(files, normalize_key, character(1))
  idx <- which(grepl(key, norm_files, fixed = TRUE))

  if (length(idx) > 0) {
    return(file.path(docs_dir, files[idx[1]]))
  }

  NULL
}

safe_basename <- function(path) {
  if (is.null(path) || length(path) == 0 || !nzchar(path)) return("")
  basename(path)
}

default_helper_key <- function(filtered_catalog, ex) {
  if (is.null(filtered_catalog) || nrow(filtered_catalog) == 0) return(NULL)

  recommended <- ex$helper_keys %||% character()
  hit <- recommended[recommended %in% filtered_catalog$key]

  if (length(hit) > 0) hit[1] else filtered_catalog$key[1]
}

render_tbl_text_ui <- function(tbl) {
  txt <- paste(capture.output(print(tbl)), collapse = "\n")
  tags$pre(
    style = paste(
      "white-space: pre-wrap;",
      "font-family: Menlo, Monaco, Consolas, monospace;",
      "font-size: 12px;",
      "line-height: 1.35;"
    ),
    txt
  )
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
      .small {
        font-size:12px;
        color:#6b7280;
      }
      .meta-label {
        font-size:12px;
        color:#6b7280;
        margin-top:8px;
        margin-bottom:2px;
      }
      .meta-value {
        font-size:13px;
        color:#111827;
        word-break:break-word;
      }
      pre {
        white-space: pre-wrap;
        word-break: break-word;
        margin: 0;
      }
      .helper-empty {
        color:#6b7280;
        font-size:13px;
      }
      .scroll-pane {
        max-height: 420px;
        overflow: auto;
        resize: both;
        border: 1px solid #e5e7eb;
        border-radius: 8px;
        padding: 6px;
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
          "Reference code is hidden and used only for grading.",
          tags$br(),
          "Exercise drafts are kept in this session when you switch between exercises.",
          tags$br(),
          "Only Save writes code to GPS/VOB."
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
          "Objects",
          div(
            class = "box",
            uiOutput("object_select_ui"),
            uiOutput("object_meta"),
            tags$hr(),
            div(
              class = "scroll-pane",
              tableOutput("object_preview")
            )
          )
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

  shell_doc_path <- reactive({
    ex <- current_ex()
    up <- user_paths()
    find_shell_doc_path(ex, up$docs_dir)
  })

  last_run <- reactiveVal(NULL)
  active_ex_id <- reactiveVal(NULL)
  drafts <- reactiveValues()

  helper_filtered <- reactive({
    filter_helper_catalog(helper_catalog, input$helper_search %||% "")
  })

  observeEvent(current_ex(), {
    ex <- current_ex()
    new_id <- ex$id
    old_id <- isolate(active_ex_id())

    if (!is.null(old_id) && nzchar(old_id)) {
      drafts[[old_id]] <- get_code_input(input, "editor")
    }

    if (is.null(drafts[[new_id]])) {
      starter_txt <- readLines(ex$starter_file, warn = FALSE)
      drafts[[new_id]] <- paste(starter_txt, collapse = "\n")
    }

    active_ex_id(new_id)
    update_code_input(session, "editor", drafts[[new_id]])

    output$ex_desc <- renderUI({
      div(
        tags$h5(ex$title),
        div(class = "muted", HTML(gsub("\n", "<br/>", ex$description)))
      )
    })

    output$result <- renderText("")
    output$tbl_preview <- renderUI(tags$em("Run to preview output."))
  }, ignoreInit = FALSE)

  observe({
    key <- active_ex_id()
    if (!is.null(key) && nzchar(key)) {
      drafts[[key]] <- get_code_input(input, "editor")
    }
  })

  output$exercise_meta <- renderUI({
    ex <- current_ex()

    up <- tryCatch(
      user_paths(),
      error = function(e) {
        list(
          user_id = "unknown_user",
          ra_root = paste0("Path error: ", conditionMessage(e)),
          save_dir = "",
          docs_dir = ""
        )
      }
    )

    shell_table <- ex$shell_table %||% "Not specified"
    grading_mode <- ex$grading_mode %||% "table_text"
    helper_keys <- ex$helper_keys %||% character()
    doc_path <- shell_doc_path()
    has_shell_doc <- !is.null(doc_path) && file.exists(doc_path)

    tagList(
      div(class = "meta-label", "Current user"),
      div(class = "meta-value", up$user_id),

      div(class = "meta-label", "Training RA root"),
      div(class = "meta-value", up$ra_root),

      div(class = "meta-label", "Save location"),
      div(class = "meta-value", up$save_dir),

      div(class = "meta-label", "Documentation path"),
      div(class = "meta-value", up$docs_dir),

      div(class = "meta-label", "Shell table"),
      div(class = "meta-value", shell_table),

      if (has_shell_doc) tagList(
        div(class = "meta-label", "Shell file"),
        div(class = "meta-value", safe_basename(doc_path)),
        downloadButton("download_shell_doc", "Open shell / documentation")
      ),

      div(class = "meta-label", "Grading mode"),
      div(class = "meta-value", grading_mode),

      if (length(helper_keys) > 0) tagList(
        div(class = "meta-label", "Recommended helpers"),
        div(class = "meta-value", paste(helper_keys, collapse = ", "))
      )
    )
  })

  output$download_shell_doc <- downloadHandler(
    filename = function() {
      p <- shell_doc_path()
      if (is.null(p) || !file.exists(p)) {
        "shell_documentation.docx"
      } else {
        basename(p)
      }
    },
    content = function(file) {
      p <- shell_doc_path()
      validate(
        need(!is.null(p) && file.exists(p), "Shell documentation file not found.")
      )
      file.copy(p, file, overwrite = TRUE)
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )

  output$helper_select_ui <- renderUI({
    filtered <- helper_filtered()

    if (is.null(filtered) || nrow(filtered) == 0) {
      return(tags$div(class = "helper-empty", "No helpers found."))
    }

    ex <- current_ex()
    selected <- input$helper_key

    if (is.null(selected) || !selected %in% filtered$key) {
      selected <- default_helper_key(filtered, ex)
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

    ex <- current_ex()
    selected_key <- input$helper_key

    if (is.null(selected_key) || !selected_key %in% filtered$key) {
      selected_key <- default_helper_key(filtered, ex)
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

  observeEvent(input$run, {
    ex <- current_ex()
    up <- user_paths()
    code <- get_code_input(input, "editor")

    out <- tryCatch(
      run_user_code(
        code = code,
        training_ra_root = up$ra_root,
        prep_code = NULL,
        timeout_sec = ex$timeout_sec
      ),
      error = function(e) {
        list(ok = FALSE, tbl = NULL, env = NULL, msg = paste0("Run failed:\n", conditionMessage(e)))
      }
    )

    if (!isTRUE(out$ok)) {
      output$result <- renderText(out$msg %||% "Unknown execution error.")
      output$tbl_preview <- renderUI(
        tags$pre(out$msg %||% "No table (execution error).")
      )
      return()
    }

    last_run(out)
    output$result <- renderText("Preview generated. Submit to grade.")
    output$tbl_preview <- renderUI(render_tbl_text_ui(out$tbl))
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
        prep_code = NULL,
        timeout_sec = ex$timeout_sec
      ),
      error = function(e) {
        list(ok = FALSE, tbl = NULL, env = NULL, msg = paste0("Run failed:\n", conditionMessage(e)))
      }
    )

    if (!isTRUE(out$ok)) {
      output$result <- renderText(out$msg %||% "Unknown execution error.")
      output$tbl_preview <- renderUI(
        tags$pre(out$msg %||% "No table (execution error).")
      )
      return()
    }

    prep_code_ref <- read_optional_text(ex$prep_file)

    ref_out <- tryCatch(
      run_reference_code(
        reference_file = ex$reference_file,
        training_ra_root = up$ra_root,
        prep_code = prep_code_ref,
        timeout_sec = ex$timeout_sec
      ),
      error = function(e) {
        list(ok = FALSE, tbl = NULL, env = NULL, msg = paste0("Reference failed:\n", conditionMessage(e)))
      }
    )

    if (!isTRUE(ref_out$ok)) {
      output$result <- renderText(ref_out$msg %||% "Unknown reference execution error.")
      output$tbl_preview <- renderUI(
        tags$pre(ref_out$msg %||% "Reference execution failed.")
      )
      return()
    }

    g <- tryCatch(
      {
        if (identical(grading_mode, "table_text")) {
          grade_rtables_text(out$tbl, ref_out$tbl)
        } else if (identical(grading_mode, "table_matrix")) {
          grade_rtables_matrix(out$tbl, ref_out$tbl)
        } else {
          list(pass = FALSE, msg = paste0("Unsupported grading mode: ", grading_mode))
        }
      },
      error = function(e) {
        list(pass = FALSE, msg = paste0("Grading failed:\n", conditionMessage(e)))
      }
    )

    last_run(out)
    output$result <- renderText(g$msg %||% "Unknown grading result.")
    output$tbl_preview <- renderUI(render_tbl_text_ui(out$tbl))
  })

  object_names <- reactive({
    out <- last_run()
    if (is.null(out) || !isTRUE(out$ok) || is.null(out$env)) return(character())

    nms <- ls(out$env, all.names = TRUE)

    keep <- vapply(nms, function(nm) {
      obj <- get(nm, envir = out$env)
      is.data.frame(obj)
    }, logical(1))

    nms[keep]
  })

  output$object_select_ui <- renderUI({
    nms <- object_names()

    if (length(nms) == 0) {
      return(tags$div(class = "muted", "No data.frame objects from last successful run."))
    }

    selectInput("object_name", "Intermediate dataset", choices = nms, selected = nms[1])
  })

  output$object_meta <- renderUI({
    out <- last_run()
    req(!is.null(out), isTRUE(out$ok), !is.null(out$env))
    req(input$object_name)

    obj <- get(input$object_name, envir = out$env)

    tagList(
      div(class = "meta-label", "Class"),
      div(class = "meta-value", paste(class(obj), collapse = ", ")),
      div(class = "meta-label", "Dimensions"),
      div(class = "meta-value", paste(nrow(obj), "rows ×", ncol(obj), "columns"))
    )
  })

  output$object_preview <- renderTable({
    out <- last_run()
    req(!is.null(out), isTRUE(out$ok), !is.null(out$env))
    req(input$object_name)

    obj <- get(input$object_name, envir = out$env)
    utils::head(obj, 50)
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  observeEvent(input$save, {
    ex <- current_ex()
    up <- user_paths()
    code <- get_code_input(input, "editor")

    saved <- tryCatch(
      save_exercise_code(
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
