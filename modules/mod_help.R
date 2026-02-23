# Help module: display help_document.html and provide PDF download
mod_help_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        div(
          style = "position: relative; min-height: calc(100vh - 120px);",
          tags$div(
            style = "position: absolute; right: 0; top: 0; z-index: 10;",
            downloadButton(
              ns("download_pdf"),
              "Download Help (PDF)",
              class = "btn-primary"
            )
          ),
          # iframe is rendered server-side to pick the correct base path
          uiOutput(ns("help_frame"))
        )
      )
    )
  )
}

mod_help_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # render iframe with a base path computed from clientData$url_pathname
    output$help_frame <- renderUI({
      base <- session$clientData$url_pathname
      if (is.null(base) || base == "") {
        base <- "/"
      }
      # ensure base ends with /
      if (!grepl("/$", base)) {
        base <- paste0(base, "/")
      }
      src <- paste0(base, "help_document.html")
      # show a short notification with the computed src so user can copy it
      try(
        {
          removeNotification("help_src_info")
        },
        silent = TRUE
      )
      # If the HTML exists on disk, embed it into an isolated iframe via srcdoc.
      # Use a <base> tag so relative links inside the help doc resolve correctly.
      html_path <- file.path("www", "help_document.html")
      if (file.exists(html_path)) {
        txt <- paste(readLines(html_path, warn = FALSE), collapse = "\n")

        # compute an absolute base href from clientData so relative resources load
        proto <- session$clientData$url_protocol
        host <- session$clientData$url_hostname
        port <- session$clientData$url_port
        if (is.null(proto) || proto == "") {
          proto <- "http:"
        }
        if (is.null(host) || host == "") {
          host <- ""
        }
        port_part <- if (
          !is.null(port) && nzchar(port) && port != "80" && port != "443"
        ) {
          paste0(":", port)
        } else {
          ""
        }

        # base (path) we computed earlier
        base_path <- base
        # ensure base_path is absolute-root-prefixed
        if (!grepl("^/", base_path)) {
          base_path <- paste0("/", base_path)
        }
        base_href <- paste0(proto, "//", host, port_part, base_path)

        # insert <base> into head if present, else prepend a minimal head
        if (grepl("<head", txt, ignore.case = TRUE)) {
          txt <- sub(
            "(?i)(<head[^>]*>)",
            paste0("\\1<base href=\"", base_href, "\">"),
            txt,
            perl = TRUE
          )
        } else {
          txt <- paste0("<head><base href=\"", base_href, "\"></head>", txt)
        }

        # create iframe with srcdoc (isolated document) so styles/scripts don't clash
        tags$iframe(
          srcdoc = HTML(txt),
          sandbox = "allow-same-origin allow-scripts allow-forms",
          style = "width:100%; height:80vh; border: none;"
        )
      } else {
        tags$iframe(src = src, style = "width:100%; height:80vh; border: none;")
      }
    })

    output$download_pdf <- downloadHandler(
      filename = function() {
        qmd_path <- file.path("www", "help_document.qmd")
        if (
          file.exists("www/help_document.pdf") ||
            (file.exists(qmd_path) &&
              requireNamespace("quarto", quietly = TRUE))
        ) {
          "help_document.pdf"
        } else {
          "help_document.html"
        }
      },
      content = function(file) {
        # prefer existing PDF
        if (file.exists("www/help_document.pdf")) {
          file.copy("www/help_document.pdf", file, overwrite = TRUE)
          return()
        }

        # try to render PDF from qmd using quarto (best-effort)
        qmd_path <- file.path("www", "help_document.qmd")
        pdf_out <- file.path(tempdir(), "help_document.pdf")
        if (
          file.exists(qmd_path) && requireNamespace("quarto", quietly = TRUE)
        ) {
          tryCatch(
            {
              quarto::quarto_render(
                input = qmd_path,
                output_format = "pdf",
                output_file = pdf_out
              )
              if (file.exists(pdf_out)) {
                file.copy(pdf_out, file, overwrite = TRUE)
                return()
              }
            },
            error = function(e) {}
          )
        }

        # fallback: copy HTML
        html_src <- file.path("www", "help_document.html")
        if (file.exists(html_src)) {
          file.copy(html_src, file, overwrite = TRUE)
        } else {
          writeLines("Help document not available", con = file)
        }
      }
    )
  })
}
