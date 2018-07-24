#' Find Files In Project
#'
#' use grep to find and open files
#'
#' @export
fileSearch <- function() {
  library(shiny)
  library(miniUI)

  ui <- miniPage(
    gadgetTitleBar("File Finder"),
    miniContentPanel(
      tags$div('please note that this is a development release and not ready for public use', class = "warning"),
      tags$div("--"),
      textInput("filename", label = "FileName", placeholder = "Enter Filename"),
      uiOutput("files")
    ),
    theme = "style.css"
  )

  server <- function(input, output, session) {

    obs <- list()

    output$files <- renderUI({
      files <- list.files(path = rstudioapi::getActiveProject(), pattern = input$filename, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (input$filename != "") {
        fileWidgets <- lapply(files, function(f) {
          if (is.null(obs[[f]])) {
            obs[[f]] <<- observeEvent(input[[f]], {
              rstudioapi::navigateToFile(f, line = -1L, column = -1L)
              stopApp()
            })
          }
          actionButton(inputId = f, label = f)
        })

        #print(fileWidgets)
        #print(class(fileWidgets))
        tagList(fileWidgets)
      } else {
        "Please Enter A Search"
      }
    })

  }


  viewer <- dialogViewer("Search File", width = 800)
  runGadget(ui, server, viewer = viewer)

}

# fileSearch()
