#' Opens a user interface to populate yaml fields
#' 
#'
#' @export
latinr_wizard <- function() {
  notification_css <- shiny::HTML("#shiny-notification-panel {
    position:fixed;
    width: 70%;
   }")
  
  
  style_card <- "background-color:#fafafa;padding:15px;margin-bottom: 10px;border:1px solid gray;box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);border-radius: 3px"
  
  
  countries <- as.character(.countries$countries)
  names(countries) <- rownames(.countries)
  
  topics <- seq_along(.topics)
  names(topics) <- .topics
  
  types <- .types
  names(types) <- c("Oral presentation", "Poster", "Round table", "Lightning talk")
  
  author_infoui <- function(id){
    ns <- shiny::NS(id)
    
    shiny::tags$div(class = "author-panel", id = id,
                    style = style_card,
                    shiny::fluidRow(
                      shiny::column(width = 6, 
                                    shiny::textInput(ns("first_name"), "First Name", value = "", width = "100%")
                      ),
                      shiny::column(width = 6,
                                    shiny::textInput(ns("last_name"), "Last Name", value = "", width = "100%")
                      )
                    ),
                    shiny::textInput(ns("affiliation"), "Affiliation", value = "", width = "100%"),
                    shiny::fluidRow(
                      shiny::column(width = 8, 
                                    shiny::textInput(ns("email"), "Email", value = "", width = "100%")
                      ),
                      shiny::column(width = 4,
                                    shinyWidgets::materialSwitch(
                                      inputId = ns("is_corresponding"),
                                      label = "Corresponding author",
                                      status = "primary", 
                                      right = TRUE
                                    )
                      ),
                      shiny::tags$style(type='text/css', paste0(".material-switch { margin-top: 30px;}")),
                      # checkboxInput(ns("is_corresponding"), "Corresponding author", value = TRUE)
                    ),
                    shiny::textInput(ns("url"), "URL", value  = "", width = "100%"),
                    shiny::fluidRow(
                      shiny::column(width = 12,
                                    shiny::selectInput(ns("country"), "Country", countries, selectize = TRUE, width = "100%"))
                    ),
                    shiny::fluidRow(
                      shiny::column(width = 6, align = "center", offset = 3,
                                    shiny::actionButton(ns("remove"), NULL, icon = shiny::icon("minus"))
                      )
                    )
    )
  }
  
  author_info <- function(input, output, session){
    ns <- session$ns
    id <- substr(ns(""), 1, nchar(ns("")) - 1)
    shiny::observeEvent( input$remove , {
      
      shiny::removeUI(paste0("#", id))
      shinyjqui::jqui_sortable('#author', operation = "destroy")
      shinyjqui::jqui_sortable('#author', operation = "enable")
    })
  }
  
  
  ui <- miniUI::miniPage(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "inst/prism.css"),
      shiny::tags$style(notification_css)
    ),
    miniUI::gadgetTitleBar("LatinR2020", 
                           right = miniUI::miniTitleBarButton("save", "Save", primary = TRUE)
    ),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel("Authors", icon = shiny::icon("users"),
                           miniUI::miniContentPanel(
                             # actionButton("inspect", "inspect"),
                             shinyjqui::jqui_sortable(
                               shiny::div(id = "author"
                               )
                             ),
                             shiny::fluidRow(
                               shiny::column(width = 6, align = "center", offset = 3,
                                             shiny::actionButton("add_author", NULL, icon = shiny::icon("plus"))        
                               )
                             )
                           )
      ),
      miniUI::miniTabPanel("Presentation", icon = shiny::icon("file"),
                           miniUI::miniContentPanel(
                             shiny::tags$div(class = "presentation-panel", style = style_card, 
                                             shiny::fluidRow(
                                               shiny::column(width = 6,
                                                             shiny::selectInput("type", "Presentation Type", types, width = "100%"),
                                               ), 
                                               shiny::column(width = 6,
                                                             shiny::uiOutput("presenter")
                                               )
                                             ),
                                             shiny::selectInput("lang", "Language", c("Spanish" = "spanish",
                                                                                      "Portuguese" = "portuguese", 
                                                                                      "English" = "english"), 
                                                                width = "100%"),
                                             shiny::textInput("title", "Title", value = "", width = "100%"),
                                             shiny::textInput("keywords", "Keywords", "", width = "100%"),
                                             shiny::selectInput("topics", "Topics", topics, multiple = TRUE, width = "100%")
                             )
                           )
      ),
      miniUI::miniTabPanel("Header", icon = shiny::icon("stream"),
                           miniUI::miniContentPanel(
                             shiny::verbatimTextOutput("yaml")
                           )
      )
    )
  )
  
  server <- function(input, output, session) {
    author_count <- shiny::reactiveVal(1)
    shiny::insertUI("#author", 
             ui = author_infoui("author_1"),
             where = "beforeEnd")
    shiny::callModule(author_info, "author_1")
    shinyjqui::jqui_sortable('#author', operation = "destroy")
    shinyjqui::jqui_sortable('#author', operation = "enable")
    
    shiny::observeEvent(input$add_author , {
      author_count(author_count() + 1)
      author_id <- paste0("author_", shiny::isolate(author_count()))
      shiny::insertUI("#author", 
               ui = author_infoui(author_id),
               where = "beforeEnd")
      
      
      shiny::callModule(author_info, author_id)
      
      shinyjqui::jqui_sortable('#author', operation = "destroy")
      shinyjqui::jqui_sortable('#author', operation = "enable")
      
      
    })
    
    shiny::observeEvent( input$inspect , {
      browser()
    })
    
    authors <- shiny::reactive({
      authors <- input$author_order$id
      lapply(authors, function(a) {
        list(first_name = input[[paste0(a, "-first_name")]],
             last_name = input[[paste0(a, "-last_name")]],
             email = input[[paste0(a, "-email")]],
             country = input[[paste0(a, "-country")]],
             affiliation = input[[paste0(a, "-affiliation")]],
             utl = input[[paste0(a, "-url")]],
             corresponding = input[[paste0(a, "-is_corresponding")]]
        )
      })
    })
    
    output$presenter <- shiny::renderUI({
      speaker_choices <- vapply(seq_along(authors()), function(a) {
        paste0("(", a, ") ", authors()[[a]]$first_name, " ", authors()[[a]]$last_name)
      }, "a")
      
      shiny::selectInput("presenter", "Presenter", 
                  choices = stats::setNames(seq_along(speaker_choices), speaker_choices),
                  width = "100%")
    })
    
    metadata <- shiny::reactive({
      yaml_list <- list(
        type = input$type,
        language = input$lang,
        title = input$title,
        topics = as.integer(input$topics),
        authors = authors(),
        speaker = as.integer(input$speaker),
        keywords = as.list(strsplit(input$keywords, ",")[[1]]),
        bibliography = "latinr_bibliography.bib",
        `biblio-style` = "apalike-es",
        output = list(`latinr::latinr_article` = list(
          keep_tex = FALSE
        )),
        params = list(
          check_is_error = FALSE,
          submission = FALSE
        )
      )
    })
    
    
    yaml <- shiny::reactive({
      yaml <- yaml::as.yaml(metadata())
      errors <- latinr_checks(shiny::isolate(metadata()), check_is_error = FALSE)
      
      if (length(errors) > 0) {
        
        text <- shiny::p("Found the following errors:", shiny::tags$br(),
                         shiny::tags$ul(
                           shiny::tagList(
                      lapply(errors, function(x) shiny::tags$li(x))
                    )
                  )
        )
        
        shiny::showNotification(text, type = "warning")
      }
      
      paste0("---\n", yaml, "---")
    })
    
    
    file <- NULL
    output$yaml <- shiny::renderText(yaml())
    
    shiny::observeEvent(input$save, {
      file <<- file.choose(new = TRUE)
      
      extension <- tools::file_ext(file)
      if (nchar(extension) == 0) {
        file <- paste0(file, ".Rmd")
      } 
      
      skeleton <- system.file("inst/rmarkdown/templates/latinr_article/skeleton/",
                              "skeleton.Rmd", package = "latinr", mustWork = TRUE)
      bib <- system.file("inst/rmarkdown/templates/latinr_article/skeleton/",
                         "latinr_bibliography.bib", package = "latinr", mustWork = TRUE)
      style <-  system.file("inst/rmarkdown/templates/latinr_article/skeleton/",
                            "RJournal.sty", package = "latinr", mustWork = TRUE)
      
      lines <- paste0(yaml(), "\n\n",
                      "```{r submission-checks, echo=FALSE, warning=TRUE}\n",
                      "# Runs some basic checks in metadata. To disable, set check_is_error to FALSE\n",
                      "latinr::latinr_checks(rmarkdown::metadata, params$check_is_error)\n",
                      "```\n\n",
                      "```{r setup, include=FALSE}\n",
                      "knitr::opts_chunk$set(echo = TRUE)\n",
                      "```\n")
      
      writeLines(lines, file)
      
      file.copy(bib, file.path(dirname(file), basename(bib)))
      file.copy(style, file.path(dirname(file), basename(style)))
      
      shiny::showModal(shiny::modalDialog(shiny::h3("All files generated!"),
                                          shiny::actionButton("open_file", "Open file")))
    })
    
    
    shiny::observeEvent(input$open_file, {
      utils::file.edit(file)
    })
    
  }
  
  shiny::runGadget(shiny::shinyApp(ui, server), viewer = shiny::dialogViewer("", width = 800, height = 800))
}
