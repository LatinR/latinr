

latinr_addin <- function() {
  library(shiny)
  library(miniUI)
  library(shinyjqui)
  
  notification_css <- HTML("#shiny-notification-panel {
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
    ns <- NS(id)
    
    tags$div(class = "author-panel", id = id,
             style = style_card,
             fluidRow(
               column(width = 6, 
                      textInput(ns("first_name"), "First Name", value = "", width = "100%")
               ),
               column(width = 6,
                      textInput(ns("last_name"), "Last Name", value = "", width = "100%")
               )
             ),
             textInput(ns("affiliation"), "Affiliation", value = "", width = "100%"),
             fluidRow(
               column(width = 8, 
                      textInput(ns("email"), "Email", value = "", width = "100%")
               ),
               column(width = 4,
                      shinyWidgets::materialSwitch(
                        inputId = ns("is_corresponding"),
                        label = "Corresponding author",
                        status = "primary", 
                        right = TRUE
                      )
               ),
               tags$style(type='text/css', paste0(".material-switch { margin-top: 30px;}")),
               # checkboxInput(ns("is_corresponding"), "Corresponding author", value = TRUE)
             ),
             textInput(ns("url"), "URL", value  = "", width = "100%"),
             fluidRow(
               column(width = 12,
                      selectInput(ns("country"), "Country", countries, selectize = TRUE, width = "100%"))
             ),
             fluidRow(
               column(width = 6, align = "center", offset = 3,
                      actionButton(ns("remove"), NULL, icon = icon("minus"))
               )
             )
    )
  }
  
  author_info <- function(input, output, session){
    ns <- session$ns
    id <- substr(ns(""), 1, nchar(ns("")) - 1)
    observeEvent( input$remove , {
      
      removeUI(paste0("#", id))
      jqui_sortable('#author', operation = "destroy")
      jqui_sortable('#author', operation = "enable")
    })
  }
  
  
  ui <- miniPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "inst/prism.css"),
      tags$style(notification_css)
    ),
    gadgetTitleBar("LatinR2020", 
                   right = miniButtonBlock(
                     miniTitleBarButton("save", "Save", primary = TRUE)
                   )
    ),
    miniTabstripPanel(
      miniTabPanel("Authors", icon = icon("users"),
                   miniContentPanel(
                     # actionButton("inspect", "inspect"),
                     jqui_sortable(
                       div(id = "author"
                       )
                     ),
                     fluidRow(
                       column(width = 6, align = "center", offset = 3,
                              actionButton("add_author", NULL, icon = icon("plus"))        
                       )
                     )
                   )
      ),
      miniTabPanel("Presentation", icon = icon("file"),
                   miniContentPanel(
                     tags$div(class = "presentation-panel", style = style_card, 
                              fluidRow(
                                column(width = 6,
                                       selectInput("type", "Presentation Type", types, width = "100%"),
                                ), 
                                column(width = 6,
                                       uiOutput("presenter")
                                )
                              ),
                              selectInput("lang", "Language", c("Spanish" = "spanish",
                                                                "Portuguese" = "porguguese", 
                                                                "English" = "english"), 
                                          width = "100%"),
                              textInput("title", "Title", value = "", width = "100%"),
                              textInput("keywords", "Keywords", "", width = "100%"),
                              selectInput("topics", "Topics", topics, multiple = TRUE, width = "100%")
                     )
                   )
      ),
      miniTabPanel("Header", icon = icon("stream"),
                   miniContentPanel(
                     verbatimTextOutput("yaml")
                   )
      )
    )
  )
  
  server <- function(input, output, session) {
    author_count <- reactiveVal(1)
    insertUI("#author", 
             ui = author_infoui("author_1"),
             where = "beforeEnd")
    callModule(author_info, "author_1")
    jqui_sortable('#author', operation = "destroy")
    jqui_sortable('#author', operation = "enable")
    
    observeEvent(input$add_author , {
      author_count(author_count() + 1)
      author_id <- paste0("author_", isolate(author_count()))
      insertUI("#author", 
               ui = author_infoui(author_id),
               where = "beforeEnd")
      
      
      callModule(author_info, author_id)
      
      jqui_sortable('#author', operation = "destroy")
      jqui_sortable('#author', operation = "enable")
      
      
    })
    
    observeEvent( input$inspect , {
      browser()
    })
    
    authors <- reactive({
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
    
    output$presenter <- renderUI({
      speaker_choices <- vapply(seq_along(authors()), function(a) {
        paste0("(", a, ") ", authors()[[a]]$first_name, " ", authors()[[a]]$last_name)
      }, "a")
      
      selectInput("speaker", "Presenter", 
                  choices = setNames(seq_along(speaker_choices), speaker_choices),
                  width = "100%")
    })
    
    metadata <- reactive({
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
    
    
    errors <- reactive({
      
    })
    
    
    
    
    yaml <- reactive({
      yaml <- yaml::as.yaml(metadata())
      errors <- latinr_checks(isolate(metadata()), check_is_error = FALSE)
      
      if (length(errors) > 0) {
        
        text <- p("Found the following errors:", tags$br(),
                  
                  tags$ul(
                    tagList(
                      lapply(errors, function(x) tags$li(x))
                    )
                  )
        )
        
        showNotification(text, type = "warning")
      }
      
      paste0("---\n", yaml, "---")
    })
    
    
    file <- NULL
    output$yaml <- renderText(yaml())
    
    observeEvent(input$save, {
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
      
      showModal(modalDialog(h3("All files generated!"),
                            actionButton("open_file", "Open file")))
    })
    
    
    observeEvent(input$open_file, {
      file.edit(file)
    })
    
  }
  
  runGadget(shinyApp(ui, server), viewer = dialogViewer("", width = 800, height = 800))
}
