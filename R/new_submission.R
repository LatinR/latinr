

latinr_addin <- function() {
  
  library(shiny)
  library(miniUI)
  library(ggplot2)
  library(shinyjqui)
  
  
  style_card <- "background-color:#fafafa;padding:15px;margin-bottom: 10px;border:1px solid gray;box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);border-radius: 3px"
  
  remove_nulls <- function(list) {
    list[!vapply(list, is.null, TRUE)]
  }
  
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
             splitLayout(
               textInput(ns("email"), "Email", value = ""),
               checkboxInput(ns("is_corresponding"), "Corresponding author", value = TRUE)
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
  
  prism_cc <- "inst/prism.css"
  
  ui <- miniPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "inst/prism.css")
    ),
    gadgetTitleBar("LatinR2020"),
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
                                column(width = 5,
                                       selectInput("type", "Presentation Type", types, width = "100%"),
                                ), 
                                column(width = 5,
                                       uiOutput("presenter")
                                )
                              ),
                              selectInput("lang", "Language", c("Spanish", "Portuguese", "English"), 
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
    author_count <- reactiveVal(0)
    
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
             correspondin = input[[paste0(a, "-is_corresponding")]]
        )
      })
    })
    
    output$presenter <- renderUI({
      speaker_choices <- vapply(seq_along(authors()), function(a) {
        paste0("(", a, ") ", authors()[[a]]$first_name, " ", authors()[[a]]$last_name)
      }, "a")
      
      selectInput("speaker", "Presenter", choices = setNames(seq_along(speaker_choices), 
                                                             speaker_choices),
                  width = "100%")
    })
    
    yaml <- reactive({
      file <- "inst/rmarkdown/templates/latinr_article/skeleton/skeleton.Rmd"
      metadata <- rmarkdown::yaml_front_matter(file)
      yaml <- yaml::as.yaml(metadata)
      paste0("---\n", yaml, "---")
    })
    
    output$yaml <- renderText(yaml())
    
    
  }
  
  runGadget(shinyApp(ui, server), viewer = dialogViewer("", width = 800, height = 800))
  # shinyApp(ui, server)
  
  
}
