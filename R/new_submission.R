library(shiny)
library(miniUI)
library(leaflet)
library(ggplot2)

style_card <- "background-color:#fafafa;padding:15px;margin-bottom: 10px;border:1px solid gray;box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);border-radius: 3px"

author_ui <- function(n) {
  
  tags$div(class = "author-panel", style = style_card,
           splitLayout(
             textInput(paste0("name", n), "First Name", value = ""),
             textInput("surname1", "Last Name", value = ""),
             shinyWidgets::switchInput("speaker1", "Speaker", onLabel = "Yes", offLabel = "No", size = "mini")
           ),
           splitLayout(
             textInput("affil1", "Affiliation", value = "", width = "100%") 
           ),
           splitLayout(
             textInput("email", "Email", value = ""),
             shinyWidgets::switchInput("cor1", "Corresponding author", onLabel = "Yes", offLabel = "No")
           ),
           selectInput("country1", "Country", c("Argentina", "Brazil"), selectize = FALSE)
  )
  
  
}

ui <- miniPage(
  gadgetTitleBar("LatinR2020"),
  miniTabstripPanel(
    miniTabPanel("Authors", icon = icon("users"),
                 miniContentPanel(
                   author_ui(1),
                   author_ui(2),
                   author_ui(3),
                 )
    ),
    miniTabPanel("Presentation", icon = icon("file"),
                 miniContentPanel(
                   tags$div(class = "presentation-panel", style = style_card, 
                            splitLayout(
                              selectInput("type", "Presentation Type", c("Oral", "Poster"), selectize = FALSE),
                              selectInput("lang", "Language", c("Spanish", "Portuguese", "English"))
                            ),
                            textInput("title", "Title", value = "", width = "100%"),
                            textInput("keywords", "Keywords", "", width = "100%")
                            
                   )
                 )
    )
  )
)

server <- function(input, output, session) {
  output$cars <- renderPlot({
    require(ggplot2)
    ggplot(cars, aes(speed, dist)) + geom_point()
  })
  
  output$map <- renderLeaflet({
    force(input$resetMap)
    
    leaflet(quakes, height = "100%") %>% addTiles() %>%
      addMarkers(lng = ~long, lat = ~lat)
  })
  
  output$table <- DT::renderDataTable({
    diamonds
  })
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
}

runGadget(shinyApp(ui, server), viewer = dialogViewer(""))
