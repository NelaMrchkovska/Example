library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readxl)


ui <- fluidPage(
  fluidRow(
    column(width = 12,
           h1(HTML("US Congregations & Sociopolitical Discourse")),   
           
           p(HTML("How do religious communities across the US discuss modern sociopolitical issues?")),
           p(HTML("Using novel text analysis tools, the content of over 700,000 online religious sermons across 9,000 US congregations were annotated for discussion on racial justice, gun violence, reproductive rights, and climate change. The sermons were also annotated for the ideological position the speaker takes on these four issues.")),
           p(HTML("The map shows locations of congregations that discuss these four sociopolitical issues.")),
           p(HTML("Make your selection of a topic, location (by state), year, and the political ideology of the congregations' surrounding communities (liberal- or conservative-leaning based on vote share data).")),
           p(HTML("Discover how, on average, congregations address controversial sociopolitical issues from the pew, and explore how location and their immediate communities might shape their narrative. Hover over a location on the map for the individual ideology score of a congregation in your area.")), 
           p(HTML("<p style='line-height:0.2'></br></p>")),
           p(HTML("Graphic and text by <a href='http://nelamrchkovska.com'>Nela Mrchkovska</a>. First publication: 15 June 2025. Data drawn from an original dataset built by author.")),
           p(HTML("<p style='line-height:0.2'></br></p>"))
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Topic", "Select Topic",
                  choices = c("All" = "", sort(unique(df$category))), selected = ""),
      selectInput("State", "Select State",
                  choices = c("All" = "", sort(unique(df$state))), selected = ""),
      selectInput("Community", "Select Ideology of Surrounding Community",
                  choices = c("All" = "", sort(unique(df$majority_rep))), selected = ""),
      selectInput("Year", "Select Year",
                  choices = c("All" = "", sort(unique(df$year))), selected = ""),
      actionButton("reset", "Reset Filters"),
      br(), br(),
      tags$p("Average Ideology Score (0–100):",
             style = "font-size: 25px; color: black; font-weight: bold;"), 
      tags$p("Scores closer to 0 indicate right-leaning ideology, and scores closer to 100 indicate left-leaning ideology",
             style = "font-size: 90%; color: gray; margin-top: -9px;"),
      tags$style("#mean_value {font-size: 26px; font-weight: bold;}"),
      textOutput("mean_value"),
    ),
    mainPanel(
      leafletOutput("map", height = 700)
    )
  )
)

server <- function(input, output, session) {
  
  filtered_points <- reactive({
    df %>%
      filter(
        (input$Community == "" | majority_rep == input$Community),
        (input$State == "" | state == input$State),
        (input$Year == "" | year == input$Year),
        (input$Topic == "" | category == input$Topic),
        !is.na(lat) & !is.na(lon) & lat >= -90 & lat <= 90 & lon >= -180 & lon <= 180
      )
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "Community", selected = "")
    updateSelectInput(session, "State", selected = "")
    updateSelectInput(session, "Year", selected = "")
    updateSelectInput(session, "Category", selected = "")
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = states, fill = FALSE, color = "#444444", weight = 1)
  })
  
  output$mean_value <- renderText({
    data <- filtered_points()
    if (nrow(data) == 0) {
      "No data for this selection."
    } else {
      avg <- mean(data$position, na.rm = TRUE)
      paste0("", round(avg, 3))
    }
  })
  
  observe({
    proxy_data <- filtered_points()
    
    categories <- sort(unique(df$category))  # You can also use proxy_data$category
    n_colors <- length(categories)
    
    # Use a ColorBrewer palette (e.g., Set3 can support up to 12 categories)
    palette_name <- "Dark2"
    if (n_colors > 12) {
      stop("Too many categories for RColorBrewer::Set3")
    }
    
    pal <- colorFactor(brewer.pal(n_colors, palette_name), domain = categories)
    
    leafletProxy("map") %>%
      clearGroup("Filtered Points") %>%
      addCircleMarkers(
        data = proxy_data,
        lng = ~lon,
        lat = ~lat,
        radius = 3,
        fillColor = ~pal(category),
        color = ~pal(category),
        fillOpacity = 0.2,
        label = lapply(proxy_data$position, function(val) paste("Value:", val)),
        popup = lapply(seq_len(nrow(proxy_data)), function(i) {
          paste0(
            "<strong>ID:</strong> ", proxy_data$UniqueID[i], "<br>",
            "<strong>Topic Score:</strong> ", proxy_data$position[i]
          )
        }),
        group = "Filtered Points"
      )
  })
}


shinyApp(ui, server)



