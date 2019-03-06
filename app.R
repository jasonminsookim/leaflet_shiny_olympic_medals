library(shiny)
library(leaflet)
library(dplyr)
library(shinydashboard)
library(DT)
library(geojsonio)
library(styler)
library(stringr)
library(rsconnect)
library(sp)
library(magrittr)
library(htmltools)

# app_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# Set wd
# setwd(app_dir)

# Load df
df <- read.csv("merged_data.csv")

# Data Wrangle
df$gold_medal <- as.numeric(df$Medal == "Gold")
df$silver_medal <- as.numeric(df$Medal == "Silver")
df$bronze_medal <- as.numeric(df$Medal == "Bronze")
df$a_medal <- 1

# Read country border geojson file
borders <- geojsonio::geojson_read("countries_small.geojson", what = "sp")

ui <- dashboardPage(
  title = "Choropleth Maps: Olympic Medals",
  dashboardHeader(title = "Dashboard Menu"),

  dashboardSidebar(
    sidebarMenu(

      # Add dashboard map option
      menuItem(
        text = "Map",
        icon = icon("globe"),
        tabName = "m_map"
      ),

      # Add sidebar data option
      menuItem(
        text = "Dataset",
        icon = icon("table"),
        tabName = "m_dataset"
      )
    ),
    collapsed = TRUE
  ),

  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),

    tabItems(
      tabItem(
        tabName = "m_map",
        box(

          leafletOutput("map"),

          absolutePanel(
            style = "opacity: .75",
            id = "controls-filters",
            class = "panel panel-default",
            fixed = TRUE,
            draggable = TRUE,
            top = "auto",
            left = "auto",
            right = 40,
            bottom = 15,
            width = "12%",
            height = "auto",

            checkboxGroupInput(
              inputId = "medal_type_f",
              label = "Medal Type:",
              choices = c("Gold", "Silver", "Bronze"),
              selected = c("Gold", "Silver", "Bronze")
            ),

            sliderInput("year_f", "Range of Years:",
              min = 1896, max = 2014,
              value = c(1896, 2014)
            ),
            
            checkboxGroupInput(
              inputId = "season_f",
              label = "Games Season:",
              choices = c("Summer", "Winter"),
              selected = c("Summer", "Winter")
            )
          ),
          width = "100%",
          height = "100%"
        )
      ),

      tabItem(
        tabName = "m_dataset",
        box(
          DT::dataTableOutput("dataset"),
          width = "100%",
          height = "100%"
        )
      )
    )
  )
)


server <- function(input, output) {
  rv <- reactiveValues(f_df = NULL)

  # Filter by year
  observeEvent(input$year_f, {

    # Filter by year slider
    filtered_df <- df[df$Year >= input$year_f[1] & df$Year <= input$year_f[2], ]

    # Store value in rv
    rv$f_df <- filtered_df
  })
  
  

  # Filter by Medal Type
  observeEvent(input$medal_type_f, {
    if (is.null(input$medal_type_f)) {
      no_medal_df <- rv$f_df
      no_medal_df$a_medal <- NA
      rv$f_df <- no_medal_df
    }

    else {
      filtered_df <- rv$f_df
      filtered_df <- filter(df, str_detect(Medal, paste(input$medal_type_f, collapse = "|")))
      rv$f_df <- filtered_df
    }
  }, ignoreNULL = FALSE)
  
  
  # Filter by Medal Type
  observeEvent(input$season_f, {
    if (is.null(input$season_f)) {
      no_medal_df <- rv$f_df
      no_medal_df$a_medal <- NA
      rv$f_df <- no_medal_df
    }
    
    else {
      filtered_df <- rv$f_df
      filtered_df <- filter(df, str_detect(Season, paste(input$season_f, collapse = "|")))
      rv$f_df <- filtered_df
    }
  }, ignoreNULL = FALSE)
  

  # create the map to be rendered in the ui
  output$map <- renderLeaflet({
    filtered_df <- rv$f_df
    if (is.null(filtered_df)) {
      filtered_df <- df
    }

    # Calculate total number of medals per each country
    df_calc <- filtered_df %>%
      group_by(alpha_3) %>%
      summarize(
        total_medal = sum(a_medal),
        gold_medals = sum(gold_medal),
        silver_medals = sum(silver_medal),
        bronze_medals = sum(bronze_medal)
      )

    # Merge data frame with SpatiaPolygonsDataFrame
    merged_spdf <- merge(borders, df_calc, by.x = "iso_a3", by.y = "alpha_3")

    # Calculate number of medals per 1M people
    merged_spdf$medal_per_pop <- merged_spdf$total_medal / merged_spdf$pop_est * 1000000

    # Create palette for choropleth map
    bins <- quantile(merged_spdf$medal_per_pop, seq(0, 1, by = .2),
      na.rm = TRUE
    )
    # Makes palette
    mypal <- colorBin("RdYlBu",
      reverse = TRUE,
      domain = merged_spdf$medal_per_pop,
      bins = bins,
      na.color = "grey"
    )
    legend_title <- "Medals per 1M Persons"


    return(leaflet(borders) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lat = 30, lng = 0, zoom = 3) %>%
      addPolygons(
        data = merged_spdf,
        fillColor = ~ mypal(medal_per_pop),
        fillOpacity = .5,
        popup = paste0(
          merged_spdf$name, " won ", as.character(merged_spdf$total_medal), " total medals! <br> - ",
          as.character(merged_spdf$gold_medals), " gold medals <br> - ",
          as.character(merged_spdf$silver_medals), " silver medals <br> - ",
          as.character(merged_spdf$bronze_medals), " bronze medals"
        )
      ) %>%
      leaflet::addLegend(
        pal = mypal, values = merged_spdf$medal_per_pop, opacity = 0.7, title = "Total Medals per 1M Persons",
        position = "bottomleft"
      ))
  })



  # create data table to be rendered in the ui
  output$dataset <- DT::renderDataTable(df, options = list(scrollX = TRUE, pageLength = 20))
}


# Run the application
shinyApp(ui = ui, server = server)
