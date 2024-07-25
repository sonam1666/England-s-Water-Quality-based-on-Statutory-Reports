# Load necessary libraries
library(sf)
library(lubridate)
library(ggplot2)
library(dplyr)
library(leaflet)
library(plotly)
library(shiny)
library(leaflet.extras)

# Load your data
data <- read.csv("EP_Incidents_Nirs2.csv", stringsAsFactors = FALSE)

print(head(data$NOT_DATE))

# Clean the data
data_cleaned <- data %>%
  # Remove unnecessary columns (REGION_PF and AREA_PF) and focus on Region and Area
  select(-REGION_PF, -AREA_PF) %>%
  # Filter for incidents related to water pollution
  filter(EIL_WATER != "No Impact") %>%
  # Remove rows where REGION_WM is empty
  filter(REGION_WM != "") %>%
  # Convert NOT_DATE to datetime format using lubridate
  mutate(NOT_DATE = dmy_hms(NOT_DATE)) %>%
  # Create an sf object using EASTING and NORTHING columns
  st_as_sf(coords = c("X_CONF", "Y_CONF"), crs = 27700) %>%
  # Transform coordinates to WGS84 (latitude/longitude)
  st_transform(crs = 4326) %>%
  # Extract latitude and longitude
  mutate(LATITUDE = st_coordinates(.)[, 2],
         LONGITUDE = st_coordinates(.)[, 1]) %>%
  # Selecting only relevant columns for further analysis
  select(NOT_ID, NOT_DATE, REGION_WM, AREA_WM, COUNTY, UNITARY, DISTRICT, NGR_CONF, LATITUDE, LONGITUDE, EP_INC, EIL_WATER)

# Add a year column to the data
data_cleaned$YEAR <- year(data_cleaned$NOT_DATE)

# Summarize the number of incidents per year
incidents_per_year <- data_cleaned %>%
  group_by(YEAR) %>%
  summarize(incident_count = n())

# Example: Correlation between water pollution incidents and incident counts
pollution_incidents <- data_cleaned %>%
  filter(EP_INC == "Yes") %>%
  group_by(YEAR, EIL_WATER) %>%
  summarize(count = n()) %>%
  ungroup()

# Define UI
ui <- fluidPage(
  titlePanel("Water Pollution Incidents Analysis"),
  tabsetPanel(
    tabPanel("Heatmap", leafletOutput("map", height = 600)),
    tabPanel("Incidents Per Year", plotOutput("incidents_plot", height = 600)),
    tabPanel("Correlation Plot", plotlyOutput("correlation_plot", height = 600))
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Create a color palette for the regions
  region_colors <- colorFactor(palette = "Set1", domain = data_cleaned$REGION_WM)
  
  output$map <- renderLeaflet({
    leaflet(data_cleaned) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~LONGITUDE, lat = ~LATITUDE,
        label = ~paste("ID:", NOT_ID, "<br>",
                       "Date:", format(NOT_DATE, "%Y-%m-%d %H:%M:%S"), "<br>",
                       "Region:", REGION_WM, "<br>",
                       "District:", DISTRICT, "<br>",
                       "Water Impact:", EIL_WATER),
        radius = 2,
        color = ~region_colors(REGION_WM),  # Apply the color palette based on region
        fillOpacity = 0.7,
        labelOptions = labelOptions(noHide = TRUE, direction = 'auto')
      ) %>%
      addLegend("bottomright", pal = region_colors, values = ~REGION_WM,
                title = "Regions")
  })
  
  output$incidents_plot <- renderPlot({
    ggplot(incidents_per_year, aes(x = YEAR, y = incident_count)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = "Number of Water Pollution Incidents Per Year", x = "Year", y = "Incident Count")
  })
  
  output$correlation_plot <- renderPlotly({
    ggplotly(
      ggplot(pollution_incidents, aes(x = YEAR, y = count, color = EIL_WATER)) +
        geom_line() +
        geom_point() +
        theme_minimal() +
        labs(title = "Correlation of Water Pollution Incidents Over Years", x = "Year", y = "Incident Count")
    )
  })
}

# Run the application
shinyApp(ui, server)
