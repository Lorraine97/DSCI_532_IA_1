library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(streamgraph)
library(lubridate)
library(plotly)


temp_path = "https://raw.githubusercontent.com/UBC-MDS/climadash/main/data/processed/temperature_data.csv"
temp_df <- read.csv(temp_path) |> 
  mutate(LOCAL_DATE = as.Date(LOCAL_DATE))

prec_path = "https://raw.githubusercontent.com/UBC-MDS/climadash/main/data/processed/percipitation_data.csv"
prec_df <- read.csv(prec_path) |> 
  mutate(LOCAL_DATE = as.Date(LOCAL_DATE))

total_df <- merge(temp_df, prec_df, all=TRUE)

cities <- unique(total_df$CITY)
temp_df$year <- year(temp_df$LOCAL_DATE)
total_df$year <- year(total_df$LOCAL_DATE)
total_df$year_month <- format(total_df$LOCAL_DATE, "%Y-%m")
year_range <- unique(total_df$year)

# UI
ui <- fluidPage(
  titlePanel('Climate Metrics in Canada'),
  sidebarLayout(
    sidebarPanel(
      uiOutput("cities"),
      hr(),
      uiOutput("comparisonCity"),
      hr(),
      uiOutput("yearInput"),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Stream Summary", streamgraphOutput("streamPlot")), 
        tabPanel("Comparison", plotlyOutput("comparisonPlot"))
      )
    )
  )
)

# SERVER
server <- shinyServer(function(input, output) {
  thematic::thematic_shiny()
  
  output$yearInput <- renderUI({
    sliderInput("yearInput", 
                "Year Range:",
                min = min(year_range), 
                max = max(year_range),
                value = c(2000, 2019), 
                step = 5,
                sep = "")
  })
  
  output$cities <- renderUI({
    selectInput("cities", 
                "Select Cities", 
                cities, 
                multiple = TRUE, 
                selected = c('VANCOUVER', 'TORONTO'))
  })
  
  output$comparisonCity <- renderUI({
    selectInput("comparisonCity", 
                "Select A City to Compare", 
                input$cities, 
                multiple = FALSE, 
                selected = input$cities[1])
  })
  
  # ======Get Data for Reactivity====== #
  filtered_temp_data <- reactive({
    temp_df |> 
      dplyr::filter(year >= input$yearInput[1] & year <= input$yearInput[2]) |>
      dplyr::filter(CITY %in% input$cities)
  })
  filtered_total_data <- reactive({
    total_df |> 
      dplyr::filter(year >= input$yearInput[1] & year <= input$yearInput[2]) |>
      dplyr::filter(CITY == input$comparisonCity) |> 
      dplyr::group_by(year) |> 
      dplyr::summarise(
        monthly_avg_temp = round(mean(MEAN_TEMP_C), 2),
        monthly_avg_prec = round(mean(TOTAL_PERCIP_mm), 2),
      )
  })
  
  output$comparisonPlot <- renderPlotly(
    ggplot(filtered_total_data(), aes(x=year)) +
    geom_line(aes(y=monthly_avg_temp), size=2, color="#69b3a2") +
    geom_line(aes(y=monthly_avg_prec), size=2, color="grey") +
    geom_point(aes(y=monthly_avg_temp), color="#69b3a2") +
    geom_point(aes(y=monthly_avg_prec), color="grey") +
    labs(y= "Temperature (C) vs. Precipitation (mm)", x = "Year") +
    theme(
      axis.text.x = element_text(angle = 90)
    ) +
    ggtitle(paste("Monthly Temperature vs. Precipitation of", input$comparisonCity))
  )
  
  output$streamPlot <- renderStreamgraph({
    filtered_temp_data() |> 
      streamgraph(key="CITY", value="MEAN_TEMP_C", date="LOCAL_DATE", height="300px", width="1000px") |> 
      sg_axis_x(1, "LOCAL_DATE", "%Y") |> 
      sg_legend(show=TRUE, label="CITY: ")
  })
  
})

shinyApp(ui = ui, server = server)