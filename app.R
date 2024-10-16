# app.R

library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)

# Increase the maximum file upload size if needed
options(shiny.maxRequestSize = 2000 * 1024^2) # 2 GB

# Load the data
# Note: Adjust the path if your data is located elsewhere
covid_data <- fread("data/owid-covid-data.csv", 
                    select = c("location", "date", "new_cases", "new_deaths", 
                               "total_cases", "total_deaths", "people_vaccinated", 
                               "people_fully_vaccinated", "population"),
                    na.strings = c("", "NA"))

# Convert date column to Date type
covid_data[, date := ymd(date)]

# UI
ui <- dashboardPage(
  dashboardHeader(title = "OWID COVID-19 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Vaccinations", tabName = "vaccinations", icon = icon("syringe")),
      menuItem("Data Table", tabName = "datatable", icon = icon("table"))
    ),
    selectInput("country", "Select Country:",
                choices = sort(unique(covid_data$location)),
                selected = "United States",
                multiple = TRUE),
    dateRangeInput("dateRange", "Select Date Range:",
                   start = min(covid_data$date, na.rm = TRUE),
                   end = max(covid_data$date, na.rm = TRUE))
  ),
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("totalCasesBox"),
                valueBoxOutput("totalDeathsBox"),
                valueBoxOutput("totalVaccinatedBox")
              ),
              fluidRow(
                box(title = "Total Cases Over Time", status = "primary", solidHeader = TRUE,
                    plotlyOutput("totalCasesPlot"), width = 12)
              )
      ),
      
      # Trends Tab
      tabItem(tabName = "trends",
              fluidRow(
                box(title = "New Cases and New Deaths", status = "warning", solidHeader = TRUE,
                    plotlyOutput("newCasesDeathsPlot"), width = 12)
              )
      ),
      
      # Vaccinations Tab
      tabItem(tabName = "vaccinations",
              fluidRow(
                box(title = "People Vaccinated", status = "success", solidHeader = TRUE,
                    plotlyOutput("vaccinationsPlot"), width = 12)
              )
      ),
      
      # Data Table Tab
      tabItem(tabName = "datatable",
              fluidRow(
                box(title = "COVID-19 Data Table", status = "info", solidHeader = TRUE,
                    dataTableOutput("dataTable"), width = 12)
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data based on user input
  filtered_data <- reactive({
    req(input$country, input$dateRange)
    covid_data %>%
      filter(location %in% input$country,
             date >= input$dateRange[1],
             date <= input$dateRange[2])
  })
  
  # Overview Boxes
  output$totalCasesBox <- renderValueBox({
    total_cases <- filtered_data() %>%
      group_by(location) %>%
      summarise(total = max(total_cases, na.rm = TRUE)) %>%
      summarise(total = sum(total, na.rm = TRUE)) %>%
      round(0)
    
    valueBox(
      formatC(total_cases, format = "d", big.mark = ","),
      "Total Cases",
      icon = icon("virus"),
      color = "yellow"
    )
  })
  
  output$totalDeathsBox <- renderValueBox({
    total_deaths <- filtered_data() %>%
      group_by(location) %>%
      summarise(total = max(total_deaths, na.rm = TRUE)) %>%
      summarise(total = sum(total, na.rm = TRUE)) %>%
      round(0)
    
    valueBox(
      formatC(total_deaths, format = "d", big.mark = ","),
      "Total Deaths",
      icon = icon("skull-crossbones"),
      color = "red"
    )
  })
  
  output$totalVaccinatedBox <- renderValueBox({
    total_vaccinated <- filtered_data() %>%
      group_by(location) %>%
      summarise(total = max(people_vaccinated, na.rm = TRUE)) %>%
      summarise(total = sum(total, na.rm = TRUE)) %>%
      round(0)
    
    valueBox(
      formatC(total_vaccinated, format = "d", big.mark = ","),
      "People Vaccinated",
      icon = icon("syringe"),
      color = "green"
    )
  })
  
  # Total Cases Plot
  output$totalCasesPlot <- renderPlotly({
    plot_data <- filtered_data() %>%
      group_by(date) %>%
      summarise(total_cases = sum(total_cases, na.rm = TRUE))
    
    p <- ggplot(plot_data, aes(x = date, y = total_cases)) +
      geom_line(color = "blue") +
      labs(title = "Total COVID-19 Cases Over Time",
           x = "Date",
           y = "Total Cases") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # New Cases and New Deaths Plot
  output$newCasesDeathsPlot <- renderPlotly({
    plot_data <- filtered_data() %>%
      group_by(date) %>%
      summarise(new_cases = sum(new_cases, na.rm = TRUE),
                new_deaths = sum(new_deaths, na.rm = TRUE)) %>%
      pivot_longer(cols = c(new_cases, new_deaths),
                   names_to = "Metric",
                   values_to = "Count")
    
    p <- ggplot(plot_data, aes(x = date, y = Count, color = Metric)) +
      geom_line() +
      labs(title = "Daily New Cases and Deaths",
           x = "Date",
           y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Vaccinations Plot
  output$vaccinationsPlot <- renderPlotly({
    plot_data <- filtered_data() %>%
      group_by(date) %>%
      summarise(people_vaccinated = sum(people_vaccinated, na.rm = TRUE),
                people_fully_vaccinated = sum(people_fully_vaccinated, na.rm = TRUE)) %>%
      pivot_longer(cols = c(people_vaccinated, people_fully_vaccinated),
                   names_to = "Metric",
                   values_to = "Count")
    
    p <- ggplot(plot_data, aes(x = date, y = Count, color = Metric)) +
      geom_line() +
      labs(title = "Vaccination Progress Over Time",
           x = "Date",
           y = "Number of People") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Data Table
  output$dataTable <- renderDataTable({
    filtered_data() %>%
      arrange(desc(date))
  }, options = list(pageLength = 10, scrollX = TRUE))
  
}

# Run the application 
shinyApp(ui = ui, server = server)
