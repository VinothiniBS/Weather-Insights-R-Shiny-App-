#
# App tittle: Weather Insights
#
# RShiny app to provide Weather statistics based on location, year and season.
#
# App has three tabs:
#
# First tab: Concise summary of the app's purpose, functionality, and 
# its key features.
#
# Second tab: Provides weather data and statistics of selected variable based 
# on location and year.
#
# Third tab: Provides weather data based on the selected season.
#

library(shiny)
library(dplyr)
library(ggplot2)
library(purrr)

# Define UI for application tabs
ui <- fluidPage(
  tags$head(
    # Styling for application
    tags$style(
      HTML('
        body {
          background-image: url("https://images.pexels.com/photos/167699/pexels-photo-167699.jpeg?auto=compress&cs=tinysrgb&w=600");
          background-repeat: no-repeat;
          background-size: cover;
        }
        .sidebar {
          background-color: rgba(0, 0, 0, 0.5) !important;
          color: white;
        }
        .transparent-container {
          background-color: rgba(0, 0, 0, 0.5) !important;
          color: white;
          padding: 5px;
        }
        .copyright {
          position: absolute;
          bottom: 5px;
          right: 5px;
          font-size: 12px;
          color: white;
        }
        .mainpanel { 
          background-color: rgba(0, 0, 0, 0.5) !important;
          width: 850px;
          padding: 2px;
        }
      ')
    )
  ),
  
  tabsetPanel(
    # First tab
    tabPanel(
      "Summary",
      titlePanel("Weather Insights"),
      # Concise summary about application
      div(class = "transparent-container",
          uiOutput("text1"),
          br(),
          uiOutput("text2"),
          br(),
          uiOutput("text3")
      ),
      div(class = "copyright",
          p("Background Image - Copyright information: Image taken 
            from https://images.pexels.com/")
      )
    ),
    
    # Second tab
    tabPanel(
      "Weather Metrics Analyzer",
      # Title
      titlePanel("Weather Metrics Analyzer"),
      # Sidebar with input fields
      sidebarLayout(
        sidebarPanel(
          class = "sidebar",
          selectInput("location",
                      "Choose the location:",
                      list('location' = c("Belmullet", "Dublin Airport", 
                                          "Malin Head"))
          ),
          selectInput("year",
                      "Choose the year:",
                      list('year' = c("2021", "2022"))
          ),
          selectInput("variable",
                      "Choose the variable:",
                      list('variable' = c("rain", "maxtp", "mintp", "soil",
                                          "wdsp", "hg", "sun", "evap"))
          ),
          uiOutput("text4")
        ),
        # Show the output plot and data
        mainPanel(
          plotOutput("variable_plot"),
          verbatimTextOutput("min_max_output")
        )
      )
    ),
    
    # Third tab
    tabPanel(
      "Seasonal weather Analyzer",
      # Title
      titlePanel("Seasonal weather Analyzer"),
      # Sidebar with input fields
      sidebarLayout(
        sidebarPanel(
          class = "sidebar",
          selectInput("location1",
                      "Choose the location:",
                      list('location1' = c("Belmullet", "Dublin Airport", 
                                           "Malin Head"))
          ),
          selectInput("year1",
                      "Choose the year:",
                      list('year1' = c("2021", "2022"))
          ),
          selectInput("season",
                      "Choose the season:",
                      list('season' = c("Autumn", "Winter",
                                          "Spring", "Summer"))
          ),
          uiOutput("text5")
        ),
        # Show the output plot and data
        mainPanel(
          class = "mainpanel",
          uiOutput("boxplots")
        )
      )
    )
  )
)


# Define server logic required
server <- function(input, output) {
  
  # Text outputs
  output$text1 <- renderUI({
    HTML(paste("<p>Welcome to the <strong>Weather Insights App!</strong></p>"))
  })
  
  output$text2 <- renderUI({
    HTML(paste("<p><strong>Purpose:</strong> The Weather Insights App is 
                designed to access the meteorological data collected at 
                three automatic weather stations. It allows us to explore 
                weather statistics based on location, year, and season.</p>"))
  })
  
  output$text3 <- renderUI({
    HTML(paste("<p><strong>Functionality Summary:</strong></p>",
               "<p><strong>Weather parameter details by Year and Location:
                </strong></p>",
               "<ul>",
               "<li>Users can select a specific year (2021 or 2022) and 
                location (Belmullet, Dublin Airport, or Malin Head).</li>",
               "<li>The app provides detailed information about the following 
                variables: rain, maxtp, mintp, soil, wdsp, hg, sun, and evap.
                </li>",
               "</ul>",
               "<p><strong>Weather parameter details by Season, Location, and 
                Year:</strong></p>",
               "<ul>",
               "<li>Users can choose a specific season (Autumn, Winter, Spring, 
                Summer), location, and year.</li>",
               "<li>The app presents comprehensive details about all the 
                variables rain, maxtp, mintp, soil, wdsp, hg, sun, and evap 
                in a single click.</li>",
               "<li>Users can explore the data for the selected parameters and 
                gain insights into the weather patterns across different 
                seasons.</li>",
               "</ul>",
               "<p>The app allows users to analyze weather data based on 
                specific years, locations, and seasons. It provides detailed 
                information about various meteorological variables and enables 
                users to explore and understand weather patterns effectively.
                </p>"
              ))
  })
  
  output$text4 <- renderUI({
    HTML(paste("<p><strong>Variable descriptions:</strong></p>",
               "<ul>",
               "<li>rain: Precipitation Amount (mm)</li>",
               "<li>maxtp: Maximum Air Temperature (C)</li>",
               "<li>mintp: Minimum Air Temperature (C)</li>",
               "<li>soil: Mean 10cm Soil Temperature (C)</li>",
               "<li>wdsp: Mean Wind Speed (knot)</li>",
               "<li>hg: Highest Gust (knot)</li>",
               "<li>sun: Sunshine Duration (hours)</li>",
               "<li>evap: Evaporation (mm)</li>",
               "</ul>"
               ))
  })
  
  output$text5 <- renderUI({
    HTML(paste("<p><strong>Variable descriptions:</strong></p>",
               "<ul>",
               "<li>rain: Precipitation Amount (mm)</li>",
               "<li>maxtp: Maximum Air Temperature (C)</li>",
               "<li>mintp: Minimum Air Temperature (C)</li>",
               "<li>soil: Mean 10cm Soil Temperature (C)</li>",
               "<li>wdsp: Mean Wind Speed (knot)</li>",
               "<li>hg: Highest Gust (knot)</li>",
               "<li>sun: Sunshine Duration (hours)</li>",
               "<li>evap: Evaporation (mm)</li>",
               "</ul>"
               ))
  })
  
  # Read the data sets
  BM <- read.csv("MetData/belmullet.csv", skip = 24)
  DA <- read.csv("MetData/dublin-airport.csv", skip = 25)
  MH <- read.csv("MetData/malin-head.csv", skip = 24)
  
  # Subset BM data set for the year 2021 & 2022 separately
  BM_21 <- BM %>%
    filter(format(as.Date(date, format = "%d-%b-%Y"), "%Y") == 2021) %>%
    select(date, rain, maxtp, mintp, soil, wdsp, hg, sun, evap)
  BM_22 <- BM %>%
    filter(format(as.Date(date, format = "%d-%b-%Y"), "%Y") == 2022) %>%
    select(date, rain, maxtp, mintp, soil, wdsp, hg, sun, evap)
  
  # Subset DA data set for the year 2021 & 2022 separately
  DA_21 <- DA %>%
    filter(format(as.Date(date, format = "%d-%b-%Y"), "%Y") == 2021) %>%
    select(date, rain, maxtp, mintp, soil, wdsp, hg, sun, evap)
  DA_22 <- DA %>%
    filter(format(as.Date(date, format = "%d-%b-%Y"), "%Y") == 2022) %>%
    select(date, rain, maxtp, mintp, soil, wdsp, hg, sun, evap)
  
  # Subset MH data set for the year 2021 & 2022 separately
  MH_21 <- MH %>%
    filter(format(as.Date(date, format = "%d-%b-%Y"), "%Y") == 2021) %>%
    select(date, rain, maxtp, mintp, soil, wdsp, hg, sun, evap)
  MH_22 <- MH %>%
    filter(format(as.Date(date, format = "%d-%b-%Y"), "%Y") == 2022) %>%
    select(date, rain, maxtp, mintp, soil, wdsp, hg, sun, evap)
  
  # First tab content
  # Function to subset data based on location, year, and variable
  subset_data <- reactive({
    if (input$location == "Belmullet") {
      if (input$year == 2021) {
        data <- BM_21
      } else if (input$year == 2022) {
        data <- BM_22
      }
    } else if (input$location == "Dublin Airport") {
      if (input$year == 2021) {
        data <- DA_21
      } else if (input$year == 2022) {
        data <- DA_22
      }
    } else if (input$location == "Malin Head") {
      if (input$year == 2021) {
        data <- MH_21
      } else if (input$year == 2022) {
        data <- MH_22
      }
    }
    subset_data <- data %>%
      select(date, all_of(input$variable))
    return(subset_data)
  })
  
  # Render the plot for Weather Metrics Analyzer
  output$variable_plot <- renderPlot({
    ggplot(subset_data(), aes(x = seq_along(!!sym(input$variable)), 
                              y = !!sym(input$variable), group = 1))+
      geom_line()+
      labs(x = input$year, y = input$variable)+
      ggtitle(paste("Exploring Weather Patterns of", input$location, 
                    "in", input$year, ":", input$variable, "analysis"))+
      theme_minimal()+
      theme(axis.text.x = element_blank(), 
            axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(),
            plot.title = element_text(size = 14, hjust = 0.5, 
                                      vjust = 1, face = "bold", 
                                      margin = margin(b = 20)),
            plot.background = element_rect(fill = c("#F5F5F5", "#FFFFFF"), 
                                           color = NA))+
      geom_vline(xintercept = seq(1, nrow(subset_data()), by = 30.4375), 
                 color = "gray")+
      annotate("text", x = seq(1, nrow(subset_data()), by = 30.4375), y = -Inf,
               label = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                         "Sep","Oct","Nov","Dec"), vjust = -0.5, hjust = -0.5)
  })
  
  # Render the min and max values of variable with corresponding dates
  output$min_max_output <- renderPrint({
    selected_data <- subset_data()
    selected_var <- selected_data[selected_data[[input$variable]] != 0, ]
    min_date <- selected_var$date[which.min(selected_var[[input$variable]])]
    max_date <- selected_var$date[which.max(selected_var[[input$variable]])]
    min_val <- min(selected_var[[input$variable]], na.rm = TRUE)
    max_val <- max(selected_var[[input$variable]], na.rm = TRUE)
    cat("Minimum value of", input$variable, "recorded over the year:", min_val,
        "at", min_date, "\n")
    cat("Maximum value of", input$variable, "recorded over the year:", max_val,
        "at", max_date)
  })
  
  # Second tab content
  # Specify the months for each season
  seasons <- list(
    Summer = c("Jun", "Jul", "Aug"),
    Autumn = c("Sep", "Oct", "Nov"),
    Winter = c("Dec", "Jan", "Feb"),
    Spring = c("Mar", "Apr", "May")
  )
  
  # Define the list of variables
  variables <- c("rain", "maxtp", "mintp", "soil", "wdsp", "hg", "sun", "evap")
  
  
  # Function to subset data based on location, year, and season
  subset_data1 <- reactive({
    if (input$location1 == "Belmullet") {
      if (input$year1 == "2021") {
        data <- BM_21
      } else if (input$year1 == "2022") {
        data <- BM_22
      }
    } else if (input$location1 == "Dublin Airport") {
      if (input$year1 == "2021") {
        data <- DA_21
      } else if (input$year1 == "2022") {
        data <- DA_22
      }
    } else if (input$location1 == "Malin Head") {
      if (input$year1 == "2021") {
        data <- MH_21
      } else if (input$year1 == "2022") {
        data <- MH_22
      }
    }
    # Subset the data based on selected season
    months <- seasons[[input$season]]
    subset_data1 <- data %>%
      filter(format(as.Date(date, format = "%d-%b-%Y"), "%b") %in% months) %>%
      select(date, all_of(variables))
    return(subset_data1)
  })
  
  output$boxplots <- renderUI({
    variables <- c("rain", "maxtp", "mintp", "soil", "wdsp", "hg", "sun", 
                   "evap")
    # Create box plots based on season
    plot_outputs <- map(variables, function(variable) {
      plot_name <- paste0(variable, "_plot")
      output[[plot_name]] <- renderPlot({
        ggplot(subset_data1(), aes(x = variable, y = .data[[variable]])) +
          geom_boxplot() +
          ggtitle(paste(variable, "in", input$location1, "-", input$year1, 
                        "-", input$season)) +
          theme_minimal()+
          theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                plot.background = element_rect(fill = c("#F5F5F5", "#FFFFFF"),
                                               color = NA))
      })
      plotOutput(plot_name, height = "3.89cm")
    })
    # Arrange the plots in rows and columns
    n_variables <- length(variables)
    n_rows <- ceiling(n_variables / 3)
    plot_rows <- split(plot_outputs, rep(1:n_rows, each = 3, 
                                         length.out = n_variables))
    fluidRows <- map(plot_rows, function(row) {
      fluidRow(
        map(row, function(plot) {
          column(width = 4, plot)
        })
      )
    })
    fluidRows_with_gap <- lapply(fluidRows, function(row) {
      tagList(row, tags$div(style = "height: 10px;"))
    })
    tagList(fluidRows_with_gap)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
