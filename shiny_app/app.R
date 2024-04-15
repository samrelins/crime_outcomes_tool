library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)

app_data <- read_csv("app_data.csv")

# UI
ui <- fluidPage(
  titlePanel("Police Data"),
  
  sidebarLayout(
    sidebarPanel(
        selectInput("group_by", "Group Data By:",
                    choices = c("Police Force" = "reported_by", "Crime Type" = "crime_type"), 
                    selected = "reported_by"),
        pickerInput("selected_forces", "Select Forces:", 
                    choices = unique(app_data$reported_by), 
                    selected = unique(app_data$reported_by), 
                    multiple = TRUE, options = pickerOptions(actionsBox = TRUE)),

        pickerInput("selected_crime_types", "Select Crime Types:",
                  choices = unique(app_data$crime_type),
                  selected = unique(app_data$crime_type),
                  multiple = TRUE, options = pickerOptions(actionsBox = TRUE)),
        
        dateRangeInput("date_range", "Select Date Range:",
                     start = paste(min(app_data$month), "-01", sep=""),
                     end = paste(max(app_data$month), "-01", sep=""),
                     format = "yyyy-mm-dd"),
        
        pickerInput("selected_outcomes", "Select Outcomes:",
                  choices = unique(app_data$outcome),
                  selected = unique(app_data$outcome),
                  multiple = TRUE, options = pickerOptions(actionsBox = TRUE))
    ),
    
    mainPanel(
      plotOutput("plot", height = "1000px")
    )
  )
)

# Server function
server <- function(input, output) {
  # Filter the data based on selected options
  filtered_data <- reactive({
    req(input$date_range[1], input$date_range[2])
    data <- app_data %>%
      filter(reported_by %in% input$selected_forces,
             crime_type %in% input$selected_crime_types,
             month >= format(input$date_range[1], "%Y-%m"),
             month <= format(input$date_range[2], "%Y-%m"),
             outcome %in% input$selected_outcomes)
    return(data)
  })
  
  # Create the plot based on the selected grouping criterion
  output$plot <- renderPlot({
    aggregated_data <- filtered_data() %>%
        group_by(!!sym(input$group_by), outcome) %>%
        summarise(n = sum(count), .groups = "drop") %>%
        mutate(proportion = n / sum(n))
    
    # Define colors for each outcome
    outcome_colors <- c("Unable to prosecute suspect" = "#7B3294",
                    "Action to be taken by another organisation" = "#C2A5CF",
                    "Suspect charged" = "#5AB4AC",
                    "Investigation complete; no suspect identified" = "#D01C8B",
                    "Offender given a caution" = "#A6DBA0",
                    "Further action not in the public interest" = "#F1B6DA",
                    "Local resolution" = "#008837",
                    "Offender given penalty notice" = "#F7F7F7",
                    "Offender given a drugs possession warning" = "#FDAE61",
                    "Suspect charged as part of another case" = "#FFFFBF")



    
    ggplot(aggregated_data, aes(x = proportion, y = fct_rev(!!sym(input$group_by)), fill = outcome)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_manual(values = outcome_colors) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 12),
            legend.position = "top",
            legend.key.size = unit(1, "cm"),
            legend.text = element_text(size = 10),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      guides(fill = guide_legend(nrow = 3))
  })
}

# Run the app
shinyApp(ui, server)