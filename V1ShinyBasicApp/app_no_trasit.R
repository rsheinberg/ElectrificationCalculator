#load pacakges 
library(shiny)
library(ggplot2)
library(stringr) 
library(readxl)
library(scales)

#load data
energy_data <- read.csv("data/energy_data.csv")
multipliers <- read.csv("data/multipliers_r.csv")

#source
source("calc_script_shiny_no_transit.R")

# Define UI ----
ui <- fluidPage(
  titlePanel("Electrification Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6,
           selectInput("home_type", h4("Home Type"),
                       #label = "Choose a variable to display",
                       choices = c("Mobile Home",
                                  "Multi-Family 2-4 Units",
                                  "Multi-Family 5+ Units",
                                  "Single Family Attached",
                                  "Single Family Detached"),
                       selected = "Single Family Detached")),
    
        column(6,
          selectInput("current_status", h4("Current Home Status"),
                       #label = "Choose current level of electrification",
                       choices = c("Baseline",
                                   "Basic Enclosure",
                                   "Enhanced Enclosure",
                                   "Heat Pump Water Heaters",
                                   "Heat Pumps High Efficiency Electric Backup",
                                   "Heat Pumps Min Efficiency Electric Backup",
                                   "Heat Pumps Min Efficiency Existing Heating as Backup",
                                   "Whole Home Electrification High Efficiency",
                                   "Whole Home Electrification High Efficiency Plus Basic Enclosure Package",
                                   "Whole Home Electrification High Efficiency Plus Enhanced Enclosure Package",
                                   "Whole Home Electrification Min Efficiency"),
                       selected = "Baseline")),
      ),
      fluidRow(
        column(6,
           selectInput("upgrade_status", h4("Upgrade Home Status"),
                       #label = "Choose current level of electrification",
                       choices = c("Baseline",
                                   "Basic Enclosure",
                                   "Enhanced Enclosure",
                                   "Heat Pump Water Heaters",
                                   "Heat Pumps High Efficiency Electric Backup",
                                   "Heat Pumps Min Efficiency Electric Backup",
                                   "Heat Pumps Min Efficiency Existing Heating as Backup",
                                   "Whole Home Electrification High Efficiency",
                                   "Whole Home Electrification High Efficiency Plus Basic Enclosure Package",
                                   "Whole Home Electrification High Efficiency Plus Enhanced Enclosure Package",
                                   "Whole Home Electrification Min Efficiency"),
                       selected = "Baseline")),
    
        column(6,
           numericInput("elec", 
                        h4("Annual Elec. Use (kWh)"), 
                        value = 3000)),
      ),
      fluidRow(
        column(6,
           numericInput("nat_gas", 
                        h4("Annual Nat. Gas Use (Therms)"), 
                        value = 250)),
        column(6,
           selectInput("zone", h4("LADWP Climate Zone"),
                       choices = c("1",
                                   "2"),
                       selected = "1")),
      ),
      fluidRow(
        column(6,
            numericInput("income", h4("Annual Household Income ($)"), 
                      value = 50000)),
    
        column(6,
           selectInput("elec_assist", h4("Electricity Assistance Rate"),
                       #label = "Choose a variable to display",
                       choices = c("None",
                                   "EZ-Save",
                                   "Lifeline"),
                       selected = "None")),  
      ),
      fluidRow(
        column(6,
           selectInput("nat_gas_assist", h4("Nat. Gas Assistance Rate"),
                       #label = "Choose a variable to display",
                       choices = c("None",
                                   "CARE"),
                       selected = "None")),  
    
        column(6,
           numericInput("elec_increase", h4("Electricity Rates Increase (%)"), 
                        value = 0)),
      ),
      fluidRow(
        column(6,
           numericInput("nat_gas_increase", h4("Nat. Gas Rates Increase (%)"), 
                        value = 0)),
      tags$style(HTML("
      #executeButton {
        background-color: #4CAF50; /* Green */
        color: white;
        padding: 15px 32px;
        text-align: center;
        text-decoration: none;
        display: inline-block;
        font-size: 16px;
        margin: 4px 2px;
        cursor: pointer;
        border-radius: 10px;
      }
    ")),
    column(6,
      actionButton("executeButton", "Calculate"))
    ),
    ),

mainPanel(
  h3("Annual Bill Impacts", align = "center"),
  plotOutput("myplot"),
  plotOutput("myplot2")
)
)
)
# Define server logic ----
server <- function(input, output) {
  click_counter <- reactiveVal(0)
  
  observeEvent(input$executeButton, {
    click_counter(click_counter() + 1)
    # Code to execute when the "Execute" button is clicked
    # Source helper functions -----
    results <- myelectricity_calculate(input$home_type, input$current_status, input$upgrade_status,
                                       input$elec,input$nat_gas,input$zone, input$elec_assist, 
                                       input$nat_gas_assist, input$elec_increase, input$nat_gas_increase)
    output$myplot <- renderPlot({ 
      print(results$plot1)
    })
    output$myplot2 <- renderPlot({ 
      print(results$plot2)
    })
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
