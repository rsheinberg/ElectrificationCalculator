#load packages
library(shiny)
library(ggplot2)
library(stringr) 
library(readxl)
library(scales)

#load data
energy_data <- read.csv("data/energy_data.csv")
multipliers <- read.csv("data/multipliers_r.csv")

source("calc_script_app.R")

# Define UI ----
ui <- fluidPage(
  titlePanel("LADWP Electrification Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6,
               selectInput("home_type", h5("Home Type"),
                           #label = "Choose a variable to display",
                           choices = c("Mobile Home",
                                       "Multi-Family 2-4 Units",
                                       "Multi-Family 5+ Units",
                                       "Single Family Attached",
                                       "Single Family Detached"),
                           selected = "Mobile Home")),
        
        column(6,
               selectInput("current_status", h5("Current Home Status"),
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
               selectInput("upgrade_status", h5("Upgrade Home Status"),
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
                            h5("Annual Elec. Use (kWh)"), 
                            value = 3000)),
      ),
      fluidRow(
        column(6,
               numericInput("nat_gas", 
                            h5("Annual Nat. Gas Use (Therms)"), 
                            value = 250)),
        column(6,
               selectInput("zone", h5("LADWP Climate Zone"),
                           choices = c("1",
                                       "2"),
                           selected = "1")),
      ),
      fluidRow(
        column(6,
               selectInput("nat_gas_assist", h5("Nat. Gas Assistance Rate"),
                           #label = "Choose a variable to display",
                           choices = c("None",
                                       "CARE"),
                           selected = "None")), 
        
        column(6,
               selectInput("elec_assist", h5("Electricity Assistance Rate"),
                           #label = "Choose a variable to display",
                           choices = c("None",
                                       "EZ-Save",
                                       "Lifeline"),
                           selected = "None")),  
      ),
      fluidRow(
        column(6,
               numericInput("nat_gas_increase", h5("Nat. Gas Rates Increase (%)"), 
                            value = 0)),
        
        column(6,
               numericInput("elec_increase", h5("Electricity Rates Increase (%)"), 
                            value = 0)),
      ),
      fluidRow(
        column(6, 
               selectInput("current_car", h5("Current Car"),
                           #label = "Choose a variable to display",
                           choices = c("None",
                                       "Gasoline Car/Sedan",
                                       "Gasoline Light Truck/Van/SUV",
                                       "EV Car/Sedan",
                                       "EV Light Truck/Van/SUV"),
                           selected = "Gasoline Car/Sedan")),
        
        column(6, 
               selectInput("upgrade_car", h5("Upgrade Car"),
                           #label = "Choose a variable to display",
                           choices = c("None",
                                       "Gasoline Car/Sedan",
                                       "Gasoline Light Truck/Van/SUV",
                                       "EV Car/Sedan",
                                       "EV Light Truck/Van/SUV"),
                           selected = "EV Car/Sedan")), 
        
      ),
      fluidRow(
      column(6,
         numericInput("vmt", h5("Annual Miles Driven"), 
                     value = 10000)), 
        
      column(6,
             selectInput("ev_charging", h5("EV Charging Behavior"),
                         #label = "Choose a variable to display",
                         choices = c("N/A",
                                     "Residential/Overnight",
                                     "Public or Workplace/Daytime Level 2",
                                     "Public or Workplace/Daytime DCFC"),
                         selected = "Residential/Overnight")),
        
      ),
      
      fluidRow(
        column(6, 
               numericInput("gas_price", h5("Gasoline Price ($)"), 
                            value = 4.50)), 
        column(6,
               numericInput("income", h5("Annual Household Income ($)"), 
                            value = 50000)),
      ),
      fluidRow(
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
      h3("Annual Bill Impacts", align = "left"),
      plotOutput("myplot"),
      plotOutput("myplot2"),
      div(style = "height: 20px;"),  # Adjust the height as needed
      htmlOutput("savings", align = "center"),
      htmlOutput("en_burd_s", align = "center"),
      htmlOutput("en_burd_u", align = "center"),
      div(style = "height: 30px;")
      
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
                                       input$nat_gas_assist, input$elec_increase, input$nat_gas_increase,
                                       input$current_car, input$upgrade_car, input$vmt, input$gas_price, 
                                       input$ev_charging, input$income)
    output$myplot <- renderPlot({ 
      print(results$plot1)
    })
    output$myplot2 <- renderPlot({ 
      print(results$plot2)
    })
    en_burd_s_text <- results$en_burd_s
    en_burd_u_text <- results$en_burd_u
    savings <- results$savings
    # Wrap the text in HTML tags to make it bigger
    formatted_en_burd_s <- paste0("<div style='font-size: 15px;'>", en_burd_s_text, "</div>")
    formatted_en_burd_u <- paste0("<div style='font-size: 15px;'>", en_burd_u_text, "</div>")
    formatted_savings <- paste0("<div style='font-size: 20px;'>", savings, "</div>")
    output$en_burd_s <- renderText({
      HTML(formatted_en_burd_s)
    })
    output$en_burd_u <- renderText({
      HTML(formatted_en_burd_u)
    })
    output$savings <- renderText({
      HTML(formatted_savings)
    })
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
