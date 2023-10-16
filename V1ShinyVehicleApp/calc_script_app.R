
myelectricity_calculate <- function(home_type, current_status, upgrade_status, elec,
                                    nat_gas,zone,elec_assist,nat_gas_assist, 
                                    elec_increase, nat_gas_increase, current_car,
                                    upgrade_car, vmt, gas_price, ev_charging, income) {
  # ---------------- read in energy and multiplier data -------
  energy_data <- read.csv("data/energy_data.csv")
  multipliers <- read.csv("data/multipliers_r.csv")

    # -------------- change format so R can use ----------------
  home_type <- gsub("[ +\\-]", ".", home_type)
  current_status <- gsub("[ +\\-]", ".", current_status)
  upgrade_status <- gsub("[ +\\-]", ".", upgrade_status)
  
  timestamp <- energy_data$timestamp
  start_elec <- paste0(current_status, ".", home_type, ".elec")
  start_gas <- paste0(current_status, ".", home_type, ".gas")
  upgrade_elec <- paste0(upgrade_status, ".", home_type, ".elec")
  upgrade_gas <- paste0(upgrade_status, ".", home_type, ".gas")
  
  # --------- Access the appropriate columns in the NREL usage dataframe -------------
  
  start_elec_cons <- energy_data[[start_elec]]
  start_gas_cons<- energy_data[[start_gas]]
  upgrade_elec_cons <- energy_data[[upgrade_elec]]
  upgrade_gas_cons<- energy_data[[upgrade_gas]]
  
  # -------------- normalize usage values ----------------------
  
  total <- sum(start_elec_cons)  # Calculate the sum of all values
  scaled_start_elec_cons <- start_elec_cons / total  # Scale each value
  total <- sum(start_gas_cons)  # Calculate the sum of all values
  scaled_start_gas_cons <- start_gas_cons / total  # Scale each value
  
  total <- sum(upgrade_elec_cons)  # Calculate the sum of all values
  scaled_upgrade_elec_cons <- upgrade_elec_cons / total  # Scale each value
  total <- sum(upgrade_gas_cons)  # Calculate the sum of all values
  scaled_upgrade_gas_cons <- upgrade_gas_cons / total  # Scale each value
  
  # -------------- scale usage values up via user inputs ----------------
  
  # using calculated multipliers for 605 start-home-upgrade combinations
  multiplier_key <- paste0(current_status,".",home_type,".",upgrade_status)
  #multiplying by user inputs
  upgrade_elec_mult <- multipliers[[multiplier_key]][1]*as.integer(elec)
  upgrade_gas_mult <- multipliers[[multiplier_key]][2]*as.integer(nat_gas)*29.3 ## therms to kwh 
  #create dataframe to hold our values 
  df <- data.frame(
    timestamp = as.POSIXct(timestamp, format = "%m/%d/%Y %H:%M"),
    scaled_start_elec_cons = scaled_start_elec_cons*as.integer(elec),
    scaled_start_gas_cons = scaled_start_gas_cons*as.integer(nat_gas)*29.3,
    scaled_upgrade_elec_cons = scaled_start_elec_cons*upgrade_elec_mult,
    scaled_upgrade_gas_cons = scaled_start_gas_cons*upgrade_gas_mult
  )
  
  # ------------- sum usage values into months ----------------
  
  # taking vals from each month specifically 
  month_vals <- list(2976,2688,2976,2880,2976,2880,2976,2976,2880,2976,2880,2976)
  integer_list <- lapply(month_vals, as.integer)
  # create dataframe to hold values
  monthly_sums <- data.frame(
    Month = month.name,
    start_elec = rep(0, 12),
    start_gas= rep(0, 12),
    upgrade_elec = rep(0, 12),
    upgrade_gas= rep(0, 12)
  )
  #for loop to populate with summed values
  count <- 1
  for (i in 1:12) {
    month_count  <- count + month_vals[[i]]
    monthly_sums$start_elec[i] <- sum(df$scaled_start_elec_cons[count:month_count-1])
    monthly_sums$start_gas[i] <- sum(df$scaled_start_gas_cons[count:month_count-1])
    monthly_sums$upgrade_elec[i] <- sum(df$scaled_upgrade_elec_cons[count:month_count-1])
    monthly_sums$upgrade_gas[i] <- sum(df$scaled_upgrade_gas_cons[count:month_count-1])
    count <- month_count
  }
  # --------------- introduce transportation usage and cost ------------
  ### pre transition ###
  # Extract values from user inputs
  current_car <- current_car  
  upgrade_car <- upgrade_car
  vmt <- as.numeric(vmt)
  gas_price <- as.numeric(gas_price)
  ev_charging <- ev_charging
  ## if you currently have a van we assume a van ev, otherwise sedan mpge 
  current_ev_mpge <- ifelse((current_car == "Gasoline Light Truck/Van/SUV" | current_car == "EV Light Truck/Van/SUV"), 93, 100) 
  upgrade_ev_mpge <- ifelse((upgrade_car == "Gasoline Light Truck/Van/SUV" | upgrade_car == "EV Light Truck/Van/SUV"), 93, 100) 
  #initialize all the variables for starting 
  gas_consumed <- 0 
  start_gas_cost <- 0 
  start_gas_cost_daily <- 0 
  start_e_trans <- 0 
  start_e_trans_daily <- 0 
  start_e_trans_cost <- 0
  start_e_trans_cost_daily <- 0 
  
  # Calculate start_gas_cost and start external charging cost
  # if current car is none, everything stays zero
  if (current_car == 'Gasoline Car/Sedan') {
    gas_consumed <- vmt / 30 ## assuming 30 mpg for cars
    start_gas_cost <- gas_price * gas_consumed
    start_gas_cost_daily <- start_gas_cost / 365
  } else if (current_car == 'Gasoline Light Truck/Van/SUV') {
    gas_consumed <- vmt / 23 ## assuming 23 mpg for suv, etc
    start_gas_cost <- gas_price * gas_consumed
    start_gas_cost_daily <- start_gas_cost / 365
  } else if (current_car != 'None') { ## if they're starting with an EV...
    ge_consumed <- vmt / current_ev_mpge
    start_e_trans <- ge_consumed * 33.7  # Assuming 1 gallon equivalent ~ 33.7 kWh
    start_e_trans_daily <- start_e_trans / 365
    if (ev_charging == 'Public or Workplace/Daytime Level 2') {
      start_e_trans_cost_daily <- start_e_trans_daily * 0.29  # Assuming $0.29 per kWh
    } else if (ev_charging == 'Public or Workplace/Daytime DCFC') {
      start_e_trans_cost_daily <- start_e_trans_daily * 0.42  # Assuming $0.42 per kWh
    }
  }
  
  ### post transition ###
  
  # Initialize variables
  gas_consumed <- 0 
  upgrade_gas_cost <- 0
  upgrade_gas_cost_daily <- 0 
  upgrade_e_trans <- start_e_trans
  upgrade_e_trans_daily <- start_e_trans_cost_daily
  upgrade_e_trans_cost <- start_e_trans_cost
  upgrade_e_trans_cost_daily <- start_e_trans_cost_daily
  
  # Assess upgrade_gas_cost and e_trans_consumed based on transition
  if (upgrade_car == 'Gasoline Car/Sedan') {
    gas_consumed <- vmt / 30
    upgrade_gas_cost <- gas_price * gas_consumed
    upgrade_gas_cost_daily <- upgrade_gas_cost / 365
  } else if (upgrade_car == 'Gasoline Light Truck/Van/SUV') {
    gas_consumed <- vmt / 23
    upgrade_gas_cost <- gas_price * gas_consumed
    upgrade_gas_cost_daily <- upgrade_gas_cost / 365
  } else if (upgrade_car != 'None') { ## if upgrading to EV
    ge_consumed <- vmt / upgrade_ev_mpge
    upgrade_e_trans <- ge_consumed * 33.7  # Assuming 1 gallon equivalent ~ 33.7 kWh
    upgrade_e_trans_daily <- upgrade_e_trans / 365
    if (ev_charging == 'Public or Workplace/Daytime Level 2') {
      upgrade_e_trans_cost_daily <- upgrade_e_trans_daily * 0.29  # Assuming $0.29 per kWh
      upgrade_e_trans_daily <- 0 
    } else if (ev_charging == 'Public or Workplace/Daytime DCFC') {
      upgrade_e_trans_cost_daily <- upgrade_e_trans_daily * 0.42  # Assuming $0.42 per kWh
      upgrade_e_trans_daily <- 0
    }
  }
  
  # Define the number of days in each month (including the total for the year)
  days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 365)
  # Calculate start_gas_cost for each month
  start_gas_cost <- days_in_month * start_gas_cost_daily 
  # Calculate upgrade_gas_cost for each month
  upgrade_gas_cost <- days_in_month * upgrade_gas_cost_daily
  # Calculate upgrade_res_ev_cost for each month
  start_res_ev_cost <- days_in_month * start_e_trans_cost_daily
  # Calculate upgrade_res_ev_elec for each month
  upgrade_res_ev_elec <- days_in_month * upgrade_e_trans_daily
  # Calculate upgrade_res_ev_cost for each month
  upgrade_res_ev_cost <- days_in_month * upgrade_e_trans_cost_daily
  
  # Add charging cost to electricity load 
  monthly_sums$upgrade_elec <- monthly_sums$upgrade_elec + upgrade_res_ev_elec[1:12]
  #----------------- calculating costs --------------------
  
  monthly_sums['start_elec_cost'] <- 0
  monthly_sums['upgrade_elec_cost'] <- 0
  monthly_sums['start_nat_gas_cost'] <- 0
  monthly_sums['upgrade_nat_gas_cost'] <-0
  
  # ----------------electricity pricing ------------------
  
  ## read in elec prices ##
  file_path <- "data/electric_pricing.xlsx"
  sheet_name <- "resi"
  # Read the Excel file into a data frame
  elec_prices <- read_excel(file_path, sheet = "resi")
  
  ## apply any percentage increase
  elec_prices$`En Charge T1`<- elec_prices$`En Charge T1`*(1+.01*elec_increase)
  elec_prices$`En Charge T2`<- elec_prices$`En Charge T2`*(1+.01*elec_increase)
  elec_prices$`En Charge T3`<- elec_prices$`En Charge T3`*(1+.01*elec_increase)
  
  ## calculate monthly electricity cost ##
  
  zone <- as.integer(zone)
  for (x in 1:12) {
    start_elec <- monthly_sums$start_elec[x]
    upgrade_elec <- monthly_sums$upgrade_elec[x]
    
    if (zone == 1) {
      if (start_elec <= 350) {
        monthly_sums$start_elec_cost[x] <- elec_prices$`Cust Charge T1`[x] + elec_prices$`En Charge T1`[x] * start_elec
      } else if (start_elec <= 1050) {
        monthly_sums$start_elec_cost[x] <- elec_prices$`Cust Charge T2`[x] + elec_prices$`En Charge T1`[x] * 350 + elec_prices$`En Charge T2`[x] * (start_elec - 350)
      } else {
        monthly_sums$start_elec_cost[x] <- elec_prices$`Cust Charge T3`[x] + elec_prices$`En Charge T1`[x] * 350 + elec_prices$`En Charge T2`[x] * 700 + elec_prices$`En Charge T3`[x] * (start_elec - 1050)
      }
      
      if (upgrade_elec <= 350) {
        monthly_sums$upgrade_elec_cost[x] <- elec_prices$`Cust Charge T1`[x] + elec_prices$`En Charge T1`[x] * upgrade_elec
      } else if (upgrade_elec <= 1050) {
        monthly_sums$upgrade_elec_cost[x] <- elec_prices$`Cust Charge T2`[x] + elec_prices$`En Charge T1`[x] * 350 + elec_prices$`En Charge T2`[x] * (upgrade_elec - 350)
      } else {
        monthly_sums$upgrade_elec_cost[x] <- elec_prices$`Cust Charge T3`[x] + elec_prices$`En Charge T1`[x] * 350 + elec_prices$`En Charge T2`[x] * 700 + elec_prices$`En Charge T3`[x] * (upgrade_elec - 1050)
      }
    } else {
      if (start_elec <= 500) {
        monthly_sums$start_elec_cost[x] <- elec_prices$`Cust Charge T1`[x] + elec_prices$`En Charge T1`[x] * start_elec
      } else if (start_elec <= 1500) {
        monthly_sums$start_elec_cost[x] <- elec_prices$`Cust Charge T2`[x] + elec_prices$`En Charge T1`[x] * 500 + elec_prices$`En Charge T2`[x] * (start_elec - 500)
      } else {
        monthly_sums$start_elec_cost[x] <- elec_prices$`Cust Charge T3`[x] + elec_prices$`En Charge T1`[x] * 500 + elec_prices$`En Charge T2`[x] * 1000 + elec_prices$`En Charge T3`[x] * (start_elec - 1500)
      }
      
      if (upgrade_elec <= 500) {
        monthly_sums$upgrade_elec_cost[x] <- elec_prices$`Cust Charge T1`[x] + elec_prices$`En Charge T1`[x] * upgrade_elec
      } else if (upgrade_elec <= 1500) {
        monthly_sums$upgrade_elec_cost[x] <- elec_prices$`Cust Charge T2`[x] + elec_prices$`En Charge T1`[x] * 500 + elec_prices$`En Charge T2`[x] * (upgrade_elec - 500)
      } else {
        monthly_sums$upgrade_elec_cost[x] <- elec_prices$`Cust Charge T3`[x] + elec_prices$`En Charge T1`[x] * 500 + elec_prices$`En Charge T2`[x] * 1000 + elec_prices$`En Charge T3`[x] * (upgrade_elec - 1500)
      }
    }
  }
  
  #need to unlist after using loop to assign values
  monthly_sums$start_elec_cost <- unlist(monthly_sums$start_elec_cost)
  monthly_sums$upgrade_elec_cost <- unlist(monthly_sums$upgrade_elec_cost)
  
  # Accounting for Lifeline or EZ-Save, Utility Users Tax (UUT)
  if (elec_assist == 'Lifeline') {
    monthly_sums$start_elec_cost <- monthly_sums$start_elec_cost - 17.71  # No UUT, Lifeline discount
    monthly_sums$upgrade_elec_cost <- monthly_sums$upgrade_elec_cost - 17.71  # No UUT, Lifeline discount
  } else if (elec_assist == 'EZ-Save') {
    monthly_sums$start_elec_cost <- (monthly_sums$start_elec_cost - 8.17) * 1.1  # Adding UUT, EZ-Save discount
    monthly_sums$upgrade_elec_cost <- (monthly_sums$upgrade_elec_cost - 8.17) * 1.1  # Adding UUT, EZ-Save discount
  } else {
    monthly_sums$start_elec_cost <- monthly_sums$start_elec_cost * 1.1  # Adding UUT, no discount
    monthly_sums$upgrade_elec_cost <- monthly_sums$upgrade_elec_cost * 1.1  # Adding UUT, no discount
  }
  # ---------- adding in transportation costs ----------------
  
  # start is just option for gasoline car, assuming if EV it is charged within home load 
  monthly_sums$start_fuel_cost <- start_gas_cost[1:12] + start_res_ev_cost[1:12]
  # upgrade includes both gasoline (no transition) and possible public charging costs
  monthly_sums$upgrade_fuel_cost <- upgrade_gas_cost[1:12] + upgrade_res_ev_cost[1:12]
  
  # -------------- natural gas pricing --------------------
  
  file_path <- "data/ng_pricing_res.xlsx"
  # Read Dataframe from Excel file
  gas_prices <- read_excel(file_path)
  
  ## apply any percentage increase
  gas_prices$`Baseline Procurement_Transmission`<- gas_prices$`Baseline Procurement_Transmission`*(1+.01*nat_gas_increase)
  gas_prices$`Non-Baseline Procurement_Transmission`<- gas_prices$`Non-Baseline Procurement_Transmission`*(1+.01*elec_increase)
  
  gas_tax <- 1.0892
  
  for (x in 1:12) {
    baseline <- gas_prices$`Baseline Allowance Z1`[x]
    start_nat_gas_usage <- monthly_sums$start_gas[x] / 29.3
    upgrade_nat_gas_usage <- monthly_sums$upgrade_gas[x] / 29.3
    
    if (nat_gas_assist == 'None' && (elec_assist == 'None' || elec_assist == 'EZ-Save')) {
      if (start_nat_gas_usage <= baseline) {
        monthly_sums$start_nat_gas_cost[x] <- gas_prices$`Customer Charge`[x] + gas_tax * (gas_prices$`Baseline Procurement_Transmission`[x] * start_nat_gas_usage + gas_prices$PPPG[x] * start_nat_gas_usage)
      } else {
        monthly_sums$start_nat_gas_cost[x] <- gas_prices$`Customer Charge`[x] + gas_tax * (gas_prices$`Baseline Procurement_Transmission`[x] * baseline + gas_prices$`Non-Baseline Procurement_Transmission`[x] * (start_nat_gas_usage - baseline) + gas_prices$PPPG[x] * start_nat_gas_usage)
      }
      
      if (upgrade_nat_gas_usage <= baseline) {
        monthly_sums$upgrade_nat_gas_cost[x] <- gas_prices$`Customer Charge`[x] + gas_tax * (gas_prices$`Baseline Procurement_Transmission`[x] * upgrade_nat_gas_usage + gas_prices$PPPG[x] * upgrade_nat_gas_usage)
      } else {
        monthly_sums$upgrade_nat_gas_cost[x] <- gas_prices$`Customer Charge`[x] + gas_tax * (gas_prices$`Baseline Procurement_Transmission`[x] * baseline + gas_prices$`Non-Baseline Procurement_Transmission`[x] * (upgrade_nat_gas_usage - baseline) + gas_prices$PPPG[x] * upgrade_nat_gas_usage)
      }
    } else if (nat_gas_assist == 'None' && elec_assist == 'Lifeline') {
      if (start_nat_gas_usage <= baseline) {
        monthly_sums$start_nat_gas_cost[x] <- gas_prices$`Customer Charge`[x] + (gas_prices$`Baseline Procurement_Transmission`[x] * start_nat_gas_usage + gas_prices$PPPG[x] * start_nat_gas_usage)
      } else {
        monthly_sums$start_nat_gas_cost[x] <- gas_prices$`Customer Charge`[x] + (gas_prices$`Baseline Procurement_Transmission`[x] * baseline + gas_prices$`Non-Baseline Procurement_Transmission`[x] * (start_nat_gas_usage - baseline) + gas_prices$PPPG[x] * start_nat_gas_usage)
      }
      
      if (upgrade_nat_gas_usage <= baseline) {
        monthly_sums$upgrade_nat_gas_cost[x] <- gas_prices$`Customer Charge`[x] + (gas_prices$`Baseline Procurement_Transmission`[x] * upgrade_nat_gas_usage + gas_prices$PPPG[x] * upgrade_nat_gas_usage)
      } else {
        monthly_sums$upgrade_nat_gas_cost[x] <- gas_prices$`Customer Charge`[x] + (gas_prices$`Baseline Procurement_Transmission`[x] * baseline + gas_prices$`Non-Baseline Procurement_Transmission`[x] * (upgrade_nat_gas_usage - baseline) + gas_prices$PPPG[x] * upgrade_nat_gas_usage)
      }
    } else if (nat_gas_assist == 'CARE' && (elec_assist == 'None' || elec_assist == 'EZ-Save')) {
      if (start_nat_gas_usage <= baseline) {
        monthly_sums$start_nat_gas_cost[x] <- (gas_prices$`Customer Charge`[x] + gas_tax * (gas_prices$`Baseline Procurement_Transmission`[x] * start_nat_gas_usage + gas_prices$`PPPG - CARE`[x] * start_nat_gas_usage)) * 0.8
      } else {
        monthly_sums$start_nat_gas_cost[x] <- (gas_prices$`Customer Charge`[x] + gas_tax * (gas_prices$`Baseline Procurement_Transmission`[x] * baseline + gas_prices$`Non-Baseline Procurement_Transmission`[x] * (start_nat_gas_usage - baseline) + gas_prices$`PPPG - CARE`[x] * start_nat_gas_usage)) * 0.8
      }
      
      if (upgrade_nat_gas_usage <= baseline) {
        monthly_sums$upgrade_nat_gas_cost[x] <- (gas_prices$`Customer Charge`[x] + gas_tax * (gas_prices$`Baseline Procurement_Transmission`[x] * upgrade_nat_gas_usage + gas_prices$`PPPG - CARE`[x] * upgrade_nat_gas_usage)) * 0.8
      } else {
        monthly_sums$upgrade_nat_gas_cost[x] <- (gas_prices$`Customer Charge`[x] + gas_tax * (gas_prices$`Baseline Procurement_Transmission`[x] * baseline + gas_prices$`Non-Baseline Procurement_Transmission`[x] * (upgrade_nat_gas_usage - baseline) + gas_prices$`PPPG - CARE`[x] * upgrade_nat_gas_usage)) * 0.8
      }
    } else {
      if (start_nat_gas_usage <= baseline) {
        monthly_sums$start_nat_gas_cost[x] <- (gas_prices$`Customer Charge`[x] + (gas_prices$`Baseline Procurement_Transmission`[x] * start_nat_gas_usage + gas_prices$`PPPG - CARE`[x] * start_nat_gas_usage)) * 0.8
      } else {
        monthly_sums$start_nat_gas_cost[x] <- (gas_prices$`Customer Charge`[x] + (gas_prices$`Baseline Procurement_Transmission`[x] * baseline + gas_prices$`Non-Baseline Procurement_Transmission`[x] * (start_nat_gas_usage - baseline) + gas_prices$`PPPG - CARE`[x] * start_nat_gas_usage)) * 0.8
      }
      
      if (upgrade_nat_gas_usage <= baseline) {
        monthly_sums$upgrade_nat_gas_cost[x] <- (gas_prices$`Customer Charge`[x] + (gas_prices$`Baseline Procurement_Transmission`[x] * upgrade_nat_gas_usage + gas_prices$`PPPG - CARE`[x] * upgrade_nat_gas_usage)) * 0.8
      } else {
        monthly_sums$upgrade_nat_gas_cost[x] <- (gas_prices$`Customer Charge`[x] + (gas_prices$`Baseline Procurement_Transmission`[x] * baseline + gas_prices$`Non-Baseline Procurement_Transmission`[x] * (upgrade_nat_gas_usage - baseline) + gas_prices$`PPPG - CARE`[x] * upgrade_nat_gas_usage)) * 0.8
      }
    }
  }
  
  ## zero out  the almost zero usage for whole-home electrification 
  monthly_sums$upgrade_nat_gas_cost <- ifelse(monthly_sums$upgrade_nat_gas_cost <= 8, 0, monthly_sums$upgrade_nat_gas_cost)
  monthly_sums$start_nat_gas_cost <- ifelse(monthly_sums$start_nat_gas_cost <= 8, 0, monthly_sums$start_nat_gas_cost)
  
  
  # ----------- visualization ------------ 
  
  start_sums <- rbind(
    data.frame(Month = monthly_sums$Month, "count" = monthly_sums$start_elec_cost, "type"="Electricity"),
    data.frame(Month = monthly_sums$Month, "count" = monthly_sums$start_nat_gas_cost, "type"="Natural Gas"),
    data.frame(Month = monthly_sums$Month, "count" = monthly_sums$start_fuel_cost, "type" = "Transportation Fuel")
  )
  
  upgrade_sums <- rbind(
    data.frame(Month = monthly_sums$Month, "count" = monthly_sums$upgrade_elec_cost, "type"="Electricity"),
    data.frame(Month = monthly_sums$Month, "count" = monthly_sums$upgrade_nat_gas_cost, "type"="Natural Gas"),
    data.frame(Month = monthly_sums$Month, "count" = monthly_sums$upgrade_fuel_cost, "type" = "Transportation Fuel")
  )
  
  # Create a vector with the desired order of months
  month_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  
  # Convert the "Month" column to a factor with the desired order
  start_sums$Month <- factor(start_sums$Month, levels = month_order)
  upgrade_sums$Month <- factor(upgrade_sums$Month, levels = month_order)
  
  # Combine the start_sums and upgrade_sums dataframes
  combined_sums <- rbind(
    data.frame(monthly_sums$Month, "count" = monthly_sums$start_elec_cost, "type" = "Electricity", "source" = "Start"),
    data.frame(monthly_sums$Month, "count" = monthly_sums$start_nat_gas_cost, "type" = "Natural Gas", "source" = "Start"),
    data.frame(monthly_sums$Month, "count" = monthly_sums$start_fuel_cost, "type" = "Transportation Fuel", "source" = "Start"),
    data.frame(monthly_sums$Month, "count" = monthly_sums$upgrade_elec_cost, "type" = "Electricity", "source" = "Upgrade"),
    data.frame(monthly_sums$Month, "count" = monthly_sums$upgrade_nat_gas_cost, "type" = "Natural Gas", "source" = "Upgrade"),
    data.frame(monthly_sums$Month, "count" = monthly_sums$upgrade_fuel_cost, "type" = "Transportation Fuel", "source" = "Upgrade")
  )
  
  # Create the stacked bar chart
  stacked <- ggplot(combined_sums, aes(x = monthly_sums.Month, y = count, fill = type)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(~ source, ncol = 1) +  # Facet by 'source' to separate Start and Upgrade bars
    labs(
      title = "Electric Costs",
      x = "Month",
      y = "Total Costs ($)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))  # Center the title horizontally
  
  # Create the grouped bar chart
  breakdown <- ggplot(combined_sums, aes(x = monthly_sums.Month, y = count, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ source, ncol = 1) +  # Facet by 'source' to separate Start and Upgrade bars
    labs(
      title = "Monthly Bill Costs",
      x = "Month",
      y = "Cost ($)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))  # Center the title horizontally
  
  # Sum the values for "Start" and "Upgrade"
  total_start_cost <- sum(combined_sums$count[combined_sums$source == "Start"])
  total_upgrade_cost <- sum(combined_sums$count[combined_sums$source == "Upgrade"])
  
  # Calculate the difference
  difference <- total_upgrade_cost - total_start_cost
  
  # Print the results
  ##cat("Total Start Cost:", total_start_cost, "\n")
  ##cat("Total Upgrade Cost:", total_upgrade_cost, "\n")
  ##cat("Difference (Upgrade - Start):", difference, "\n")
  
  
  # Create a data frame for the totals
  totals_df <- data.frame(
    Source = c("Start", "Upgrade"),
    Total_Cost = c(total_start_cost, total_upgrade_cost)
  )
  
  # Create a bar plot
  bar_plot <- ggplot(totals_df, aes(x = Source, y = Total_Cost, fill = Source)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = dollar(Total_Cost)), vjust = -0.5, size = 4) +  # Format as currency
    labs(title = "Annual Start vs. Upgrade Costs", y = "Total Cost ($)") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5)  # Center the title horizontally
    )
  
  energy_burden_start <- paste0("Household starting energy burden: ",sprintf("%.2f%%", total_start_cost/income * 100), ".")
  energy_burden_upgrade <- paste0("Household upgrade energy burden: ",sprintf("%.2f%%", total_upgrade_cost/income * 100),".")
  difference <- paste0("Change in Annual Spending: ", dollar(difference),"!")
    
  result <- list(plot1 = breakdown, plot2 = bar_plot, en_burd_s = energy_burden_start,
                 en_burd_u = energy_burden_upgrade, savings = difference)
  return(result)
  
}
