# app.R - NeoCharter: Voyage Planner
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)

# small helper to safely coerce numeric inputs (treat NULL/NA/"" as default)
safe_num <- function(x, default = 0) {
  if (is.null(x)) return(default)
  if (is.na(x)) return(default)
  if (is.character(x) && !nzchar(x)) return(default)
  as.numeric(x)
}

# --------------------------
# Default constants
# --------------------------
DEFAULTS <- list(
  vessel_name = "MV Princess Marina",
  DWT = 80000,
  draft_m = 12,
  grain_capacity_m3 = 78000,
  bale_capacity_m3 = 76000,
  TPC = 64,
  ship_constants_t = 300,
  daily_opex = 7000,
  freight_default = 28,
  BROB_default = 5000,
  adverse_days = 2,
  safety_margin = 0.15,
  bunkering_fee_except_loading = 10000
)

# consumption
CONSUMPTION <- tibble::tibble(
  speed = c(12,13,14),
  hfo_laden = c(38,40,42),
  hfo_ballast = c(34,36,38)
)
MGO_DAY <- 4

# Load/discharge rates
LOAD_RATES <- tibble::tibble(
  port = c("Saldanha","Newcastle","New Orleans"),
  rate_tpd = c(10000,11000,12000),
  SHEX = c(TRUE, TRUE, TRUE)
)
DISCHARGE_DEFAULT <- tibble::tibble(
  port = c("Rotterdam","Osaka","Cristobal"),
  rate_tpd = c(15000,14000,12000),
  SHINC = c(TRUE, TRUE, TRUE)
)

# Ports and fuel prices
PORTS <- tibble::tibble(
  port = c("Saldanha","Newcastle","Rotterdam","New Orleans"),
  port_cost = c(75000, 95000, 110000, 90000)
)
FUEL_PRICES <- tibble::tibble(
  port = c("Colombo","Saldanha","Las Palmas","Newcastle","Port Said","New Orleans","Cristobal","Rotterdam","Osaka","En route / bunkering hub"),
  HFO = c(450,480,410,450,710,810,715,725,815,440),
  MGO = c(780,790,700,750,425,480,420,400,485,720)
)

# route legs
ROUTE_LEGS <- tibble::tribble(
  ~from, ~to, ~nm,
  "Colombo","Saldanha", 4410,
  "Saldanha","Port Said", 5394,
  "Port Said","Rotterdam", 3274,
  "Saldanha","Rotterdam", 6113,
  "Saldanha","Las Palmas", 4377,
  "Las Palmas","Rotterdam", 1746,
  "Colombo","Newcastle", 5216,
  "Newcastle","Port Said", 14000,
  "Colombo","New Orleans (via Suez)", 9964,
  "Colombo","New Orleans (via Cape)", 11636,
  "Colombo","New Orleans (via Panama)", 13504,
  "New Orleans","Osaka (via Panama)", 9413,
  "New Orleans","Cristobal", 1403,
  "Saldanha","Las Palmas", 4377
)

CANAL_COSTS <- list(suez = 150000, panama = 100000)

# templates (voyage defaults)
VOYAGE_TEMPLATES <- list(
  voyage1 = list(
    name = "Voyage 1 (Coal) - Colombo -> Saldanha -> Las Palmas -> Rotterdam",
    cargo_type = "Coal",
    cargo_qty = 70000,
    stowage = 1.3,
    loading_port = "Saldanha",
    discharge_port = "Rotterdam",
    current_port = "Colombo",
    commission = 0.0375,
    legs = list(c("Colombo","Saldanha"), c("Saldanha","Las Palmas"), c("Las Palmas","Rotterdam"))
  ),
  voyage2 = list(
    name = "Voyage 2 (Iron ore) - Colombo -> Newcastle -> Port Said -> Rotterdam",
    cargo_type = "Iron ore",
    cargo_qty = 65000,
    stowage = 0.4,
    loading_port = "Newcastle",
    discharge_port = "Rotterdam",
    current_port = "Colombo",
    commission = 0.035,
    legs = list(c("Colombo","Newcastle"), c("Newcastle","Port Said"), c("Port Said","Rotterdam"))
  ),
  voyage3 = list(
    name = "Voyage 3 (Grain) - Colombo -> New Orleans -> Cristobal -> Osaka",
    cargo_type = "Grain",
    cargo_qty = 74000,
    stowage = 1.5,
    loading_port = "New Orleans",
    discharge_port = "Osaka",
    current_port = "Colombo",
    commission = 0.03,
    legs = list(c("Colombo","New Orleans (via Panama)"), c("New Orleans","Cristobal"), c("Cristobal","Osaka"))
  )
)

# helper to find nm
find_leg_nm <- function(frm, to) {
  row <- ROUTE_LEGS %>% filter(from == frm & to == to)
  if (nrow(row)>0) return(row$nm[1])
  stripped_to <- gsub("\\(.*\\)","", to) %>% trimws()
  row2 <- ROUTE_LEGS %>% filter(from == frm & grepl(stripped_to, to, ignore.case = TRUE))
  if (nrow(row2)>0) return(row2$nm[1])
  return(NA_real_)
}

ALL_PORTS <- sort(unique(c(PORTS$port, ROUTE_LEGS$from, ROUTE_LEGS$to, FUEL_PRICES$port)))

# --------------------------
# UI
# --------------------------
ui <- navbarPage("NeoCharter: Chartering Simulation",
                 
                 tabPanel("Welcome",
                          fluidRow(
                            column(12,
                                   h2("NeoCharter: A voyage planner interactive app!"),
                                   p(strong("You will play the role of a decision-maker.")),
                                   p("Imagine you are the ship’s operator for its voyages with a real bulk carrier. Your job is to design a safe and profitable trip by choosing the right specifications and voyage strategy, and then comparing alternative decisions to pick the best one!"),
                                   p(strong("Welcome to our vessel, MV Princess Marina!")),
                                   br(),
                                   tags$div(style = "max-width:850px; margin:auto;",
                                            tags$img(src = "image1.png", alt = "Welcome image (place www/image1.png)", style = "width:100%; border: 1px solid #ddd; padding:4px; background:#fff;"),
                                            tags$p(style = "font-size:0.9em; color:#555;", "If you do not see the image, contact the developers to put the image1 into the app's www/folder!")
                                   )
                            )
                          )
                 ),
                 
                 tabPanel("Problem description",
                          fluidRow(
                            column(12,
                                   h4("Problem description"),
                                   p("This is a learning game with a scientific backbone. You will be given data and constraints. You will then create one or more voyage scenarios by selecting from predefined options:"),
                                   tags$ul(
                                     tags$li("the cargo to carry (Coal, Iron Ore, Grain),"),
                                     tags$li("the route (preset legs with known distances),"),
                                     tags$li("the service speed (12, 13 or 14 knots),"),
                                     tags$li("where to bunker (loading port, discharge port, or en-route hub), and how much fuel to load"),
                                     tags$li("whether to transit a canal (Suez / Panama), and a few easy numeric overrides (freight rate, cargo amount, initial bunkers on board).")
                                   ),
                                   p("For each scenario the model computes, in a traceable way:"),
                                   tags$ul(
                                     tags$li("voyage time (days at sea, port days for loading/unloading, allowances for bad weather and safety margins),"),
                                     tags$li("fuel consumption split by HFO (at sea) and MGO (in port / auxiliaries),"),
                                     tags$li("fuel purchases and fuel cost (based on the bunkering choice),"),
                                     tags$li("port, canal and operating costs,"),
                                     tags$li("gross freight revenue, commissions,"),
                                     tags$li(strong("net profit and TCE (Time Charter Equivalent) — the two key measures you will use to compare scenarios."))
                                   ),
                                   h5("Your task as the decision-maker:"),
                                   tags$ol(
                                     tags$li("Build at least one plausible scenario and inspect the detailed results."),
                                     tags$li("Try alternative choices (faster vs slower speed, different bunkering port, different fueling) and compare the Net Profit and TCE."),
                                     tags$li("Explain why one scenario is better — what are the tradeoffs?")
                                   ),
                                   p("Tips: start with the defaults, inspect the results, download some plots to compare, run multiple scenarios and compare the cost breakdowns. Be efficient, and think multiple factors under a strong feasibility framing!")
                            )
                          )
                 ),
                 
                 tabPanel("Background / Data",
                          fluidRow(
                            column(6,
                                   h4("Ship specifications (editable)"),
                                   numericInput("DWT", "DWT (t)", value = DEFAULTS$DWT, min = 10000),
                                   numericInput("draft_m", "Draft (m)", value = DEFAULTS$draft_m),
                                   numericInput("grain_capacity", "Grain capacity (m³)", value = DEFAULTS$grain_capacity_m3),
                                   numericInput("bale_capacity", "Bale capacity (m³)", value = DEFAULTS$bale_capacity_m3),
                                   numericInput("TPC", "TPC (tonnes/cm)", value = DEFAULTS$TPC),
                                   numericInput("constants_t", "Constants (t)", value = DEFAULTS$ship_constants_t),
                                   numericInput("BROB_default", "BROB default (t)", value = DEFAULTS$BROB_default),
                                   numericInput("daily_opex", "Daily vessel OPEX ($/day)", value = DEFAULTS$daily_opex)
                            ),
                            column(6,
                                   h4("Economic & port defaults"),
                                   numericInput("freight_default", "Default freight ($/MT)", value = DEFAULTS$freight_default),
                                   selectInput("port_for_costs", "Show port costs for", choices = PORTS$port, selected = "Saldanha"),
                                   verbatimTextOutput("port_cost_display"),
                                   br(),
                                   h5("Fuel price presets (editable)"),
                                   DTOutput("fuel_prices_dt")
                            )
                          )
                 ),
                 
                 tabPanel("Scenarios",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("n_sc", "Number of scenarios to run", choices = c(1,2,3), selected = 1),
                              br(),
                              tags$div(
                                h5("Voyage defaults:"),
                                tags$b("Voyage 1:"), tags$span(" Quantity: 70,000 t Coal | Terms: 15% moloo | Stowage: 1.3 m³/t | Loading: Saldanha | Discharging: Rotterdam | Current: Colombo | Commission: 3.75%"), tags$br(),
                                tags$b("Voyage 2:"), tags$span(" Quantity: 65,000 t Iron ore | Terms: 10% moloo | Stowage: 0.4 m³/t | Loading: Newcastle | Discharging: Rotterdam | Current: Colombo | Commission: 3.50%"), tags$br(),
                                tags$b("Voyage 3:"), tags$span(" Quantity: 74,000 t Grain (min-max 72k-76k) | Stowage: 1.5 m³/t | Loading: New Orleans | Discharging: Osaka | Current: Colombo | Commission: 3.00%")
                              ),
                              width = 3
                            ),
                            mainPanel(
                              tags$img(src="worldmap1.png", style="width:100%; max-width:1000px;"),
                              uiOutput("scenarioBlocks"),
                              hr(),
                              actionButton("compute_all", "Compute all scenarios", class="btn-primary"),
                              br(),br(),
                              fluidRow(
                                column(6, h4("Summary"), tableOutput("summary_table")),
                                column(6, h4("Compare: Net profit per scenario"), plotOutput("compare_plot"))
                              ),
                              hr(),
                              h4("Scenario details"),
                              uiOutput("details_all")
                            )
                          )
                 ),
                 
                 tabPanel("About",
                          fluidRow(column(12,
                                          h4("About NeoCharter"),
                                          p("This app is an educational voyage planner for an indicative vessel, the MV Princess Marina."),
                                          p("Developed by: Angelos Alamanos & Photis Panayides, under the project 'Maritime data analysis'"),
                                          p(strong("Reference: Alamanos, A. & Panayides, P. (2025). NeoCharter: An educational voyage planner interactive app. Available at: https://github.com/Alamanos11/NeoCharter. DOI:10.13140/RG.2.2.17205.95205"))
                          ))
                 )
)

# --------------------------
# compute_scenario
# --------------------------
compute_scenario <- function(inputs, defaults, consumption, fuel_prices, load_rates_table, discharge_defaults, canal_costs, bunkering_fee_except_loading) {
  DWT <- defaults$DWT
  constants_t <- defaults$ship_constants_t
  
  # cargo constraints
  DWCC <- DWT - inputs$BROB - constants_t
  max_by_volume <- NA_real_
  if (!is.null(inputs$stowage) && !is.na(inputs$stowage) && !is.null(defaults$grain_capacity_m3)) {
    max_by_volume <- floor(defaults$grain_capacity_m3 / inputs$stowage)
  }
  cargo_loaded <- inputs$cargo_qty
  truncated_by_weight <- FALSE
  truncated_by_volume <- FALSE
  if (cargo_loaded > DWCC) {
    cargo_loaded <- DWCC
    truncated_by_weight <- TRUE
  }
  if (!is.na(max_by_volume) && cargo_loaded > max_by_volume) {
    cargo_loaded <- max_by_volume
    truncated_by_volume <- TRUE
  }
  
  # per-leg compute
  leg_list <- inputs$legs
  leg_rows <- list()
  total_nm <- 0
  total_days_sea <- 0
  total_hfo_est <- 0
  
  for (i in seq_along(leg_list)) {
    leg <- leg_list[[i]]
    frm <- leg$from; to <- leg$to; state <- toupper(substr(leg$state,1,1))
    nm <- find_leg_nm(frm,to)
    if (is.na(nm)) nm <- 0
    speed <- ifelse(state=="L", inputs$speed_L, inputs$speed_B)
    nm_per_day <- speed * 24
    days_leg <- ifelse(nm_per_day>0, nm / nm_per_day, 0)
    rowc <- consumption %>% filter(speed==speed)
    if (nrow(rowc)==0) rowc <- consumption %>% filter(speed==consumption$speed[which.min(abs(consumption$speed - speed))])
    hfo_per_day <- ifelse(state=="L", rowc$hfo_laden, rowc$hfo_ballast)
    hfo_leg_est <- hfo_per_day * days_leg
    total_nm <- total_nm + nm
    total_days_sea <- total_days_sea + days_leg
    total_hfo_est <- total_hfo_est + hfo_leg_est
    leg_rows[[i]] <- tibble::tibble(
      leg = paste0(frm," → ",to),
      from = frm,
      to = to,
      nm = nm,
      days_sea = days_leg,
      hfo_t_est = hfo_leg_est,
      state = state
    )
  }
  
  # adverse/weather and margin
  days_with_adverse <- total_days_sea + defaults$adverse_days
  days_total_sea_with_margin <- days_with_adverse * (1 + defaults$safety_margin)
  # scale HFO proportionally
  if (total_days_sea>0) total_hfo <- total_hfo_est * (days_total_sea_with_margin / total_days_sea)
  else total_hfo <- total_hfo_est
  
  # port days
  loading_port <- inputs$loading_port
  discharge_port <- inputs$discharge_port
  load_rate_val <- load_rates_table %>% filter(port==loading_port) %>% pull(rate_tpd)
  if (length(load_rate_val)==0) load_rate_val <- 10000
  is_shex <- nrow(load_rates_table %>% filter(port==loading_port))>0
  days_loading <- cargo_loaded / load_rate_val
  if (is_shex) days_loading <- days_loading * 1.4
  disc_rate_val <- discharge_defaults %>% filter(port==discharge_port) %>% pull(rate_tpd)
  if (length(disc_rate_val)==0) disc_rate_val <- 14000
  days_discharge <- cargo_loaded / disc_rate_val
  port_days_total <- days_loading + days_discharge
  
  voyage_days_total <- days_total_sea_with_margin + port_days_total
  # MGO total
  total_mgo <- MGO_DAY * voyage_days_total
  
  # per-leg dataframe and allocation
  leg_df <- bind_rows(leg_rows)
  if (nrow(leg_df) == 0) {
    leg_df <- tibble::tibble(leg=character(), from=character(), to=character(), nm=numeric(), days_sea=numeric(), hfo_t= numeric(), mgo_t = numeric(), fuel_cost_leg = numeric())
  } else {
    # allocate MGO proportional to leg days
    if (sum(leg_df$days_sea, na.rm = TRUE) > 0) {
      leg_df <- leg_df %>% mutate(mgo_t = (days_sea / sum(days_sea)) * total_mgo)
    } else {
      leg_df <- leg_df %>% mutate(mgo_t = 0)
    }
    # adjust hfo per leg proportionally to scaled total_hfo
    if (sum(leg_df$hfo_t_est, na.rm = TRUE) > 0) {
      leg_df <- leg_df %>% mutate(hfo_t = hfo_t_est * (total_hfo / sum(hfo_t_est)))
    } else {
      leg_df <- leg_df %>% mutate(hfo_t = 0)
    }
  }
  
  # Bunkering allocation and pricing
  bunk1_qty <- safe_num(inputs$bunker_qty1, 0)
  bunk2_qty <- safe_num(inputs$bunker_qty2, 0)
  total_bunk_purchased <- bunk1_qty + bunk2_qty
  remaining_hfo_to_buy <- max(0, total_hfo - total_bunk_purchased)
  
  # get prices (fall back to en-route hub)
  price_b1 <- FUEL_PRICES %>% filter(port == inputs$bunker_choice1) %>% pull(HFO)
  if (length(price_b1)==0) price_b1 <- FUEL_PRICES %>% filter(port=="En route / bunkering hub") %>% pull(HFO)
  price_b2 <- FUEL_PRICES %>% filter(port == inputs$bunker_choice2) %>% pull(HFO)
  if (length(price_b2)==0) price_b2 <- FUEL_PRICES %>% filter(port=="En route / bunkering hub") %>% pull(HFO)
  price_rem <- FUEL_PRICES %>% filter(port=="En route / bunkering hub") %>% pull(HFO)
  
  purchase_vec <- c(bunk1_qty, bunk2_qty, remaining_hfo_to_buy)
  price_vec <- c(price_b1, price_b2, price_rem)
  total_purchased <- sum(purchase_vec)
  if (total_purchased <= 0) {
    avg_hfo_price <- price_rem
  } else {
    avg_hfo_price <- sum(purchase_vec * price_vec) / total_purchased
  }
  
  # MGO prices - allocate similarly
  price_mgo_b1 <- FUEL_PRICES %>% filter(port == inputs$bunker_choice1) %>% pull(MGO)
  if (length(price_mgo_b1)==0) price_mgo_b1 <- FUEL_PRICES %>% filter(port=="En route / bunkering hub") %>% pull(MGO)
  price_mgo_b2 <- FUEL_PRICES %>% filter(port == inputs$bunker_choice2) %>% pull(MGO)
  if (length(price_mgo_b2)==0) price_mgo_b2 <- FUEL_PRICES %>% filter(port=="En route / bunkering hub") %>% pull(MGO)
  price_mgo_rem <- FUEL_PRICES %>% filter(port=="En route / bunkering hub") %>% pull(MGO)
  
  if (total_purchased <= 0) {
    avg_mgo_price <- price_mgo_rem
  } else {
    avg_mgo_price <- sum(purchase_vec * c(price_mgo_b1, price_mgo_b2, price_mgo_rem)) / total_purchased
  }
  
  # compute per-leg fuel cost using average prices
  if (nrow(leg_df) > 0) {
    leg_df <- leg_df %>% mutate(fuel_cost_leg = hfo_t * avg_hfo_price + mgo_t * avg_mgo_price)
  }
  
  # total fuel cost
  fuel_cost_total <- sum(leg_df$fuel_cost_leg, na.rm = TRUE)
  
  # port costs
  port_cost_loading <- PORTS %>% filter(port == loading_port) %>% pull(port_cost)
  if (length(port_cost_loading)==0 || is.na(port_cost_loading)) port_cost_loading <- 0
  port_cost_discharge <- PORTS %>% filter(port == discharge_port) %>% pull(port_cost)
  if (length(port_cost_discharge)==0 || is.na(port_cost_discharge)) port_cost_discharge <- 0
  port_costs_total <- port_cost_loading + port_cost_discharge
  
  # bunkering fee (count per distinct bunkering port not equal to loading)
  bunk_fee <- 0
  if (!is.null(inputs$bunker_choice1) && inputs$bunker_choice1 != loading_port) bunk_fee <- bunk_fee + bunkering_fee_except_loading
  if (!is.null(inputs$bunker_choice2) && inputs$bunker_choice2 != loading_port && inputs$bunker_choice2 != inputs$bunker_choice1) bunk_fee <- bunk_fee + bunkering_fee_except_loading
  
  # canal cost
  canal_cost <- 0
  if (!is.null(inputs$canal_choice) && tolower(inputs$canal_choice) == "suez") canal_cost <- canal_costs$suez
  if (!is.null(inputs$canal_choice) && tolower(inputs$canal_choice) == "panama") canal_cost <- canal_costs$panama
  
  # voyage opex
  voyage_opex <- defaults$daily_opex * voyage_days_total
  
  # revenue & commissions
  gross_freight <- inputs$freight_rate * cargo_loaded
  commissions <- gross_freight * inputs$commission
  
  voyage_expenses <- fuel_cost_total + port_costs_total + bunk_fee + canal_cost + voyage_opex
  net_profit <- gross_freight - voyage_expenses - commissions
  TCE <- ifelse(voyage_days_total>0, net_profit / voyage_days_total, NA_real_)
  
  # Produce assumption messages
  hfo_purchased_total <- total_bunk_purchased
  hfo_deficit <- round(remaining_hfo_to_buy, 2)
  hfo_deficit_cost <- round(remaining_hfo_to_buy * price_rem, 2)
  if (hfo_deficit <= 0) {
    hfo_purchase_message <- paste0("Total purchased HFO (", hfo_purchased_total, " t) covered the consumption (", round(total_hfo,2), " t).")
  } else {
    hfo_purchase_message <- paste0("Total purchased HFO (", hfo_purchased_total, " t) did NOT cover consumption (", round(total_hfo,2), " t). ",
                                   hfo_deficit, " t were bought at 'En route / bunkering hub' price (approx $", formatC(hfo_deficit_cost, big.mark=",", format="f", digits=2), ").")
  }
  mgo_assumption_message <- paste0(
    "Assumption: MGO is purchased proportionally to voyage days and priced using the same allocation approach as HFO purchases (simplified accounting)."
  )
  
  tibble::tibble(
    scenario_name = inputs$scenario_name,
    template = inputs$template,
    cargo_requested = inputs$cargo_qty,
    cargo_loaded = cargo_loaded,
    truncated_by_weight = truncated_by_weight,
    truncated_by_volume = truncated_by_volume,
    DWCC = DWCC,
    max_by_volume = max_by_volume,
    distance_nm = total_nm,
    days_at_sea = total_days_sea,
    days_with_adverse = days_with_adverse,
    days_total_sea_with_margin = days_total_sea_with_margin,
    days_loading = days_loading,
    days_discharge = days_discharge,
    port_days = port_days_total,
    voyage_days_total = voyage_days_total,
    hfo_consumed_t = total_hfo,
    mgo_consumed_t = total_mgo,
    hfo_price_avg = avg_hfo_price,
    mgo_price_avg = avg_mgo_price,
    fuel_cost = fuel_cost_total,
    port_costs = port_costs_total,
    bunk_fee = bunk_fee,
    canal_cost = canal_cost,
    voyage_opex = voyage_opex,
    gross_freight = gross_freight,
    commissions = commissions,
    voyage_expenses = voyage_expenses,
    net_profit = net_profit,
    TCE = TCE,
    # extra bookkeeping fields & messages
    hfo_purchased_total = hfo_purchased_total,
    hfo_deficit = hfo_deficit,
    hfo_deficit_cost = hfo_deficit_cost,
    hfo_purchase_message = hfo_purchase_message,
    mgo_assumption_message = mgo_assumption_message,
    leg_table = list(leg_df)
  )
}

# --------------------------
# Server
# --------------------------
server <- function(input, output, session) {
  # reactive defaults
  defaults <- reactive({
    DEFAULTS$DWT <- input$DWT
    DEFAULTS$draft_m <- input$draft_m
    DEFAULTS$grain_capacity_m3 <- input$grain_capacity
    DEFAULTS$bale_capacity_m3 <- input$bale_capacity
    DEFAULTS$TPC <- input$TPC
    DEFAULTS$ship_constants_t <- input$constants_t
    DEFAULTS$BROB_default <- input$BROB_default
    DEFAULTS$daily_opex <- input$daily_opex
    DEFAULTS$freight_default <- input$freight_default
    DEFAULTS
  })
  
  # fuel prices editable
  output$fuel_prices_dt <- renderDT({
    datatable(FUEL_PRICES, editable=TRUE, rownames=FALSE, options=list(dom='t'))
  })
  proxy_fuel <- dataTableProxy("fuel_prices_dt")
  observeEvent(input$fuel_prices_dt_cell_edit, {
    info <- input$fuel_prices_dt_cell_edit
    i <- info$row; j <- info$col; v <- info$value
    FUEL_PRICES[i, j+0] <<- type.convert(v)
    replaceData(proxy_fuel, FUEL_PRICES, resetPaging = FALSE, rownames = FALSE)
  })
  
  output$port_cost_display <- renderText({
    p <- input$port_for_costs
    val <- PORTS %>% filter(port==p) %>% pull(port_cost)
    if (length(val)==0 || is.na(val)) return(paste0(p, ": (no default cost set)"))
    paste0(p, " port cost default: $", formatC(val, big.mark=","))
  })
  
  # scenario UI blocks
  output$scenarioBlocks <- renderUI({
    n <- as.integer(input$n_sc)
    blocks <- lapply(1:n, function(i) {
      ns <- as.character(i)
      wellPanel(
        h4(paste("Scenario", i)),
        fluidRow(
          column(4,
                 selectInput(paste0("template_",ns), "Template",
                             choices = c("Voyage 1 (Coal)"="voyage1",
                                         "Voyage 2 (Iron ore)"="voyage2",
                                         "Voyage 3 (Grain)"="voyage3",
                                         "Custom"="custom"),
                             selected="voyage1"),
                 numericInput(paste0("freight_",ns), "Freight rate ($/MT)", value = input$freight_default),
                 numericInput(paste0("BROB_",ns), "BROB (t)", value = defaults()$BROB_default)
          ),
          column(4,
                 numericInput(paste0("cargo_",ns), "Cargo quantity (MT)", value=70000, min=1000, step=1000),
                 numericInput(paste0("stowage_",ns), "Stowage factor (m³/MT)", value=1.3, step=0.1)
          ),
          column(4,
                 selectInput(paste0("bunker1_",ns), "Bunkering choice 1", choices = ALL_PORTS, selected = "En route / bunkering hub"),
                 numericInput(paste0("bunker1_qty_",ns), "Fuel quantity 1 (HFO, t)", value = NA, min = 0, step = 1),
                 selectInput(paste0("bunker2_",ns), "Bunkering choice 2", choices = ALL_PORTS, selected = "En route / bunkering hub"),
                 numericInput(paste0("bunker2_qty_",ns), "Fuel quantity 2 (HFO, t)", value = NA, min = 0, step = 1),
                 tags$small(style="color:#444; display:block; margin-top:6px;",
                            "Note: MGO is assumed to be purchased proportionally to voyage days and priced using the same allocation approach as HFO purchases (simplified accounting).")
          )
        ),
        fluidRow(
          column(6,
                 selectInput(paste0("canal_",ns), "Canal transit", choices = c("none","Suez","Panama"), selected="none")
          ),
          column(6,
                 fluidRow(column(6, selectInput(paste0("speed_L_",ns),"Laden speed (kn)", choices=c(12,13,14), selected=13)),
                          column(6, selectInput(paste0("speed_B_",ns),"Ballast speed (kn)", choices=c(12,13,14), selected=12)))
          )
        ),
        conditionalPanel(sprintf("input.template_%s == 'custom'", ns),
                         helpText("Custom: pick up to 3 legs (from -> to)"),
                         fluidRow(
                           column(4, selectInput(paste0("leg1_from_",ns), "Leg1 from", choices=ALL_PORTS, selected=ALL_PORTS[1])),
                           column(4, selectInput(paste0("leg1_to_",ns), "Leg1 to", choices=ALL_PORTS, selected=ALL_PORTS[2])),
                           column(4, selectInput(paste0("leg1_state_",ns), "Leg1 state", choices=c("Laden"="L","Ballast"="B"), selected="L"))
                         ),
                         fluidRow(
                           column(4, selectInput(paste0("leg2_from_",ns), "Leg2 from", choices=ALL_PORTS, selected=ALL_PORTS[2])),
                           column(4, selectInput(paste0("leg2_to_",ns), "Leg2 to", choices=ALL_PORTS, selected=ALL_PORTS[3])),
                           column(4, selectInput(paste0("leg2_state_",ns), "Leg2 state", choices=c("L","B"), selected="L"))
                         ),
                         fluidRow(
                           column(4, selectInput(paste0("leg3_from_",ns), "Leg3 from", choices=ALL_PORTS, selected=ALL_PORTS[3])),
                           column(4, selectInput(paste0("leg3_to_",ns), "Leg3 to", choices=ALL_PORTS, selected=ALL_PORTS[4])),
                           column(4, selectInput(paste0("leg3_state_",ns), "Leg3 state", choices=c("L","B"), selected="L"))
                         )
        ),
        hr(),
        p("Tip: choose a template first. Use Custom to craft your own.")
      )
    })
    do.call(tagList, blocks)
  })
  
  # populate template defaults when selected
  observe({
    n <- as.integer(input$n_sc)
    for (i in seq_len(n)) {
      ns <- as.character(i)
      tpl <- input[[paste0("template_",ns)]]
      if (!is.null(tpl) && tpl %in% c("voyage1","voyage2","voyage3")) {
        vt <- VOYAGE_TEMPLATES[[tpl]]
        updateNumericInput(session, paste0("cargo_",ns), value = vt$cargo_qty)
        updateNumericInput(session, paste0("stowage_",ns), value = vt$stowage)
        updateSelectInput(session, paste0("speed_L_",ns), selected = 13)
        updateSelectInput(session, paste0("speed_B_",ns), selected = 12)
      }
    }
  })
  
  # compute all scenarios
  results_all <- eventReactive(input$compute_all, {
    n <- as.integer(input$n_sc)
    out <- list()
    for (i in seq_len(n)) {
      ns <- as.character(i)
      tpl <- input[[paste0("template_",ns)]]
      if (is.null(tpl)) tpl <- "voyage1"
      if (tpl %in% c("voyage1","voyage2","voyage3")) {
        vt <- VOYAGE_TEMPLATES[[tpl]]
        legs <- lapply(vt$legs, function(x) list(from=x[1], to=x[2], state="L"))
        inputs <- list(
          scenario_name = vt$name,
          template = tpl,
          cargo_qty = safe_num(input[[paste0("cargo_",ns)]], vt$cargo_qty),
          stowage = vt$stowage,
          loading_port = vt$loading_port,
          discharge_port = vt$discharge_port,
          freight_rate = safe_num(input[[paste0("freight_",ns)]], DEFAULTS$freight_default),
          commission = vt$commission,
          BROB = safe_num(input[[paste0("BROB_",ns)]], DEFAULTS$BROB_default),
          speed_L = as.numeric(input[[paste0("speed_L_",ns)]]),
          speed_B = as.numeric(input[[paste0("speed_B_",ns)]]),
          bunker_choice1 = input[[paste0("bunker1_",ns)]],
          bunker_qty1 = safe_num(input[[paste0("bunker1_qty_",ns)]], 0),
          bunker_choice2 = input[[paste0("bunker2_",ns)]],
          bunker_qty2 = safe_num(input[[paste0("bunker2_qty_",ns)]], 0),
          canal_choice = input[[paste0("canal_",ns)]],
          legs = legs
        )
        out[[i]] <- compute_scenario(inputs, defaults(), CONSUMPTION, FUEL_PRICES,
                                     LOAD_RATES, DISCHARGE_DEFAULT, CANAL_COSTS,
                                     DEFAULTS$bunkering_fee_except_loading)
      } else {
        # custom
        legs <- list()
        for (k in 1:3) {
          fromk <- input[[paste0("leg1_from_",ns)]] # note: minor copy fix not to break
          # better to read each leg normally:
          fromk <- input[[paste0("leg",k,"_from_",ns)]]
          tok <- input[[paste0("leg",k,"_to_",ns)]]
          statek <- input[[paste0("leg",k,"_state_",ns)]]
          if (!is.null(fromk) && !is.null(tok) && nzchar(fromk) && nzchar(tok)) {
            legs[[length(legs)+1]] <- list(from=fromk, to=tok, state=statek)
          }
        }
        inputs <- list(
          scenario_name = paste0("Custom #", i),
          template = "custom",
          cargo_qty = safe_num(input[[paste0("cargo_",ns)]], 70000),
          stowage = safe_num(input[[paste0("stowage_",ns)]], 1.3),
          loading_port = ifelse(length(legs)>0, legs[[1]]$from, NA_character_),
          discharge_port = ifelse(length(legs)>0, legs[[length(legs)]]$to, NA_character_),
          freight_rate = safe_num(input[[paste0("freight_",ns)]], DEFAULTS$freight_default),
          commission = 0.03,
          BROB = safe_num(input[[paste0("BROB_",ns)]], DEFAULTS$BROB_default),
          speed_L = as.numeric(input[[paste0("speed_L_",ns)]]),
          speed_B = as.numeric(input[[paste0("speed_B_",ns)]]),
          bunker_choice1 = input[[paste0("bunker1_",ns)]],
          bunker_qty1 = safe_num(input[[paste0("bunker1_qty_",ns)]], 0),
          bunker_choice2 = input[[paste0("bunker2_",ns)]],
          bunker_qty2 = safe_num(input[[paste0("bunker2_qty_",ns)]], 0),
          canal_choice = input[[paste0("canal_",ns)]],
          legs = legs
        )
        out[[i]] <- compute_scenario(inputs, defaults(), CONSUMPTION, FUEL_PRICES,
                                     LOAD_RATES, DISCHARGE_DEFAULT, CANAL_COSTS,
                                     DEFAULTS$bunkering_fee_except_loading)
      }
    }
    bind_rows(out)
  })
  
  # summary table
  output$summary_table <- renderTable({
    df <- results_all()
    if (is.null(df) || nrow(df)==0) return(NULL)
    df %>% select(scenario_name, cargo_loaded, distance_nm, voyage_days_total, net_profit, TCE) %>%
      mutate(across(c(distance_nm, voyage_days_total, net_profit, TCE), ~round(.x,2)))
  })
  
  # compare plot: one bar per scenario showing Net profit, with TCE label
  output$compare_plot <- renderPlot({
    df <- results_all()
    if (is.null(df) || nrow(df)==0) return(NULL)
    df_plot <- df %>% mutate(
      scenario_label = paste0("S", row_number(), ": ", substr(scenario_name,1,30))
    )
    ggplot(df_plot, aes(x = scenario_label, y = net_profit)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = paste0("TCE=", round(TCE,1))), vjust = -0.5, size = 3.5) +
      labs(x = "", y = "Net profit ($)", title = "Net profit per scenario (TCE labeled above)") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  # Scenario details: per-leg tables + stacked fuel cost bar + messages
  output$details_all <- renderUI({
    df <- results_all()
    if (is.null(df) || nrow(df)==0) return(NULL)
    panels <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i,]
      leg_df <- row$leg_table[[1]]
      sid <- paste0("sc", i)
      wellPanel(
        h4(paste0("Scenario ", i, ": ", row$scenario_name)),
        h5("Per-leg metrics"),
        tableOutput(paste0("legtable_",sid)),
        br(),
        h5("Totals"),
        tableOutput(paste0("totable_",sid)),
        br(),
        verbatimTextOutput(paste0("hfo_msg_", sid)),
        verbatimTextOutput(paste0("mgo_msg_", sid)),
        br(),
        h5("Cost breakdown (Fuel stacked by leg)"),
        plotOutput(paste0("costplot_",sid)),
        downloadButton(paste0("dl_cost_",sid), "Download cost plot")
      )
    })
    # create renderers for each
    for (i in seq_len(nrow(df))) {
      local({
        idx <- i
        row <- df[idx,]
        sid <- paste0("sc", idx)
        leg_df <- row$leg_table[[1]]
        # leg table output
        output[[paste0("legtable_", sid)]] <- renderTable({
          if (is.null(leg_df) || nrow(leg_df)==0) return(NULL)
          leg_df %>%
            transmute(
              Leg = leg,
              Distance_nm = round(nm,2),
              Days_at_sea = round(days_sea,3),
              HFO_t = round(hfo_t,3),
              MGO_t = round(mgo_t,3),
              Fuel_cost_USD = round(fuel_cost_leg,2)
            )
        }, digits = 3)
        # totals table
        output[[paste0("totable_", sid)]] <- renderTable({
          tib <- tibble::tibble(
            Metric = c("Distance (nm)","Voyage days (total)","HFO consumed (t)","MGO consumed (t)","Fuel cost ($)"),
            Value = c(round(row$distance_nm,2), round(row$voyage_days_total,2), round(row$hfo_consumed_t,2), round(row$mgo_consumed_t,2), round(row$fuel_cost,2))
          )
          tib
        }, digits = 2)
        # HFO coverage message
        output[[paste0("hfo_msg_", sid)]] <- renderText({
          paste0(row$hfo_purchase_message)
        })
        # MGO assumption message
        output[[paste0("mgo_msg_", sid)]] <- renderText({
          paste0(row$mgo_assumption_message)
        })
        # cost plot: Fuel stacked by leg + other bars
        output[[paste0("costplot_", sid)]] <- renderPlot({
          lf <- leg_df %>% mutate(leg_label = leg) %>% select(leg_label, fuel_cost_leg)
          fuel_stack <- lf %>% transmute(item = "Fuel", subgroup = leg_label, cost = fuel_cost_leg)
          others <- tibble::tibble(
            item = c("Port","Bunker fee","Canal","OPEX"),
            subgroup = c("Port","Bunker fee","Canal","OPEX"),
            cost = c(row$port_costs, row$bunk_fee, row$canal_cost, row$voyage_opex)
          )
          plot_df <- bind_rows(fuel_stack, others)
          plot_df$item <- factor(plot_df$item, levels = c("Fuel","Port","Bunker fee","Canal","OPEX"))
          ggplot(plot_df, aes(x = item, y = cost, fill = subgroup)) +
            geom_col(show.legend = TRUE) +
            labs(y = "$", title = paste("Cost breakdown - Scenario", idx), fill = "") +
            theme_minimal(base_size = 13)
        })
        # download handler
        output[[paste0("dl_cost_", sid)]] <- downloadHandler(
          filename = function() paste0("cost_breakdown_scenario", idx, ".png"),
          content = function(file) {
            leg_df2 <- row$leg_table[[1]]
            lf <- leg_df2 %>% mutate(leg_label = leg) %>% select(leg_label, fuel_cost_leg)
            fuel_stack <- lf %>% transmute(item = "Fuel", subgroup = leg_label, cost = fuel_cost_leg)
            others <- tibble::tibble(
              item = c("Port","Bunker fee","Canal","OPEX"),
              subgroup = c("Port","Bunker fee","Canal","OPEX"),
              cost = c(row$port_costs, row$bunk_fee, row$canal_cost, row$voyage_opex)
            )
            plot_df <- bind_rows(fuel_stack, others)
            plot_df$item <- factor(plot_df$item, levels = c("Fuel","Port","Bunker fee","Canal","OPEX"))
            g <- ggplot(plot_df, aes(x = item, y = cost, fill = subgroup)) + geom_col() +
              labs(y="$", title=paste("Cost breakdown - Scenario", idx), fill="") + theme_minimal(base_size=13)
            ggsave(file, plot = g, width = 9, height = 6, dpi = 150)
          }
        )
      })
    }
    do.call(tagList, panels)
  })
}

shinyApp(ui, server)
