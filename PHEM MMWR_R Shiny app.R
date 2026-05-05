# =============================================================================
#  APHI PHEM Dashboard — Fixed Table Rendering & Compact Layout
# =============================================================================

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(scales)
library(readr)

# 1. DATA PREPARATION ---------------------------------------------------------
file_path <- "C:/Users/HP Ultra/OneDrive - The Ohio State University/TT_Documnts/PHEM/APHI-PHEM _ Power BI Reports/APHI-PHEM _ Interactive Dashboards Using Power BI/APHI_PHEM PowerBI Visualization/PHEM_PBI_Dashboard/PHEM_MMWR.csv"

# Load data and handle potential numeric conversion issues immediately
df_raw <- read_csv(file_path, na = c("", "NA", "N/A"), show_col_types = FALSE) %>%
  mutate(across(where(is.numeric), ~replace_na(as.numeric(.), 0)))

disease_meta <- list(
  "Malaria"        = list(op = "Malaria_cases_OP", ip = "Malaria_cases_IP", d = "Malaria_deaths_HF"),
  "Meningitis"     = list(op = "Meningitis_OP", ip = "Meningitis_IP", d = "Meningitis_Deaths"),
  "Dysentery"      = list(op = "Dysentery_OP", ip = "Dysentery_IP", d = "Dysentery_Deaths"),
  "Scabies"        = list(op = "Scabies_OP", ip = "Scabies_IP", d = "Scabies_Deaths"),
  "AJS"            = list(op = "AJS_OP", ip = "AJS_IP", d = "AJS_Death"),
  "Measles"        = list(op = "Measles_OP", ip = "Measles_IP", d = "Measles_Death"),
  "Cholera"        = list(op = "Cholera_OP", ip = "Cholera_IP", d = "Cholera_Death"),
  "SAM (U5)"       = list(op = "SAM_U5C_OP", ip = "SAM_U5C_IP", d = "SAM_U5C_Deaths"),
  "Anthrax"        = list(op = "Anthrax_OP", ip = "Anthrax_IP", d = "Anthrax_Death"),
  "Human Rabies"   = list(op = "Human_rabies_OP", ip = "Human_rabies_IP", d = "Human_rabies_Death"),
  "Rabies Exposure"= list(op = "Rabies_exposure_OP", ip = "Rabies_exposure_IP", d = "Rabies_exposure_Death"),
  "MPox"           = list(op = "Mpox virus_OP", ip = "Mpox virus_IP", d = "Mpox virus_Deaths"),
  "Human Influenza" = list(op = "Human_influenza_newsubtype_OP", ip = "Human_influenza_newsubtype_IP", d = "Human_influenza_newsubtype_deaths"),
  "Relapsing Fever"= list(op = "RF_OP", ip = "RF_IP", d = "RF_Death"),
  "AFP"            = list(op = "AFP_OP", ip = "AFP_IP", d = "AFP_Death"),
  "Pertussis"      = list(op = "Pertussis_OP", ip = "Pertussis_IP", d = "Pertussis_Death"),
  "AEFI"           = list(op = "AEFI_OP", ip = "AEFI_IP", d = "AEFI_Death")
)

# Pre-calculate main incident and death columns
for (incident in names(disease_meta)) {
  df_raw[[paste0("Total_", incident)]] <- (df_raw[[disease_meta[[incident]]$op]] + df_raw[[disease_meta[[incident]]$ip]])
  df_raw[[paste0("Deaths_", incident)]] <- df_raw[[disease_meta[[incident]]$d]]
}

all_years <- sort(unique(df_raw$Year_EFY))
all_zones <- sort(unique(df_raw$Zone))

# 2. UI DESIGN ----------------------------------------------------------------
ui <- dashboardPage(
  header = dashboardHeader(title = "APHI PHEM Dashboard", titleWidth = 300),
  sidebar = dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Malaria Lab", tabName = "malaria_lab", icon = icon("microscope")),
      menuItem("Hierarchical Analysis", tabName = "drilldown", icon = icon("sitemap")),
      menuItem("Trends & Rankings", tabName = "trends", icon = icon("chart-line"))
    ),
    hr(),
    pickerInput("sel_incident", "Incident", choices = names(disease_meta), selected = "Malaria"),
    pickerInput("sel_year", "Year (EFY)", choices = all_years, selected = 2018, multiple = TRUE),
    sliderInput("sel_week", "Week Range", min = 1, max = 53, value = c(1, 53)),
    pickerInput("sel_zone", "Zone", choices = all_zones, selected = all_zones, multiple = TRUE, options = list(`live-search` = TRUE)),
    uiOutput("ui_woreda")
  ),
  body = dashboardBody(
    tags$style(HTML("
      .content-wrapper { background-color: #f4f7f6; }
      .shiny-output-error { visibility: hidden; }
      .shiny-output-error:before { visibility: hidden; }
      .dataTables_wrapper { font-size: 11px; }
      .table.dataTable tbody td { padding: 2px 5px !important; }
      .small-box { margin-bottom: 10px; }
    ")),
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("card_total_cases", width = 2), valueBoxOutput("card_weekly_cases", width = 2),
                valueBoxOutput("card_wow_case_pct", width = 2), valueBoxOutput("card_total_deaths", width = 2),
                valueBoxOutput("card_weekly_deaths", width = 2), valueBoxOutput("card_wow_death_pct", width = 2)
              ),
              fluidRow(box(title = "Zonal Weekly Summary", width = 12, status = "primary", solidHeader = TRUE, DTOutput("table_zonal_weekly")))
      ),
      tabItem(tabName = "malaria_lab",
              fluidRow(box(title = "Malaria Lab Indicators", width = 12, status = "danger", solidHeader = TRUE, DTOutput("table_malaria_lab"))),
              fluidRow(
                box(title = "Age Distribution", width = 4, status = "info", plotlyOutput("pie_malaria_age", height = "400px")),
                box(title = "Test Types", width = 4, status = "info", plotlyOutput("pie_malaria_tests", height = "400px")),
                box(title = "Species Proportion", width = 4, status = "info", plotlyOutput("pie_malaria_species", height = "400px"))
              )
      ),
      tabItem(tabName = "drilldown", 
              box(title = "Zone > Woreda Hierarchical Analysis", width = 12, status = "success", solidHeader = TRUE, DTOutput("table_hierarchical"))
      ),
      tabItem(tabName = "trends",
              fluidRow(box(title = "Weekly Trend", width = 12, status = "info", solidHeader = TRUE, plotlyOutput("plot_weekly_trend", height = "300px"))),
              fluidRow(box(title = "Top 10 Woredas", width = 6, status = "primary", plotlyOutput("plot_top10_woreda")),
                       box(title = "Top 40 Woredas", width = 6, status = "primary", plotlyOutput("plot_top40_woreda")))
      )
    )
  )
)

# 3. SERVER LOGIC -------------------------------------------------------------
server <- function(input, output, session) {
  
  output$ui_woreda <- renderUI({
    woredas <- df_raw %>% filter(Zone %in% input$sel_zone) %>% pull(Woreda) %>% unique() %>% sort()
    pickerInput("sel_woreda", "Woreda", choices = woredas, selected = woredas, multiple = TRUE, options = list(`live-search` = TRUE))
  })
  
  filtered_data <- reactive({
    req(input$sel_zone, input$sel_woreda, input$sel_year, input$sel_week, input$sel_incident)
    df_raw %>% filter(Zone %in% input$sel_zone, Woreda %in% input$sel_woreda,
                      Year_EFY %in% input$sel_year, Week_ISO >= input$sel_week[1], Week_ISO <= input$sel_week[2])
  })
  
  target_col <- reactive({ paste0("Total_", input$sel_incident) })
  death_col  <- reactive({ paste0("Deaths_", input$sel_incident) })
  
  # --- OVERVIEW TABLE (Zonal Weekly Summary) ---
  output$table_zonal_weekly <- renderDT({
    dat <- filtered_data()
    latest_w <- max(dat$Week_ISO, na.rm=TRUE)
    
    tab <- dat %>% group_by(Zone) %>%
      summarise(`Total Cases` = sum(get(target_col()), na.rm=TRUE), 
                `New Cases` = sum(get(target_col()) * (Week_ISO == latest_w), na.rm=TRUE), 
                `Prev Cases` = sum(get(target_col()) * (Week_ISO == (latest_w-1)), na.rm=TRUE),
                `Total Deaths` = sum(get(death_col()), na.rm=TRUE), 
                `New Deaths` = sum(get(death_col()) * (Week_ISO == latest_w), na.rm=TRUE), 
                `Prev Deaths` = sum(get(death_col()) * (Week_ISO == (latest_w-1)), na.rm=TRUE), .groups='drop')
    
    grand <- tab %>% summarise(Zone="GRAND TOTAL", across(where(is.numeric), sum))
    
    bind_rows(tab, grand) %>%
      mutate(`WOW Case %` = round(ifelse(`Prev Cases` > 0, ((`New Cases`-`Prev Cases`)/`Prev Cases`)*100, 0), 1),
             `WOW Death %` = round(ifelse(`Prev Deaths` > 0, ((`New Deaths`-`Prev Deaths`)/`Prev Deaths`)*100, 0), 1)) %>%
      datatable(style = "bootstrap", options = list(pageLength = 25, dom = 'tip', compact = TRUE), rownames = FALSE) %>%
      formatStyle('Zone', target = 'row', fontWeight = styleEqual("GRAND TOTAL", "bold"), backgroundColor = styleEqual("GRAND TOTAL", "#d9edf7"))
  })
  
  # --- MALARIA LAB TABLE (FIXED DISPLAY) ---
  output$table_malaria_lab <- renderDT({
    dat <- filtered_data()
    latest_w <- max(dat$Week_ISO, na.rm=TRUE)
    
    zones_df <- data.frame(Zone = all_zones)
    
    lab_tab <- dat %>% group_by(Zone) %>%
      summarise(Tests_Cum = sum(Malaria_tests_microscopy + Malaria_tests_RDT, na.rm=TRUE), 
                Tests_New = sum((Malaria_tests_microscopy + Malaria_tests_RDT) * (Week_ISO == latest_w), na.rm=TRUE),
                Conf_Cum  = sum(Malaria_PF_Microscopy + Malaria_PF_RDT + Malaria_PV_Microscopy + Malaria_PV_RDT + Malaria_Mixed_Microscopy + Malaria_Mixed_RDT, na.rm=TRUE),
                Conf_New  = sum((Malaria_PF_Microscopy + Malaria_PF_RDT + Malaria_PV_Microscopy + Malaria_PV_RDT + Malaria_Mixed_Microscopy + Malaria_Mixed_RDT) * (Week_ISO == latest_w), na.rm=TRUE),
                PF_Cum    = sum(Malaria_PF_Microscopy + Malaria_PF_RDT, na.rm=TRUE), .groups = 'drop')
    
    final_lab <- zones_df %>% left_join(lab_tab, by="Zone") %>% mutate(across(where(is.numeric), ~replace_na(.,0)))
    grand_lab <- final_lab %>% summarise(Zone="GRAND TOTAL", across(where(is.numeric), sum))
    
    bind_rows(final_lab, grand_lab) %>%
      mutate(`T. TPR%` = round(ifelse(Tests_Cum > 0, (Conf_Cum/Tests_Cum)*100, 0), 1), 
             `W. TPR%` = round(ifelse(Tests_New > 0, (Conf_New/Tests_New)*100, 0), 1), 
             `PF%`     = round(ifelse(Conf_Cum > 0, (PF_Cum/Conf_Cum)*100, 0), 1)) %>%
      select(Zone, `T. Test`=Tests_Cum, `W. Test`=Tests_New, `T. Conf.`=Conf_Cum, `W. Conf.`=Conf_New, `T. TPR%`, `W. TPR%`, `PF%`) %>%
      datatable(style = "bootstrap", options = list(dom = 'tp', pageLength = 25, compact = TRUE), rownames = FALSE) %>%
      formatStyle('Zone', target = 'row', fontWeight = styleEqual("GRAND TOTAL", "bold"), backgroundColor = styleEqual("GRAND TOTAL", "#ecf0f1"))
  })
  
  # --- HIERARCHICAL ANALYSIS (FIXED DISPLAY) ---
  output$table_hierarchical <- renderDT({
    dat <- filtered_data()
    latest_w <- max(dat$Week_ISO, na.rm=TRUE)
    
    raw_tab <- dat %>% group_by(Zone, Woreda) %>%
      summarise(`Total Cases` = sum(get(target_col()), na.rm=TRUE), 
                `New Month Cases` = sum(get(target_col()) * (Week_ISO > (latest_w - 4)), na.rm=TRUE), 
                `Prev Month Cases` = sum(get(target_col()) * (Week_ISO <= (latest_w - 4) & Week_ISO > (latest_w - 8)), na.rm=TRUE),
                `Total Deaths` = sum(get(death_col()), na.rm=TRUE), 
                `New Month Deaths` = sum(get(death_col()) * (Week_ISO > (latest_w - 4)), na.rm=TRUE), 
                `Prev Month Deaths` = sum(get(death_col()) * (Week_ISO <= (latest_w - 4) & Week_ISO > (latest_w - 8)), na.rm=TRUE), .groups = 'drop')
    
    subtotals <- raw_tab %>% group_by(Zone) %>% 
      summarise(Woreda = "ZONE SUB-TOTAL", across(where(is.numeric), sum), .groups = 'drop')
    
    grand_total <- raw_tab %>% summarise(Zone = "GRAND TOTAL", Woreda = "", across(where(is.numeric), sum))
    
    final_h <- bind_rows(grand_total, subtotals, raw_tab) %>%
      arrange(match(Zone, c("GRAND TOTAL", setdiff(unique(Zone), "GRAND TOTAL"))), Zone, desc(Woreda == "ZONE SUB-TOTAL")) %>%
      mutate(`MoM Case %` = round(ifelse(`Prev Month Cases` > 0, ((`New Month Cases`-`Prev Month Cases`)/`Prev Month Cases`)*100, 0), 1),
             `MoM Death %` = round(ifelse(`Prev Month Deaths` > 0, ((`New Month Deaths`-`Prev Month Deaths`)/`Prev Month Deaths`)*100, 0), 1))
    
    datatable(final_h, style = "bootstrap", options = list(pageLength = 25, scrollX = TRUE, compact = TRUE), rownames = FALSE) %>%
      formatStyle('Zone', target = 'row', backgroundColor = styleEqual("GRAND TOTAL", "#3c8dbc"), color = styleEqual("!!! GRAND TOTAL !!!", "white"), fontWeight = styleEqual("!!! GRAND TOTAL !!!", "bold")) %>%
      formatStyle('Woreda', target = 'row', backgroundColor = styleEqual("ZONE SUB-TOTAL", "#d2d6de"), fontWeight = styleEqual("ZONE SUB-TOTAL", "bold"))
  })
  
  # --- CARD OUTPUTS ---
  output$card_total_cases  <- renderValueBox({ valueBox(comma(sum(filtered_data()[[target_col()]], na.rm=T)), "Total Cases", color = "purple") })
  output$card_weekly_cases <- renderValueBox({ 
    latest_w <- max(filtered_data()$Week_ISO, na.rm=T)
    val <- sum(filtered_data() %>% filter(Week_ISO == latest_w) %>% pull(target_col()), na.rm=T)
    valueBox(comma(val), paste("W", latest_w, "Cases"), color = "blue")
  })
  output$card_wow_case_pct <- renderValueBox({
    latest_w <- max(filtered_data()$Week_ISO, na.rm=T)
    curr <- sum(filtered_data() %>% filter(Week_ISO == latest_w) %>% pull(target_col()), na.rm=T)
    prev <- sum(filtered_data() %>% filter(Week_ISO == (latest_w-1)) %>% pull(target_col()), na.rm=T)
    wow  <- if(prev > 0) ((curr - prev)/prev)*100 else 0
    valueBox(paste0(round(wow,1),"%"), "WoW Δ", color = if(wow>0) "red" else "green")
  })
  output$card_total_deaths <- renderValueBox({ valueBox(comma(sum(filtered_data()[[death_col()]], na.rm=T)), "Total Deaths", color = "maroon") })
  output$card_weekly_deaths <- renderValueBox({
    latest_w <- max(filtered_data()$Week_ISO, na.rm=T)
    val <- sum(filtered_data() %>% filter(Week_ISO == latest_w) %>% pull(death_col()), na.rm=T)
    valueBox(comma(val), "Weekly Deaths", color = "orange")
  })
  output$card_wow_death_pct <- renderValueBox({
    latest_w <- max(filtered_data()$Week_ISO, na.rm=T)
    curr <- sum(filtered_data() %>% filter(Week_ISO == latest_w) %>% pull(death_col()), na.rm=T)
    prev <- sum(filtered_data() %>% filter(Week_ISO == (latest_w-1)) %>% pull(death_col()), na.rm=T)
    wow  <- if(prev > 0) ((curr - prev)/prev)*100 else 0
    valueBox(paste0(round(wow,1),"%"), "WoW Δ Deaths", color = if(wow>0) "red" else "green")
  })
  
  # --- PLOTS ---
  output$pie_malaria_age <- renderPlotly({
    d <- filtered_data() %>% summarise(U5=sum(Malaria_U5Yrs_OP+Malaria_U5Yrs_IP, na.rm=T), `5-14`=sum(`Malaria_05-14Yrs_OP`+`Malaria_05-14Yrs_IP`, na.rm=T), `15+`=sum(`Malaria_15+Yrs_OP`+`Malaria_15+Yrs_IP`, na.rm=T)) %>% pivot_longer(cols=everything())
    plot_ly(d, labels=~name, values=~value, type='pie', textinfo='label+percent', textposition='outside', showlegend=FALSE)
  })
  output$pie_malaria_tests <- renderPlotly({
    d <- filtered_data() %>% summarise(Micro=sum(Malaria_tests_microscopy, na.rm=T), RDT=sum(Malaria_tests_RDT, na.rm=T)) %>% pivot_longer(cols=everything())
    plot_ly(d, labels=~name, values=~value, type='pie', textinfo='label+percent', textposition='outside', showlegend=FALSE)
  })
  output$pie_malaria_species <- renderPlotly({
    d <- filtered_data() %>% summarise(PF=sum(Malaria_PF_Microscopy+Malaria_PF_RDT, na.rm=T), PV=sum(Malaria_PV_Microscopy+Malaria_PV_RDT, na.rm=T), Mixed=sum(Malaria_Mixed_Microscopy+Malaria_Mixed_RDT, na.rm=T)) %>% pivot_longer(cols=everything())
    plot_ly(d, labels=~name, values=~value, type='pie', textinfo='label+percent', textposition='outside', showlegend=FALSE)
  })
  output$plot_weekly_trend <- renderPlotly({
    t <- filtered_data() %>% group_by(Week_ISO, Year_EFY) %>% summarise(Cases = sum(get(target_col()), na.rm=T), .groups='drop')
    plot_ly(t, x=~Week_ISO, y=~Cases, color=~as.character(Year_EFY), type='scatter', mode='lines+markers')
  })
  output$plot_top10_woreda <- renderPlotly({
    p <- filtered_data() %>% group_by(Woreda) %>% summarise(C=sum(get(target_col()), na.rm=T)) %>% arrange(desc(C)) %>% slice_head(n=10)
    plot_ly(p, x=~reorder(Woreda, -C), y=~C, type='bar', text=~comma(C), textposition='outside')
  })
  output$plot_top40_woreda <- renderPlotly({
    p <- filtered_data() %>% group_by(Woreda) %>% summarise(C=sum(get(target_col()), na.rm=T)) %>% arrange(desc(C)) %>% slice_head(n=40)
    plot_ly(p, x=~reorder(Woreda, -C), y=~C, type='bar')
  })
}

shinyApp(ui, server)