# ==============================================================================
# ENTERPRISE CLINICAL COMMAND & METABOLIC SURVEILLANCE ECOSYSTEM (R SHINY PRO)
# COMPLIANCE: AUDIT-READY BIOSTATISTICAL INFRASTRUCTURE | TARGET MARKET: IRELAND
# ARCHITECTURE: HIGH-INTERACTIVE REACTIVE GRAPHICS & DATA ENGINEERING PIPELINE
# ==============================================================================

library(shiny)
library(tidyverse)
library(bslib)
library(bsicons)
library(maps)
library(plotly)
library(DT)
library(forecast)

# --- MOCK DATA GENERATION (Replace with actual data pipeline source) ---
set.seed(42)
countries_list <- c("Ireland", "India", "UK", "USA", "Germany", "France", "China", "Brazil")
years_list <- 2010:2024

cleaned_global_data <- expand.grid(Country = countries_list, Year = years_list) %>%
  mutate(Prevalence = runif(n(), min = 2.0, max = 15.0))

# --- ADVANCED GLOBAL DATA PRE-PROCESSING & AUDIT TIMELINE ---
audited_data <- cleaned_global_data %>%
  filter(!is.na(Prevalence)) %>%
  mutate(
    Year = as.integer(Year),
    Risk_Tier = case_when(
      Prevalence < 5.0 ~ "Low Cohort Risk",
      Prevalence >= 5.0 & Prevalence < 10.0 ~ "Moderate Sentinel Risk",
      Prevalence >= 10.0 ~ "Critical Escalation Risk"
    )
  )

world_map_geometry <- map_data("world")

# --- EXECUTIVE CLINICAL USER INTERFACE ---
ui <- page_navbar(
  title = "Maternal-Fetal Metabolic Intelligence Network",
  theme = bs_theme(version = 5, bootswatch = "slate"), # Dark high-tech executive command skin
  bg = "#0B132B", 
  
  # --- TAB 1: INTERACTIVE WORLD RISKS MAP ---
  nav_panel(
    title = "Global Risk Topology Map",
    icon = bs_icon("globe-central-south-asia"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Temporal Parameter Controls",
        width = 300,
        sliderInput("map_year", "Select Active Trial Protocol Year:", 
                    min = min(audited_data$Year), max = max(audited_data$Year), 
                    value = max(audited_data$Year), step = 1, sep = "")
      ),
      card(
        card_header(textOutput("map_title")),
        plotlyOutput("interactive_global_heatmap", height = "520px") 
      )
    )
  ),
  
  # --- TAB 2: INTERACTIVE COHORT STRATIFICATION & AUTOMATED INSIGHTS ---
  nav_panel(
    title = "Regional Analytics & Automated Clinical Brief",
    icon = bs_icon("sliders"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Cohort Assessment Matrix",
        width = 320,
        selectInput("country_one", "Primary Target Location:", choices = unique(audited_data$Country), selected = "Ireland"),
        selectInput("country_two", "Comparative Target Location:", choices = unique(audited_data$Country), selected = "India")
      ),
      
      layout_column_wrap(
        width = 1/2,
        value_box(title = textOutput("name_one"), value = textOutput("val_one"), showcase = bs_icon("activity"), theme = "primary"),
        value_box(title = textOutput("name_two"), value = textOutput("val_two"), showcase = bs_icon("exclamation-triangle"), theme = "danger")
      ),
      br(),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Longitudinal Trajectory Mapping (Hover Nodes for Precise % Values)"),
          plotlyOutput("interactive_trend_plot", height = "350px")
        ),
        card(
          card_header("Automated AI Clinical Brief Generation (Real-Time Report)"),
          card_body(
            div(style = "background-color: #1C2541; padding: 15px; border-radius: 8px; border-left: 5px solid #3A86FF;",
                uiOutput("automated_clinical_brief")
            )
          )
        )
      )
    )
  ),
  
  # --- TAB 3: LONG-TERM ALGORITHMIC FORECAST MODELLING ---
  nav_panel(
    title = "Predictive Longitudinal Forecasting",
    icon = bs_icon("graph-up"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Algorithmic Control Layer",
        width = 320,
        selectInput("forecast_country", "Target Profile Country:", choices = unique(audited_data$Country), selected = "India"),
        sliderInput("forecast_horizon", "Extend Prediction Envelope (Year):", min = 2025, max = 2045, value = 2035, sep = ""),
        hr(),
        radioButtons(
          "forecast_model",
          "Biostatistical Engine:",
          choices = c(
            "Logit-Transformed Linear" = "logit",
            "Auto-ARIMA (Time Series)" = "arima"
          ),
          selected = "logit"
        ),
        helpText("Logit model enforces 0–100% bounds; Auto-ARIMA models serial autocorrelation.")
      ),
      card(
        card_header(textOutput("forecast_title")),
        plotlyOutput("interactive_forecast_plot", height = "450px")
      )
    )
  ),
  
  # --- TAB 4: AUDIT LINEAGE LOGS & COMPLIANCE ---
  nav_panel(
    title = "Regulatory Data Audit Trail",
    icon = bs_icon("shield-check"),
    card(
      card_header("Audited Trial Master Dataset Explorer (Regulatory Verified)"),
      DTOutput("master_table")
    )
  )
)

# --- SERVER CORE COMPUTATIONAL ENGINE ---
server <- function(input, output, session) {
  
  # --- TAB 1 BACKEND ---
  output$map_title <- renderText({ 
    paste("Global Metabolic Risk Topology Assessment (Reporting Matrix:", input$map_year, ")") 
  })
  
  output$interactive_global_heatmap <- renderPlotly({
    req(input$map_year)
    map_year_df <- audited_data %>% filter(Year == input$map_year)
    
    map_data_merged <- world_map_geometry %>%
      left_join(map_year_df, by = c("region" = "Country"))
    
    p <- ggplot(map_data_merged, aes(x = long, y = lat, group = group, 
                                     text = paste("Territory:", region, 
                                                  "<br>Prevalence Rate:", round(Prevalence, 2), "%",
                                                  "<br>Stratification Tier:", ifelse(is.na(Risk_Tier), "No Data", Risk_Tier)))) +
      geom_polygon(aes(fill = Prevalence), color = "#1C2541", linewidth = 0.05) +
      scale_fill_gradientn(colors = c("#2196F3", "#00BCD4", "#FFC107", "#FF5722"), na.value = "#374151") +
      theme_void() +
      theme(plot.background = element_rect(fill = "#111827", color = NA))
    
    ggplotly(p, tooltip = "text") %>% 
      layout(clickmode = "event+select", paper_bgcolor = '#111827', plot_bgcolor = '#111827')
  })
  
  # --- TAB 2 BACKEND ---
  primary_filtered <- reactive({ 
    req(input$country_one)
    audited_data %>% 
      filter(Country == input$country_one) %>% 
      slice_max(Year, n = 1, with_ties = FALSE)
  })
  
  comparison_filtered <- reactive({ 
    req(input$country_two)
    audited_data %>% 
      filter(Country == input$country_two) %>% 
      slice_max(Year, n = 1, with_ties = FALSE)
  })
  
  output$name_one <- renderText({ paste(input$country_one, "Latest Baseline Status") })
  output$name_two <- renderText({ paste(input$country_two, "Latest Baseline Status") })
  
  output$val_one <- renderText({
    df <- primary_filtered()
    if(nrow(df) > 0) paste0(round(df$Prevalence, 2), "% (", df$Risk_Tier, ")") else "Data Pending"
  })
  output$val_two <- renderText({
    df <- comparison_filtered()
    if(nrow(df) > 0) paste0(round(df$Prevalence, 2), "% (", df$Risk_Tier, ")") else "Data Pending"
  })
  
  output$interactive_trend_plot <- renderPlotly({
    req(input$country_one, input$country_two)
    plot_df <- audited_data %>% filter(Country %in% c(input$country_one, input$country_two))
    
    g <- ggplot(plot_df, aes(x = Year, y = Prevalence, color = Country, group = Country,
                             text = paste("Country:", Country, "<br>Cycle Year:", Year, "<br>Prevalence Index:", round(Prevalence, 2), "%"))) +
      geom_line(linewidth = 1) + 
      geom_point(size = 3) +
      scale_color_manual(values = c("#00E5FF", "#FF3D00")) +
      theme_minimal() +
      theme(panel.grid.major = element_line(color = "#1C2541"), 
            panel.grid.minor = element_blank(),
            text = element_text(color = "white"))
    
    ggplotly(g, tooltip = "text")
  })
  
  output$automated_clinical_brief <- renderUI({
    df1 <- primary_filtered()
    df2 <- comparison_filtered()
    
    if(nrow(df1) == 0 || nrow(df2) == 0) return("Awaiting full active cohort parameters...")
    
    variance <- abs(df1$Prevalence - df2$Prevalence)
    higher_country <- if(df1$Prevalence > df2$Prevalence) input$country_one else input$country_two
    higher_tier <- if(df1$Prevalence > df2$Prevalence) df1$Risk_Tier else df2$Risk_Tier
    
    HTML(paste0(
      "<h5 style='color: #00E5FF; margin-top:0;'>🧬 Automated Diagnostic Summary Sheet</h5>",
      "<p>Retrospective evaluation confirms an absolute metabolic variance gap of <strong>", round(variance, 2), "%</strong> between targeted evaluation regions.</p>",
      "<p>The territory of <strong>", higher_country, "</strong> exhibits the highest cross-border patient saturation profile, placing it into the <strong>", higher_tier,"</strong> parameters.</p>",
      "<p><em>CRO Protocol Advisory:</em> Clinical trials focused on metabolic interventions should prioritize patient recruiting nodes across the <strong>", higher_country, "</strong> pipeline to maximize target biomarker selection capture speeds.</p>"
    ))
  })
  
  # --- TAB 3 BACKEND: DYNAMIC MODEL SWITCHING ENGINE ---
  output$forecast_title <- renderText({
    model_label <- if (input$forecast_model == "logit") "Logit Linear Fit" else "Auto-ARIMA Time-Series Fit"
    paste0("Predictive Projections: ", input$forecast_country, " (Engine: ", model_label, ")")
  })
  
  output$interactive_forecast_plot <- renderPlotly({
    req(input$forecast_country, input$forecast_horizon, input$forecast_model)
    
    country_df <- audited_data %>% 
      filter(Country == input$forecast_country) %>% 
      arrange(Year)
    
    # Minimum sample check
    req(nrow(country_df) >= 3)
    
    current_max_year <- max(country_df$Year)
    target_year <- input$forecast_horizon
    
    req(target_year > current_max_year)
    
    inv_logit <- function(x) (1 / (1 + exp(-x))) * 100
    
    # --------------------------------------------------------------------------
    # DYNAMIC MODEL SWITCHING LOGIC
    # --------------------------------------------------------------------------
    if (input$forecast_model == "logit") {
      
      # Logit-Transformed Regression
      country_df$logit_val <- log((country_df$Prevalence / 100) / (1 - (country_df$Prevalence / 100)))
      
      model <- lm(logit_val ~ Year, data = country_df)
      future_years <- data.frame(Year = seq(min(country_df$Year), target_year, by = 1))
      
      pred <- predict(model, newdata = future_years, interval = "confidence", level = 0.95)
      
      forecast_df <- data.frame(
        Year = future_years$Year,
        fit  = inv_logit(pred[, "fit"]),
        lwr  = inv_logit(pred[, "lwr"]),
        upr  = inv_logit(pred[, "upr"])
      )
      
    } else if (input$forecast_model == "arima") {
      
      # Auto-ARIMA Model
      p <- pmin(pmax(country_df$Prevalence / 100, 0.001), 0.999)
      logit_p <- log(p / (1 - p))
      
      ts_data <- ts(logit_p, start = min(country_df$Year), frequency = 1)
      arima_fit <- auto.arima(ts_data, stepwise = FALSE, approximation = FALSE)
      
      h_steps <- target_year - current_max_year
      fc <- forecast(arima_fit, h = h_steps, level = 95)
      
      hist_df <- data.frame(
        Year = country_df$Year,
        fit  = country_df$Prevalence,
        lwr  = country_df$Prevalence,
        upr  = country_df$Prevalence
      )
      
      future_df <- data.frame(
        Year = seq(current_max_year + 1, target_year, by = 1),
        fit  = inv_logit(as.numeric(fc$mean)),
        lwr  = inv_logit(as.numeric(fc$lower[, 1])),
        upr  = inv_logit(as.numeric(fc$upper[, 1]))
      )
      
      forecast_df <- rbind(hist_df, future_df)
    }
    
    # --------------------------------------------------------------------------
    # RENDER INTERACTIVE VISUALIZATION
    # --------------------------------------------------------------------------
    p <- ggplot(forecast_df, aes(x = Year, y = fit)) +
      geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "#3A86FF", alpha = 0.2) +
      geom_line(color = "#3A86FF", linewidth = 1.2) +
      geom_point(
        data = country_df, 
        aes(
          x = Year, 
          y = Prevalence, 
          text = paste("Historical Point:", Year, "<br>Observed Prevalence:", round(Prevalence, 2), "%")
        ), 
        color = "#FF0055", 
        size = 2.5
      ) +
      scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
      labs(x = "Cycle Year", y = "Prevalence Rate (%)") +
      theme_minimal() +
      theme(
        panel.grid.major = element_line(color = "#1C2541"), 
        panel.grid.minor = element_blank(), 
        text = element_text(color = "#FFFFFF"),
        plot.background = element_rect(fill = "#0B132B", color = NA)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#0B132B", plot_bgcolor = "#0B132B")
  })
  
  # --- TAB 4 BACKEND ---
  output$master_table <- renderDT({
    datatable(audited_data,
              options = list(pageLength = 10, scrollX = TRUE),
              selection = "none", rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)