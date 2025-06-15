options(repos = c(CRAN = "https://cloud.r-project.org"))
options(shiny.maxRequestSize = 50*1024^2)

source("install.R")

library(shiny)
library(bs4Dash)
library(shinyjs)
library(leaflet)
library(leafdown)
library(stars)
library(echarts4r)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(htmlwidgets)
library(geobr)
library(sf)
library(stringr)

source("data.R")

# ────────────────────────────────────────────────────────────────────────────────
# 1. Enhanced UI with Improved Layout
# ────────────────────────────────────────────────────────────────────────────────
ui <- bs4DashPage(
  title = "Investimento do PRONAF no Brasil",
  header = bs4DashNavbar(
    title = tags$div(
      tags$h3("PRONAF Dashboard", style = "margin-bottom:.2rem;")
    )
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    sidebarMenu(
      selectInput(
        "selected_products", 
        "Categorias de Produtos:",
        choices = c("Todos","Animais","Maquinário","Infraestrutura","Frutas",
                    "Hortaliças", "Árvores", "Turismo Rural", "Café",
                    "Recuperação Ambiental", "Agroartesanato", "Energia",
                    "Forrageiras", "Irrigação", "Solo", "Lavoura", "Transporte",
                    "Água", "Escolas", "Flores", "Prestação De Serviços", "Outros"),
        selected = "Todos",
        multiple = TRUE
      ),
      selectInput(
        "year",
        "Filtrar por Ano:",
        choices = c("Todos"),
        selected = "Todos"
      ),
      radioButtons(
        "kpi",
        "Métrica:",
        choices = c(
          "Número de Contratos" = "contracts",
          "Investimento Total (R$)" = "investment"
        ),
        selected = "investment"
      )
    )
  ),
  body = bs4DashBody(
    tags$head(
      tags$style(HTML("
        .leaflet-container { background: #fff; }
        .card { height: 100%; }
        .chart-container { height: calc(100% - 40px); }
      "))
    ),
    fluidRow(
      bs4Card(
        title = "Mapa PRONAF",
        width = 6,
        closable = FALSE,
        collapsible = FALSE,
        leafletOutput("pronaf_map", height = 450),
        footer = tagList(
          actionButton("drill_down", "Detalhar", icon = icon("search-plus")),
          actionButton("drill_up", "Voltar", icon = icon("search-minus")),
          htmlOutput("map_summary")
        )
      ),
      bs4Card(
        title = "Série Temporal Anual",
        width = 6,
        closable = FALSE,
        echarts4rOutput("time_series", height = 450)
      )
    ),
    fluidRow(
      bs4Card(
        title = textOutput("stacked_bar_title"),
        width = 12,
        closable = FALSE,
        echarts4rOutput("stacked_bar", height = 380),
        footer = uiOutput("municipality_warning")
      )
    )
  )
)

# ────────────────────────────────────────────────────────────────────────────────
# 2. Enhanced Server Logic
# ────────────────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  # Load data
  load("data/pronaf_clean.RData")
  load("data/geo_data.RData")
  
  # Create region-state key - ensure column names match
  estados_key <- spdfs_list[[2]]@data %>% 
    distinct(abbrev_state, name_region) %>%
    mutate(abbrev_state = as.character(abbrev_state))
  
  # Add region to PRONAF data - ensure join columns match
  Mun_prod_reg <- Mun_prod %>%
    left_join(estados_key, by = c("nomeUF" = "abbrev_state"))
  
  # Update year choices
  updateSelectInput(
    session, 
    "year",
    choices = c("Todos", sort(unique(Mun_prod_reg$AnoEmissao)))
  )
  
  # ──────────────────────────────────────────────────────────────────────────────
  # Leafdown Setup with Robust Joins
  # ──────────────────────────────────────────────────────────────────────────────
  my_leafdown <- Leafdown$new(
    spdfs_list,
    "pronaf_map",
    input,
    join_map_levels_by = c("join_1_2" = "join_1_2", "join_2_3" = "join_2_3")
  )
  
  rv <- reactiveValues(update_leafdown = 0)
  
  # Drill handlers
  observeEvent(input$drill_down, {
    my_leafdown$drill_down()
    rv$update_leafdown <- rv$update_leafdown + 1
  })
  
  observeEvent(input$drill_up, {
    my_leafdown$drill_up()
    rv$update_leafdown <- rv$update_leafdown + 1
  })
  
  # ──────────────────────────────────────────────────────────────────────────────
  # Reactive Data Processing with safer joins
  # ──────────────────────────────────────────────────────────────────────────────
  filtered_data <- reactive({
    dat <- Mun_prod_reg
    
    # Product filter
    if (!("Todos" %in% input$selected_products)) {
      dat <- dat %>% 
        filter(product_category %in% input$selected_products)
    }
    
    # Year filter
    if (input$year != "Todos") {
      dat <- dat %>% 
        filter(AnoEmissao == as.numeric(input$year))
    }
    
    dat
  })
  
  # ──────────────────────────────────────────────────────────────────────────────
  # Enhanced Map Visualization with safer merge
  # ──────────────────────────────────────────────────────────────────────────────
  output$pronaf_map <- renderLeaflet({
  req(rv$update_leafdown)
  
  lvl <- my_leafdown$curr_map_level
  curr_data <- my_leafdown$curr_data
  
  # 1. First verify we have data to join
  if (is.null(curr_data)) {
    showNotification("No spatial data available", type = "error")
    return(leaflet() %>% addTiles())
  }
  
  # 2. Aggregate the PRONAF data with proper checks
  pronaf_agg <- tryCatch({
    group_var <- case_when(
      lvl == 1 ~ "name_region",
      lvl == 2 ~ "nomeUF",
      TRUE ~ "Municipio"
    )
    
    filtered_data() %>%
      group_by(!!sym(group_var)) %>%
      summarise(
        total_contracts = n(),
        total_investment = sum(VlInvest, na.rm = TRUE),
        .groups = "drop"
      )
  }, error = function(e) {
    showNotification("Failed to aggregate data", type = "error")
    return(NULL)
  })
  
  # 3. Verify the aggregation worked
  if (is.null(pronaf_agg) || nrow(pronaf_agg) == 0) {
    showNotification("No data available for selected filters", type = "warning")
    return(leaflet() %>% addTiles())
  }
  
  # 4. Prepare for join - convert to data frame if spatial
  if (inherits(curr_data, "Spatial")) {
    curr_df <- curr_data@data
  } else {
    curr_df <- curr_data
  }
  
  # 5. Verify join columns exist in both datasets
  join_cols <- list(
    "1" = c(left = "name_region", right = "name_region"),
    "2" = c(left = "abbrev_state", right = "nomeUF"),
    "3" = c(left = "name_muni", right = "Municipio")
  )[[as.character(lvl)]]
  
  if (!all(join_cols["left"] %in% names(curr_df)) || 
      !all(join_cols["right"] %in% names(pronaf_agg))) {
    showNotification("Missing columns for data join", type = "error")
    return(leaflet() %>% addTiles())
  }
  
  # 6. Perform the join with verified columns
  merged_data <- curr_df %>%
    left_join(pronaf_agg, by = setNames(join_cols["right"], join_cols["left"]))
  
  # 7. Put data back into spatial object if needed
  if (inherits(curr_data, "Spatial")) {
    curr_data@data <- merged_data
  } else {
    curr_data <- merged_data
  }
    
    # Configure visualization based on KPI
    if (input$kpi == "contracts") {
      curr_data$y <- curr_data$total_contracts
      blue_ramp <- colorRampPalette(c("#A0D8F3", "#08306B")) # light to dark
      pal <- colorNumeric(blue_ramp(10), curr_data$y, na.color = "#808080")
      legend_title <- "Número de Contratos"
      format_fn <- function(x) format(x, big.mark = ".", decimal.mark = ",")
    } else {
      curr_data$y <- curr_data$total_investment
      green_ramp <- colorRampPalette(c("#A7F3A0", "#006400")) # light to dark
      pal <- colorNumeric(green_ramp(10), curr_data$y, na.color = "#808080")
      legend_title <- "Investimento Total (R$)"
      format_fn <- function(x) {
        ifelse(is.na(x), "NA", paste0("R$ ", format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)))
      }
    }
    
    # Create interactive labels
  lab <- switch(
    as.character(lvl),
    "1" = curr_data$name_region,
    "2" = curr_data$name_state,
    "3" = curr_data$name_muni
  )
  
  labels <- sprintf(
    "%s : %s",
    lab,
    ifelse(is.na(curr_data$y), "Sem dados", format_fn(curr_data$y))
  )
  
  lab_fmt <- function(type, cuts, p) {
    sel <- unique(c(1, ceiling(length(cuts)/2), length(cuts)))
    pretty_vals <- signif(cuts[sel], 3)   # or round() if you prefer
    format(
      pretty_vals,
      big.mark = ".", decimal.mark = ",", scientific = FALSE
    )
  }
    # Draw the map with consistent styling
    my_leafdown$draw_leafdown(
      fillColor = ~pal(curr_data$y),
      weight = 1,
      fillOpacity = 0.8,
      color = "white",
      label = labels
    ) %>%
      setView(-54, -15, 4) %>%
      addLegend(
        pal = pal,
        values = ~curr_data$y,
        title = legend_title,
        opacity = 1,
        labFormat = lab_fmt,
        na.label = "Sem dados"
      )
  })
  
  # ──────────────────────────────────────────────────────────────────────────────
  # Enhanced Time Series Chart with product_category check
  # ──────────────────────────────────────────────────────────────────────────────
  output$time_series <- renderEcharts4r({
    
    # ── 1. base, filter by map selection ────────────────────────────────────────
    dat <- filtered_data()
    sel <- my_leafdown$curr_sel_data()
    lvl <- my_leafdown$curr_map_level
    
    if (nrow(sel) > 0) {
      filter_col <- switch(lvl, "1" = "name_region", "2" = "nomeUF", "3" = "Municipio")
      filter_val <- switch(lvl, "1" = sel$name_region, "2" = sel$abbrev_state, "3" = sel$name_muni)
      dat <- dat %>% filter(.data[[filter_col]] == filter_val)
    }
    
    # ── 2. constants ────────────────────────────────────────────────────────────
    metric   <- if (input$kpi == "investment") "total_investment" else "total_contracts"
    y_title  <- if (input$kpi == "investment") "Investimento (R$)" else "Número de Contratos"
    years_seq <- 2013:2025                   # full x-axis
    pal <- brewer.pal(5, "Set2")             # up to 5 distinct colours
    
    # ── 3. data prep ────────────────────────────────────────────────────────────
    todos_selected   <- "Todos" %in% input$selected_products
    specific_products <- setdiff(input$selected_products, "Todos")
    
    # helper to aggregate
    agg_fun <- function(d) {
      d %>%
        group_by(AnoEmissao, product_category) %>%
        summarise(
          total_investment = sum(VlInvest, na.rm = TRUE),
          total_contracts  = n(),
          .groups = "drop"
        )
    }
    
    # New: Format numbers for display
    format_investment <- function(value) {
      if (is.na(value)) return("0")
      if (value >= 1000000) {
        paste0(round(value/1000000, 1), "M")
      } else if (value >= 1000) {
        paste0(round(value/1000, 1), " mil")
      } else {
        as.character(round(value, 1))
      }
    }
    
    if (todos_selected && length(specific_products) == 0) {
      # 3A. only "Todos" clicked → one average line
      dat <- agg_fun(dat) %>%
        group_by(AnoEmissao) %>%
        summarise(
          total_investment = mean(total_investment, na.rm = TRUE),
          total_contracts  = mean(total_contracts,  na.rm = TRUE),
          .groups = "drop"
        )
      
      # New: Format tooltip if investment is selected
      tooltip_formatter <- if (input$kpi == "investment") {
        htmlwidgets::JS("
          function(params) {
            var value = params[0].value[1];
            if (value >= 1000000) {
              return (value/1000000).toFixed(1) + 'M R$';
            } else if (value >= 1000) {
              return (value/1000).toFixed(1) + ' mil R$';
            } else {
              return value.toFixed(1) + ' R$';
            }
          }
        ")
      } else {
        NULL
      }
      
      dat %>%
        right_join(tibble(AnoEmissao = years_seq), by = "AnoEmissao") %>%   # fill missing yrs
        arrange(AnoEmissao) %>%
        e_charts(AnoEmissao) %>%
        e_line_(serie = metric, name = "Média Total") %>%
        e_tooltip(
          trigger = "axis",
          formatter = tooltip_formatter
        ) %>%
        e_y_axis(
          name = y_title,
          axisLabel = if (input$kpi == "investment") {
            list(formatter = htmlwidgets::JS("
              function(value) {
                if (value >= 1000000) {
                  return (value/1000000).toFixed(0) + 'M';
                } else if (value >= 1000) {
                  return (value/1000).toFixed(0) + 'K';
                } else {
                  return value;
                }
              }
            "))
          } else NULL
        ) %>%
        e_x_axis(
          type = "category",
          name = "Ano",
          axisLabel = list(interval = 0, formatter = htmlwidgets::JS("value => value"))
        )
      
    } else {
      # 3B. one or more specific products
      if (length(specific_products) > 0) {
        dat <- dat %>% filter(product_category %in% specific_products)
      }
      
      dat <- agg_fun(dat)
      
      top5 <- dat %>%
        group_by(product_category) %>%
        summarise(tot = sum(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(tot)) %>%
        slice_head(n = 5) %>%
        pull(product_category)
      
      dat <- dat %>% filter(product_category %in% top5)
      
      # New: Format tooltip if investment is selected
      tooltip_formatter <- if (input$kpi == "investment") {
        htmlwidgets::JS("
          function(params) {
            var result = params[0].axisValue + '<br/>';
            params.forEach(function(item) {
              var value = item.value[1];
              var formattedValue;
              if (value >= 1000000) {
                formattedValue = (value/1000000).toFixed(1) + 'M R$';
              } else if (value >= 1000) {
                formattedValue = (value/1000).toFixed(1) + ' mil R$';
              } else {
                formattedValue = value.toFixed(1) + ' R$';
              }
              result += item.marker + ' ' + item.seriesName + ': ' + formattedValue + '<br/>';
            });
            return result;
          }
        ")
      } else {
        NULL
      }
      
      dat %>%
        right_join(
          expand.grid(AnoEmissao = years_seq, product_category = top5),
          by = c("AnoEmissao", "product_category")
        ) %>%                           # ensure every year/prod combo exists
        arrange(product_category, AnoEmissao) %>%
        group_by(product_category) %>%
        e_charts(AnoEmissao) %>%
        e_line_(serie = metric) %>%     # one line per group() thanks to echarts4r
        e_tooltip(
          trigger = "axis",
          formatter = tooltip_formatter
        ) %>%
        e_color(pal) %>%
        e_y_axis(
          name = y_title,
          axisLabel = if (input$kpi == "investment") {
            list(formatter = htmlwidgets::JS("
              function(value) {
                if (value >= 1000000) {
                  return (value/1000000).toFixed(0) + 'M';
                } else if (value >= 1000) {
                  return (value/1000).toFixed(0) + 'K';
                } else {
                  return value;
                }
              }
            "))
          } else NULL
        ) %>%
        e_x_axis(
          type = "category",
          name = "Ano",
          axisLabel = list(interval = 0, formatter = htmlwidgets::JS("value => value"))
        ) %>%
        e_legend(right = 0)
    }
  })  
  # ──────────────────────────────────────────────────────────────────────────────
  # Enhanced Stacked Bar Chart with product_category check
  # ──────────────────────────────────────────────────────────────────────────────
  output$stacked_bar <- renderEcharts4r({
    # Get data with safety checks
    lvl <- my_leafdown$curr_map_level
    sel_data <- my_leafdown$curr_sel_data()
    dat <- filtered_data()
    
    # Ensure product_category exists
    if (!"product_category" %in% names(dat)) {
      dat$product_category <- "OUTROS"
    }
    
    # ── 1. Aggregation ──
    if (nrow(sel_data) == 0) {
      agg_data <- dat %>%
        group_by(product_category) %>%
        summarise(
          total_investment = sum(VlInvest, na.rm = TRUE),
          total_contracts = n(),
          .groups = "drop"
        ) %>%
        filter(!is.na(product_category)) %>%
        arrange(desc(total_investment)) %>%
        {if(nrow(.) > 0) slice_head(., n = 12) else .}
      
      title <- "Distribuição Nacional por Categoria (Top 12)"
    } else {
      filter_col <- switch(lvl, 
                           "1" = "name_region",
                           "2" = "nomeUF", 
                           "3" = "Municipio")
      
      filter_val <- switch(lvl,
                           "1" = sel_data$name_region,
                           "2" = sel_data$abbrev_state,
                           "3" = sel_data$name_muni)
      
      agg_data <- dat %>%
        filter(.data[[filter_col]] == filter_val) %>%
        group_by(product_category) %>%
        summarise(
          total_investment = sum(VlInvest, na.rm = TRUE),
          total_contracts = n(),
          .groups = "drop"
        ) %>%
        filter(!is.na(product_category))
      
      title <- paste(
        "Distribuição em",
        switch(lvl,
               "1" = paste("Região", sel_data$name_region),
               "2" = paste("Estado", sel_data$name_state),
               "3" = sel_data$name_muni)
      )
    }
    
    # ── Critical Check ──
    if (nrow(agg_data) == 0 || !"product_category" %in% names(agg_data)) {
      return(e_charts() %>% e_title("Sem dados disponíveis"))
    }
    
    # ── 2. Prepare Plot ──
    metric <- ifelse(input$kpi == "investment", 
                     "total_investment", 
                     "total_contracts")
    y_title <- ifelse(input$kpi == "investment",
                      "Investimento (R$)", 
                      "Número de Contratos")
    
    # ── 3. Create Plot (SAFE VERSION) ──
    chart <- agg_data %>%
      # Convert to character to avoid factor issues
      mutate(product_category = as.character(product_category)) %>%
      e_charts(product_category)
    
    # Add bars based on metric
    if (input$kpi == "investment") {
      chart <- chart %>% e_bar(total_investment, name = "Investimento")
    } else {
      chart <- chart %>% e_bar(total_contracts, name = "Contratos")
    }
    
    # Finalize chart
    chart %>%
      e_x_axis(
        name = "Categoria",
        axisLabel = list(interval = 0, rotate = 45)
      ) %>%
      e_y_axis(
        name = y_title,
        axisLabel = list(
          formatter = htmlwidgets::JS("value => value.toLocaleString('pt-BR')")
        )
      ) %>%
      e_tooltip(
        trigger = "item",
        formatter = if (input$kpi == "investment") {
          e_tooltip_item_formatter("currency", currency = "R$", digits = 2)
        } else {
          e_tooltip_item_formatter("decimal", digits = 0)
        }
      ) %>%
      e_color(rep(brewer.pal(12, "Set3"), length.out = nrow(agg_data))) %>%
      e_grid(left = "15%", bottom = "20%")
  })
  
  # ──────────────────────────────────────────────────────────────────────────────
  # UI Components
  # ──────────────────────────────────────────────────────────────────────────────
  output$debug_console <- renderPrint({
    # This will show the last debug messages
    "Check R console for detailed debug output"
  })
  
  output$municipality_warning <- renderUI({
    if (nrow(my_leafdown$curr_sel_data()) == 0) {
      tags$div(
        style = "color: #666; font-style: italic;",
        "Nenhuma região selecionada. Mostrando dados nacionais."
      )
    }
  })
  
  output$map_summary <- renderUI({
    dat <- filtered_data()
    tags$div(
      style = "margin-top: 10px;",
      tags$p(
        tags$strong("Total de Contratos: "),
        format(nrow(dat), big.mark = ".", decimal.mark = ",")
      ),
      tags$p(
        tags$strong("Investimento Total: "),
        paste0(
          "R$ ", 
          format(
            sum(dat$VlInvest, na.rm = TRUE),
            big.mark = ".",
            decimal.mark = ",",
            digits = 2,
            scientific = FALSE
          )
        )
      )
    )
  })
}

shinyApp(ui, server)
