###############################################################################
# Script Name: pronaf_analysis.R
# Title: PRONAF's Role in Rural Tourism in the State of Santa Catarina
# Purpose: This report aims to illustrate PRONAF
# investment data across three levels: Brazil as a whole,
# the state of Santa Catarina, and specifically Rural Tourism
# within these scales.
# The analysis seeks to highlight the evolution of resources
# allocated to this activity, identifying opportunities
# and limitations for strengthening it as a sustainable
# economic alternative for family farming in Santa Catarina.
# Author: João P. Donadio
# Email: jp.donadio@gmail.com
# Date Created:  2025-05-24
# Last Modified: 2025-05-24
# R Version: 4.4.3
# Notes:
###############################################################################

# Load required packages
library(dplyr)
library(tidyverse)
library(sf)
library(geobr)
library(patchwork)
library(scales)
library(viridis)
library(ggplot2)
library(readxl)
library(purrr)
library(dplyr)
library(writexl)
library(ggrepel)

# Set working directory
setwd("~/pronaf")

# 1. Create list of all file paths
file_paths <- paste0("data_sicor/", 2013:2024, ".xlsx")

# 2. Read all Excel files and add year column
combined_data <- map_df(file_paths, ~{
  year <- str_extract(.x, "\\d{4}")  # Extract year from filename
  read_excel(.x) %>% 
    mutate(year = year)  # Add year column
})



# 3. Check the combined structure
names(combined_data)

str(Mun_prod)

glimpse(combined_data)

combined_data <- combined_data %>%
  select(-c(...21, ...20, ...19, ...18, ...17, ...16, `372530`, year))

combined_data <- combined_data %>%
  filter(PROGRAMA == "PRONAF - PROGRAMA NACIONAL DE FORTALECIMENTO DA AGRICULTURA FAMILIAR")

combined_dataNA <- combined_data %>%
  filter(!is.na(MUNICIPIO)) %>%
  select(-c(PROGRAMA, SEMESTRE_EMISSAO, CD_IBGE_MUNICIPIO, ATIVIDADE, FONTE_RECURSOS, areaFinanc))

str(combined_dataNA)

combined_data_matched <- combined_dataNA %>%
  rename(
    Municipio = MUNICIPIO,
    nomeProduto = PRODUTO,
    AnoEmissao = `#ANO_EMISSAO`,  # Note: backticks for special column name
    VlInvest = valorContratos,
    nomeUF = UF,
    nomeSubPrograma = SUBPROGRAMA
  )

str(combined_data_matched)
write.csv(combined_data_matched, "Mun_pronaf_testingtotal.csv", row.names = FALSE)


# Create a named vector for column renaming
new_names <- c(
  "#ANO_EMISSAO" = "ano_emissao",
  "SEMESTRE_EMISSAO" = "semestre_emissao",
  "UF" = "uf",
  "CD_IBGE_MUNICIPIO" = "codigo_ibge",
  "MUNICIPIO" = "municipio",
  "PRODUTO" = "produto_agricola",
  "ATIVIDADE" = "atividade_agricola",
  "FINALIDADE" = "finalidade_financiamento",
  "MODALIDADE" = "modalidade_credito",
  "PROGRAMA" = "programa",
  "SUBPROGRAMA" = "subprograma",
  "FONTE_RECURSOS" = "fonte_recursos",
  "qtdContratos" = "quantidade_contratos",
  "valorContratos" = "valor_total_contratos",
  "areaFinanc" = "area_financiada_ha")

# Apply the renaming
combined_data <- combined_data %>%
  rename_with(~ new_names, all_of(names(new_names))) %>%
  mutate(
    ano_emissao = as.factor(ano_emissao),
    quantidade_contratos = as.numeric(quantidade_contratos),
    valor_total_contratos = as.numeric(valor_total_contratos),
    area_financiada_ha = as.numeric(area_financiada_ha)
  )

# Verify the new names
names(combined_data)


# Calculate metrics by year (using ano_emissao or ano_emissao)
dados_agregados <- combined_data %>%
  group_by(ano_emissao) %>%  # Or group_by(ano_emissao) if you prefer calendar year
  summarise(
    Operações = sum(quantidade_contratos, na.rm = TRUE),
    Valor = sum(valor_total_contratos, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    Valor_Medio = Valor / Operações
  )

past_data <- read_excel("data/1995-2012.xlsx")

# First, create labels for the past_data similar to dados_agregados
past_data_with_labels <- past_data %>%
  mutate(
    Valor_label = paste0(" ", round(Valor / 1e9, 0), "bi "),
    Operacoes_label = case_when(
      Operações >= 1e6 ~ paste0(round(Operações / 1e6, 1), " M"),
      Operações >= 1e3 ~ paste0(format(round(Operações / 1e3, 0), big.mark = "."), " mil"),
      TRUE ~ as.character(Operações)
    )
  )

# Convert ano_emissao to numeric in dados_agregados to match types
dados_agregados <- dados_agregados %>%
  mutate(ano_emissao = as.numeric(as.character(ano_emissao)))

# Now combine both datasets
full_combined <- bind_rows(
  past_data_with_labels,
  dados_agregados
) %>%
  arrange(ano_emissao)

# View the result
full_combined


# Set scale parameters
max_operacoes <- 2000000  # 2 million operations
max_valor <- max(full_combined$Valor, na.rm = TRUE)
scale_factor <- max_valor / max_operacoes
operacoes_breaks <- seq(0, 2000000, by = 500000)  # Breaks every 500k

# Create formatted labels
full_combined <- full_combined %>%
  mutate(
    Valor_label = ifelse(Valor >= 1e9,
                         paste0(" ", format(Valor/1e9, digits=1, decimal.mark=","), "B "),
                         paste0(" ", format(Valor/1e6, digits=1, decimal.mark=","), "M ")),
    Operacoes_label = paste0(format(Operações/1e6, digits=2, decimal.mark=","), " M")
  )

# Plot 1: Dual-axis plot (Value and Operations)
ggplot(full_combined, aes(x = as.factor(ano_emissao))) +
  geom_col(aes(y = Valor, fill = "Valor (R$)"), 
           alpha = 0.6, width = 0.7) +
  # Modified text position and color for Valor labels
  geom_text(aes(y = Valor, label = Valor_label),
            vjust = -0.5, color = "black", size = 3.5) +
  geom_line(aes(y = Operações * scale_factor, group = 1, color = "Operações"), 
            linewidth = 1) +
  geom_point(aes(y = Operações * scale_factor, color = "Operações"), 
             size = 3) +
  geom_text(aes(y = Operações * scale_factor, label = Operacoes_label),
            vjust = -1, color = "firebrick", size = 3.5) +
  scale_y_continuous(
    name = "Valor Total (R$)",
    labels = function(x) {
      ifelse(x >= 1e9,
             paste0("R$ ", format(x/1e9, digits=1, decimal.mark=","), " B"),
             paste0("R$ ", format(x/1e6, digits=1, decimal.mark=","), " M"))
    },
    breaks = scales::pretty_breaks(n = 5),
    sec.axis = sec_axis(
      trans = ~./scale_factor,
      name = "Número de Operações",
      labels = function(x) paste0(format(x/1e6, digits=2, decimal.mark=","), " M"),
      breaks = operacoes_breaks
    )
  ) +
  scale_color_manual(name = NULL, values = c("Operações" = "firebrick")) +
  scale_fill_manual(name = NULL, values = c("Valor (R$)" = "steelblue")) +
  labs(x = "Ano") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.y.left = element_text(color = "steelblue", size = 10),
    axis.title.y.right = element_text(color = "firebrick", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y.left = element_text(color = "steelblue"),
    axis.text.y.right = element_text(color = "firebrick"),
    axis.line.y = element_blank(),
    legend.position = "top",
    legend.box = "horizontal",
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # Add some extra space at the top for the labels
  expand_limits(y = max(full_combined$Valor) * 1.1)

# Plot 2: Average Value per Operation
y_breaks <- seq(0, max(full_combined$Valor_Medio/1000, na.rm = TRUE) + 5, by = 5)

ggplot(full_combined, aes(x = as.factor(ano_emissao), y = Valor_Medio/1000, group = 1)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 3) +
  scale_y_continuous(
    labels = function(x) {
      paste0("R$ ", format(x, big.mark=".", decimal.mark=","), " mil")
    },
    breaks = y_breaks,
    limits = c(0, max(y_breaks)),
    expand = expansion(mult = c(0, 0.15))  # Slightly more expansion for repel
  ) +
  labs(
    x = "Ano",
    y = "Valor Médio por Operação (R$ mil)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black")
  ) +
  geom_text_repel(
    aes(label = paste0(" ", format(round(Valor_Medio/1000, 1), 
                                   big.mark=".", decimal.mark=","), " mil")),
    color = "darkgreen",
    size = 3.5,
    fontface = "bold",
    direction = "y",       # Prefer vertical movement
    box.padding = 0.5,     # Padding around text
    point.padding = 0.3,   # Padding around points
    min.segment.length = 0.1,  # Always draw segments
    segment.color = "grey50",
    seed = 42              # For reproducibility
  )


#MAX E MIN
combined_data <- combined_data %>%
  filter(valor_total_contratos > 1, 
         quantidade_contratos > 0)

summary_table <- combined_data %>%
  group_by(ano_emissao) %>%
  summarise(
    `Valor Total (R$)` = sum(valor_total_contratos, na.rm = TRUE),
    `Operações` = sum(quantidade_contratos, na.rm = TRUE),
    `Valor Médio por Operação (R$)` = sum(valor_total_contratos, na.rm = TRUE) / sum(quantidade_contratos, na.rm = TRUE),
    `Valor Mediano por Operação (R$)` = median(valor_total_contratos / quantidade_contratos, na.rm = TRUE),
    `Valor Máximo de 1 Operação (R$)` = max(valor_total_contratos / quantidade_contratos, na.rm = TRUE),
    `Valor Mínimo de 1 Operação (R$)` = min(valor_total_contratos / quantidade_contratos, na.rm = TRUE)
  ) %>%
  rename(Ano = ano_emissao) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .))) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# Format numeric columns with Brazilian formatting
summary_table_formatted <- summary_table %>%
  mutate(
    across(c(`Valor Total (R$)`, `Valor Médio por Operação (R$)`, 
             `Valor Mediano por Operação (R$)`, `Valor Máximo de 1 Operação (R$)`,
             `Valor Mínimo de 1 Operação (R$)`),
           ~ format(., big.mark = ".", decimal.mark = ",", nsmall = 2))
  )

# Export to Excel
write_xlsx(
  list("Valor_contratos" = summary_table_formatted),
  "valor_contratos2.xlsx"
)
# Plot 3: Subprogramas over time
names(combined_data)

unique(combined_data$subprograma)

# Create a named vector for renaming
library(RColorBrewer)

subprogram_data <- combined_data %>%
  filter(!is.na(subprograma)) %>%
  mutate(
    # First round of name shortening
    subprograma_short = case_when(
      grepl("MAIS ALIMENTOS", subprograma) ~ "Mais Alimentos",
      grepl("MICROCRÉDITO", subprograma) ~ "Microcrédito Rural",
      grepl("CUSTEIO", subprograma) ~ "Custeio",
      grepl("REFORMA AGRÁRIA.*PNCF", subprograma) ~ "Reforma Agrária PNCF",
      grepl("REFORMA AGRÁRIA", subprograma) ~ "Reforma Agrária",
      grepl("SEMIÁRIDO", subprograma) ~ "Semiárido",
      grepl("AGROINDÚSTRIA", subprograma) ~ "Agroindústria",
      grepl("PRODUTIVO ORIENTADO", subprograma) ~ "Produtivo Orientado",
      grepl("MULHER", subprograma) ~ "Mulher",
      grepl("JOVEM", subprograma) ~ "Jovem",
      grepl("BIOECONOMIA.*SILVICULTURA", subprograma) ~ "ABC+ Silvicultura",
      grepl("BIOECONOMIA", subprograma) ~ "Bioeconomia",
      grepl("FLORESTA", subprograma) ~ "Agroflorestal",
      grepl("INDUSTRIALIZAÇÃO", subprograma) ~ "Industrialização",
      grepl("AGROECOLOGIA", subprograma) ~ "Agroecologia",
      grepl("PRONAMP", subprograma) ~ "PRONAMP",
      grepl("COTAS PARTES", subprograma) ~ "Cotas Partes",
      grepl("FGPP", subprograma) ~ "FGPP",
      TRUE ~ subprograma
    )
  ) %>%
  group_by(ano_emissao, subprograma_short) %>%
  summarise(valor = sum(valor_total_contratos, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    ano_emissao = as.factor(ano_emissao),
    subprograma_short = fct_reorder(subprograma_short, -valor)  # Order by total value
  )

# 2. Create a 19-color palette
n_colors <- length(unique(subprogram_data$subprograma_short))
subprogram_palette <- c(
  "#4E79A7", "#A0CBE8", "#F28E2B", "#FFBE7D", "#59A14F",  # Tableau 10
  "#8CD17D", "#B6992D", "#F1CE63", "#499894", "#86BCB6", # Extended analogs
  "#E15759", "#FF9D9A", "#79706E", "#BAB0AC", "#D37295", # Complementary
  "#FABFD2", "#B07AA1"                                  # Extra colors
)

# 3. Plot with strategic labeling
ggplot(subprogram_data, 
       aes(x = ano_emissao, y = valor/1e9, fill = subprograma_short)) +
  geom_col(position = "stack", width = 0.8) +
  # Label only segments >5% of annual total
  scale_fill_manual(values = subprogram_palette) +
  scale_y_continuous(
    name = "Valor Investido (Bilhões de R$)",
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    fill = "Subprograma",
    x = "Ano",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.4, "cm"),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  ) +
  guides(fill = guide_legend(ncol = 1, override.aes = list(size = 3)))

# Plot 4: Pronaf por região
unique(combined_data$uf)

# Define state to region mapping
estado_para_regiao <- function(uf) {
  case_when(
    uf %in% c("RS", "SC", "PR") ~ "Sul",
    uf %in% c("SP", "RJ", "MG", "ES") ~ "Sudeste",
    uf %in% c("MT", "MS", "GO", "DF") ~ "Centro-Oeste",
    uf %in% c("AM", "AC", "RR", "RO", "PA", "AP", "TO") ~ "Norte",
    uf %in% c("BA", "SE", "AL", "PE", "PB", "RN", "CE", "PI", "MA") ~ "Nordeste",
    TRUE ~ NA_character_
  )
}
combined_data <- combined_data %>%
  mutate(
    regiao = estado_para_regiao(uf),
    regiao = factor(regiao, 
                    levels = c("Norte", "Nordeste", "Centro-Oeste", 
                               "Sudeste", "Sul"))
  )

# Check distribution
combined_data %>%
  count(uf, regiao) %>%
  arrange(regiao, uf)

# 2. Apply mapping and prepare data
regional_data <- combined_data %>%
  mutate(
    regiao = estado_para_regiao(uf),
    regiao = factor(regiao, levels = c("Norte", "Nordeste", "Centro-Oeste",
                                       "Sudeste", "Sul")),
    ano_emissao = as.factor(ano_emissao)  # Ensure proper year format
  ) %>%
  group_by(regiao, ano_emissao) %>%
  summarise(
    valor_total = sum(valor_total_contratos, na.rm = TRUE),
    quantidade_contratos = sum(quantidade_contratos, na.rm = TRUE),
    valor_medio = (valor_total/quantidade_contratos),
    .groups = "drop"
  )
view(regional_data)
ggplot(regional_data, 
       aes(x = factor(ano_emissao),  # Treat years as factors
           y = valor_total/1e9, 
           color = regiao,
           group = regiao)) +  # Important for connecting lines
  geom_line(linewidth = 1.2) +  # This will connect points within each region
  geom_point(size = 3) +
  scale_y_continuous(
    "Valor Investido (Bilhões de R$)",
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  scale_x_discrete("Ano") +  # Changed to discrete scale for factor years
  scale_color_brewer(
    "Região",  # Legend title
    palette = "Set1",
    guide = guide_legend(reverse = FALSE)  # Control legend order
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",  # Show legend on right
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(face = "bold")
  )

# Plot 5
names(combined_data)
unique(combined_data$uf)

# 1. Load Brazil states shapefile
br_shp <- geobr::read_state(year = 2020)

# 2. Prepare investment data by state (using valor_total_contratos)
state_invest <- combined_data %>%
  group_by(uf) %>%
  summarise(TotalInvest = sum(valor_total_contratos, na.rm = TRUE))

# 3. Join with spatial data (matching UF codes)
br_shp <- br_shp %>% 
  left_join(state_invest, by = c("abbrev_state" = "uf"))

# 4. Create Brazil map plot with improved formatting
ggplot(br_shp) +
  geom_sf(aes(fill = TotalInvest), color = "white", size = 0.2) +
  geom_sf_text(aes(label = abbrev_state), 
               color = "black", 
               size = 3) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "gray90",
    labels = scales::dollar_format(prefix = "R$ ", big.mark = ".", decimal.mark = ","),
    trans = "log10",
    name = "Investimento Total (R$)",
    guide = guide_colorbar(
      title.position = "top",
      barwidth = unit(10, "cm"),
      barheight = unit(0.5, "cm"))
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title.align = 0.5,
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(size = 11, vjust = 0.8)
  )

# Plot SC 

# 1. Filter for SC and aggregate by municipality
sc_invest <- combined_data %>%
  filter(uf == "SC") %>%
  mutate(municipio = str_to_title(municipio)) %>%
  group_by(municipio) %>%
  summarise(TotalInvest = sum(valor_total_contratos, na.rm = TRUE))

# 2. Get SC municipalities shapefile
sc_shp <- geobr::read_municipality(code_muni = "SC", year = 2020)

# 3. Join data with shapefile
sc_shp <- sc_shp %>% 
  left_join(sc_invest, by = c("name_muni" = "municipio"))

# 4. Create the map plot with fixed number formatting
format_currency <- function(x) {
  # Handle NA values
  x <- na.omit(x)
  if (length(x) == 0) return(character(0))
  
  # Format as currency with appropriate decimal places
  scales::dollar(x, 
                 prefix = "R$ ", 
                 big.mark = ".", 
                 decimal.mark = ",",
                 accuracy = ifelse(all(x > 100), 1, 0.01))
}

# Create the plot
ggplot(sc_shp) +
  geom_sf(aes(fill = TotalInvest), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "gray90",
    trans = "log10",
    breaks = clean_breaks,
    labels = scales::dollar_format(prefix = "R$ ", big.mark = ".", decimal.mark = ","),
    name = "Investimento Total"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.text = element_text(size = 10)
  ) +
  guides(fill = guide_colorbar(
    barheight = unit(5, "cm"),
    barwidth = unit(0.5, "cm")
  ))

# Plot 6: Rural Tourism

names(combined_data)
unique(combined_data$produto_agricola)

# Prepare the data
library(stringr)

plot_data <- combined_data %>%
  # Clean product names (truncate at first comma or parenthesis)
  mutate(
    produto_agricola_clean = case_when(
      str_detect(produto_agricola, "^TERRAÇOS|^Terraços|^terraços") ~ "TERRAÇOS",
      str_detect(produto_agricola, "^ARMAZÉM|^Armazém|^armazém") ~ "ARMAZÉM",
      str_detect(produto_agricola, "^COLHEITADEIRAS|^Colheitadeiras|^colheitadeiras") ~ "COLHEITADEIRAS",
      TRUE ~ str_extract(produto_agricola, "^[^,]+")  
    )
  ) %>%
  # Group by cleaned names
  group_by(produto_agricola_clean) %>%
  summarise(TotalInvest = sum(valor_total_contratos, na.rm = TRUE)) %>%
  arrange(desc(TotalInvest)) %>%
  # Keep top 15 + "TURISMO E LAZER RURAL"
  filter(row_number() <= 15 | produto_agricola_clean == "TURISMO E LAZER RURAL") %>%
  # Highlight the special product
  mutate(highlight = ifelse(produto_agricola_clean == "TURISMO E LAZER RURAL", "Highlight", "Normal"))

# Create the plot
ggplot(plot_data, aes(
  x = reorder(produto_agricola_clean, TotalInvest), 
  y = TotalInvest, 
  fill = highlight
)) + 
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0("R$ ", round(x/1e9, 1), " bi"),  # Format as billions
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_manual(values = c("Highlight" = "#E69F00", "Normal" = "#56B4E9")) +
  labs(
    x = "Produtos Agrícolas",
    y = "Investimento Total (R$)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(color = "gray50"),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank()
  ) +
  geom_text(
    aes(label = paste0("R$ ", round(TotalInvest/1e9, 1), " bi")),  # Billions in labels
    hjust = -0.1,
    size = 3.5,
    color = "black"
  )

# Plot 7: Rural Tourism in subprograms

# Filter for "TURISMO E LAZER RURAL" and summarize by subprogram
# 1. Filter and prepare the data
plot_data <- combined_data %>%
  filter(produto_agricola == "TURISMO E LAZER RURAL") %>%
  mutate(
    # Handle NA values first
    subprograma = ifelse(is.na(subprograma), "PROGRAMA NÃO INFORMADO", subprograma),
    # Shorten subprogram names
    subprograma_short = case_when(
      grepl("MAIS ALIMENTOS", subprograma, ignore.case = TRUE) ~ "Mais Alimentos",
      grepl("MICROCRÉDITO", subprograma, ignore.case = TRUE) ~ "Microcrédito Rural",
      grepl("CUSTEIO", subprograma, ignore.case = TRUE) ~ "Custeio",
      grepl("REFORMA AGRÁRIA.*PNCF", subprograma, ignore.case = TRUE) ~ "Reforma Agrária PNCF",
      grepl("REFORMA AGRÁRIA", subprograma, ignore.case = TRUE) ~ "Reforma Agrária",
      grepl("SEMIÁRIDO", subprograma, ignore.case = TRUE) ~ "Semiárido",
      grepl("AGROINDÚSTRIA", subprograma, ignore.case = TRUE) ~ "Agroindústria",
      grepl("PRODUTIVO ORIENTADO", subprograma, ignore.case = TRUE) ~ "Produtivo Orientado",
      grepl("MULHER", subprograma, ignore.case = TRUE) ~ "Mulher",
      grepl("JOVEM", subprograma, ignore.case = TRUE) ~ "Jovem",
      grepl("BIOECONOMIA.*SILVICULTURA", subprograma, ignore.case = TRUE) ~ "ABC+ Silvicultura",
      grepl("BIOECONOMIA", subprograma, ignore.case = TRUE) ~ "Bioeconomia",
      grepl("FLORESTA", subprograma, ignore.case = TRUE) ~ "Agroflorestal",
      grepl("INDUSTRIALIZAÇÃO", subprograma, ignore.case = TRUE) ~ "Industrialização",
      grepl("AGROECOLOGIA", subprograma, ignore.case = TRUE) ~ "Agroecologia",
      grepl("PRONAMP", subprograma, ignore.case = TRUE) ~ "PRONAMP",
      grepl("COTAS PARTES", subprograma, ignore.case = TRUE) ~ "Cotas Partes",
      grepl("FGPP", subprograma, ignore.case = TRUE) ~ "FGPP",
      TRUE ~ subprograma  # Keep original if no match
    )
  ) %>%
  group_by(subprograma_short) %>%
  summarise(TotalInvest = sum(valor_total_contratos, na.rm = TRUE)) %>%
  arrange(desc(TotalInvest))

# 2. Create the plot
ggplot(plot_data, 
       aes(x = reorder(subprograma_short, TotalInvest), 
           y = TotalInvest, 
           fill = subprograma_short)) +
  geom_col(width = 0.8) +
  geom_text(
    aes(label = dollar(TotalInvest, prefix = "R$", big.mark = ".", decimal.mark = ",")),
    hjust = -0.1,
    size = 3.5,
    color = "black",
    fontface = "bold"
  ) +
  coord_flip()  +
  scale_y_continuous(
    labels = function(x) paste0("R$ ", round(x/1e6, 1), "M"),  # Format as billions
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_viridis_d(option = "D", guide = "none") +
  labs(
    x = NULL,
    y = "Investimento Total (R$)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#7f8c8d", hjust = 0.5),
    axis.text.y = element_text(size = 10, color = "#34495e"),
    axis.title.x = element_text(color = "#34495e", margin = margin(t = 10)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

# Plot 8: Rural Tourism in SC - map

# Plot SC 
# 1. Filter for SC and aggregate by municipality
sc_turism <- combined_data %>%
  filter(uf == "SC") %>%
  mutate(municipio = str_to_title(municipio)) %>%
  group_by(municipio, produto_agricola) %>%
  summarise(TotalInvest = sum(valor_total_contratos, na.rm = TRUE))

sc_turism <- sc_turism %>%
  filter(produto_agricola == "TURISMO E LAZER RURAL")

# 2. Get SC municipalities shapefile
sc_shp <- geobr::read_municipality(code_muni = "SC", year = 2020)

# 3. Join data with shapefile
sc_shp2 <- sc_shp %>% 
  left_join(sc_turism, by = c("name_muni" = "municipio"))

# 4. Create the map plot with fixed number formatting
format_currency <- function(x) {
  # Handle NA values
  x <- na.omit(x)
  if (length(x) == 0) return(character(0))
  
  # Format as currency with appropriate decimal places
  scales::dollar(x, 
                 prefix = "R$ ", 
                 big.mark = ".", 
                 decimal.mark = ",",
                 accuracy = ifelse(all(x > 100), 1, 0.01))
}

# Create the plot
ggplot(sc_shp) +
  geom_sf(aes(fill = TotalInvest), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "gray90",
    trans = "log10",
    breaks = clean_breaks,
    labels = scales::dollar_format(prefix = "R$ ", big.mark = ".", decimal.mark = ","),
    name = "Investimento Total"
  ) +
  labs(
    title = "Investimento do PRONAF nos Municípios Catarinenses",
    subtitle = "Escala logarítmica | Valores em reais (R$)",
    caption = "Fonte: Banco Central do Brasil"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.text = element_text(size = 10)
  ) +
  guides(fill = guide_colorbar(
    barheight = unit(5, "cm"),
    barwidth = unit(0.5, "cm")
  ))

## Turismo Rural em SC - Pronaf
Mun_prod <- read.csv("data/Contratos de Investimento por Município e Produto2.csv")
UFcod <- read.csv("data/Contratos por Município.csv")
Prog <- read.csv("data/Contratos por Programa, Subprograma e UF.csv")


# Organizar Mun-prod
UFcod <- UFcod %>% distinct()
Prog <- Prog %>% distinct()

# Arrumar codigos por nomes

Mun_prod <- Mun_prod %>%
  left_join(UFcod, by = "cdEstado")

Mun_prod <- Mun_prod %>%
  left_join(Prog, by = c("cdPrograma", "cdSubPrograma"))

Mun_prod <- select(Mun_prod, -c(cdFonteRecurso, cdPrograma,
                                cdSubPrograma, cdEstado))

Mun_prod$VlInvest <- as.numeric(Mun_prod$VlInvest)

Mun_prod <- filter(Mun_prod, nomePrograma == "PRONAF - PROGRAMA NA...")

names(Mun_prod)
str(Mun_prod)

write.csv(Mun_prod, "data/mun_pronaf.csv", row.names = FALSE)

unique(Mun_prod$nomeProduto)
Mun_prod <- Mun_prod %>%
  mutate(
    nomeProduto = str_remove_all(nomeProduto, '^"|"$'),
    nomeProduto = str_trim(nomeProduto),
    Municipio = str_trim(Municipio),
    nomeUF = str_trim(nomeUF)
  )

Mun_prod <- Mun_prod %>%
  mutate(
    Municipio = as.factor(Municipio),
    nomeUF = as.factor(nomeUF),
    nomeProduto = as.factor(nomeProduto),
    nomeSubPrograma = as.factor(nomeSubPrograma)
  )

product_ranking <- Mun_prod %>%
  group_by(nomeProduto) %>%
  summarise(TotalInvest = sum(VlInvest, na.rm = TRUE)) %>%
  arrange(desc(TotalInvest))

view(product_ranking)


product_categories <- c(
  "ANIMAIS", "MAQUINÁRIO", "INFRAESTRUTURA", "FRUTAS", "HORTALIÇAS", "ÁRVORES", "TURISMO RURAL", 
  "CAFÉ", "RECUPERAÇÃO AMBIENTAL", "AGROARTESANATO", "ENERGIA",
  "FORRAGEIRAS", "IRRIGAÇÃO", "SOLO", "LAVOURA", "TRANSPORTE", "ÁGUA", "ESCOLAS",
  "FLORES", "PRESTAÇÃO DE SERVIÇOS", "OUTROS"
)

Mun_prod <- Mun_prod %>%
  mutate(
    product_category = case_when(
      str_detect(nomeProduto, "ANIMAIS|PEIXE|CARCINICULTURA|MUARES|ASININOS|SERICICULTURA|REPRODUTORES|PISCICULTURA|BOVINOS|OVINOS|SUÍNOS|BÚFALOS|CAPRINOS|AVICULTURA|PESCADO|APICULTURA|ABELHAS|EQUINOS") ~ "ANIMAIS",
      str_detect(nomeProduto, "TURISMO") ~ "TURISMO RURAL",
      str_detect(nomeProduto, "FLORESTAMENTO|AMBIENTAL") ~ "RECUPERAÇÃO AMBIENTAL",
      str_detect(nomeProduto, "EUCALIPTO|MADEIRA|PINUS|CEDRO|KIRI|ARAUCÁRIA|ACÁCIA NEGRA") ~ "ÁRVORES",
      str_detect(nomeProduto, "AGROARTESANATO|SIRGARIAS|VIME") ~ "AGROARTESANATO",
      str_detect(nomeProduto, "ENERGIA|GERADORES") ~ "ENERGIA",
      str_detect(nomeProduto, "PASTAGEM|ALFAFA") ~ "FORRAGEIRAS",
      str_detect(nomeProduto, "CAFÉ|CAFEZAIS") ~ "CAFÉ",
      str_detect(nomeProduto, "IRRIGAÇÃO") ~ "IRRIGAÇÃO",
      str_detect(nomeProduto, "SOLO|ADUBAÇÃO") ~ "SOLO",
      str_detect(nomeProduto, "FRUT|PITAYA|MANGABA|NESPERA|CEREJA|CAJÁ|CARAMBOLA|TORANJA|SAPOTI|TAPEREBÁ|JABUTICABA|MARMELO|LICHIA|TAMARINDO|PERA|FIGO|LIMA|ATEMOIA|GUARIROBA|NECTARINA|MIRTILO|CASTANHA|CUPUAÇU|PINHA|FRAMBOESA|AMORA|KIWI|UVA|CAQUI|GRAVIOLA|MAÇÃ|MAMÃO|ACEROLA|ABACATE|MORANGO|COCO|URUCUM|AMEIXA|PÊSSEGO|GOIABA|ABACAXI|MANGA|LIMÃO|BANANA|MELANCIA|MELÃO|MAMÃO|CAJU|MARACUJÁ|TANGERINA|CACAU|LARANJA|AÇAÍ") ~ "FRUTAS",
      str_detect(nomeProduto, "HORTALIÇ|ALFACE|TOMATE|CHUCHU|PIMENTA|CEBOLA") ~ "HORTALIÇAS",
      str_detect(nomeProduto, "CANA-DE-AÇUCAR|MILHO|SOJA|MANDIOCA|ALGODÃO") ~ "LAVOURA",
      str_detect(nomeProduto, "MÁQUIN|EQUIP|EMBARCA|EMPILHADEIRA|TRATOR|IMPLEMENT|CULTIVADOR|COLHEITADEIRAS|CARRETAS|REBOQUES") ~ "MAQUINÁRIO",
      str_detect(nomeProduto, "CAMINHÕES|CAMIONETAS|BICICLETAS|AVIÕES|JIPES|VEÍCULOS|ÔNIBUS|QUADRICLOS|MOTOCICLETAS") ~ "TRANSPORTE",
      str_detect(nomeProduto, "ÁGUA|POÇO") ~ "ÁGUA",
      str_detect(nomeProduto, "ESCOLAS") ~ "ESCOLAS",
      str_detect(nomeProduto, "FLORES") ~ "FLORES",
      str_detect(nomeProduto, "SERVIÇOS|ASSESORIA") ~ "PRESTAÇÃO DE SERVIÇOS",
      str_detect(nomeProduto, "INSTALA|TULHA|ESTUFA|DESPOLPADOR|DEPÓSITO|FRIGORÍFICO|UNIDADE|TANQUES|CONSTRU|ARMAZ|FERRAMENTAS|GALPÃO|UTENSÍLIOS|GRANJAS|SILO|TERRAÇOS|ESTUFAS|RESIDÊNCIAS|ELETRIFICAÇÃO|AGROINDÚSTRIA") ~ "INFRAESTRUTURA",
      TRUE ~ "OUTROS"
    )
  )


# Filtragens SC, Turismo e Turismo em SC
SC_prod <- filter(Mun_prod, Mun_prod$nomeUF == "SC")
Turism_sc <- filter(Mun_prod, Mun_prod$nomeProduto == '"TURISMO E LAZER RURAL"')
Turism_sc <- filter(Turism_sc, Turism_sc$nomeUF == "SC")

# PLOT 1: Mapa SC pronaf geral
# Map plot

sc_shp_sc <- geobr::read_municipality(code_muni = "SC", year = 2020)

SC_prod_sc <- SC_prod %>%
  mutate(Municipio = str_to_title(Municipio))

mun_invest <- SC_prod_sc %>%
  group_by(Municipio) %>%
  summarise(TotalInvest = sum(VlInvest, na.rm = TRUE))

sc_shp_sc <- sc_shp_sc %>% 
  left_join(mun_invest, by = c("name_muni" = "Municipio"))

# Base map
ggplot() +
  geom_sf(data = sc_shp_sc, aes(fill = TotalInvest), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "gray90",
    labels = scales::dollar_format(prefix = "R$ "),
    trans = "log10",
    breaks = c(1e5, 1e6, 1e7, 1e8, 1e9),  # 100,000 to 1,000,000,000
    limits = c(1e5, 1e9)  # Ensures the scale covers this range
  ) +
  labs(fill = "Investimento total (R$)") +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# PLOT 2: Mapa SC pronaf turismo
sc_shp_sc <- geobr::read_municipality(code_muni = "SC", year = 2020)

SC_prod_turism <- Turism_sc %>%
  mutate(Municipio = str_to_title(Municipio))

mun_invest <- SC_prod_turism %>%
  group_by(Municipio) %>%
  summarise(TotalInvest = sum(VlInvest, na.rm = TRUE)) %>%
  filter(TotalInvest > 0)


min(mun_invest$TotalInvest)
sc_shp_sc <- sc_shp_sc %>% 
  left_join(mun_invest, by = c("name_muni" = "Municipio"))

# Base map
ggplot() +
  geom_sf(data = sc_shp_sc, aes(fill = TotalInvest), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "gray90",
    labels = scales::dollar_format(prefix = "R$ "),
    trans = "log10",
    breaks = c(1e4, 3e4, 1e5, 3e5, 1e6, 3e6),  # 100,000 to 1,000,000,000
    limits = c(1e4, 3e6)  # Ensures the scale covers this range
  ) +
  labs(fill = "Investimento total (R$)") +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# PLOT 3: Quantidade de contratos por ano em SC no turismo rural
Qtd_op <- Turism_sc %>%
  group_by(AnoEmissao) %>%
  summarise(QtdContratos = n()) %>%
  arrange(AnoEmissao)

ggplot(Qtd_op, aes(x = AnoEmissao, y = QtdContratos)) +
  geom_line(color = "#1f77b4", linewidth = 1) +  # Blue line
  geom_point(color = "#00008B", size = 3) +      # Orange points
  geom_text(aes(label = QtdContratos), 
            vjust = -1, color = "black", size = 3.5) + # Annotate values
  labs(x = "Ano de Emissão",
       y = "Número de Contratos") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2)))

# PLOT 4: Lista TOP 10 municipios turismo rural
ranked_data <- Turism_sc %>%
  group_by(Municipio) %>%
  filter(VlInvest > 0) %>%
  summarise(TotalInvest = sum(VlInvest, na.rm = TRUE)) %>%
  arrange(desc(TotalInvest)) %>%
  mutate(
    Rank = row_number(),
    Label = paste0(Rank, ". ", Municipio),
    Highlight = Municipio %in% c("SANTA ROSA DE LIMA", "PRAIA GRANDE", 
                                 "PRESIDENTE NEREU", "CONCÓRDIA", 
                                 "SEARA", "JOINVILLE", 
                                 "GAROPABA", "ITAPIRANGA", "URUBICI", "CAMPO ALEGRE")
  )

plot_data <- ranked_data %>%
  filter(Rank <= 10 | Highlight) %>%
  arrange(desc(TotalInvest)) %>%
  mutate(Label = factor(Label, levels = Label))

# Create custom dollar formatter with Brazilian decimal format
real_format <- function(x) {
  paste0("R$", format(round(x, 0), nsmall = 0, big.mark = ".", decimal.mark = ","))
}

ggplot(plot_data, aes(x = reorder(Label, TotalInvest), y = TotalInvest, 
                      fill = Highlight)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", 
                                                    big.mark = ".",
                                                    decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("TRUE" = "#FFA500", "FALSE" = "#9370DB")) +
  labs(x = "Município", y = "Investimento total") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank()) +
  geom_text(
    aes(
      label = scales::number(TotalInvest, accuracy = 1, 
                             big.mark = ".", decimal.mark = ",",
                             prefix = "R$ "),
      hjust = ifelse(TotalInvest < 500000, -0.1, 1.1))
    ,
    size = 3
  )

# PLOT 5: Quantidade de municipios por ano em SC no turismo rural
Qtd_mun <- Turism_sc %>%
  group_by(AnoEmissao) %>%
  summarise(QtdMunicipios = n_distinct(Municipio)) %>%
  arrange(AnoEmissao)

ggplot(Qtd_mun, aes(x = AnoEmissao, y = QtdMunicipios)) +
  geom_bar(stat = "identity", fill = "#1f77b4", width = 0.7) +  # Blue bars
  geom_text(aes(label = QtdMunicipios), 
            vjust = -1, color = "black", size = 3.5) + # Annotate values
  labs(x = "Ano",
       y = "Quantidade de Municípios") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    panel.grid = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +
  scale_x_continuous(breaks = unique(Qtd_mun$AnoEmissao))  # Show all years on x-axis

#### TABELA
# Step 2: Calculate total investment per year
total_investment <- combined_data %>%
  group_by(ano_emissao) %>%
  summarise(total_investment = sum(valor_total_contratos, na.rm = TRUE))

# Step 3: Filter rural tourism investments (adjust the condition as needed)
rural_tourism_investment <- combined_data %>%
  filter(
    produto_agricola == "TURISMO E LAZER RURAL"
  ) %>%
  group_by(ano_emissao) %>%
  summarise(rural_tourism = sum(valor_total_contratos, na.rm = TRUE))

# Step 4: Merge and calculate percentage
final_table <- total_investment %>%
  left_join(rural_tourism_investment, by = "ano_emissao") %>%
  mutate(
    rural_tourism = replace_na(rural_tourism, 0),  # Replace NA with 0 if no tourism data
    percent_tourism = (rural_tourism / total_investment) * 100
  ) %>%
  arrange(ano_emissao)  # Sort by year

# Step 5: Print the table
print(final_table)

# 1. Define the municipalities of interest
target_municipalities <- c(
  "URUBICI", "SANTA ROSA DE LIMA", "CAMPO ALEGRE", "PRAIA GRANDE",
  "PRESIDENTE NEREU", "CONCÓRDIA", "SEARA", "JOINVILLE", 
  "GAROPABA", "ITAPIRANGA"
)
Mun_prod <- Mun_prod %>%
  mutate(VlInvest = as.numeric(VlInvest))
# 3. Calculate total investment per municipality (all years)
total_investment_mun <- Mun_prod %>%
  group_by(Municipio) %>%
  filter(Municipio %in% target_municipalities) %>%
  summarise(total_investment = sum(VlInvest, na.rm = TRUE))

Turism_sc <- Turism_sc %>%
  mutate(VlInvest = as.numeric(VlInvest))
# 4. Filter rural tourism investments (adjust 'nomeProduto' if needed)
rural_tourism_mun <- Turism_sc %>%
  filter(
    Municipio %in% target_municipalities) %>%
  group_by(Municipio) %>%
  summarise(rural_tourism = sum(VlInvest, na.rm = TRUE))

# 5. Merge and calculate percentage
final_table_mun <- total_investment_mun %>%
  full_join(rural_tourism_mun, by = "Municipio") %>%
  mutate(
    rural_tourism = replace_na(rural_tourism, 0),  # Replace NA with 0 if no tourism data
    percent_tourism = (rural_tourism / total_investment) * 100
  ) %>%
  arrange(desc(total_investment))  # Sort by highest total investment

# 6. Print the table
print(final_table_mun)

write_xlsx(final_table_mun, "pronaf_municipalities_summary2.xlsx")


## GERAL TURISMO RURAL
combined_data_t <- combined_data %>%
  filter(produto_agricola == "TURISMO E LAZER RURAL") %>%
  group_by(ano_emissao) %>%
  mutate(
    Valor_por_contrato = valor_total_contratos / quantidade_contratos
  )

combined_data_t <- combined_data_t %>%
  summarise(
    Operações = sum(quantidade_contratos, na.rm = TRUE),
    Valor = sum(valor_total_contratos, na.rm = TRUE),
    Valor_Max = max(Valor_por_contrato, na.rm = TRUE),
    Valor_Min = min(Valor_por_contrato, na.rm = TRUE),
    Valor_Med = median(Valor_por_contrato, na.rm = TRUE)
    ) %>%
  mutate(Valor_Medio = Valor / Operações)

# Set scale parameters
max_operacoes <- 1250  # 2 million operation
max_valor <- max(combined_data_t$Valor, na.rm = TRUE)
scale_factor <- max_valor / max_operacoes
operacoes_breaks <- seq(0, 1250, by = 250)  # Breaks every 500k

# Create formatted labels
combined_data_t <- combined_data_t %>%
  mutate(
    Valor_label = ifelse(Valor >= 1e9,
                         paste0(" ", format(Valor/1e9, digits=1, decimal.mark=","), "B "),
                         paste0(" ", format(Valor/1e6, digits=1, decimal.mark=","), "M ")),
    Operacoes_label = paste0(format(Operações/1, digits=2, decimal.mark=","), " ")
  )

# Plot 1: Dual-axis plot (Value and Operations)
ggplot(combined_data_t, aes(x = as.factor(ano_emissao))) +
  geom_col(aes(y = Valor, fill = "Valor (R$)"), 
           alpha = 0.6, width = 0.7) +
  # Modified text position and color for Valor labels
  geom_text(aes(y = Valor, label = Valor_label),
            vjust = -0.5, color = "black", size = 3.5) +
  geom_line(aes(y = Operações * scale_factor, group = 1, color = "Operações"), 
            linewidth = 1) +
  geom_point(aes(y = Operações * scale_factor, color = "Operações"), 
             size = 3) +
  geom_text(aes(y = Operações * scale_factor, label = Operacoes_label),
            vjust = -1, color = "firebrick", size = 3.5) +
  scale_y_continuous(
    name = "Valor Total (R$)",
    labels = function(x) {
      ifelse(x >= 1e9,
             paste0("R$ ", format(x/1e9, digits=1, decimal.mark=","), " B"),
             paste0("R$ ", format(x/1e6, digits=1, decimal.mark=","), " M"))
    },
    breaks = scales::pretty_breaks(n = 4),
    sec.axis = sec_axis(
      trans = ~./scale_factor,
      name = "Número de Operações",
      labels = function(x) paste0(format(x/1, digits=2, decimal.mark=","), " "),
      breaks = operacoes_breaks
    )
  ) +
  scale_color_manual(name = NULL, values = c("Operações" = "firebrick")) +
  scale_fill_manual(name = NULL, values = c("Valor (R$)" = "steelblue")) +
  labs(x = "Ano") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.y.left = element_text(color = "steelblue", size = 10),
    axis.title.y.right = element_text(color = "firebrick", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y.left = element_text(color = "steelblue"),
    axis.text.y.right = element_text(color = "firebrick"),
    axis.line.y = element_blank(),
    legend.position = "top",
    legend.box = "horizontal",
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  # Add some extra space at the top for the labels
  expand_limits(y = max(combined_data_t$Valor) * 1.1)

# Plot 2: Average Value per Operation
y_breaks <- seq(0, 70, by = 10)

ggplot(combined_data_t, aes(x = as.factor(ano_emissao), y = Valor_Medio/1000, group = 1)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 3) +
  scale_y_continuous(
    labels = function(x) {
      paste0("R$ ", format(x, big.mark=".", decimal.mark=","), " mil")
    },
    breaks = y_breaks,
    limits = c(0, max(y_breaks)),
    expand = expansion(mult = c(0, 0.15))  # Slightly more expansion for repel
  ) +
  labs(
    x = "Ano",
    y = "Valor Médio por Operação (R$ mil)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black")
  ) +
  geom_text_repel(
    aes(label = paste0(" ", format(round(Valor_Medio/1000, 1), 
                                   big.mark=".", decimal.mark=","), " mil")),
    color = "darkgreen",
    size = 3.5,
    fontface = "bold",
    direction = "y",       # Prefer vertical movement
    box.padding = 0.5,     # Padding around text
    point.padding = 0.3,   # Padding around points
    min.segment.length = 0.1,  # Always draw segments
    segment.color = "grey50",
    seed = 42              # For reproducibility
  )


#ADD MAX E MIN
# Plot 2: Adjusted for small min values
# First determine if we should show values in thousands or original units
combined_data_t <- combined_data_t %>%
  mutate(
    # Format min values in original units if < 1000, otherwise in thousands
    Min_Formatted = ifelse(Valor_Min < 1000,
                           paste0("R$ ", format(round(Valor_Min, 2), 
                                                big.mark=".", decimal.mark=",")),
                                  paste0("R$ ", format(round(Valor_Min/1000, 1), 
                                                       big.mark=".", decimal.mark=","), " mil")),
                           
                           Max_Formatted = paste0("R$ ", format(round(Valor_Max/1000, 1), 
                                                                big.mark=".", decimal.mark=","), " mil"),
                           Avg_Formatted = paste0("R$ ", format(round(Valor_Medio/1000, 1), 
                                                                big.mark=".", decimal.mark=","), " mil")
    )


# Create plot with adjusted scales
y_max <- max(combined_data_t$Valor_Max/1000, na.rm = TRUE)
y_breaks <- seq(0, ceiling(y_max * 1.1), by = ifelse(y_max > 50, 10, 5))

ggplot(combined_data_t, aes(x = as.factor(ano_emissao), y = Valor_Medio/1000, group = 1)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 3) +
  
  # Error bars for min/max range
  geom_errorbar(
    aes(ymin = Valor_Min/1000, ymax = Valor_Max/1000),
    width = 0.2,
    color = "darkgreen",
    alpha = 0.5
  ) +
  
  # Points for min/max
  geom_point(aes(y = Valor_Max/1000), color = "red", size = 2, shape = 1) +
  geom_point(aes(y = Valor_Min/1000), color = "blue", size = 2, shape = 1) +
  
  scale_y_continuous(
    breaks = y_breaks,
    limits = c(0, max(y_breaks)),
    expand = expansion(mult = c(0, 0.15))
  ) +
  
  labs(
    x = "Ano",
    y = "Valor por Operação",
    caption = "Valores médios em mil reais. Barras mostram variação mínimo-máximo."
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black")
  ) +
  
  # Label for average
  geom_text_repel(
    aes(label = Avg_Formatted),
    color = "darkgreen",
    size = 3.5,
    fontface = "bold",
    direction = "y",
    box.padding = 0.5,
    point.padding = 0.3,
    min.segment.length = 0.1,
    segment.color = "grey50",
    seed = 42
  ) +
  
  # Label for max (only if significantly different from average)
  geom_text_repel(
    aes(y = Valor_Max/1000, label = ifelse(Valor_Max/Valor_Medio > 1.5, Max_Formatted, "")),
    color = "red",
    size = 3,
    nudge_y = 1,
    direction = "y",
    segment.color = "grey50"
  ) +
  
  # Label for min (only if significantly different from average)
  geom_text_repel(
    aes(y = Valor_Min/1000, label = ifelse(Valor_Medio/Valor_Min > 1.5, Min_Formatted, "")),
    color = "blue",
    size = 3,
    nudge_y = -1,
    direction = "y",
    segment.color = "grey50"
  )


# REGION DATA for turism

# Define state to region mapping
estado_para_regiao <- function(uf) {
  case_when(
    uf %in% c("RS", "SC", "PR") ~ "Sul",
    uf %in% c("SP", "RJ", "MG", "ES") ~ "Sudeste",
    uf %in% c("MT", "MS", "GO", "DF") ~ "Centro-Oeste",
    uf %in% c("AM", "AC", "RR", "RO", "PA", "AP", "TO") ~ "Norte",
    uf %in% c("BA", "SE", "AL", "PE", "PB", "RN", "CE", "PI", "MA") ~ "Nordeste",
    TRUE ~ NA_character_
  )
}
combined_data_turism <- combined_data %>%
  mutate(
    regiao = estado_para_regiao(uf),
    regiao = factor(regiao, 
                    levels = c("Norte", "Nordeste", "Centro-Oeste", 
                               "Sudeste", "Sul"))
  ) %>%
  filter(produto_agricola == "TURISMO E LAZER RURAL")

# Check distribution
combined_data_turism %>%
  count(uf, regiao) %>%
  arrange(regiao, uf)

# 2. Apply mapping and prepare data
regional_data <- combined_data_turism %>%
  mutate(
    regiao = estado_para_regiao(uf),
    regiao = factor(regiao, levels = c("Norte", "Nordeste", "Centro-Oeste",
                                       "Sudeste", "Sul")),
    ano_emissao = as.factor(ano_emissao)  # Ensure proper year format
  ) %>%
  group_by(regiao, ano_emissao) %>%
  summarise(
    valor_total = sum(valor_total_contratos, na.rm = TRUE),
    quantidade_contratos = sum(quantidade_contratos, na.rm = TRUE),
    valor_medio = (valor_total/quantidade_contratos),
    .groups = "drop"
  )
view(regional_data)
ggplot(regional_data, 
       aes(x = factor(ano_emissao),  # Treat years as factors
           y = valor_total, 
           color = regiao,
           group = regiao)) +  # Important for connecting lines
  geom_line(linewidth = 1.2) +  # This will connect points within each region
  geom_point(size = 3) +
  scale_y_continuous(
    "Valor Investido (R$)",
    labels = function(x) {
      ifelse(x >= 1e6,
             paste0("R$ ", format(x/1e6, digits=1, decimal.mark=","), " M"),
             paste0("R$", format(x/1e3, digits=1, decimal.mark=","), " "))}
  ) +
  scale_x_discrete("Ano") +  # Changed to discrete scale for factor years
  scale_color_brewer(
    "Região",  # Legend title
    palette = "Set1",
    guide = guide_legend(reverse = FALSE)  # Control legend order
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",  # Show legend on right
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(face = "bold")
  )


