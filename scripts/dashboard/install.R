# Set CRAN mirror first
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install binary versions where possible
install.packages(c(
  "shiny", "bs4Dash", "shinyjs", "leaflet", "leafdown", 
  "echarts4r", "dplyr", "tidyr", "RColorBrewer", 
  "htmlwidgets", "geobr", "stringr", "arrow", "tzdb"
), dependencies = TRUE)

# Install sf without compiling (use binary)
install.packages(c("sf", "stars"), type = "binary")  # Use binaries
