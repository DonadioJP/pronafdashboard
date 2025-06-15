# PRONAF Investment Analysis: Geospatial Evaluation of Agricultural Loans in Brazil

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-4.3.0+-blue.svg)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-1.7.0+-brightgreen.svg)](https://shiny.rstudio.com/)

![PRONAF Brazil Map](images/pronaf.jpg)

## Project Overview

This project analyzes the National Program for Family Agriculture (PRONAF) loans across Brazil from 2013-2024, evaluating R$64 billion (2024 value) in agricultural loans through geospatial analysis and interactive visualization.

Key features:
- Comprehensive evaluation at national, regional, and municipal levels
- Interactive RShiny dashboard for exploratory analysis
- Focused examination of specific product investments (e.g., rural tourism)
- Policy impact assessment and regional disparity analysis

## Data Sources

The primary datasets for this project are too large to host directly in this repository but can be obtained from the following sources:

1. **PRONAF Loan Records** - Official microdata from:
   [![Banco Central do Brasil](https://img.shields.io/badge/Source-BCB-1e40af)](https://www.bcb.gov.br/estabilidadefinanceira/micrrural)
   - Requires registration on the BCB website
   - Look for "Microdados do Crédito Rural" (Rural Credit Microdata)
   - Recommended filters: PRONAF program, 2013-2024 date range

2. **Geospatial Boundaries** (included in repository):
   
   ```bash
   # Brazilian municipal boundaries (2020) via geobr package
   municipalities <- geobr::read_municipality(code_muni="all", year=2020)
   ```
   
## Dashboard Demonstration

See the PRONAF dashboard in action with this walkthrough video:

[![PRONAF Dashboard Demo](https://img.youtube.com/vi/17zhB9qcFMc/0.jpg)](https://youtu.be/17zhB9qcFMc)

*(Click the image above to watch the full demonstration on YouTube)*

### Key Features Shown in Demo:
- Dynamic filtering by year and region
- Geospatial visualization of loan distribution by products
- Comparative analysis tools

## Repository Structure

pronafdashboard/

├── scripts/

│ ├── Analysis.R

│ ├── dashboard/

│ │ ├── data.R

│ │ ├── install.R

│ │ ├── app.R


├── LICENSE

└── README.md
