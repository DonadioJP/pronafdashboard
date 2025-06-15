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

ps: Data is too big to be uploaded here, but can be obtained at [![Banco Central do Brasil website](https://www.bcb.gov.br/estabilidadefinanceira/micrrural).

## Repository Structure
pronafdashboard/

├── scripts/

│ ├── 01_data_cleaning.R

│ ├── 02_geospatial.R

│ ├── 03_analysis.R

├── app.R

├── LICENSE

└── README.md
