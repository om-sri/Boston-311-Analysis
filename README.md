# Boston 311 Service Request Analysis

A comprehensive statistical analysis of Boston's 311 non-emergency service requests from 2015 to 2019, exploring patterns in request volumes, resolution times, neighborhood disparities, and predictive modeling of service outcomes.

## Overview

Boston's 311 system handles hundreds of thousands of non-emergency service requests annually — from pothole repairs to streetlight outages. This project dives deep into that data to uncover actionable insights about how the city responds to residents, which neighborhoods are underserved, and what factors drive faster (or slower) resolution times.

The analysis is split into two parts, each containing full end-to-end pipelines:

- **Analysis 1** — Full EDA, outlier management, logistic regression, linear regression, and LASSO on a subset of 311 data
- **Analysis 2** — Independent analysis with EDA, outlier treatment, all three regression methods, plus population-adjusted neighborhood analysis and presentation-ready visualizations

Both analyses follow a complete workflow: data cleaning → exploratory analysis → outlier detection & treatment → modeling → interpretation.

## Dataset

- **Source:** [Boston 311 Service Requests – Analyze Boston](https://data.boston.gov/dataset/311-service-requests)
- **Period:** 2015–2019
- **Key Fields:** Open/Close dates, Request Type, Department, Neighborhood, Source (phone, app, web), Location, On-Time Status

## Key Analyses & Methods

### Exploratory Data Analysis
- Yearly and monthly trends in 311 request volumes
- Department-level workload distribution
- Request source analysis (Citizens Connect app, phone, web, etc.)
- Geographic distribution across Boston neighborhoods
- Correlation analysis and feature profiling

### Outlier Detection & Treatment
- Identified and handled outliers in resolution times and request volumes using statistical methods (IQR, z-scores)
- Ensured model robustness by cleaning extreme values before fitting

### Predictive Modeling (Applied in Both Analyses)
- **Logistic Regression** — Predicting whether a 311 request will be resolved on time based on department, request type, source, and neighborhood
- **Linear Regression** — Modeling resolution time as a continuous outcome
- **LASSO Regularization** — Feature selection and shrinkage to identify the most influential predictors of service performance

### Population-Adjusted Neighborhood Analysis
- Per-capita 311 request rates by neighborhood using Census population data
- Identified neighborhoods with disproportionately high or low request volumes relative to their population
- Controlled for population size to surface true service demand hotspots vs. simply high-population areas

### Visualizations
- Professional presentation-quality bar charts, trend lines, and comparative plots
- All visuals designed for clarity and formatted for executive-level presentation

## Tech Stack

- **R** — Primary analysis language
- **tidyverse** (dplyr, ggplot2, tidyr) — Data wrangling and visualization
- **glmnet** — LASSO and Ridge regularization
- **caret** — Model training and evaluation
- **lubridate** — Date/time processing

## Project Structure

```
Boston-311-Analysis/
├── Analysis_1/          # Full EDA, outlier treatment, logistic/linear/LASSO regression
├── Analysis_2/          # Full EDA, outlier treatment, logistic/linear/LASSO, population analysis
└── README.md
```

## Key Findings

- Resolution times vary significantly across departments and neighborhoods
- The Citizens Connect app drove a major shift in how requests were submitted over the 2015–2019 period
- LASSO regularization identified department and request type as the strongest predictors of on-time resolution
- Population-adjusted analysis revealed that some smaller neighborhoods had disproportionately high per-capita request rates, highlighting potential service equity concerns
