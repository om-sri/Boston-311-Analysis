# ============================================================================
# Boston 311 Service Requests (2015-2019)
# Final Analysis - Group 1
# ============================================================================

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(forcats)
library(janitor)
library(broom)
library(tidyr)
library(glmnet)
library(stargazer)

data_dir <- "C:/Users/Omsri/OneDrive/Documents/R-Files"

cases_all <- bind_rows(
  read_csv(file.path(data_dir, "2015.csv")) %>% mutate(file_year = 2015),
  read_csv(file.path(data_dir, "2016.csv")) %>% mutate(file_year = 2016),
  read_csv(file.path(data_dir, "2017.csv")) %>% mutate(file_year = 2017),
  read_csv(file.path(data_dir, "2018.csv")) %>% mutate(file_year = 2018),
  read_csv(file.path(data_dir, "2019.csv")) %>% mutate(file_year = 2019)
)

cases <- cases_all %>% clean_names()

# 2) DATA PREPARATION ----

cases <- cases %>%
  mutate(
    open_dt = parse_date_time(open_dt, orders = c("ymd HMS", "mdy HMS", "ymd HM", "mdy HM")),
    closed_dt = parse_date_time(closed_dt, orders = c("ymd HMS", "mdy HMS", "ymd HM", "mdy HM")),
    sla_target_dt = parse_date_time(sla_target_dt, orders = c("ymd HMS", "mdy HMS", "ymd HM", "mdy HM"))
  ) %>%
  filter(!is.na(open_dt)) %>%
  mutate(open_year = year(open_dt)) %>%
  filter(open_year >= 2015 & open_year <= 2019)

# 3) FEATURE ENGINEERING ----

cases <- cases %>%
  mutate(
    open_month = month(open_dt, label = TRUE, abbr = TRUE),
    open_weekday = wday(open_dt, label = TRUE, abbr = TRUE),
    is_weekend = if_else(open_weekday %in% c("Sat", "Sun"), "Weekend", "Weekday"),
    
    season = case_when(
      open_month %in% c("Dec", "Jan", "Feb") ~ "Winter",
      open_month %in% c("Mar", "Apr", "May") ~ "Spring",
      open_month %in% c("Jun", "Jul", "Aug") ~ "Summer",
      open_month %in% c("Sep", "Oct", "Nov") ~ "Fall"
    ),
    
    closure_days = as.numeric(difftime(closed_dt, open_dt, units = "days")),
    
    case_status_clean = str_to_lower(str_squish(as.character(case_status))),
    is_closed = case_when(
      case_status_clean %in% c("closed", "case closed") ~ 1,
      case_status_clean %in% c("open", "case open") ~ 0,
      TRUE ~ NA_real_
    ),
    
    on_time_text = str_to_lower(str_squish(as.character(on_time))),
    on_time_text = str_replace_all(on_time_text, "_", " "),
    on_time_flag = case_when(
      on_time_text %in% c("on time", "ontime", "yes", "y", "true", "t", "1") ~ 1,
      on_time_text %in% c("overdue", "late", "not on time", "no", "n", "false", "f", "0") ~ 0,
      TRUE ~ NA_real_
    )
  )

cases_clean <- cases %>% filter(!is.na(is_closed))

# NOW MERGE POPULATION DATA ----

# Read population data
pop_data <- read_csv(file.path(data_dir, "boston_population.csv"))

# Convert to long format
pop_long <- pop_data %>%
  rename(neighborhood = 1) %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "year", 
               values_to = "population") %>%
  mutate(open_year = as.numeric(year)) %>%
  select(neighborhood, open_year, population)

# Create combined populations to match 311 combined names
pop_combined <- pop_long %>%
  mutate(
    neighborhood_combined = case_when(
      # Combine Allston + Brighton
      neighborhood %in% c("Allston", "Brighton") ~ "Allston / Brighton",
      
      # Combine South Boston + South Boston Waterfront
      neighborhood %in% c("South Boston", "South Boston Waterfront") ~ "South Boston / South Boston Waterfront",
      
      # Map Fenway
      neighborhood %in% c("Fenway", "Kenmore", "Longwood")  ~ "Fenway / Kenmore / Audubon Circle / Longwood",
      
      # Map simple renames
      neighborhood == "Downtown" ~ "Downtown / Financial District",
      neighborhood == "Mattapan" ~ "Greater Mattapan",
      
      # Keep everything else as-is
      TRUE ~ neighborhood
    )
  ) %>%
  # Sum populations for combined neighborhoods
  group_by(neighborhood_combined, open_year) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
  rename(neighborhood = neighborhood_combined)

# Aggregate 311 cases by neighborhood (EXCLUDE Chestnut Hill)
cases_by_nbhd_year <- cases_clean %>%
  filter(neighborhood != "Chestnut Hill" | is.na(neighborhood)) %>%  # Drop Chestnut Hill
  group_by(neighborhood, open_year) %>%
  summarise(total_cases = n(), .groups = "drop")

# Merge population
merged_data <- cases_by_nbhd_year %>%
  left_join(pop_combined, by = c("neighborhood", "open_year"))

# Calculate per-capita rate (remove any remaining NAs)
merged_data <- merged_data %>%
  filter(!is.na(population)) %>%
  mutate(cases_per_1000 = (total_cases / population) * 1000)

library(sf)

# Download Boston neighborhood boundaries
# URL: https://data.boston.gov/dataset/boston-neighborhoods
neighborhoods_sf <- st_read("https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::boston-neighborhoods.geojson")

# Check the neighborhood names in the shapefile
cat("\n=== Neighborhoods in Shapefile ===\n")
print(unique(neighborhoods_sf$Name))

# ============================================================================
# VISUALIZATION: TOP 5 NEIGHBORHOODS - CASES PER 1,000 (YEAR-BY-YEAR) ----
# ============================================================================

# Identify top 5 neighborhoods by cases
top5_neighborhoods <- merged_data %>%
  group_by(neighborhood) %>%
  summarise(total_cases_all_years = sum(total_cases, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_cases_all_years)) %>%
  slice_head(n = 5) %>%
  pull(neighborhood)

# Filter data for top 5 neighborhoods
plot_data_top5 <- merged_data %>%
  filter(neighborhood %in% top5_neighborhoods)

# Create plot directory if it doesn't exist
plot_dir <- file.path(data_dir, "plots_final_analysis")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

# Create line plot
p_top5_trend <- ggplot(plot_data_top5, aes(x = factor(open_year), y = cases_per_1000, 
                                           group = neighborhood, color = neighborhood)) +
  geom_line(size = 1.3) +
  geom_point(size = 2.5) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Year - Cases per 1000 Residents",
    x = "Year",
    y = "Cases per 1,000 Residents",
    color = "Neighborhood"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(limits = c(150, 1000), expand = expansion(mult = c(0, 0.05)))  

# Save plot
ggsave(file.path(plot_dir, "top5_neighborhoods_cases_per_1000.png"), 
       p_top5_trend, width = 12, height = 7, dpi = 300)

# Print plot
print(p_top5_trend)

# Show which neighborhoods are top 5
cat("\n=== Top 5 Neighborhoods by Average Case Rate ===\n")
top5_summary <- merged_data %>%
  filter(neighborhood %in% top5_neighborhoods) %>%
  group_by(neighborhood) %>%
  summarise(
    avg_cases_per_1000 = round(mean(cases_per_1000), 1),
    min_year = round(min(cases_per_1000), 1),
    max_year = round(max(cases_per_1000), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_cases_per_1000))
print(top5_summary)

# ============================================================================
# LINE PLOT: TOTAL NUMBER OF CASES (TOP 5 NEIGHBORHOODS) -----
# ============================================================================

# Filter data for top 5 neighborhoods
plot_data_top5 <- merged_data %>%
  filter(neighborhood %in% top5_neighborhoods)

# Create line plot for TOTAL CASES
p_total_cases <- ggplot(plot_data_top5, aes(x = factor(open_year), y = total_cases, 
                                            group = neighborhood, color = neighborhood)) +
  geom_line(size = 1.3) +
  geom_point(size = 2.5) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = " Year - Total Number of Cases",
    x = "Year",
    y = "Total Number of Cases",
    color = "Neighborhood"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = scales::comma,
                     limits = c(10000, 40000),  # Set Y-axis range
                     breaks = seq(10000, 40000, by = 15000),
                     expand = expansion(mult = c(0.05, 0.15)))

# Save plot
ggsave(file.path(plot_dir, "top5_total_cases_line.png"), 
       p_total_cases, width = 12, height = 7, dpi = 300)

print(p_total_cases)

# ============================================================================
# SEASONAL REQUEST TYPE ANALYSIS ----
# ============================================================================

# Get top request types overall
top_request_types <- cases_clean %>%
  filter(!is.na(type), type != "") %>%
  count(type, sort = TRUE) %>%
  slice_head(n = 10) %>%  # Top 10 types
  pull(type)

# Aggregate by season and request type
seasonal_requests <- cases_clean %>%
  filter(type %in% top_request_types, !is.na(season)) %>%
  group_by(season, type) %>%
  summarise(total_cases = n(), .groups = "drop")

# Calculate percentage within each season
seasonal_requests <- seasonal_requests %>%
  group_by(season) %>%
  mutate(pct_of_season = (total_cases / sum(total_cases)) * 100) %>%
  ungroup()

# ============================================================================
# VISUALIZATION : HEATMAP -----
# ============================================================================

p_heatmap <- ggplot(seasonal_requests, aes(x = season, y = type, fill = total_cases)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = format(total_cases, big.mark = ",")), 
            color = "white", fontface = "bold", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkred", 
                      labels = scales::comma,
                      name = "Total Cases") +
  labs(
    title = "Season - Request Type",
    x = "Season",
    y = "Request Type"
  ) +
  theme_minimal() +
  theme(
    
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 11),
    legend.position = "right",
    panel.grid = element_blank()
  ) +
  scale_x_discrete(limits = c("Winter", "Spring", "Summer", "Fall"))

ggsave(file.path(plot_dir, "seasonal_request_heatmap.png"), 
       p_heatmap, width = 10, height = 8, dpi = 500)

print(p_heatmap)

# Filter cases with closure data
cases_closure <- cases_clean %>%
  filter(!is.na(closure_days), closure_days >= 0)

# Remove outliers using IQR method
Q1 <- quantile(cases_closure$closure_days, 0.25, na.rm = TRUE)
Q3 <- quantile(cases_closure$closure_days, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1
lower_bd <- Q1 - 1.5 * IQR_val
upper_bd <- Q3 + 1.5 * IQR_val

cases_closure_clean <- cases_closure %>%
  filter(closure_days >= lower_bd & closure_days <= upper_bd)

cat("\n=== Closure Data Summary ===\n")
cat("Total cases with closure data:", nrow(cases_closure), "\n")
cat("After outlier removal:", nrow(cases_closure_clean), "\n")
cat("Outliers removed:", nrow(cases_closure) - nrow(cases_closure_clean), "\n")
cat("Median closure days:", round(median(cases_closure_clean$closure_days), 2), "\n")
cat("Mean closure days:", round(mean(cases_closure_clean$closure_days), 2), "\n\n")

# Create binary target for 24-hour closure
cases_24hr <- cases_closure_clean %>%
  mutate(is_24hr = if_else(closure_days < 1, 1, 0))

# Calculate class distribution
class_balance <- cases_24hr %>%
  count(is_24hr) %>%
  mutate(
    percentage = round((n / sum(n)) * 100, 2),
    label = if_else(is_24hr == 1, "< 24 hours", "≥ 24 hours")
  )

cat("\n=== Class Balance: 24-Hour Closure ===\n")
print(class_balance)

# ============================================================================
# Department Performance Metrics Correlation (4x4) ----
# ============================================================================

# Calculate department-level metrics
dept_performance <- cases_closure_clean %>%
  filter(!is.na(department), department != "") %>%
  mutate(date = as.Date(open_dt)) %>%
  group_by(department) %>%
  summarise(
    total_cases = n(),
    median_closure_days = median(closure_days, na.rm = TRUE),
    pct_ontime = mean(on_time_flag == 1, na.rm = TRUE) * 100,
    
    # Calculate daily intensity
    n_days = as.numeric(difftime(max(date), min(date), units = "days")) + 1,
    avg_cases_per_day = total_cases / n_days,
    
    .groups = "drop"
  ) %>%
  filter(total_cases >= 500)  # Only departments with sufficient data

cat("\n=== Department Performance Summary ===\n")
cat("Number of departments analyzed:", nrow(dept_performance), "\n")
print(dept_performance %>% select(department, total_cases, median_closure_days))

# Select 4 continuous variables for correlation
correlation_vars <- dept_performance %>%
  select(
    `Total Cases` = total_cases,
    `Median Closure Days` = median_closure_days,
    `On-Time (%)` = pct_ontime,
    `Cases per Day` = avg_cases_per_day
  )

# Calculate correlation matrix
cor_matrix <- cor(correlation_vars, use = "pairwise.complete.obs")

cat("\n=== Department Performance Correlation Matrix (4x4) ===\n")
print(round(cor_matrix, 3))

# Create correlation heatmap
library(reshape2)

cor_melted <- melt(cor_matrix)

p_cor_heatmap <- ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4, fontface = "bold") +
  scale_fill_gradient2(
    low = "#4575b4",
    mid = "#f7f7f7",
    high = "#a50026",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    legend.position = "right",
    panel.grid = element_blank()
  ) +
  coord_equal()

ggsave(file.path(plot_dir, "department_correlation_heatmap.png"), 
       p_cor_heatmap, width = 10, height = 8, dpi = 500)

print(p_cor_heatmap)

# ============================================================================
# NOW CREATE YEARLY SUMMARY (AFTER cases_closure_clean EXISTS!) ----
# ============================================================================

# Calculate yearly citywide metrics
yearly_summary <- cases_closure_clean %>%
  group_by(open_year) %>%
  summarise(
    total_cases = n(),
    median_closure = median(closure_days, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate scaling factor for dual axis
scaling_factor <- max(yearly_summary$total_cases) / max(yearly_summary$median_closure)

# Create dual-axis plot (clean style)
p_yearly_summary <- ggplot(yearly_summary, aes(x = factor(open_year))) +
  # Bars for total cases
  geom_col(aes(y = total_cases), fill = "#4472C4", alpha = 0.9, width = 0.65) +
  
  # Line for median closure days (scaled to fit same axis)
  geom_line(aes(y = median_closure * scaling_factor, group = 1), 
            color = "#ED7D31", size = 1.8) +
  geom_point(aes(y = median_closure * scaling_factor), 
             color = "#ED7D31", size = 5) +
  
  # Add value labels ON the bars (white)
  geom_text(aes(y = total_cases, label = format(total_cases, big.mark = ",")), 
            vjust = 1.5, size = 3.5, fontface = "bold", color = "white") +
  
  # Add value labels on the line points (BLACK)
  geom_text(aes(y = median_closure * scaling_factor, 
                label = round(median_closure, 2)), 
            vjust = -1.2, size = 3.5, fontface = "bold", color = "black") +
  
  # Dual y-axes
  scale_y_continuous(
    name = "Total Cases",
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.15)),
    sec.axis = sec_axis(~./scaling_factor, name = "Median Closure Days")
  ) +
  
  labs(
    x = "Year"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
    axis.title.y.left = element_text(color = "black", size = 12),
    axis.title.y.right = element_text(color = "black", size = 12),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10, color = "gray30"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(plot_dir, "yearly_summary_dual_axis.png"), 
       p_yearly_summary, width = 11, height = 6, dpi = 500)

print(p_yearly_summary)


# ============================================================================
# Logistic Model (<24 hrs) ----
# ============================================================================

# Prepare modeling dataset
top5_depts <- cases_clean %>%
  filter(!is.na(department), department != "") %>%
  count(department, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(department)

top10_types <- cases_clean %>%
  filter(!is.na(type), type != "") %>%
  count(type, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(type)

top5_nbhd <- cases_clean %>%
  filter(!is.na(neighborhood), neighborhood != "") %>%
  count(neighborhood, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(neighborhood)

top5_sources <- cases_24hr %>%
  filter(!is.na(source), source != "") %>%
  count(source, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(source)

# Create modeling dataset with factors
cases_model_24hr <- cases_24hr %>%
  filter(!is.na(department), department != "", 
         !is.na(type), type != "", 
         !is.na(source), source != "", 
         !is.na(neighborhood), neighborhood != "") %>%
  mutate(
    dept_group = if_else(department %in% top5_depts, department, "Other"),
    dept_group = factor(dept_group, levels = c("Other", top5_depts)),
    
    type_group = if_else(type %in% top10_types, type, "Other"),
    type_group = factor(type_group),
    
    nbhd_group = if_else(neighborhood %in% top5_nbhd, neighborhood, "Other"),
    nbhd_group = factor(nbhd_group),
    
    source_group = if_else(source %in% top5_sources, source, "Other"),
    source_group = factor(source_group, levels = c("Other", top5_sources)),
    
    open_year = factor(open_year),
    season = factor(season),
    is_weekend = factor(is_weekend)
  )

# Train/test split
set.seed(123)
train_indices <- sample(1:nrow(cases_model_24hr), size = 0.7 * nrow(cases_model_24hr))
train_data <- cases_model_24hr[train_indices, ]
test_data <- cases_model_24hr[-train_indices, ]

cat("\n=== Train/Test Split ===\n")
cat("Training set:", nrow(train_data), "cases\n")
cat("Test set:", nrow(test_data), "cases\n")
cat("Training set - <24hr rate:", round(mean(train_data$is_24hr) * 100, 2), "%\n")
cat("Test set - <24hr rate:", round(mean(test_data$is_24hr) * 100, 2), "%\n\n")

# Build 4 models progressively
logit_24hr_m1 <- glm(is_24hr ~ open_year + season + is_weekend, 
                      data = train_data, family = binomial())

logit_24hr_m2 <- glm(is_24hr ~ open_year + season + is_weekend + dept_group, 
                      data = train_data, family = binomial())

logit_24hr_m3 <- glm(is_24hr ~ open_year + season + is_weekend + dept_group + type_group, 
                      data = train_data, family = binomial())

logit_24hr_m4 <- glm(is_24hr ~ dept_group + type_group + source_group + nbhd_group + 
                       open_year + season + is_weekend, 
                      data = train_data, family = binomial())

# Helper function for checking variable groups
check_var_group <- function(model, pattern) {
  coefs <- broom::tidy(model)
  vars <- coefs %>% filter(grepl(pattern, term))
  if(nrow(vars) == 0) return("No")
  n_sig <- sum(vars$p.value < 0.05)
  if(n_sig > 0) return("Yes")
  return("No")
}

# Create custom rows for stargazer
const_row <- c(
  "Constant",
  paste0(round(coef(logit_24hr_m1)["(Intercept)"], 3), " (", 
         round(summary(logit_24hr_m1)$coefficients["(Intercept)", "Std. Error"], 3), ")"),
  paste0(round(coef(logit_24hr_m2)["(Intercept)"], 3), " (", 
         round(summary(logit_24hr_m2)$coefficients["(Intercept)", "Std. Error"], 3), ")"),
  paste0(round(coef(logit_24hr_m3)["(Intercept)"], 3), " (", 
         round(summary(logit_24hr_m3)$coefficients["(Intercept)", "Std. Error"], 3), ")"),
  paste0(round(coef(logit_24hr_m4)["(Intercept)"], 3), " (", 
         round(summary(logit_24hr_m4)$coefficients["(Intercept)", "Std. Error"], 3), ")")
)

odds_ratio_row <- c(
  "<b>Odds Ratio (Constant)</b>",
  paste0("<b>", round(exp(coef(logit_24hr_m1)["(Intercept)"]), 3), "</b>"),
  paste0("<b>", round(exp(coef(logit_24hr_m2)["(Intercept)"]), 3), "</b>"),
  paste0("<b>", round(exp(coef(logit_24hr_m3)["(Intercept)"]), 3), "</b>"),
  paste0("<b>", round(exp(coef(logit_24hr_m4)["(Intercept)"]), 3), "</b>")
)

# Calculate accuracy for all models
acc_m1 <- mean(
  if_else(predict(logit_24hr_m1, newdata = test_data, type = "response") >= 0.5, 1, 0) == test_data$is_24hr,
  na.rm = TRUE
)
acc_m2 <- mean(
  if_else(predict(logit_24hr_m2, newdata = test_data, type = "response") >= 0.5, 1, 0) == test_data$is_24hr,
  na.rm = TRUE
)
acc_m3 <- mean(
  if_else(predict(logit_24hr_m3, newdata = test_data, type = "response") >= 0.5, 1, 0) == test_data$is_24hr,
  na.rm = TRUE
)
acc_m4 <- mean(
  if_else(predict(logit_24hr_m4, newdata = test_data, type = "response") >= 0.5, 1, 0) == test_data$is_24hr,
  na.rm = TRUE
)

accuracy_row <- c(
  "<b>Test Accuracy</b>",
  paste0("<b>", round(acc_m1, 3), "</b>"),
  paste0("<b>", round(acc_m2, 3), "</b>"),
  paste0("<b>", round(acc_m3, 3), "</b>"),
  paste0("<b>", round(acc_m4, 3), "</b>")
)

weekend_row <- c("Weekend", 
                 check_var_group(logit_24hr_m1, "is_weekend"),
                 check_var_group(logit_24hr_m2, "is_weekend"),
                 check_var_group(logit_24hr_m3, "is_weekend"),
                 check_var_group(logit_24hr_m4, "is_weekend"))

season_row <- c("Season",
                check_var_group(logit_24hr_m1, "season"),
                check_var_group(logit_24hr_m2, "season"),
                check_var_group(logit_24hr_m3, "season"),
                check_var_group(logit_24hr_m4, "season"))

year_row <- c("Year",
              check_var_group(logit_24hr_m1, "open_year"),
              check_var_group(logit_24hr_m2, "open_year"),
              check_var_group(logit_24hr_m3, "open_year"),
              check_var_group(logit_24hr_m4, "open_year"))

dept_row <- c("Department", "No",
              check_var_group(logit_24hr_m2, "dept_group"),
              check_var_group(logit_24hr_m3, "dept_group"),
              check_var_group(logit_24hr_m4, "dept_group"))

type_row <- c("Request Type", "No", "No",
              check_var_group(logit_24hr_m3, "type_group"),
              check_var_group(logit_24hr_m4, "type_group"))

nbhd_row <- c("Neighborhood", "No", "No", "No",
              check_var_group(logit_24hr_m4, "nbhd_group"))

source_row <- c("Source", "No", "No", "No",
                check_var_group(logit_24hr_m4, "source_group"))

# Create stargazer table
stargazer(logit_24hr_m1, logit_24hr_m2, logit_24hr_m3, logit_24hr_m4,
          type = "html",
          dep.var.labels = "24-Hour Resolution (< 24 hours = 1)",
          
          column.labels = c(
            "Temporal Only<br>(1)",
            "Add Department<br>(2)",
            "Add Request Type<br>(3)",
            "Full Model<br>(4)"
          ),
          model.numbers = FALSE,
          
          omit = "Constant",
          
          add.lines = list(
            const_row,
            odds_ratio_row,
            accuracy_row,
            weekend_row,
            season_row,
            year_row,
            dept_row,
            type_row,
            nbhd_row,
            source_row
          ),
          
          table.layout = "-ldc-a-s-n",
          keep.stat = c("n", "ll", "aic"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          
          notes = "'Yes' indicates the variable category is included.",
          notes.align = "l",
          header = FALSE,
          out = file.path(data_dir, "plots_final_analysis", "logistic_24hr_regression_table.html")
)

# Evaluate Model 4 on test set
test_data <- test_data %>%
  mutate(
    predicted_prob = predict(logit_24hr_m4, newdata = test_data, type = "response"),
    predicted_class = if_else(predicted_prob >= 0.5, 1, 0)
  )

test_accuracy <- mean(test_data$predicted_class == test_data$is_24hr, na.rm = TRUE)

# Confusion matrix
confusion_matrix <- table(
  Predicted = factor(test_data$predicted_class, levels = c(0, 1), 
                     labels = c("Predicted: ≥24hrs", "Predicted: <24hrs")),
  Actual = factor(test_data$is_24hr, levels = c(0, 1), 
                  labels = c("Actual: ≥24hrs", "Actual: <24hrs"))
)

print(confusion_matrix)

confusion_matrix_df <- as.data.frame.matrix(confusion_matrix)
confusion_matrix_df <- cbind(Classification = rownames(confusion_matrix_df), confusion_matrix_df)

write_csv(confusion_matrix_df, file.path(data_dir, "plots_final_analysis", "logistic_24hr_confmatrix.csv"))

# Calculate performance metrics
TP <- confusion_matrix[2, 2]
TN <- confusion_matrix[1, 1]
FP <- confusion_matrix[2, 1]
FN <- confusion_matrix[1, 2]

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)

model_performance_24hr <- data.frame(
  Model = "Full Model",
  Accuracy = paste0(round(test_accuracy * 100, 2), "%"),
  Precision = paste0(round(precision * 100, 2), "%"),
  Recall = paste0(round(recall * 100, 2), "%"),
  F1_Score = round(f1_score, 4)
)

print(model_performance_24hr)
write_csv(model_performance_24hr, file.path(data_dir, "plots_final_analysis", "logistic_24hr_classimetrics.csv"))


# ============================================================================
# ROC Curve for Logistic Regression (Model 4) ----
# ============================================================================

library(pROC)

# Calculate ROC curve
roc_obj <- roc(test_data$is_24hr, test_data$predicted_prob)

# Extract AUC
auc_value <- auc(roc_obj)

cat("\n=== ROC Curve Analysis ===\n")
cat("AUC (Area Under Curve):", round(auc_value, 4), "\n")

# Create ROC curve plot
roc_data <- data.frame(
  sensitivity = roc_obj$sensitivities,
  specificity = roc_obj$specificities,
  threshold = roc_obj$thresholds
)

p_roc <- ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", size = 0.8) +
  annotate("text", x = 0.6, y = 0.4, 
           label = paste0("AUC = ", round(auc_value, 3)), 
           size = 6, fontface = "bold", color = "black") +
  labs(
    x = "FPR (1 - Specificity)",
    y = "TPR (Sensitivity)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01))

ggsave(file.path(plot_dir, "logistic_roc_curve.png"), 
       p_roc, width = 8, height = 8, dpi = 500)

print(p_roc)



# ============================================================================
# LASSO Model ----
# ============================================================================

# Prepare data for LASSO
cases_lasso_prep <- cases_closure_clean %>%
  filter(!is.na(open_dt))

# Create temporal features
cases_lasso_prep <- cases_lasso_prep %>%
  mutate(
    hour_of_day = hour(open_dt),
    day_of_month = day(open_dt),
    week_of_year = week(open_dt),
    month_numeric = month(open_dt),
    year_numeric = as.numeric(as.character(open_year))
  )

# Calculate average closure times by category
neighborhood_avg <- cases_closure_clean %>%
  group_by(neighborhood) %>%
  summarise(neighborhood_avg_closure = mean(closure_days, na.rm = TRUE), .groups = "drop")

dept_avg <- cases_closure_clean %>%
  group_by(department) %>%
  summarise(dept_avg_closure = mean(closure_days, na.rm = TRUE), .groups = "drop")

type_avg <- cases_closure_clean %>%
  group_by(type) %>%
  summarise(type_avg_closure = mean(closure_days, na.rm = TRUE), .groups = "drop")

# Join average closure times
cases_lasso_prep <- cases_lasso_prep %>%
  left_join(neighborhood_avg, by = "neighborhood") %>%
  left_join(dept_avg, by = "department") %>%
  left_join(type_avg, by = "type")

# Final dataset - keep only complete cases
cases_lasso <- cases_lasso_prep %>%
  filter(!is.na(closure_days), !is.na(hour_of_day), !is.na(day_of_month),
         !is.na(week_of_year), !is.na(neighborhood_avg_closure), 
         !is.na(dept_avg_closure), !is.na(type_avg_closure))

cat("\n=== LASSO Data Preparation ===\n")
cat("Original cases:", nrow(cases_closure_clean), "\n")
cat("After feature engineering:", nrow(cases_lasso), "\n")
cat("Cases dropped:", nrow(cases_closure_clean) - nrow(cases_lasso), "\n\n")

# Create predictor matrix - 8 predictors total
x_vars <- as.matrix(cases_lasso %>%
                      select(hour_of_day, day_of_month, week_of_year,
                             month_numeric, year_numeric,
                             neighborhood_avg_closure, dept_avg_closure, type_avg_closure))

# Outcome variable
y_var <- cases_lasso$closure_days

# Set seed for reproducibility
set.seed(123)

# Split into train/test (70/30)
train_indices <- sample(1:nrow(cases_lasso), size = 0.7 * nrow(cases_lasso))
x_train <- x_vars[train_indices, ]
y_train <- y_var[train_indices]
x_test <- x_vars[-train_indices, ]
y_test <- y_var[-train_indices]

cat("=== Train/Test Split ===\n")
cat("Training set:", nrow(x_train), "cases\n")
cat("Test set:", nrow(x_test), "cases\n")
cat("Number of predictors:", ncol(x_vars), "\n\n")

# Perform 10-fold cross-validation on training data
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)

# Extract optimal lambda values
lambda_min <- cv_lasso$lambda.min
lambda_1se <- cv_lasso$lambda.1se

cat("=== LASSO Cross-Validation Results ===\n")
cat("Optimal Lambda (min):", round(lambda_min, 6), "\n")
cat("Optimal Lambda (1SE):", round(lambda_1se, 6), "\n\n")

# Fit with lambda.min
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_min)

# Extract coefficients
lasso_coefs <- coef(lasso_model) %>%
  as.matrix() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("variable") %>%
  rename(coefficient = s0) %>%
  arrange(desc(abs(coefficient)))

# Predictions on test set
pred_lasso <- predict(lasso_model, newx = x_test, s = lambda_min)

# Calculate LASSO metrics
lasso_mse <- mean((y_test - pred_lasso)^2)
lasso_rmse <- sqrt(lasso_mse)
lasso_mae <- mean(abs(y_test - pred_lasso))
lasso_r2 <- 1 - (sum((y_test - pred_lasso)^2) / sum((y_test - mean(y_test))^2))

# Fit OLS for comparison
train_df <- as.data.frame(x_train)
train_df$closure_days <- y_train
ols_model <- lm(closure_days ~ ., data = train_df)
pred_ols <- predict(ols_model, newdata = as.data.frame(x_test))

# Calculate OLS metrics
ols_mse <- mean((y_test - pred_ols)^2)
ols_rmse <- sqrt(ols_mse)
ols_mae <- mean(abs(y_test - pred_ols))
ols_r2 <- 1 - (sum((y_test - pred_ols)^2) / sum((y_test - mean(y_test))^2))

# Number of predictors
n_lasso <- sum(lasso_coefs$coefficient != 0) - 1  
n_ols <- ncol(x_vars)

cat("Number of OLS predictors:", n_ols, "\n")
cat("Number of LASSO predictors (non-zero):", n_lasso, "\n\n")

# Model comparison table
model_comparison <- data.frame(
  Model = c("OLS Regression", "LASSO Regression"),
  N_Predictors = c(n_ols, n_lasso),
  Test_R2 = round(c(ols_r2, lasso_r2), 4),
  Test_RMSE = round(c(ols_rmse, lasso_rmse), 4),
  Test_MAE = round(c(ols_mae, lasso_mae), 4)
)

cat("=== Model Performance on Test Set ===\n")
print(model_comparison)

# LASSO Cross-Validation Curve
cv_results <- data.frame(
  lambda = cv_lasso$lambda,
  mse = cv_lasso$cvm,
  mse_lower = cv_lasso$cvlo,
  mse_upper = cv_lasso$cvup,
  n_nonzero = cv_lasso$nzero
)
print(cv_results)

# LASSO Cross-Validation Curve with variable count on top
p_lasso_cv <- ggplot(cv_results, aes(x = log(lambda), y = mse)) +
  geom_errorbar(aes(ymin = mse_lower, ymax = mse_upper), 
                width = 0.1,        # Increased from 0.05
                color = "gray30",    # Darker color
                alpha = 1,           # Full opacity
                size = 0.8) +        # Thicker lines
  geom_point(color = "red", size = 2.5, alpha = 0.8) +
  geom_line(color = "red", size = 0.8, alpha = 0.8) +
  geom_vline(xintercept = log(lambda_min), linetype = "dashed", color = "black", size = 0.8) +
  geom_vline(xintercept = log(lambda_1se), linetype = "dashed", color = "black", size = 0.8) +
  annotate("text", x = log(lambda_min), y = max(cv_results$mse) * 0.95, 
           label = "λ min", color = "black", fontface = "bold", hjust = -0.2) +
  annotate("text", x = log(lambda_1se), y = max(cv_results$mse) * 0.95, 
           label = "λ 1SE", color = "black", fontface = "bold", hjust = -0.2) +
  labs(
    x = "log(Lambda)",
    y = "Mean-Squared Error"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  
  # Add secondary x-axis on top showing number of variables
  scale_x_continuous(
    sec.axis = sec_axis(
      trans = ~ .,
      breaks = log(cv_results$lambda[seq(1, nrow(cv_results), length.out = 8)]),
      labels = cv_results$n_nonzero[seq(1, nrow(cv_results), length.out = 8)]    )
  )

ggsave(file.path(plot_dir, "lasso_cv_curve.png"), p_lasso_cv, width = 10, height = 6, dpi = 300)

print(p_lasso_cv)

# Extract OLS coefficients
ols_coefs <- coef(ols_model) %>%
  as.matrix() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("variable") %>%
  rename(ols_coefficient = `V1`) %>%
  filter(variable != "(Intercept)")

# Extract LASSO coefficients (already have this)
lasso_coefs_clean <- lasso_coefs %>%
  filter(variable != "(Intercept)") %>%
  select(variable, lasso_coefficient = coefficient)

# Merge OLS and LASSO
comparison_coefs <- lasso_coefs_clean %>%
  left_join(ols_coefs, by = "variable") %>%
  mutate(
    variable_clean = case_when(
      variable == "hour_of_day" ~ "Hour of Day",
      variable == "day_of_month" ~ "Day of Month",
      variable == "week_of_year" ~ "Week of Year",
      variable == "month_numeric" ~ "Month",
      variable == "year_numeric" ~ "Year",
      variable == "neighborhood_avg_closure" ~ "Neighborhood Avg Closure",
      variable == "dept_avg_closure" ~ "Department Avg Closure",
      variable == "type_avg_closure" ~ "Request Type Avg Closure",
      TRUE ~ variable
    ),
    # Calculate shrinkage
    abs_ols = abs(ols_coefficient),
    abs_lasso = abs(lasso_coefficient),
    shrinkage_pct = if_else(abs_ols > 0, 
                            (abs_ols - abs_lasso) / abs_ols * 100, 
                            0)
  ) %>%
  arrange(desc(abs_ols))

# Prepare data for plotting (long format)
plot_comparison <- comparison_coefs %>%
  select(variable_clean, ols_coefficient, lasso_coefficient) %>%
  slice_head(n = 11) %>%  # Top 11 variables by OLS
  pivot_longer(cols = c(ols_coefficient, lasso_coefficient),
               names_to = "model",
               values_to = "coefficient") %>%
  mutate(
    model = case_when(
      model == "ols_coefficient" ~ "OLS",
      model == "lasso_coefficient" ~ "LASSO"
    ),
    model = factor(model, levels = c("OLS", "LASSO"))
  )
print(plot_comparison)

# Create grouped bar chart
p_comparison <- ggplot(plot_comparison, aes(x = reorder(variable_clean, abs(coefficient)), 
                                            y = coefficient, fill = model)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75, alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.3) +
  coord_flip() +
  scale_fill_manual(
    values = c("OLS" = "#e74c3c", "LASSO" = "#3498db"),
    name = "Model"
  ) +
  labs(
    x = "Variable",
    y = "Coefficient Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
    limits = c(-0.1, 1),                    # Set min and max
    breaks = seq(-0.1, 1, by = 0.1),       # Custom break points
    expand = expansion(mult = c(0.02, 0.02))  # Small padding
  )

print(p_comparison)
ggsave(file.path(plot_dir, "lasso_vs_ols_coefficients.png"), 
       p_comparison, width = 12, height = 8, dpi = 500)





