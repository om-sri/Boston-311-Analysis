# ============================================================================
# Boston 311 Service Requests (2015-2019)
# 2nd Analysis Report - Group 1
# Methods: Logistic Regression, Linear Regression, Chi-Square Test
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

# ============================================================================
# 1) LOAD AND COMBINE DATA
# ============================================================================

data_dir <- "C:/Users/Omsri/OneDrive/Documents/R-Files"

cases_all <- bind_rows(
  read_csv(file.path(data_dir, "2015.csv")) %>% mutate(file_year = 2015),
  read_csv(file.path(data_dir, "2016.csv")) %>% mutate(file_year = 2016),
  read_csv(file.path(data_dir, "2017.csv")) %>% mutate(file_year = 2017),
  read_csv(file.path(data_dir, "2018.csv")) %>% mutate(file_year = 2018),
  read_csv(file.path(data_dir, "2019.csv")) %>% mutate(file_year = 2019)
)

cases <- cases_all %>% clean_names()

# ============================================================================
# 2) DATA PREPARATION
# ============================================================================

cases <- cases %>%
  mutate(
    open_dt = parse_date_time(open_dt, orders = c("ymd HMS", "mdy HMS", "ymd HM", "mdy HM")),
    closed_dt = parse_date_time(closed_dt, orders = c("ymd HMS", "mdy HMS", "ymd HM", "mdy HM")),
    sla_target_dt = parse_date_time(sla_target_dt, orders = c("ymd HMS", "mdy HMS", "ymd HM", "mdy HM"))
  ) %>%
  filter(!is.na(open_dt)) %>%
  mutate(open_year = year(open_dt)) %>%
  filter(open_year >= 2015 & open_year <= 2019)

# ============================================================================
# 3) FEATURE ENGINEERING
# ============================================================================

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

# ============================================================================
# 4) IDENTIFY TOP CATEGORIES
# ============================================================================

top5_nbhd <- cases_clean %>%
  filter(!is.na(neighborhood), neighborhood != "") %>%
  count(neighborhood, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(neighborhood)

top10_types <- cases_clean %>%
  filter(!is.na(type), type != "") %>%
  count(type, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(type)

top5_depts <- cases_clean %>%
  filter(!is.na(department), department != "") %>%
  count(department, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(department)

# Create groupings
cases_clean <- cases_clean %>%
  mutate(
    nbhd_group = if_else(!is.na(neighborhood) & neighborhood != "" & neighborhood %in% top5_nbhd, neighborhood, "Other"),
    type_group = if_else(!is.na(type) & type != "" & type %in% top10_types, type, "Other"),
    dept_group = if_else(!is.na(department) & department != "" & department %in% top5_depts, department, "Other")
  )

# ============================================================================
# 5) REMOVE OUTLIERS
# ============================================================================

cases_closure <- cases_clean %>%
  filter(!is.na(closure_days), closure_days >= 0)

Q1 <- quantile(cases_closure$closure_days, 0.25, na.rm = TRUE)
Q3 <- quantile(cases_closure$closure_days, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1
lower_bd <- Q1 - 1.5 * IQR_val
upper_bd <- Q3 + 1.5 * IQR_val

cases_closure_clean <- cases_closure %>%
  filter(closure_days >= lower_bd & closure_days <= upper_bd) %>%
  mutate(
    nbhd_group = if_else(!is.na(neighborhood) & neighborhood != "" & neighborhood %in% top5_nbhd, neighborhood, "Other"),
    type_group = if_else(!is.na(type) & type != "" & type %in% top10_types, type, "Other"),
    dept_group = if_else(!is.na(department) & department != "" & department %in% top5_depts, department, "Other")
  )

# ============================================================================
# 6) CREATE OUTPUT DIRECTORIES
# ============================================================================

out_dir <- file.path(data_dir, "outputs_2nd_analysis")
plot_dir <- file.path(data_dir, "plots_2nd_analysis")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

# ============================================================================
# TABLE 0: DATASET OVERVIEW
# ============================================================================
overall_summary <- data.frame(
  Metric = c(
    "Time Period",
    "Total Service Requests",
    "Average Daily Volume",
    "Closure Rate (%)",
    "On-Time Performance (%)"
  ),
  Value = c(
    "2015-2019",
    format(nrow(cases_clean), big.mark = ","),
    format(round(nrow(cases_clean) / (365 * 5)), big.mark = ","),
    paste0(round(mean(cases_clean$is_closed == 1, na.rm = TRUE) * 100, 2), "%"),
    paste0(round(mean(cases_clean$on_time_flag == 1, na.rm = TRUE) * 100, 2), "%")
  )
)

write_csv(overall_summary, file.path(out_dir, "table0_dataset_overview.csv"))

cat("\n=== Dataset Overview ===\n")
print(overall_summary)

# ============================================================================
# 7) SUBSET ANALYSIS - TABLE 1: BY YEAR
# ============================================================================

closure_by_year <- cases_closure_clean %>%
  group_by(open_year) %>%
  summarise(median_closure_days = median(closure_days, na.rm = TRUE), .groups = "drop")
print(closure_by_year)

closure_rate_by_year <- cases_clean %>%
  group_by(open_year) %>%
  summarise(n_cases = n(), closure_rate = mean(is_closed == 1, na.rm = TRUE) * 100, .groups = "drop")
print(closure_rate_by_year)

ontime_by_year <- cases_clean %>%
  filter(!is.na(on_time_flag)) %>%
  group_by(open_year) %>%
  summarise(on_time_pct = mean(on_time_flag == 1, na.rm = TRUE) * 100, .groups = "drop")
print(ontime_by_year)

table1_by_year <- closure_rate_by_year %>%
  left_join(ontime_by_year, by = "open_year") %>%
  left_join(closure_by_year, by = "open_year") %>%
  mutate(
    closure_rate = paste0(round(closure_rate, 2), "%"),
    on_time_pct = paste0(round(on_time_pct, 2), "%"),
    median_closure_days = round(median_closure_days, 2)
  ) %>%
  select(
    Year = open_year,
    `Number of Cases` = n_cases,
    `Closure Rate` = closure_rate,
    `On-Time (%)` = on_time_pct,
    `Median Closure Days` = median_closure_days
  )

write_csv(table1_by_year, file.path(out_dir, "table1_summary_by_year.csv"))

# ============================================================================
# 8) PLOT 1: YEAR TRENDS (TWO LINES)
# ============================================================================

# Read the saved table (since it has renamed columns)
plot_data_year <- read_csv(file.path(out_dir, "table1_summary_by_year.csv"), show_col_types = FALSE) %>%
  mutate(
    Year = factor(Year),
    `Closure Rate` = as.numeric(str_remove(`Closure Rate`, "%")),
    `On-Time (%)` = as.numeric(str_remove(`On-Time (%)`, "%"))
  )

p1 <- ggplot(plot_data_year, aes(x = Year, group = 1)) +
  geom_line(aes(y = `Closure Rate`, color = "Closure Rate (%)"), size = 1.5) +
  geom_point(aes(y = `Closure Rate`, color = "Closure Rate (%)"), size = 4) +
  geom_line(aes(y = `On-Time (%)`, color = "On-Time (%)"), size = 1.5, linetype = "dashed") +
  geom_point(aes(y = `On-Time (%)`, color = "On-Time (%)"), size = 4) +
  geom_text(aes(y = `Closure Rate`, label = paste0(`Closure Rate`, "%")), vjust = -1, size = 3.5, fontface = "bold", color = "steelblue") +
  geom_text(aes(y = `On-Time (%)`, label = paste0(`On-Time (%)`, "%")), vjust = 2, size = 3.5, fontface = "bold", color = "darkgreen") +
  scale_y_continuous(name = "Percentage (%)", expand = expansion(mult = c(0.1, 0.1))) +
  scale_color_manual(values = c("Closure Rate (%)" = "steelblue", "On-Time (%)" = "darkgreen")) +
  labs(title = "Case Performance Trends by Year (2015-2019)", x = "Year", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11), 
        plot.title = element_text(size = 13, hjust = 0.5))

ggsave(file.path(plot_dir, "plot1_metrics_by_year.png"), p1, width = 10, height = 6, dpi = 300)

# ============================================================================
# 9) SUBSET ANALYSIS - TABLE 2: BY DEPARTMENT
# ============================================================================

ontime_by_dept <- cases_clean %>%
  filter(!is.na(on_time_flag)) %>%
  group_by(dept_group) %>%
  summarise(on_time_pct = mean(on_time_flag == 1, na.rm = TRUE) * 100, .groups = "drop")

closure_by_dept <- cases_closure_clean %>%
  group_by(dept_group) %>%
  summarise(median_closure_days = median(closure_days, na.rm = TRUE), .groups = "drop")

cases_by_dept <- cases_clean %>%
  group_by(dept_group) %>%
  summarise(n_cases = n(), .groups = "drop")

table2_by_dept <- cases_by_dept %>%
  left_join(ontime_by_dept, by = "dept_group") %>%
  left_join(closure_by_dept, by = "dept_group") %>%
  mutate(
    dept_group = str_replace(dept_group, "PWDx", "PWD"),
    on_time_pct = paste0(round(on_time_pct, 2), "%"),
    median_closure_days = round(median_closure_days, 2)
  ) %>%
  arrange(dept_group == "Other", desc(n_cases)) %>%
  select(
    Department = dept_group,
    `Cases` = n_cases,
    `On-Time (%)` = on_time_pct,
    `Median Closure Days` = median_closure_days
  )

write_csv(table2_by_dept, file.path(out_dir, "table2_summary_by_department.csv"))

# ============================================================================
# 10) PLOT 2: DEPARTMENT PERFORMANCE
# ============================================================================
# Read the saved table
plot_data_dept <- read_csv(file.path(out_dir, "table2_summary_by_department.csv"), show_col_types = FALSE) %>%
  mutate(
    `On-Time (%)` = as.numeric(str_remove(`On-Time (%)`, "%"))
  ) %>%
  arrange(`On-Time (%)`) %>%
  mutate(Department = factor(Department, levels = Department))

p2 <- ggplot(plot_data_dept, aes(x = Department, y = `On-Time (%)`)) +
  geom_col(fill = "darkgreen", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(`On-Time (%)`, "%")), hjust = -0.2, size = 3.5, fontface = "bold") +
  scale_y_continuous(name = "On-Time Percentage (%)", expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(title = "On-Time Performance by Department (Top 5 + Other)", x = "Department") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 11), 
        plot.title = element_text(size = 13, hjust = 0.5))

ggsave(file.path(plot_dir, "plot2_metrics_by_department.png"), p2, width = 10, height = 7, dpi = 300)

# ============================================================================
# 11) SUBSET ANALYSIS - TABLE 3: BY SEASON 
# ============================================================================
ontime_by_season <- cases_clean %>%
  filter(!is.na(on_time_flag)) %>%
  group_by(season) %>%
  summarise(on_time_pct = mean(on_time_flag == 1, na.rm = TRUE) * 100, .groups = "drop")

closure_by_season <- cases_closure_clean %>%
  group_by(season) %>%
  summarise(median_closure_days = median(closure_days, na.rm = TRUE), .groups = "drop")

closure_rate_by_season <- cases_clean %>%
  group_by(season) %>%
  summarise(n_cases = n(), closure_rate = mean(is_closed == 1, na.rm = TRUE) * 100, .groups = "drop")

table3_by_season <- closure_rate_by_season %>%
  left_join(ontime_by_season, by = "season") %>%
  left_join(closure_by_season, by = "season") %>%
  mutate(
    closure_rate = paste0(round(closure_rate, 2), "%"),
    on_time_pct = paste0(round(on_time_pct, 2), "%"),
    median_closure_days = round(median_closure_days, 2)
  ) %>%
  arrange(desc(on_time_pct)) %>%
  select(
    Season = season,
    `Cases` = n_cases,
    `Closure Rate` = closure_rate,
    `On-Time (%)` = on_time_pct,
    `Median Closure Days` = median_closure_days
  )

write_csv(table3_by_season, file.path(out_dir, "table3_summary_by_season.csv"))

# ============================================================================
# 12) PLOT: MEDIAN CLOSURE DAYS BY SEASON
# ============================================================================

plot_data_season <- read_csv(file.path(out_dir, "table3_summary_by_season.csv"), show_col_types = FALSE) %>%
  mutate(Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")))

p_season <- ggplot(plot_data_season, aes(x = Season, y = `Median Closure Days`)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = `Median Closure Days`), vjust = -0.5, size = 4, fontface = "bold") +
  labs(title = "Season - Closure Days (Median)", x = "Season", y = "Median Closure Days") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave(file.path(plot_dir, "plot_seasonal_median_closure.png"), p_season, width = 8, height = 6, dpi = 300)

# ============================================================================
# 13) METHOD 1: LINEAR REGRESSION ----
# ============================================================================

#Research Question: What factors predict the time required to close a 311 service request?

cases_linear <- cases_closure_clean %>%
  mutate(
    dept_group = factor(dept_group),
    type_group = factor(type_group),
    nbhd_group = factor(nbhd_group),
    source_group = fct_lump_n(factor(source), n = 5, other_level = "Other"),
    open_year = factor(open_year),
    season = factor(season),
    is_weekend = factor(is_weekend)
  )

# Build 4 models
linear_m1 <- lm(closure_days ~ open_year + season + is_weekend, data = cases_linear)
linear_m2 <- lm(closure_days ~ open_year + season + is_weekend + dept_group, data = cases_linear)
linear_m3 <- lm(closure_days ~ open_year + season + is_weekend + dept_group + type_group, data = cases_linear)
linear_m4 <- lm(closure_days ~ dept_group + type_group + source_group + nbhd_group + open_year + season + is_weekend, data = cases_linear)

# ============================================================================
# STARGAZER TABLE 
# ============================================================================

library(stargazer)

# Helper function
check_var_group <- function(model, pattern) {
  coefs <- broom::tidy(model)
  vars <- coefs %>% filter(grepl(pattern, term))
  if(nrow(vars) == 0) return("No")
  n_sig <- sum(vars$p.value < 0.05)
  n_total <- nrow(vars)
  if(n_sig > 0) return(paste0("Yes"))
  return(paste0("No"))
}

# Create custom rows
dept_row <- c("Department", "No", check_var_group(linear_m2, "dept_group"),
              check_var_group(linear_m3, "dept_group"), check_var_group(linear_m4, "dept_group"))

type_row <- c("Request Type", "No", "No", check_var_group(linear_m3, "type_group"),
              check_var_group(linear_m4, "type_group"))

source_row <- c("Source", "No", "No", "No", check_var_group(linear_m4, "source_group"))

nbhd_row <- c("Neighborhood", "No", "No", "No", check_var_group(linear_m4, "nbhd_group"))

year_row <- c("Year", check_var_group(linear_m1, "open_year"), check_var_group(linear_m2, "open_year"),
              check_var_group(linear_m3, "open_year"), check_var_group(linear_m4, "open_year"))

season_row <- c("Season", check_var_group(linear_m1, "season"), check_var_group(linear_m2, "season"),
                check_var_group(linear_m3, "season"), check_var_group(linear_m4, "season"))

weekend_row <- c("Weekend", check_var_group(linear_m1, "is_weekend"), check_var_group(linear_m2, "is_weekend"),
                 check_var_group(linear_m3, "is_weekend"), check_var_group(linear_m4, "is_weekend"))

const_row <- c(
  "Constant",
  paste0(
    round(coef(linear_m1)["(Intercept)"], 3),
    " (",
    round(summary(linear_m1)$coefficients["(Intercept)", "Std. Error"], 3),
    ")"
  ),
  paste0(
    round(coef(linear_m2)["(Intercept)"], 3),
    " (",
    round(summary(linear_m2)$coefficients["(Intercept)", "Std. Error"], 3),
    ")"
  ),
  paste0(
    round(coef(linear_m3)["(Intercept)"], 3),
    " (",
    round(summary(linear_m3)$coefficients["(Intercept)", "Std. Error"], 3),
    ")"
  ),
  paste0(
    round(coef(linear_m4)["(Intercept)"], 3),
    " (",
    round(summary(linear_m4)$coefficients["(Intercept)", "Std. Error"], 3),
    ")"
  )
)


stargazer(linear_m1, linear_m2, linear_m3, linear_m4,
          type = "html",
          dep.var.labels = "Closure Time (Days)",
          
          column.labels = c(
            "Temporal Only<br>(1)",
            "Add Department<br>(2)",
            "Add Request<br>(3)",
            "Full Model<br>(4)"
          ),
          
          model.numbers = FALSE,
          
          add.lines = list(
            const_row,weekend_row,season_row,year_row,dept_row,type_row,nbhd_row,source_row
          ),
          
          table.layout = "-ldc-a-s-n",
          
          keep.stat = c("n", "rsq", "adj.rsq", "ser", "f"),
          digits = 3,
          
          notes = "'Yes' indicates the variable category is included in the model.",
          notes.align = "l",
          header = FALSE,
          out = file.path(out_dir, "linear_regression_table.html")
)

# ============================================================================
# 14) METHOD 2: LOGISTIC REGRESSION ----
# ============================================================================

library(stargazer)

# predict fast(< 5 days) closures

top5_sources <- cases_closure_clean %>%
  filter(!is.na(source), source != "") %>%
  count(source, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(source)

cases_model <- cases_closure_clean %>%
  filter(!is.na(closure_days), !is.na(department), department != "", !is.na(type), type != "", 
         !is.na(source), source != "", !is.na(neighborhood), neighborhood != "") %>%
  mutate(
    is_fast = if_else(closure_days <= 5.0, 1, 0),
    dept_group = factor(dept_group, levels = c("Other", top5_depts)),
    type_group = factor(type_group),
    nbhd_group = factor(nbhd_group),
    source_group = case_when(source %in% top5_sources ~ source, TRUE ~ "Other"),
    source_group = factor(source_group, levels = c("Other", top5_sources)),
    open_year = factor(open_year),
    season = factor(season),
    is_weekend = factor(is_weekend)
  )

set.seed(123)
train_indices <- sample(1:nrow(cases_model), size = 0.7 * nrow(cases_model))
train_data <- cases_model[train_indices, ]
test_data <- cases_model[-train_indices, ]

# Build 4 logistic models
logit_m1 <- glm(is_fast ~ open_year + season + is_weekend, data = train_data, family = binomial())
logit_m2 <- glm(is_fast ~ open_year + season + is_weekend + dept_group, data = train_data, family = binomial())
logit_m3 <- glm(is_fast ~ open_year + season + is_weekend + dept_group + type_group, data = train_data, family = binomial())
logit_m4 <- glm(is_fast ~ dept_group + type_group + source_group + nbhd_group + open_year + season + is_weekend, data = train_data, family = binomial())

# --- Constant row with coefficient (SE)
const_row_logit <- c(
  "Constant",
  paste0(round(coef(logit_m1)["(Intercept)"], 3), " (", 
         round(summary(logit_m1)$coefficients["(Intercept)", "Std. Error"], 3), ")"),
  paste0(round(coef(logit_m2)["(Intercept)"], 3), " (", 
         round(summary(logit_m2)$coefficients["(Intercept)", "Std. Error"], 3), ")"),
  paste0(round(coef(logit_m3)["(Intercept)"], 3), " (", 
         round(summary(logit_m3)$coefficients["(Intercept)", "Std. Error"], 3), ")"),
  paste0(round(coef(logit_m4)["(Intercept)"], 3), " (", 
         round(summary(logit_m4)$coefficients["(Intercept)", "Std. Error"], 3), ")")
)

# --- Odds ratio row 
odds_ratio_row <- c(
  "<b>Odds Ratio (Constant)</b>",
  paste0("<b>", round(exp(coef(logit_m1)["(Intercept)"]), 3), "</b>"),
  paste0("<b>", round(exp(coef(logit_m2)["(Intercept)"]), 3), "</b>"),
  paste0("<b>", round(exp(coef(logit_m3)["(Intercept)"]), 3), "</b>"),
  paste0("<b>", round(exp(coef(logit_m4)["(Intercept)"]), 3), "</b>")
)

# Calculate accuracy for all 4 models
test_data_m1 <- test_data %>%
  mutate(pred_m1 = predict(logit_m1, newdata = test_data, type = "response"),
         class_m1 = if_else(pred_m1 >= 0.5, 1, 0))
acc_m1 <- mean(test_data_m1$class_m1 == test_data$is_fast, na.rm = TRUE)

test_data_m2 <- test_data %>%
  mutate(pred_m2 = predict(logit_m2, newdata = test_data, type = "response"),
         class_m2 = if_else(pred_m2 >= 0.5, 1, 0))
acc_m2 <- mean(test_data_m2$class_m2 == test_data$is_fast, na.rm = TRUE)

test_data_m3 <- test_data %>%
  mutate(pred_m3 = predict(logit_m3, newdata = test_data, type = "response"),
         class_m3 = if_else(pred_m3 >= 0.5, 1, 0))
acc_m3 <- mean(test_data_m3$class_m3 == test_data$is_fast, na.rm = TRUE)

test_data_m4 <- test_data %>%
  mutate(pred_m4 = predict(logit_m4, newdata = test_data, type = "response"),
         class_m4 = if_else(pred_m4 >= 0.5, 1, 0))
acc_m4 <- mean(test_data_m4$class_m4 == test_data$is_fast, na.rm = TRUE)

# --- Accuracy row 
accuracy_row <- c(
  "<b>Test Accuracy</b>",
  paste0("<b>", round(acc_m1, 3), "</b>"),
  paste0("<b>", round(acc_m2, 3), "</b>"),
  paste0("<b>", round(acc_m3, 3), "</b>"),
  paste0("<b>", round(acc_m4, 3), "</b>")
)

# --- Stargazer table
stargazer(logit_m1, logit_m2, logit_m3, logit_m4,
          type = "html",
          dep.var.labels = "Fast Closure (≤5 days = 1)",
          
          column.labels = c(
            "Temporal Only<br>(1)",
            "Add Department<br>(2)",
            "Add Request Type<br>(3)",
            "Full Model<br>(4)"
          ),
          model.numbers = FALSE,
          
          omit = "Constant",   # handled manually
          
          add.lines = list(
            const_row_logit,odds_ratio_row,accuracy_row,weekend_row,season_row,year_row,dept_row,
            type_row,nbhd_row,source_row
          ),
          
          table.layout = "-ldc-a-s-n",
          keep.stat = c("n", "ll", "aic"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          
          notes = "'Yes' indicates the variable category is included.",
          notes.align = "l",
          header = FALSE,
          out = file.path(out_dir, "logistic_regression_table.html")
)

# Evaluate Model 4
test_data <- test_data %>%
  mutate(predicted_prob = predict(logit_m4, newdata = test_data, type = "response"),
         predicted_class = if_else(predicted_prob >= 0.5, 1, 0))

test_accuracy <- mean(test_data$predicted_class == test_data$is_fast, na.rm = TRUE)

# Create confusion matrix
confusion_matrix <- table(
  Predicted = factor(test_data$predicted_class, levels = c(0, 1), labels = c("Predicted: Slow", "Predicted: Fast")),
  Actual = factor(test_data$is_fast, levels = c(0, 1), labels = c("Actual: Slow", "Actual: Fast"))
)

print(confusion_matrix)

# Convert to dataframe with row names
confusion_matrix_df <- as.data.frame.matrix(confusion_matrix)
confusion_matrix_df <- cbind(Classification = rownames(confusion_matrix_df), confusion_matrix_df)

write_csv(confusion_matrix_df, file.path(out_dir, "logistic_confusion_matrix.csv"))

TP <- confusion_matrix[2, 2]
TN <- confusion_matrix[1, 1]
FP <- confusion_matrix[2, 1]
FN <- confusion_matrix[1, 2]

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)

model_performance_logit <- data.frame(
  Model = "Model 4",
  Accuracy = paste0(round(test_accuracy * 100, 2), "%"),
  Precision = paste0(round(precision * 100, 2), "%"),
  Recall = paste0(round(recall * 100, 2), "%"),
  F1_Score = round(f1_score, 4)
)

write_csv(model_performance_logit, file.path(out_dir, "logistic_model_performance.csv"))

logit_results <- broom::tidy(logit_m4) %>%
  mutate(odds_ratio = exp(estimate), conf_low = exp(estimate - 1.96 * std.error),
         conf_high = exp(estimate + 1.96 * std.error), significant = if_else(p.value < 0.05, "Yes", "No")) %>%
  arrange(p.value)


# ============================================================================
# 15) METHOD 3: LASSO REGRESSION ----
# ============================================================================

# Research Question: Can we predict the SLA target duration (expected resolution time) 
# using temporal and case volume patterns?

library(glmnet)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

# DATA PREPARATION - CREATE CONTINUOUS FEATURES

# Calculate SLA duration (outcome variable)
cases_lasso_prep <- cases_clean %>%
  filter(!is.na(sla_target_dt), !is.na(open_dt)) %>%
  mutate(
    sla_duration_days = as.numeric(difftime(sla_target_dt, open_dt, units = "days"))
  ) %>%
  filter(sla_duration_days >= 0, sla_duration_days < 365)  # Remove outliers

# Create temporal features
cases_lasso_prep <- cases_lasso_prep %>%
  mutate(
    hour_of_day = hour(open_dt),
    day_of_month = day(open_dt),
    day_of_year = yday(open_dt),
    week_of_year = week(open_dt),
    open_date = as.Date(open_dt)
  )

# Calculate daily case volume
daily_volume <- cases_lasso_prep %>%
  count(open_date, name = "daily_case_volume")

# Calculate hourly case volume
hourly_volume <- cases_lasso_prep %>%
  mutate(open_hour = floor_date(open_dt, "hour")) %>%
  count(open_hour, name = "hourly_case_volume")

cases_lasso_prep <- cases_lasso_prep %>%
  mutate(open_hour = floor_date(open_dt, "hour"))

# Join volume data
cases_lasso_prep <- cases_lasso_prep %>%
  left_join(daily_volume, by = "open_date") %>%
  left_join(hourly_volume, by = "open_hour")

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
  filter(!is.na(sla_duration_days), !is.na(hour_of_day), !is.na(day_of_month),
         !is.na(day_of_year), !is.na(week_of_year), !is.na(daily_case_volume),
         !is.na(hourly_case_volume), !is.na(neighborhood_avg_closure),
         !is.na(dept_avg_closure), !is.na(type_avg_closure))

# --------------------
# LASSO MODEL SETUP
# --------------------

# Create predictor matrix (all continuous variables)
x_vars <- as.matrix(cases_lasso %>%
                      select(hour_of_day, day_of_month, day_of_year, week_of_year,
                             daily_case_volume, hourly_case_volume,
                             neighborhood_avg_closure, dept_avg_closure, type_avg_closure))

# Outcome variable
y_var <- cases_lasso$sla_duration_days

# Set seed for reproducibility
set.seed(123)

# Split into train/test (70/30)
train_indices <- sample(1:nrow(cases_lasso), size = 0.7 * nrow(cases_lasso))
x_train <- x_vars[train_indices, ]
y_train <- y_var[train_indices]
x_test <- x_vars[-train_indices, ]
y_test <- y_var[-train_indices]

# --------------------
# LASSO: CROSS-VALIDATION
# --------------------

# Perform 10-fold cross-validation on training data
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)

# Extract optimal lambda values
lambda_min <- cv_lasso$lambda.min
lambda_1se <- cv_lasso$lambda.1se

cat("\n=== LASSO Cross-Validation Results ===\n")
cat("Optimal Lambda (min):", round(lambda_min, 6), "\n")
cat("Optimal Lambda (1SE):", round(lambda_1se, 6), "\n\n")

# ----------------------------------------
# LASSO: FIT FINAL MODEL
# ----------------------------------------

# Fit with lambda.min
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_min)

# Extract coefficients
lasso_coefs <- coef(lasso_model) %>%
  as.matrix() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("variable") %>%
  rename(coefficient = s0) %>%
  arrange(desc(abs(coefficient)))

# ----------------------------------------
# LASSO: MODEL PERFORMANCE
# ----------------------------------------

# Predictions on test set
pred_lasso <- predict(lasso_model, newx = x_test, s = lambda_min)

# Calculate LASSO metrics
lasso_mse <- mean((y_test - pred_lasso)^2)
lasso_rmse <- sqrt(lasso_mse)
lasso_mae <- mean(abs(y_test - pred_lasso))
lasso_r2 <- 1 - (sum((y_test - pred_lasso)^2) / sum((y_test - mean(y_test))^2))

# Fit OLS for comparison
train_df <- as.data.frame(x_train)
train_df$sla_duration_days <- y_train
ols_model <- lm(sla_duration_days ~ ., data = train_df)
pred_ols <- predict(ols_model, newdata = as.data.frame(x_test))

# Calculate OLS metrics
ols_mse <- mean((y_test - pred_ols)^2)
ols_rmse <- sqrt(ols_mse)
ols_mae <- mean(abs(y_test - pred_ols))
ols_r2 <- 1 - (sum((y_test - pred_ols)^2) / sum((y_test - mean(y_test))^2))

# Number of predictors
n_lasso <- sum(lasso_coefs$coefficient != 0) - 1  # Exclude intercept
n_ols <- ncol(x_vars)

# Model comparison table
model_comparison <- data.frame(
  Model = c("OLS Regression", "LASSO Regression"),
  N_Predictors = c(n_ols, n_lasso),
  Test_R2 = round(c(ols_r2, lasso_r2), 4),
  Test_RMSE = round(c(ols_rmse, lasso_rmse), 4),
  Test_MAE = round(c(ols_mae, lasso_mae), 4)
)

cat("\n=== Model Performance on Test Set ===\n")
print(model_comparison)

# ----------------------------------------
# PLOT 3: LASSO CROSS-VALIDATION CURVE
# ----------------------------------------

cv_results <- data.frame(
  lambda = cv_lasso$lambda,
  mse = cv_lasso$cvm,
  mse_lower = cv_lasso$cvlo,
  mse_upper = cv_lasso$cvup,
  n_nonzero = cv_lasso$nzero
)

#LASSO Cross-Validation: Predicting SLA Duration
p3 <- ggplot(cv_results, aes(x = log(lambda), y = mse)) +
  geom_errorbar(aes(ymin = mse_lower, ymax = mse_upper), width = 0.05, color = "gray50", alpha = 1) +
  geom_point(color = "red", size = 2.5, alpha = 0.8) +
  geom_line(color = "red", size = 0.8, alpha = 0.8) +
  geom_vline(xintercept = log(lambda_min), linetype = "dashed", color = "gray30", size = 0.8) +
  geom_vline(xintercept = log(lambda_1se), linetype = "dashed", color = "gray30", size = 0.8) +
  labs( x = "log(Lambda)", y = "Mean-Squared Error") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))

print(p3)

ggsave(file.path(plot_dir, "plot3_lasso_cv.png"), p3, width = 10, height = 6, dpi = 300)

# ----------------------------------------
# VARIABLE IMPORTANCE TABLE
# ----------------------------------------

var_importance <- lasso_coefs %>%
  filter(variable != "(Intercept)") %>%
  mutate(
    abs_coefficient = abs(coefficient),
    variable_clean = case_when(
      variable == "hour_of_day" ~ "Hour of Day",
      variable == "day_of_month" ~ "Day of Month",
      variable == "day_of_year" ~ "Day of Year",
      variable == "week_of_year" ~ "Week of Year",
      variable == "daily_case_volume" ~ "Daily Case Volume",
      variable == "hourly_case_volume" ~ "Hourly Case Volume",
      variable == "neighborhood_avg_closure" ~ "Neighborhood Avg Closure",
      variable == "dept_avg_closure" ~ "Department Avg Closure",
      variable == "type_avg_closure" ~ "Type Avg Closure",
      TRUE ~ variable
    )
  ) %>%
  arrange(desc(abs_coefficient)) %>%
  select(variable_clean, coefficient, abs_coefficient)

print(var_importance)



library(knitr)
library(kableExtra)

# Function to save table as PDF
save_table_pdf <- function(csv_file, pdf_filename) {
  
  df <- read_csv(csv_file, show_col_types = FALSE)
  
  pdf(file.path(out_dir, pdf_filename), width = 10, height = 5)
  
  kable_output <- kable(df, format = "latex", booktabs = TRUE) %>%
    kable_styling(latex_options = c("striped", "hold_position"))
  
  print(kable_output)
  
  dev.off()
}

# OR simpler approach - print all tables to one PDF
pdf(file.path(out_dir, "all_tables.pdf"), width = 11, height = 8.5)

# Table 0
df0 <- read_csv(file.path(out_dir, "table0_dataset_overview.csv"), show_col_types = FALSE)
grid.table(df0)
grid.newpage()

# Table 1
df1 <- read_csv(file.path(out_dir, "table1_summary_by_year.csv"), show_col_types = FALSE)
grid.table(df1)
grid.newpage()

# Table 2
df2 <- read_csv(file.path(out_dir, "table2_summary_by_department.csv"), show_col_types = FALSE)
grid.table(df2)
grid.newpage()

# Table 3
df3 <- read_csv(file.path(out_dir, "table3_summary_by_season.csv"), show_col_types = FALSE)
grid.table(df3)
grid.newpage()

# Logistic performance
df4 <- read_csv(file.path(out_dir, "logistic_model_performance.csv"), show_col_types = FALSE)
grid.table(df4)
grid.newpage()

# Confusion matrix
df5 <- read_csv(file.path(out_dir, "logistic_confusion_matrix.csv"), show_col_types = FALSE)
grid.table(df5)

dev.off()

library(gridExtra)
library(grid)

# Create PDF with all tables directly
pdf(file.path(out_dir, "all_tables.pdf"), width = 11, height = 8.5)

# Table 0: Dataset Overview
grid.table(overall_summary)
grid.newpage()

# Table 1: By Year
grid.table(table1_by_year)
grid.newpage()

# Table 2: By Department
grid.table(table2_by_dept)
grid.newpage()

# Table 3: By Season
grid.table(table3_by_season)
grid.newpage()

# Logistic Model Performance
grid.table(model_performance_logit)
grid.newpage()

# Confusion Matrix
grid.table(confusion_matrix_df)

dev.off()

cat("\n✓ All tables saved to: all_tables.pdf\n")
cat("✓ Location:", file.path(out_dir, "all_tables.pdf"), "\n")
cat("✓ Open PDF and copy tables into Word!\n")