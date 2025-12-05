# ============================================================================
# Salary Analysis R Script
# Cross-Country Comparison: Japan vs Taiwan
# ============================================================================

# Research Questions:
# 1. Is there a significant salary difference between Japan and Taiwan overall?
# 2. Does higher education (Junior College → University → Graduate School) 
#    lead to salary increases within each country?
# 3. How do salaries compare across countries for the same education level?

# 1. Package Installation and Loading -----------------------------------------
required_packages <- c("tidyverse", "rstatix", "effsize", "knitr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# 2. Load Data -----------------------------------------------------------------
df <- read_csv("df_combined.csv", show_col_types = FALSE)
head(df)

# ============================================================================
# Analysis 1: Overall Salary Comparison (All Employees)
# ============================================================================

# Nominal Salary
jp_nom <- df %>% filter(Country == "Japan", Category == "All Employees") %>% pull(Salary)
tw_nom <- df %>% filter(Country == "Taiwan", Category == "All Employees") %>% pull(Salary)

t_test_nom <- t.test(jp_nom, tw_nom, alternative = "two.sided", var.equal = FALSE)
cat("\n=== Nominal Salary: Japan vs Taiwan ===\n")
print(t_test_nom)

# Real Salary
jp_real <- df %>% filter(Country == "Japan", Category == "All Employees") %>% pull(`Real Salary`)
tw_real <- df %>% filter(Country == "Taiwan", Category == "All Employees") %>% pull(`Real Salary`)

t_test_real <- t.test(jp_real, tw_real, alternative = "two.sided", var.equal = FALSE)
cat("\n=== Real Salary: Japan vs Taiwan ===\n")
print(t_test_real)

# ============================================================================
# Analysis 2: Impact of Education Level on Salary (Within-Country)
# ============================================================================

# Pre-filter data by country and industry
df_taiwan <- df %>% filter(Country == "Taiwan", Category_Type == "Industry")
df_japan <- df %>% filter(Country == "Japan", Category_Type == "Industry")

# --- Taiwan: Graduate School vs University ---
cat("\n=== Taiwan: Graduate School vs University ===\n")

salary_grad_tw <- df_taiwan %>% filter(Education == "Graduate School") %>% pull(Salary)
salary_uni_tw <- df_taiwan %>% filter(Education == "University") %>% pull(Salary)

t_test_tw_grad_uni <- t.test(salary_grad_tw, salary_uni_tw, 
                              alternative = "two.sided", var.equal = FALSE)
print(t_test_tw_grad_uni)

# --- Japan: Graduate School vs University ---
cat("\n=== Japan: Graduate School vs University ===\n")

salary_grad_jp <- df_japan %>% filter(Education == "Graduate School") %>% pull(Salary)
salary_uni_jp <- df_japan %>% filter(Education == "University") %>% pull(Salary)

t_test_jp_grad_uni <- t.test(salary_grad_jp, salary_uni_jp, 
                              alternative = "two.sided", var.equal = FALSE)
print(t_test_jp_grad_uni)

# --- Taiwan: Junior College vs University ---
cat("\n=== Taiwan: Junior College vs University ===\n")

salary_jc_tw <- df_taiwan %>% filter(Education == "Junior College") %>% pull(Salary)
salary_uni_tw_2 <- df_taiwan %>% filter(Education == "University") %>% pull(Salary)

t_test_tw_jc_uni <- t.test(salary_jc_tw, salary_uni_tw_2, 
                            alternative = "two.sided", var.equal = FALSE)
print(t_test_tw_jc_uni)

# --- Japan: Junior College vs University ---
cat("\n=== Japan: Junior College vs University ===\n")

salary_jc_jp <- df_japan %>% filter(Education == "Junior College") %>% pull(Salary)
salary_uni_jp_2 <- df_japan %>% filter(Education == "University") %>% pull(Salary)

t_test_jp_jc_uni <- t.test(salary_jc_jp, salary_uni_jp_2, 
                            alternative = "two.sided", var.equal = FALSE)
print(t_test_jp_jc_uni)

# ============================================================================
# Analysis 3: Cross-Country Salary Comparison by Education Level
# ============================================================================

# --- University: Nominal Salary ---
cat("\n=== University Nominal Salary: Japan vs Taiwan ===\n")

salary_uni_jp_nom <- df %>%
  filter(Country == "Japan", Education == "University", Category_Type == "Industry") %>%
  pull(Salary)

salary_uni_tw_nom <- df %>%
  filter(Country == "Taiwan", Education == "University", Category_Type == "Industry") %>%
  pull(Salary)

t_test_uni_nom <- t.test(salary_uni_jp_nom, salary_uni_tw_nom, 
                          alternative = "two.sided", var.equal = FALSE)
print(t_test_uni_nom)

# --- University: Real Salary ---
cat("\n=== University Real Salary: Japan vs Taiwan ===\n")

salary_uni_jp_real <- df %>%
  filter(Country == "Japan", Education == "University", Category_Type == "Industry") %>%
  pull(`Real Salary`)

salary_uni_tw_real <- df %>%
  filter(Country == "Taiwan", Education == "University", Category_Type == "Industry") %>%
  pull(`Real Salary`)

t_test_uni_real <- t.test(salary_uni_jp_real, salary_uni_tw_real, 
                           alternative = "two.sided", var.equal = FALSE)
print(t_test_uni_real)

# --- Junior College: Nominal Salary ---
cat("\n=== Junior College Nominal Salary: Japan vs Taiwan ===\n")

salary_jc_jp_nom <- df %>%
  filter(Country == "Japan", Education == "Junior College", Category_Type == "Industry") %>%
  pull(Salary)

salary_jc_tw_nom <- df %>%
  filter(Country == "Taiwan", Education == "Junior College", Category_Type == "Industry") %>%
  pull(Salary)

t_test_jc_nom <- t.test(salary_jc_jp_nom, salary_jc_tw_nom, 
                         alternative = "two.sided", var.equal = FALSE)
print(t_test_jc_nom)

# --- Junior College: Real Salary ---
cat("\n=== Junior College Real Salary: Japan vs Taiwan ===\n")

salary_jc_jp_real <- df %>%
  filter(Country == "Japan", Education == "Junior College", Category_Type == "Industry") %>%
  pull(`Real Salary`)

salary_jc_tw_real <- df %>%
  filter(Country == "Taiwan", Education == "Junior College", Category_Type == "Industry") %>%
  pull(`Real Salary`)

t_test_jc_real <- t.test(salary_jc_jp_real, salary_jc_tw_real, 
                          alternative = "two.sided", var.equal = FALSE)
print(t_test_jc_real)

# --- Graduate School: Nominal Salary ---
cat("\n=== Graduate School Nominal Salary: Japan vs Taiwan ===\n")

salary_grad_jp_nom <- df %>%
  filter(Country == "Japan", Education == "Graduate School", Category_Type == "Industry") %>%
  pull(Salary)

salary_grad_tw_nom <- df %>%
  filter(Country == "Taiwan", Education == "Graduate School", Category_Type == "Industry") %>%
  pull(Salary)

t_test_grad_nom <- t.test(salary_grad_jp_nom, salary_grad_tw_nom, 
                           alternative = "two.sided", var.equal = FALSE)
print(t_test_grad_nom)

# --- Graduate School: Real Salary ---
cat("\n=== Graduate School Real Salary: Japan vs Taiwan ===\n")

salary_grad_jp_real <- df %>%
  filter(Country == "Japan", Education == "Graduate School", Category_Type == "Industry") %>%
  pull(`Real Salary`)

salary_grad_tw_real <- df %>%
  filter(Country == "Taiwan", Education == "Graduate School", Category_Type == "Industry") %>%
  pull(`Real Salary`)

t_test_grad_real <- t.test(salary_grad_jp_real, salary_grad_tw_real, 
                            alternative = "two.sided", var.equal = FALSE)
print(t_test_grad_real)


