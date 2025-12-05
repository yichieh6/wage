# ============================================================================
# Regression Analysis R Script
# Cross-Country Salary Analysis: Japan vs Taiwan
# ============================================================================

# Research Focus:
# - Linear regression models to analyze factors affecting salary
# - Comparison of nominal vs real salary models
# - Investigation of interaction effects between education and country

# 1. Package Installation and Loading -----------------------------------------
required_packages <- c("readr", "tidyverse", "stargazer", "coefplot")

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
# Model 1: Nominal Salary Regression (Industry Data)
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("Model 1: Nominal Salary ~ Education + Country + Category\n")
cat(strrep("=", 70), "\n\n")

model_industry <- lm(Salary ~ Education + Country + Category, 
                     data = df, 
                     subset = Category_Type == "Industry")
summary(model_industry)

# Display formatted regression table
stargazer(model_industry, type = "text")

# Coefficient plot
old_par <- par(no.readonly = TRUE)
par(mar = c(5, 18, 4, 2) + 0.1)

coefplot(model_industry,
         title = "Regression Coefficients Plot for Industry Salaries (Nominal)",
         xlab = "Coefficient Estimate",
         ylab = "Predictor Variables",
         sort = "magnitude")

par(old_par)

# Diagnostic plots
old_par <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
par(oma = c(0, 0, 3, 0))

plot(model_industry)
mtext("Diagnostic Plots for Nominal Salary", outer = TRUE, line = 1, cex = 1.2)

par(old_par)

# ============================================================================
# Model 2: Real Salary Regression (Industry Data)
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("Model 2: Real Salary ~ Education + Country + Category\n")
cat(strrep("=", 70), "\n\n")

model_industry_real <- lm(`Real Salary` ~ Education + Country + Category, 
                          data = df, 
                          subset = Category_Type == "Industry")
summary(model_industry_real)

# Coefficient plot
old_par <- par(no.readonly = TRUE)
par(mar = c(5, 18, 4, 2) + 0.1)

coefplot(model_industry_real,
         title = "Regression Coefficients Plot for Industry Salaries (Real)",
         xlab = "Coefficient Estimate",
         ylab = "Predictor Variables",
         sort = "magnitude")

par(old_par)

# Diagnostic plots
old_par <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
par(oma = c(0, 0, 3, 0))

plot(model_industry_real)
mtext("Diagnostic Plots for Real Salary", outer = TRUE, line = 1, cex = 1.2)

par(old_par)

# ============================================================================
# Model 3: Log-Transformed Nominal Salary
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("Model 3: log(Nominal Salary) ~ Education + Country + Category\n")
cat(strrep("=", 70), "\n\n")

model_industry_log_salary <- lm(log(Salary) ~ Education + Country + Category, 
                                data = df, 
                                subset = Category_Type == "Industry")
summary(model_industry_log_salary)

# Display formatted regression table
stargazer(model_industry_log_salary, type = "text")

# Coefficient plot
old_par <- par(no.readonly = TRUE)
par(mar = c(5, 18, 4, 2) + 0.1)

coefplot(model_industry_log_salary,
         title = "Regression Coefficients Plot (Log Nominal Salary)",
         xlab = "Coefficient Estimate",
         ylab = "Predictor Variables",
         sort = "magnitude")

par(old_par)

# Diagnostic plots
old_par <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
par(oma = c(0, 0, 3, 0))

plot(model_industry_log_salary)
mtext("Diagnostic Plots for Log(Salary)", outer = TRUE, line = 1, cex = 1.2)

par(old_par)

# ============================================================================
# Model 4: Log-Transformed Real Salary
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("Model 4: log(Real Salary) ~ Education + Country + Category\n")
cat(strrep("=", 70), "\n\n")

model_industry_log_real_salary <- lm(log(`Real Salary`) ~ Education + Country + Category, 
                                     data = df, 
                                     subset = Category_Type == "Industry")
summary(model_industry_log_real_salary)

# Coefficient plot
old_par <- par(no.readonly = TRUE)
par(mar = c(5, 18, 4, 2) + 0.1)

coefplot(model_industry_log_real_salary,
         title = "Regression Coefficients Plot (Log Real Salary)",
         xlab = "Coefficient Estimate",
         ylab = "Predictor Variables",
         sort = "magnitude")

par(old_par)

# Diagnostic plots
old_par <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
par(oma = c(0, 0, 3, 0))

plot(model_industry_log_real_salary)
mtext("Diagnostic Plots for Log(Real Salary)", outer = TRUE, line = 1, cex = 1.2)

par(old_par)

# ============================================================================
# Exploratory Data Visualization
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("Data Visualization: Salary Distribution by Education and Country\n")
cat(strrep("=", 70), "\n\n")

# Filter industry data
df_industry <- subset(df, Category_Type == "Industry")

# Set education factor levels
df_industry$Education <- factor(df_industry$Education,
                                levels = c("Junior High",
                                           "Senior High",
                                           "Junior College",
                                           "University",
                                           "Graduate School"))

# Plot 1: Nominal Salary by Education and Country
p1 <- ggplot(df_industry, aes(x = Education, y = Salary, color = Country)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.7) +
  labs(
    title = "Nominal Salary Distribution by Education and Country",
    x = "Education Level",
    y = "Nominal Salary",
    color = "Country"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)

# Plot 2: Real Salary by Education and Country
p2 <- ggplot(df_industry, aes(x = Education, y = `Real Salary`, color = Country)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.7) +
  labs(
    title = "Real Salary Distribution by Education and Country",
    x = "Education Level",
    y = "Real Salary",
    color = "Country"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)

# Plot 3: Nominal Salary by Category and Country
p3 <- ggplot(df_industry, aes(x = Category, y = Salary, color = Country)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.7) +
  labs(
    title = "Nominal Salary Distribution by Category and Country",
    x = "Category",
    y = "Nominal Salary",
    color = "Country"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)

# Plot 4: Real Salary by Category and Country
p4 <- ggplot(df_industry, aes(x = Category, y = `Real Salary`, color = Country)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.7) +
  labs(
    title = "Real Salary Distribution by Category and Country",
    x = "Category",
    y = "Real Salary",
    color = "Country"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p4)

# ============================================================================
# Model 5: Interaction Model (Education × Country)
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("Model 5: Interaction Model - Education × Country\n")
cat(strrep("=", 70), "\n\n")

model_eduxcon <- lm(Salary ~ Education * Country + Category, 
                    data = df, 
                    subset = Category_Type == "Industry")
summary(model_eduxcon)

# ANOVA test to compare models
cat("\nANOVA Test: Comparing Main Effects vs Interaction Model\n")
cat(strrep("-", 70), "\n")
anova(model_industry, model_eduxcon)

# ============================================================================
# Analysis Complete
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("Regression Analysis Complete\n")
cat(strrep("=", 70), "\n")
cat("Models fitted:\n")
cat("  1. Nominal Salary (Linear)\n")
cat("  2. Real Salary (Linear)\n")
cat("  3. log(Nominal Salary)\n")
cat("  4. log(Real Salary)\n")
cat("  5. Interaction Model (Education × Country)\n")
cat(strrep("=", 70), "\n")
