# ── Step 1: Multiple Regression with Control Variables ───────────────────────
reg_model_full <- lm(Avg_Pts ~ Cluster_Name + Avg_GF + Avg_GA + Seasons_Played,
                     data = team_data)
summary(reg_model_full)

#  Step 2: Confidence Intervals
confint(reg_model_full, level = 0.95)

# ── Step 3: Compare Model III vs Model IV ────────────────────────────────────
# R-squared comparison
cat("Model III R-squared:", round(summary(reg_model)$r.squared, 4), "\n")
cat("Model IV R-squared: ", round(summary(reg_model_full)$r.squared, 4), "\n")

# Formal model comparison using ANOVA
anova(reg_model, reg_model_full)

# ── Step 4: Check Regression Assumptions ─────────────────────────────────────

# 4a. Normality of residuals
shapiro.test(residuals(reg_model_full))

ggplot(data.frame(residuals = residuals(reg_model_full)),
       aes(sample = residuals)) +
  stat_qq(color = "steelblue", size = 2) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(title    = "Q-Q Plot of Residuals",
       subtitle = "Points should follow the red line for normality",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# 4b. Homoscedasticity (constant variance)
ggplot(data.frame(fitted   = fitted(reg_model_full),
                  residuals = residuals(reg_model_full)),
       aes(x = fitted, y = residuals)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, color = "red",
             linetype = "dashed", linewidth = 1) +
  labs(title    = "Residuals vs Fitted Values",
       subtitle = "Random scatter around 0 = homoscedasticity satisfied",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# 4c. Multicollinearity among control variables
library(car)
vif(reg_model_full)

# 4d. Influential observations
plot(reg_model_full, which = 4,
     main = "Cook's Distance — Influential Observations")