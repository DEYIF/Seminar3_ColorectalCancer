library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(corrplot)

# read file
setwd("D:/Code/R_code/seminar3")
data <- read.csv("data_task3.csv")
str(data)
summary(data)
# Log-transform the drug concentration
data$log_DV <- log(data$DV)
# Plot log-transformed concentration over time, remove legend, and save with customized size
p <- ggplot(data, aes(x = Time, y = log_DV, color = as.factor(Sex))) +
  geom_point(shape = 16, size = 2) +  # Shape 16 is a solid circle
  geom_line(aes(group = ID), alpha = 0.3) +
  labs(title = "Log-Transformed Concentration Over Time by Sex",
       x = "Time (hours)", y = "Log Concentration (mg/L)", color = "Sex") +
  scale_color_manual(values = c("0" = "#4141c9", "1" = "#c15050"),
                     labels = c("0" = "Female", "1" = "Male")) +  # Add legend labels for Sex
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
# Save the plot with customized dimensions
ggsave("log_concentration_time_plot_with_legend.jpg", plot = p, 
       width = 8, height = 6, units = "in", dpi = 300)
# Display the plot
print(p)

# Build the mixed-effects model with Time, BW, SEX, and AGE as fixed effects, and ID as a random effect
model <- lmer(log_DV ~ Time + Bw + Sex + Age + (1 | ID), data = data)
# Display model summary to examine the significance of each fixed effect
summary(model)
# Interpretation of fixed effects significance
fixed_effects <- summary(model)$coefficients
# Plot fitted values to see model prediction
data$fitted_log_DV <- fitted(model)
ggplot(data, aes(x = Time, y = log_DV, color = as.factor(Sex))) +
  geom_point() +
  geom_line(aes(y = fitted_log_DV, group = ID), linetype = "dashed") +
  labs(x = "Time (hours)", y = "Log Concentration (mg/L)", color = "Sex") +
  theme_minimal()

library(survival)
library(survminer)

data <- read.csv("data_task4.csv")
str(data)
head(data)

surv_obj <- Surv(time = data$Time, event = data$Status)

surv_diff <- survdiff(surv_obj ~ Group, data = data)
print(surv_diff)

cox_model <- coxph(surv_obj ~ Group, data = data)
summary(cox_model)

fit <- survfit(surv_obj ~ Group, data = data)
# Create the survival plot with customizations
surv_plot <- ggsurvplot(fit, data = data, pval = TRUE, 
                        xlab = "Time (Months)", ylab = "Survival Probability",
                        legend.labs = c("Control", "Treatment"),
                        legend.title = "Group", 
                        title = "Survival Analysis of Control vs Treatment Groups",
                        font.main = c(16, "bold", "black"), # Title font size, bold, and color
                        font.legend = c(14, "bold", "black")) # Legend font size and color

# Customize the theme: center the title and move the legend to the right
surv_plot$plot <- surv_plot$plot +
  theme(plot.title = element_text(hjust = 0.5), # Center the title
        legend.position = "right",               # Move the legend to the right
        legend.title = element_text(size = 14),  # Increase the legend title size
        legend.text = element_text(size = 12))   # Increase the legend text size
# Extract the plot (this is the ggplot object)
plot <- surv_plot$plot

# Save the plot with a specific size (e.g., 10 inches by 8 inches)
ggsave("survival_plot.png", plot = plot, width = 10, height = 8, dpi = 300)
