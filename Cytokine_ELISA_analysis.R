# Simulate cytokine ELISA data
cytokine_elisa_data <- data.frame(
  Sample_ID = paste0("S", 1:10),
  IL6_pg_ml = round(runif(10, 10, 200), 1),
  TNF_alpha_pg_ml = round(runif(10, 5, 150), 1),
  IL10_pg_ml = round(runif(10, 2, 100), 1),
  Group = rep(c("Control", "Treatment"), each = 5)
)

# Save to 'data' folder, first create the data folder
write.csv(cytokine_elisa_data, "data/cytokine_elisa_data.csv", row.names = FALSE)


# Load libraries
library(tidyverse)
# Load data
df <- read.csv("data/cytokine_elisa_data.csv")

# Convert from wide to long format
df_long <- df %>%
  pivot_longer(
    cols = starts_with("IL") | starts_with("TNF"), 
    names_to = "Cytokine",
    values_to = "Concentration_pg_mL"
  ) %>%
  rename(Condition = Group)

# Preview data
head(df)

# Now calculate basic stats
summary_stats <- df_long %>%
  group_by(Cytokine, Condition) %>%
  summarise(
    Mean = mean(Concentration_pg_mL),
    SD = sd(Concentration_pg_mL),
    .groups = "drop"
  )

# View the results
print(summary_stats)

# Visualization: Boxplot (Use df_long!)
my_plot <- ggplot(df_long, aes(x = Condition, y = Concentration_pg_mL, fill = Condition)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape = 21, width = 0.2, alpha = 0.6, color = "black") +
  facet_wrap(~ Cytokine) +
  scale_fill_brewer(palette = "Set2") +
  theme_classic()  +
  labs(title = "Cytokine Concentrations under Control vs. Treatment Conditions",
       y = "Concentration (pg/mL)",
       x = "Condition")
ggsave("cytokine_plot.png", plot = my_plot, width = 8, height = 6, dpi = 300)
print(my_plot)

# Optional: t-test per cytokine
library(dplyr)
library(purrr)

# Run t-test per Cytokine group
t_test_results <- df_long %>%
  group_by(Cytokine) %>%
  summarise(
    p_value = tryCatch(
      t.test(Concentration_pg_mL ~ Condition)$p.value,
      error = function(e) NA
    )
  )


