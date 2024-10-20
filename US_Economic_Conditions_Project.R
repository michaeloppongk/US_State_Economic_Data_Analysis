# Load necessary libraries
library(readr)
library(tidyverse)
library(janitor)
library(viridis)
library(scales)

# Step 1: Read the data
state_data <- read_csv("US_state_economic_data_1980_2018.csv")

# Step 2: Clean and transform the data
state_data_clean <- state_data %>%
  clean_names() %>%
  mutate(after_2000 = ifelse(year >= 2000, "Post-2000", "Pre-2000")) %>%
  mutate(population_growth_rate = (population - lag(population)) / lag(population) * 100) %>%
  mutate(personalincome = personalincome / 1e6)  # Convert personal income to millions for better readability

# Step 3: Data Aggregation
# Unemployment rate trends
unemployment_trend <- state_data_clean %>%
  group_by(state_name) %>%
  summarise(avg_unemployment_rate = mean(unemploymentrate, na.rm = TRUE))

# Personal income trend over time
income_trend <- state_data_clean %>%
  group_by(state_name) %>%
  summarise(avg_personal_income = mean(personalincome, na.rm = TRUE))

# Population growth rate trend by state
avg_population_growth <- state_data_clean %>%
  group_by(state_name) %>%
  summarise(avg_growth_rate = mean(population_growth_rate, na.rm = TRUE))

# Step 4: Visualization
# 1. Bar Plot for Unemployment Rate Trends by State
p1 <- ggplot(unemployment_trend, aes(x = reorder(state_name, -avg_unemployment_rate), 
                                     y = avg_unemployment_rate, 
                                     fill = state_name)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Average Unemployment Rate by State (1980-2018)",
       x = "State",
       y = "Average Unemployment Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

# 2. Bar Plot for Personal Income Trends by State
p2 <- ggplot(income_trend, aes(x = reorder(state_name, -avg_personal_income), 
                               y = avg_personal_income, 
                               fill = state_name)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_continuous(labels = comma) +  # Format y-axis labels to use comma notation
  labs(title = "Average Personal Income by State (1980-2018)",
       x = "State",
       y = "Average Personal Income (Thousands $)") +  # Adjusted the label for clarity
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

# 3. Bar Plot for Population Growth Rate by State
p3 <- ggplot(avg_population_growth, aes(x = reorder(state_name, -avg_growth_rate), 
                                        y = avg_growth_rate, 
                                        fill = state_name)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Average Population Growth Rate by State (1980-2018)",
       x = "State",
       y = "Population Growth Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

# 4. Compare pre- and post-2000 trends in unemployment rate
p4 <- ggplot(state_data_clean, aes(x = after_2000, y = unemploymentrate, fill = after_2000)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Unemployment Rate Comparison: Pre-2000 vs. Post-2000",
       x = "Period",
       y = "Unemployment Rate (%)") +
  theme_minimal()

# Step 5: Display plots
print(p1)
print(p2)
print(p3)
print(p4)
