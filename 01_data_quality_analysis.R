# Load libraries
library(tidyverse) # clean and transform data
library(skimr)     # understand my data fast
library(naniar)    # Find and handle missing values

# Load the raw data
raw_data <- read_csv("data/raw/customer_shopping_behavior_toclean.csv")

# First look at the data (Structure check)
View(raw_data)  # View the data
dim(raw_data)   # Check dimensions
str(raw_data)   # Check structure and data types
names(raw_data) # Column names

# Missing value detection
missing_count <- raw_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

missing_count               # Missing values count

missing_pct <- raw_data %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100))

missing_pct                # Missing percent

# Duplicate detection
sum(duplicated(raw_data))  # duplicates
raw_data %>%               # Business-key duplicates
  count(`Customer ID`) %>%
  filter(n > 1)

# Inconsistency & validity checks
summary(raw_data)   # Descriptive statistics
raw_data %>% filter(`Purchase Amount (USD)` <= 0) # Logical checks
raw_data %>% filter(Age < 15 | Age > 100) 

# Outlier detection
numeric_cols <- raw_data %>%
  select(where(is.numeric))

names(numeric_cols)   # Extracting numerical columns

numeric_long <- numeric_cols %>%  
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  )                    # Convert data to long format(turns many numeric columns into:one column called variableone, column called value for plotting multiple boxplots.)

ggplot(numeric_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Outlier Detection Across Numeric Variables",
    x = "Variable",
    y = "Value"
  )                      # Plot boxplots for ALL numeric columns
ggplot(numeric_long, aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ variable, scales = "free") +
  labs(
    title = "Distribution of Numeric Variables",
    x = "Value",
    y = "Count"
  )                   # to look for skewness, long tails, and spike using Histogram for all column



ggplot(raw_data, aes(y = `Purchase Amount (USD)`)) +  
  geom_boxplot()            # Plot Boxplot for single column
ggplot(raw_data, aes(`Purchase Amount (USD)`)) +
  geom_histogram(bins = 50) # to look for skewness, long tails, and spike using Histogram for single column

# Optional but VERY useful) Outlier counts using IQR
outlier_summary <- numeric_cols %>%
  summarise(across(everything(), ~ {
    Q1 <- quantile(.x, 0.25, na.rm = TRUE)
    Q3 <- quantile(.x, 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    sum(.x < (Q1 - 1.5 * IQR_val) | .x > (Q3 + 1.5 * IQR_val), na.rm = TRUE)
  }))

outlier_summary

