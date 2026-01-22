# Load libraries
library(tidyverse)

# Load the analysis-ready dataset
clean_data <- read_csv("data/processed/consumer_behavior_clean.csv")
View(clean_data)
dim(clean_data)

# BASIC DESCRIPTIVE TRENDS (WHAT IS HAPPENING?)
clean_data %>%                    # Overall business snapshot
  summarise(
    total_customers = n_distinct(customer_id),
    total_transactions = n(),
    total_sales = sum(purchase_amount_usd),
    average_purchase = mean(purchase_amount_usd)
  )

category_trend <- clean_data %>%  # Trend by product category
  group_by(category) %>%
  summarise(
    total_sales = sum(purchase_amount_usd),
    avg_purchase = mean(purchase_amount_usd),
    transactions = n()
  ) %>%
  arrange(desc(total_sales))

category_trend

ggplot(category_trend,
       aes(x = reorder(category, total_sales),
           y = total_sales)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Total Sales by Product Category",
    x = "Product Category",
    y = "Total Sales (USD)"
  )
age_trend <- clean_data %>%   # Trend by age group
  group_by(age_group) %>%
  summarise(
    avg_purchase = mean(purchase_amount_usd),
    total_sales = sum(purchase_amount_usd),
    customers = n_distinct(customer_id)
  )

age_trend

ggplot(age_trend, aes(x = age_group, y = avg_purchase)) +
  geom_col() +
  labs(
    title = "Average Purchase Amount by Age Group",
    x = "Age Group",
    y = "Average Purchase (USD)"
  )

review_trend <- clean_data %>%      # Trend by review group (customer satisfaction)
  group_by(review_group) %>%
  summarise(
    avg_purchase = mean(purchase_amount_usd),
    transactions = n()
  )

review_trend
 
ggplot(review_trend, aes(x = review_group, y = avg_purchase)) +
  geom_col() +
  labs(
    title = "Average Purchase by Review Rating Group",
    x = "Review Group",
    y = "Average Purchase (USD)"
  )

discount_trend <- clean_data %>%       # Discount impact
  group_by(discount_applied) %>%
  summarise(
    avg_purchase = mean(purchase_amount_usd),
    total_sales = sum(purchase_amount_usd),
    transactions = n()
  )

discount_trend

ggplot(clean_data,
       aes(x = discount_applied,
           y = purchase_amount_usd)) +
  geom_boxplot() +
  labs(
    title = "Purchase Amount With vs Without Discount",
    x = "Discount Applied",
    y = "Purchase Amount (USD)"
  )

season_trend <- clean_data %>%      # Trend by season
  group_by(season) %>%
  summarise(
    avg_purchase = mean(purchase_amount_usd),
    transactions = n()
  )

season_trend

ggplot(season_trend, aes(x = season, y = avg_purchase)) +
  geom_col() +
  labs(
    title = "Average Purchase by Season",
    x = "Season",
    y = "Average Purchase (USD)"
  )

# SIMPLE PROJECTIONS
sales_model <- lm(
  purchase_amount_usd ~ age + discount_applied + previous_purchases,
  data = clean_data
)

summary(sales_model)    # Simple driver model (linear regression)

scenario_data <- clean_data %>%
  mutate(discount_applied = "Yes") # Create a scenario dataset



predicted_values <- predict(sales_model, newdata = scenario_data)

mean(predicted_values)  #  Predict what happens if more customers receive discounts?
       


