# Load libraries
library(tidyverse) # clean and transform data
library(janitor)   # clean messy data quickly and safely

# Load the raw data
raw_data <- read_csv("data/raw/customer_shopping_behavior_toclean.csv")

# Standardize column names (safe, structural step)
raw_data <- raw_data %>% clean_names()

# Create a working copy
clean_data <- raw_data

# Handle missing value
colSums(is.na(clean_data)) # Missing values count

missing_pct <- clean_data %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100))

missing_pct                # Missing percent

clean_data <- clean_data %>%  # Impute missing values with o
  mutate(
    review_rating = ifelse(
      is.na(review_rating),
      0,
      review_rating
    )
  )

clean_data <- clean_data %>%    # Convert those 0 s back to NA
  mutate(
    review_rating = ifelse(review_rating == 0, NA, review_rating)
  )

clean_data <- clean_data %>%  # Replace missing values with the mean
  mutate(
    review_rating = replace_na(
      review_rating,
      mean(review_rating, na.rm = TRUE)
    )
  )

sum(is.na(clean_data$review_rating)) # check if missing values are removed

# Duplicate detection
sum(duplicated(clean_data))  # Full-raw duplicates
clean_data %>%               # Business-key duplicates
  count(customer_id) %>%
  filter(n > 1)

clean_data <- clean_data %>% distinct() # remove Full-raw duplicates
clean_data <- clean_data %>%    # Business-key duplicates remove
  arrange(customer_id) %>%
  distinct(customer_id, .keep_all = TRUE)

# Standardize data
View(clean_data)  # View the data, when amount of data are not huge
head(clean_data, 10)
clean_data %>% 
  slice(1:10) %>% 
  print(width = Inf) # To display all column
str(clean_data)   # Check structure and data types
names(clean_data) # Column names

clean_data <- clean_data %>%     # Trim spaces
  mutate(across(where(is.character), str_trim))

clean_data <- clean_data %>%  # Standardize cases in a data (To start with high capital letter and other remains to be smallest one)
  mutate(across(where(is.character), str_to_title))

count(clean_data, gender)   # Count to check inconsistency
count(clean_data, payment_method)
count(clean_data, discount_applied)
count(clean_data, subscription_status)
count(clean_data, season)

clean_data <- clean_data %>%  # Map inconsistent values, when it is found
  mutate(
    gender = case_when(
      gender %in% c("Male", "M") ~ "Male",
      gender %in% c("Female", "F") ~ "Female",
      TRUE ~ "Unknown"
    )
  )

clean_data <- clean_data %>%  # Fix numeric stored as text, once it was found
  mutate(
    previous_purchases = as.numeric(previous_purchases)
  )     

summary(clean_data)   # to check ranges in general
clean_data <- clean_data %>%  # Set  valid ranges, when it is invalid
  filter(
    age >= 15 & age <= 100,
    purchase_amount_usd > 0
  )

# Creating a anew columns to categorize values of some variables (columns)
summary(clean_data$age) # check the range

clean_data <- clean_data %>% # Create age_group column
  mutate(
    age_group = case_when(
      age >= 18 & age <= 29 ~ "18–29",
      age >= 30 & age <= 44 ~ "30–44",
      age >= 45 & age <= 59 ~ "45–59",
      age >= 60             ~ "60+",
      TRUE                  ~ NA_character_
    )
  )
table(clean_data$age_group) # Check result

ggplot(clean_data, aes(x = age_group)) +
  geom_bar() +
  labs(title = "Customers by Age Group")  # Visual check 

summary(clean_data$review_rating) # Understand review rating

clean_data <- clean_data %>%   # Define meaningful review groups
  mutate(
    review_group = case_when(
      review_rating < 3.10                     ~ "Very Poor",
      review_rating >= 3.10  & review_rating < 3.57   ~ "Poor",
      review_rating >= 3.57  & review_rating < 4.40   ~ "Average",
      review_rating >= 4.40  & review_rating < 5.00 ~ "Good",
      review_rating >= 5.00                  ~ "Excellent",
      TRUE                                  ~ NA_character_
    )
  )
table(clean_data$review_group)

ggplot(clean_data, aes(x = review_group)) +
  geom_bar() +
  labs(title = "Customer Review Rating Groups")

# Removing non-necessary column
table(clean_data$promo_code_used, clean_data$discount_applied) # check wheather it is similar

clean_data <- clean_data %>%  # Remve one
  select(-promo_code_used)

names(clean_data) # Check result


# Outlier treatment (When exist)

Q1 <- quantile(clean_data$purchase_amount_usd, 0.25)
Q3 <- quantile(clean_data$purchase_amount_usd, 0.75)
IQR_val <- Q3 - Q1
upper_limit <- Q3 + 1.5 * IQR_val

clean_data <- clean_data %>%
  mutate(
    purchase_amount_usd = ifelse(
      purchase_amount_usd > upper_limit,
      upper_limit,
      purchase_amount_usd
    )
  )
# CREATE ANALYSIS-READY DATASET
summary(clean_data)
colSums(is.na(clean_data)) # Final validation

write_csv(clean_data, "data/processed/consumer_behavior_clean.csv") # Saving cleaned datasets