##############################
## 
## Excel to R
## Apra Data Science Now 2020
##
## Matt Farrow
## @_mattfarrow
##
## https://matt-farrow.shinyapps.io/excel-to-r/
##
##############################

# Setup -------------------------------------------------------------------

# Install "tidyverse" packages
install.packages("tidyverse")

# Once the packages are installed, we need to tell R that you want to use them.
# Load tidyverse packages
library(tidyverse)

# Load Data ---------------------------------------------------------------

# Read data
df <- read_csv(
  "excel-to-r.csv",
  col_types = cols(
    `most recent gift amount` = col_number(),
    `most recent gift date` = col_date(format = "%m/%d/%y"),
    `lifetime giving` = col_number()
  )
)

# Look at column names
colnames(df)

# Consistently format column names using the janitor package.
df <- janitor::clean_names(df)
colnames(df)

# Look at the data --------------------------------------------------------

# Structure: this is a built-in function of R
str(df)

# Glimpse: a more compact version of str that comes with the tidyverse
glimpse(df)

# Filtering ---------------------------------------------------------------

# Find prospects whose most recent gift was on or after 6/1/2019
df %>%
  filter(most_recent_gift_date >= "2019-06-01")

# Find prospects whose most recent gift was on or after 6/1/2019 and who live in Texas
df %>%
  filter(most_recent_gift_date >= "2019-06-01",
         st == "TX")

df %>%
  filter(most_recent_gift_date >= "2019-06-01" |
           st == "TX")

# Joining Data ------------------------------------------------------------

# Look at the unique values of the rating column
unique(df$rating)

# Who are the prospects rated $10M+
df %>% 
  filter(rating == "$10M+")

# While that's certainly useful, it would be easier if we could enter a
# numerical value for rating instead of writing out the actual rating each time.
# In Excel, you might have used a VLOOKUP function to accomplish that task. In
# R, we'll use a join.

# Create a rating lookup table
rating_values <- tibble(rating = c("Less than $10K",
                                   "$10K-$24K",
                                   "$25K-$49K",
                                   "$50K-$99K",
                                   "$100K-$249K",
                                   "$250K-$999K",
                                   "$1M-$2.49M",
                                   "$2.5M-$4.99M",
                                   "$5M-$9.99M",
                                   "$10M+"),
                        rating_value = c(1,
                                         10000,
                                         25000,
                                         50000,
                                         100000,
                                         250000,
                                         1000000,
                                         2500000,
                                         5000000,
                                         10000000))

# Print results
rating_values

# Turn off scientific notiation if needed
options(scipen = 50)  # any significantly high number will work, I tend to use 50

# Print results again
rating_values

# Join together df & rating_values
prospecting <- left_join(df, rating_values)

# If you have lookup column names that don't match:
# prospecting <- left_join(df, rating_values, by = c("df_id_column" = "rating_values_id_column"))

# Look at the newly merged data
prospecting

# Mutating Data -----------------------------------------------------------

# Create a rating bucket column
prospecting <- prospecting %>%
  mutate(rating_bucket = if_else(
    rating_value >= 1000000,
    "PG",
    if_else(rating_value >= 100000, "MG", "LG")
  ))

# Look at the changes
prospecting

# Select ------------------------------------------------------------------

# View only the three rating columns
prospecting %>% 
  select(rating,
         rating_value,
         rating_bucket)

# Rearrange the columns so that all of the rating columns are together
prospecting %>% 
  select(lookup_id,
         name_full,
         city,
         st,
         school,
         rating,
         rating_value,             # New location
         rating_bucket,            # New location
         most_recent_gift_amount,
         most_recent_gift_date,
         lifetime_giving)

# Prospecting -------------------------------------------------------------

# How many prospects do we have by rating?
prospecting %>% 
  count(rating_bucket)

# How many prospects are there in Texas?
prospecting %>% 
  filter(st == "TX") %>% 
  count(rating_bucket)

# How many prospects are rated at least at the major gift level, live in Texas,
# and have lifetime giving of at least $10,000?
prospecting %>% 
  filter(rating_value >= 100000,
         st == "TX",
         lifetime_giving >= 10000) %>% 
  count() %>% 
  rename(prospects = n)  # Here's how you can rename columns (new_name = old_name)

# How many prospects are rated at least at the major gift level, live in Texas,
# have lifetime giving of at least $10,000, and are alumni?
pool <- prospecting %>% 
  filter(rating_value >= 100000) %>% 
  filter(st %in% c("TX", "CA")) %>% 
  filter(!is.na(school)) %>% 
  filter(lifetime_giving >= 10000)

# Prospecting practice! Feel free to comment/uncomment the rows and play with
# different options to filter the data set.
prospecting %>% 
  # filter(st %in% c("CA", "TX", "NY")) %>% 
  # filter(school == "Engineering") %>% 
  # filter(rating == "$25K-$49K") %>% 
  # filter(rating_value >= 100000) %>% 
  # filter(rating_bucket == "MG") %>% 
  # filter(most_recent_gift_amount > 1000) %>% 
  # filter(most_recent_gift_date >= "2019-01-01") %>% 
  # filter(lifetime_giving >= 100000) %>% 
  {.}

# Visualization -----------------------------------------------------------

# The scales package isn't necessary, but it does allow me to format numbers as
# currency, percentages, numbers with commas, etc.
library(scales)

# Lifetime Giving
prospecting %>% 
  filter(!is.na(school)) %>% 
  ggplot(aes(school, lifetime_giving)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "How much have our alumni given over their lifetimes?",
       x = "School",
       y = "Lifetime Giving",
       fill = "") +
  theme_minimal()

# Lifetime Giving (another view)
prospecting %>% 
  filter(!is.na(school)) %>% 
  ggplot(aes(school, lifetime_giving)) +
  # geom_point(color = "steelblue") +
  geom_jitter(color = "steelblue", alpha = 0.5) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "How much have our alumni given over their lifetimes?",
       x = "School",
       y = "Lifetime Giving",
       fill = "") +
  theme_gray()

# Prospects by State
prospecting %>% 
  ggplot(aes(st, fill = rating_bucket)) +
  geom_bar() +
  labs(title = "How many prospects do we have in each state?",
       fill = "Rating Bucket") +
  coord_flip() +
  theme_bw()

# Most recent gift
prospecting %>% 
  mutate(last_gift = if_else(most_recent_gift_date >= "2019-06-01", "This Year",
                             if_else(most_recent_gift_date >= "2018-06-01", "LYBUNT",
                                     if_else(most_recent_gift_date < "2018-06-01", "SYBUNT", "Non-Donor"))),
         last_gift = replace_na(last_gift, "Non-Donor")) %>% 
  ggplot(aes(last_gift)) +
  geom_bar()

# Giving by school
prospecting %>% 
  filter(!is.na(school)) %>% 
  group_by(school) %>% 
  summarise(mean_gift = mean(most_recent_gift_amount, na.rm = TRUE)) %>% 
  ggplot(aes(school, mean_gift, label = dollar(mean_gift))) +
  geom_point(color = "red") +
  geom_text(hjust = 1.15) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Average Size of Most Recent Gift by School",
       x = "",
       y = "Average of Most Recent Gift") +
  theme_classic()

