
# Setup -------------------------------------------------------------------

# Install "tidyverse" packages
install.packages("tidyverse")

# Load tidyverse packages
library(tidyverse)

# Load data
df <- read_csv("excel-to-r.csv", col_types = cols(`most recent gift amount` = col_number(), 
                                                  `most recent gift date` = col_date(format = "%m/%d/%y"), 
                                                  `lifetime giving` = col_number()))

# Clean column names
colnames(df)
df <- janitor::clean_names(df)

# Looking at the Data -----------------------------------------------------

# There are a number of ways to take a look at your data at this point. The
# first is to click on the name of the data set in the Environment tab. Here are
# some other ways from within code.

# Structure: this is a built-in function of R
str(df)

# Glimpse: a more compact version of glimpse that comes with the tidyverse
glimpse(df)

# View: creates a new window to look at the data
view(df)

# Getting Started ---------------------------------------------------------

# We're going to approach this session from the perspective of doing prospecting
# in Excel. We've started with a data set from our CRM and we're going to
# append some additional data, create some new columns, and filter the data to
# create a list of potential prospects for our gift officer.

# Conventions -------------------------------------------------------------

# Consider this a cheat sheet of sorts for what's to come:

# #         Comment out a line
# <-        The assignment operator (Alt/Option + -)
# %>%       The pipe symbol (Ctrl/Cmd + Shift + M)
# $         Let's us pick a specific column of a data set
# ==        Comparison operator (x equals y)
# !=        Comparison operator (x does not equal y)
# in.na()   Blank cells
# !is.na()  Is not blank

# Filter    Very similar to the filter functionality in Excel
# Select    Pick columns
# Mutate    Create new data (calculations, new columns, modify existing data, etc.)
# Count
# Group By
# Summarize 

# Ratings -----------------------------------------------------------------

# Let's start by looking at our ratings. What values do we have in this data
# set? The "unique" function returns all of the different values within the
# specific field(s). In this case, we've put df$rating inside the parenthases.
# That tells R that we want the unique values of the rating column inside the df
# data set.
unique(df$rating)

# In the console, you can see the list of ratings there. Let's look at who our
# $10M+ rated folks are. Here's where we really start diving into R and the
# tidyverse. The first thing we're going to do is call "df" to tell R that where
# we're starting. Next, we'll put in this weird symbol "%>%" called a pipe. The
# pipe is part of the tidyverse and connects pieces of code like a workflow. On
# the next line we see "filter(rating == "$10M+)". Altogether, you could read
# this as:
# 
# (1) Take the df data frame
# (2) Then pipe or send it to the filter command
# (3) Where we want to find ratings that equal (note: ==) $10M+
# (4) Display the results in the console
df %>% 
  filter(rating == "$10M+")

# If only our real data sets could get us 24,709 prospects rated that highly!

# While that's certainly useful, wouldn't it be easier if we could enter a
# numerical value for rating instead of writing out the actual rating each time?
# This would be especially helpful if you want a range of ratings.

# VLOOKUP (Excel) vs. Join (R) --------------------------------------------

# Let's append numerical rating values by creating a reference table and
# recreating the VLOOKUP functionality that you know from Excel. We're going to
# use a tibble for this functionality. A tibble is simply the name for a
# tidyverse-styled data set.

# The structure of a tibble is:   tibble(column_1_name = c(val1, val2, etc.),
#                                        column_2_name = c(val1, val2, etc.),
#                                        column_3_name = c(val1, val2, etc.))

# Create a lookup table of values to append to df. Here he use the assignment
# operator <- to assign our tibble the name "rating_values"
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

# Join df & rating_values (VLOOKUP)
df <- left_join(df, rating_values)
# df <- left_join(df, rating_values, by = c("rating" = "rating")) # if your columns to match on aren't named the same

# Create a rating bucket. Here we're using the mutate command to create a new
# variable based on the rating field. We're using an if_else statement, just
# like you would in Excel.
df <- df %>%
  mutate(rating_bucket = if_else(
    rating_value >= 1000000,
    "PG",
    if_else(rating_value >= 100000, "MG", "LG")
  ))
# What that code did was look at the rating value column that we joined in in
# the last command and, if it was greater than 1,000,000, make the value in the
# new rating_bucket column "PG" for principal gift. If the value didn't match
# that criteria, it moves down to the 100,000 level and assigns anything that is
# greater than or equal to 100,000  to the "MG" bucket, and everything else is
# given the "LG" bucket.

# Add a Giving Bucket -----------------------------------------------------

# Here we're going to do a similar exercise with our giving amounts, but using the cut command.
df <- df %>% 
  mutate(most_recent_gift_amount_bucket = cut(df$most_recent_gift_amount,
                                              breaks = c(0, 1, 99999, 999999, Inf),
                                              labels = c("No Giving",
                                                         "Leadership Gift",
                                                         "Major Gift",
                                                         "Principal Gift")))

# Prospecting Using R -----------------------------------------------------

# How many prospects do we have by rating?
df %>% 
  count(rating_bucket)

# How many prospects are there in Texas?
df %>% 
  filter(st == "TX") %>% 
  count(rating_bucket)

# How many prospects are rated at least at the major gift level, live in Texas,
# and have lifetime giving of at least $10,000?
df %>% 
  filter(rating_value >= 100000,
         st == "TX",
         lifetime_giving >= 10000) %>% 
  count()

# How many prospects are rated at least at the major gift level, live in Texas,
# have lifetime giving of at least $10,000, and are alumni?
pool <- df %>% 
  filter(rating_value >= 100000) %>% 
  filter(st %in% c("TX", "CA")) %>% 
  filter(!is.na(school)) %>% 
  filter(lifetime_giving >= 10000) %>% 
  # filter(school == "Engineering") %>% 
  {.}

# Data Visualization ------------------------------------------------------

options(scipen = 99)
library(scales)

df %>% 
  filter(!is.na(school)) %>% 
  ggplot(aes(school, lifetime_giving)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = dollar) +
  labs(title = "How much have our alumni giving over their lifetimes?",
       x = "School",
       y = "Lifetime Giving",
       fill = "") +
  theme_minimal()

df %>% 
  filter(!is.na(school)) %>% 
  ggplot(aes(school, lifetime_giving)) +
  # geom_point(color = "steelblue") +
  geom_jitter(color = "steelblue", alpha = 0.5) +
  scale_y_continuous(labels = dollar) +
  labs(title = "How much have our alumni giving over their lifetimes?",
       x = "School",
       y = "Lifetime Giving",
       fill = "") +
  theme_minimal()

# Exporting Data ----------------------------------------------------------

write_csv(df, "filepath.csv")

