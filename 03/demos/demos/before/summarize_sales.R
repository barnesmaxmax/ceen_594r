library(tidyverse)

# generate some fake data that looks like the real data
sales <- data.frame(
  neighborhood = c("Peaceful Glen", "Peaceful Glen", "Peaceful Glen", 
    "Peaceful Glen", "Pleasant Valley", "Pleasant Valley", "Pleasant Valley", 
    "Pleasant Valley", "Pleasant Valley", "Pleasant Valley"), 
  property_class = c("Dwelling", "Dwelling", "Mobile Home", 
    "Mobile Home", "Commercial", "Commercial", "Dwelling", "Dwelling", 
    "Mobile Home", "Mobile Home"), 
  sales_price = c(229900, 349900, 235000, 290000, 3400000, 940250, 
    510000, 429382, 90000, 40000)
)

# first five rows of fake data
head(sales, n = 5)

# number of rows of fake data
nrow(sales)

# keep just "Dwelling" sales
dwelling_sales <- filter(sales, 
  property_class == "Dwelling")

# average price by neighborhood
dwelling_sales %>% 
  group_by(neighborhood) %>% 
  summarize(
    n_sales = n(),
    mean_price = mean(sales_price),
    median_price = median(sales_price)
  ) %>% 
  arrange(median_price)

# plot all sale prices by neighborhood
dwelling_sales %>% 
  ggplot(aes(sales_price)) +
  geom_histogram() + 
  scale_x_log10(labels = scales::label_dollar()) +
  facet_wrap(~ neighborhood, scale = "free_y") +
  labs(
    x = "Sales price",
    y = "Number of sales"
  )