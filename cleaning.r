library(dplyr)
library(ggplot2)
library(plotrix)

# Read the dirty sales data from an external CSV file
sales <- read.csv("C:/Users/HP/Desktop/x-company/x-company/unclean data/sales.csv")

# Read the dirty customers data from an external CSV file
customers <- read.csv("C:/Users/HP/Desktop/x-company/x-company/unclean data/customers.csv")

# Check the structure of the data frames
str(sales)
str(customers)


# Check for missing values in sales table
missing_sales <- sum(is.na(sales))

# Remove rows with missing values
clean_sales <- na.omit(sales)

# Remove duplicates in sales table
clean_sales <- distinct(clean_sales)



# Check for missing values in customers table
missing_customers <- sum(is.na(customers))

# Remove rows with missing values
clean_customers <- na.omit(customers)

# Remove duplicates in customers table
clean_customers <- distinct(clean_customers)


# Specify the storage location for the cleaned files
clean_sales_file <- "C:/Users/HP/Desktop/x-company/x-company/clean_sales.csv"
clean_customers_file <- "C:/Users/HP/Desktop/x-company/x-company/clean_customers.csv"

# Save cleaned data to new files
write.csv(clean_sales, file = clean_sales_file, row.names = FALSE)
write.csv(clean_customers, file = clean_customers_file, row.names = FALSE)



# Summary of cleaning process
cat("Cleaning Summary:\n")
cat("Sales Table:\n")
cat("Initial rows:", nrow(sales), "\n")
cat("Rows after removing missing values:", nrow(clean_sales), "\n")
cat("Rows after removing duplicates:", nrow(distinct(clean_sales)), "\n")
cat("\n")
cat("Customers Table:\n")
cat("Initial rows:", nrow(customers), "\n")
cat("Rows after removing missing values:", nrow(clean_customers), "\n")
cat("Rows after removing duplicates:", nrow(distinct(clean_customers)), "\n")


#viewing the clean data
View(clean_customers)
View(clean_sales)

#merging the sales and customer data
mergedData<-merge(clean_sales[, c("ID", "Sales")],clean_customers[, c("ID", "Age", "Country")])

# viewing the merged data
View(mergedData)

# Exploratory Data Analysis (EDA)
summary(mergedData)
head(mergedData)
dim(mergedData)

# Visualizing the data set
ggplot2::ggplot(mergedData, ggplot2::aes(x = Country, y = Sales)) +
  geom_bar(stat = "identity", color = "red") +  # "identity" preserves data values
  labs(title = "Sales by Country", x = "Country", y = "Sales") +
  theme_classic()

# 2. Sales by Age Group
mergedData$Age_Group <- cut(mergedData$Age, breaks = c(0, 20, 30, 40, 50, 60, Inf),
                            labels = c("0-20", "21-30", "31-40", "41-50", "51-60", "61+"))

ggplot(mergedData, aes(x = Age_Group, y = Sales)) +
  geom_bar(stat = "summary", fun = "sum", fill = "lightgreen") +
  labs(title = "Sales by Age Group", x = "Age Group", y = "Total Sales") +
  theme_bw()

# 3. Sales by Country and Age Group
ggplot(mergedData, aes(x = Country, y = Sales, fill = Age_Group)) +
  geom_bar(stat = "summary", fun = "sum", position = "dodge") +
  labs(title = "Sales by Country and Age Group", x = "Country", y = "Total Sales", fill = "Age Group") +
  theme_minimal()

# 4. Customer Distribution by Country
ggplot(mergedData, aes(x = Country)) +
  geom_bar(fill = "salmon") +
  labs(title = "Customer Distribution by Country", x = "Country", y = "Number of Customers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Pie Chart Age
age_groups <- cut(mergedData$Age, breaks = c(0, 20, 30, 40, 50, 60, Inf),
                  labels = c("0-20", "21-30", "31-40", "41-50", "51-60", "61+"))

pie_data <- table(age_groups)
pie(pie_data, main = "Age Group Distribution")

#3d pie chart
library(plotrix)

# Create pie chart data
pie_data <- table(mergedData$Age)

# Get colors for each segment
pie_colors <- rainbow(length(pie_data))

# Convert data to proportions
pie_data <- prop.table(pie_data)

# Plot 3D pie chart
pie3D(pie_data, labels = names(pie_data), col = pie_colors, main = "3D Pie Chart")


#scatter graph

ggplot(mergedData, aes(x = Age, y = Sales)) +
  geom_point() +  # Add points for scatter plot
  geom_smooth(method = "lm", se = FALSE) +  # Add regression line
  labs(title = "Scatter Plot with Line of Best Fit", x = "Age", y = "Sales")

#scatter graph 3d



# Line Graph Example
ggplot(mergedData, aes(x = Age, y = Sales)) +
  geom_line(color = "blue") +
  labs(title = "Sales Trend by Age", x = "Age", y = "Sales")

sessionInfo()