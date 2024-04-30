library(ggplot2)
# viewing the sales and customer data
View(sales)
View(customers)

#merging the sales and customer data
mergedData<-merge(sales[, c("ID", "Sales")],customers[, c("ID", "Age", "Country")])

# viewing the merged data
View(mergedData)

# Exploratory Data Analysis (EDA)
summary(mergedData)
head(mergedData)
dim(mergedData)

# Visualizing th data set
ggplot2::ggplot(mergedData, ggplot2::aes(x = Country, y = Sales)) +
  geom_bar(stat = "identity", color = "red") +  # "identity" preserves data values
  labs(title = "Sales by Country", x = "Country", y = "Sales") +
  theme_classic()

sessionInfo()