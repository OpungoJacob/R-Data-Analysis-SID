import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Assuming 'sales' and 'customers' are pandas DataFrames loaded from some data source
# For example, if they were CSV files:
sales = pd.read_csv('sales.csv')
customers = pd.read_csv('customers.csv')

df_sales = pd.DataFrame(sales)
df_customers = pd.DataFrame(customers)


# Merging the sales and customer data
mergedData = pd.merge(df_sales[['ID', 'Sales']], df_customers[['ID', 'Age', 'Country']], on='ID')

df_mergedData = pd.DataFrame(mergedData)

# Viewing the merged data - in Python, this would typically be done in a Jupyter Notebook or similar
print(df_mergedData.head())  # For a quick view in a script

# Exploratory Data Analysis (EDA)
print(df_mergedData.describe())  # Summary statistics for numerical columns
print(df_mergedData.head(20))  # View the first few rows
print(df_mergedData.shape)  # View the dimensions of the DataFrame

# Visualizing the data set
plt.figure(figsize=(10, 10))
sns.barplot(x='Country', y='Sales', data=df_mergedData, color='red')
plt.title('Sales by Country')
plt.xlabel('Country')
plt.ylabel('Sales')
plt.xticks(rotation=45)  # Rotate labels to avoid overlap
plt.show()

plt.figure(figsize=(10, 10))
sns.barplot(x='Country', y='Age', data=df_mergedData, color='red')
plt.title('Age by Country')
plt.xlabel('Country')
plt.ylabel('Age')
plt.xticks(rotation=45)  # Rotate labels to avoid overlap
plt.show()
