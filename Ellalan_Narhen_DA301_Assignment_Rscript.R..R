#Visualise data to gather insights

# Determining the working directory.
getwd()

# Install the tidyverse library.
install.packages('tidyverse')
# Import the tidyverse library.
library(tidyverse)

#Importing the CSV File
turtle_sales <- read.csv(file.choose(), header=TRUE) 

# Print/return the data frame.
turtle_sales

##Exploring the dataset

#Remove redundant columns (Ranking, Year, Genre, Publisher) 
salesdf <- subset(turtle_sales, select = -c(Ranking, Year, 
                                                  Genre, 
                                                  Publisher))
#Viewing new dataframe to ensure columns are removed.
View(salesdf)

#Summary of dataset
summary(salesdf)

#First 5 rows of new df
head(salesdf, n=5)

#Last 5 rows of new df
tail(salesdf, n=5)

#Dimensions of new df
dim(salesdf)

#Using str() function to see datatypes
str(salesdf)

# Importing psych library.
install.packages("psych")
library("psych")

# Importing DescTools library.
install.packages("DescTools")
library ("DescTools")

## Create plots to review and determine insights into the sales data set
# Importing ggplot2.
install.packages('ggplot2')
library("cowplot")

# Scatterplot for NA_Sales vs Global_Sales.
qplot(NA_Sales, Global_Sales, data=salesdf) + 
  ggtitle("NA_Sales vs Global_Sales")

# Scatterplot of NA_Sales vs EU_Sales.
qplot(NA_Sales, EU_Sales, data=salesdf) + 
  ggtitle("NA_Sales vs EU_Sales")

# Scatterplot for Global_Sales vs EU_Sales.
qplot(EU_Sales, Global_Sales, data=salesdf) + 
  ggtitle("Global_Sales vs EU_Sales")

# Scatterplot for Product on its own.
qplot(y=Product, data=salesdf) + 
  ggtitle("Product")

# Scatterplot for product vs EU_Sales.
qplot(Product, EU_Sales, data=salesdf) + 
  ggtitle("Product vs EU_Sales")

## Scatterplot for product vs Global_Sales.
qplot(Product, Global_Sales, data=salesdf) + 
  ggtitle("Product vs Global_Sales")

## Histogram for Product
qplot(Product, bins=12, data=salesdf) + ggtitle("Product Histogram")

## Boxplot for Product
qplot(Product, data=salesdf, geom='boxplot') + 
  ggtitle("Product Boxplot")

## Barchart for Platform
qplot(Platform, data=salesdf) + ggtitle("Platform Barchart")

## Boxplot for NA_Sales
qplot(NA_Sales, data=salesdf, geom='boxplot') + 
  ggtitle("NA_Sales Boxplot")

## Boxplot for EU_Sales
qplot(EU_Sales, data=salesdf, geom='boxplot') + 
  ggtitle("EU_Sales Boxplot")

## Boxplot for Global_Sales
qplot(Global_Sales, data=salesdf, geom='boxplot') + 
  ggtitle("Global_Sales Boxplot")

## Boxplot for NA_Sales split by Platform
qplot(NA_Sales, Platform, data=salesdf, geom='boxplot') + 
  ggtitle("NA_Sales by Platform Boxplot")

## Boxplot for EU_Sales split by Platform
qplot(EU_Sales, Platform, data=salesdf, geom='boxplot') + 
  ggtitle("EU_Sales by Platform Boxplot")

## Boxplot for Global_Sales split by Platform
qplot(Global_Sales, Platform, data=salesdf, geom='boxplot') + 
  ggtitle("Global_Sales by Platform Boxplot")

################################################################################

# Clean, manipulate, and visualise the data

################################################################################

#Checking dimensions of Salesdf
dim(salesdf)

#Datatypes of Salesdf
str(salesdf)

##Determining the min, max and mean values of all the sales data.

#NA_Sales
min(salesdf$NA_Sales)
max(salesdf$NA_Sales)
mean(salesdf$NA_Sales)

#EU_Sales
min(salesdf$EU_Sales)
max(salesdf$EU_Sales)
mean(salesdf$EU_Sales)

#Global_Sales
min(salesdf$Global_Sales)
max(salesdf$Global_Sales)
mean(salesdf$Global_Sales)

#Summary of Salesdf
summary(salesdf)

##Determining the impact on sales per product_id

# Creating new Data Frame without Platform.
sales_prod_df <- subset(salesdf, select = -c(Platform))

# Viewing new Data Frame.
head(sales_prod_df, n=2)


# Summing the sales value grouped by product_id
# and saving this into a new Data Frame.
sales_prod_sum <- aggregate(. ~ Product, sales_prod_df, sum)

dim(sales_prod_sum)

# Viewing first two rows of Data Frame.
head(sales_prod_sum, n=2)

# Summary of sales_product_sum Data Frame.
summary(sales_prod_sum)

# Creating scatter plots, histograms, and box plots to gain insight into the 
# sales data.

# Histogram for frequency of product.
Product_NA_Histogram <- hist(sales_prod_sum$Product) 

# Barplot for product vs sum of NA_Sales.
barplot(sales_prod_sum$NA_Sales ~ salesprod_sum$Product, 
        main="Product vs sum of NA_Sales",
        xlab="Product",
        ylab="NA_Sales sum",
        density=10)

# Barplot for product vs sum of EU_Sales.
barplot(sales_prod_sum$EU_Sales ~ salesprod_sum$Product, 
        main="Product vs sum of EU_Sales",
        xlab="Product",
        ylab="EU_Sales sum",
        density=10)

# Barplot for product vs sum of Global_Sales.
barplot(sales_prod_sum$Global_Sales ~ salesprod_sum$Product, 
        main="Product vs sum of Global_Sales",
        xlab="Product",
        ylab="Global_Sales sum",
        density=10)

remove.packages("tidyverse");install.packages("tidyverse")

# Box plot for sum of EU_Sales.
EU_Sales_Sum_boxplot <- ggplot(sales_prod_sum, aes(x=EU_Sales), xlim=40) +
  stat_boxplot(geom='errorbar', width=0.5) +
  geom_boxplot() +
  coord_cartesian(xlim = c(0, 25)) 

EU_Sales_Sum_boxplot

# Box plot for sum of Global_Sales.
Global_Sales_Sum_boxplot <- ggplot(sales_prod_sum, aes(x=Global_Sales), 
                                   xlim=40) +
  stat_boxplot(geom='errorbar', width=0.5) +
  geom_boxplot() +
  coord_cartesian(xlim = c(0, 65)) 

Global_Sales_Sum_boxplot

#Box plot for sum of NA_Sales.
NASales_Sum_boxplot <- ggplot(sales_prod_sum, aes(x=NA_Sales), xlim=40) +
  stat_boxplot(geom='errorbar', width=0.5) +
  geom_boxplot() +
  coord_cartesian(xlim = c(0, 35)) 

NASales_Sum_boxplot

#NA_Sales Scatterplot
qplot(Product, NA_Sales, data=sales_prod_sum)

#EU_Sales Scatterplot
qplot(Product, EU_Sales, data=sales_prod_sum)

#Global_Sales Scatterplot
qplot(Product, Global_Sales, data=sales_prod_sum)

## Determining Normality of dataset

# QQ plot for NA_Sales.
qqnorm(sales_prod_sum$NA_Sales, main="NA_Sales Normal Q-Q Plot")
qqline(sales_prod_sum$NA_Sales)

# QQ plot for EU_Sales.
qqnorm(sales_prod_sum$EU_Sales, main="EU_Sales Normal Q-Q Plot")
qqline(sales_prod_sum$EU_Sales)

# QQ plot for Global_Sales.
qqnorm(sales_prod_sum$Global_Sales, main="Global_Sales Normal Q-Q Plot")
qqline(sales_prod_sum$Global_Sales)

## Performing a Shapiro-Wilk test on all the sales data.

# Shapiro-Wilk test for NA_Sales.
shapiro.test(sales_prod_sum$NA_Sales)

# Shapiro-Wilk test for EU_Sales.
shapiro.test(sales_prod_sum$EU_Sales)

# Shapiro-Wilk test for Global_Sales.
shapiro.test(sales_prod_sum$Global_Sales)

# All p values are <= to 0.05 which means that we can reject the hypothesis
# of normality. This means that all 3 datasets are not normally distributed.

## Determining the Skewness and Kurtosis of all the sales data.

#Install necessary packages
install.packages("moments")
library(moments)

#Important to note that: 
#skewness > 1 indicates highly skewed data and
#kurtosis < 2 indicates normal distribution.

# Skewness and Kurtosis for NA_Sales.
skewness(sales_prod_sum$NA_Sales) # = 3.048198>1, so highly skewed data.
kurtosis(sales_prod_sum$NA_Sales) # = 15.6026>2, so not normally distributed.

# Skewness and Kurtosis for EU_Sales.
skewness(sales_prod_sum$EU_Sales) # =  2.886029>1, highly skewed data.
kurtosis(sales_prod_sum$EU_Sales) # = 16.22554>2, not normally distributed.

# Skewness and Kurtosis for Global_Sales.
skewness(sales_prod_sum$Global_Sales) # = 3.066769>1, highly skewed data.
kurtosis(sales_prod_sum$Global_Sales) # = 17.79072>2, not normally distributed.

## Determining correlation between the sales data columns.

#Correlation between NA_Sales and EU_Sales.

# Pearson correlation test.
cor.test(sales_prod_sum$NA_Sales, sales_prod_sum$EU_Sales, 
    method=c("pearson"))

# Kendall rank correlation test.
cor.test(sales_prod_sum$NA_Sales, sales_prod_sum$EU_Sales,
    method=c("kendall"))

# Spearman rank correlation coefficient.
cor.test(sales_prod_sum$NA_Sales, sales_prod_sum$EU_Sales,
    method = c("spearman"))


# Correlation between NA_Sales and Global_Sales.
# Pearson correlation test.
cor(sales_prod_sum$NA_Sales, sales_prod_sum$Global_Sales, 
    method=c("pearson"))

# Kendall rank correlation test.
cor(sales_prod_sum$NA_Sales, sales_prod_sum$Global_Sales,
    method=c("kendall"))

# Spearman rank correlation coefficient.
cor(sales_prod_sum$NA_Sales, sales_prod_sum$Global_Sales,
    method = c("spearman"))


# Correlation between EU_Sales and Global_Sales.
# Pearson correlation test.
cor(sales_prod_sum$EU_Sales, sales_prod_sum$Global_Sales, 
    method=c("pearson"))

# Kendall rank correlation test.
cor(sales_prod_sum$EU_Sales, sales_prod_sum$Global_Sales,
    method=c("kendall"))

# Spearman rank correlation coefficient.
cor(sales_prod_sum$EU_Sales, sales_prod_sum$Global_Sales,
    method = c("spearman"))

################################################################################

# Making recommendations to the business

################################################################################

# Exploring sales_prod_sum df
dim(sales_prod_sum)
head(sales_prod_sum)
tail(sales_prod_sum)

##Creating a simple linear regression model (SLRM) and visualizing them.

#SLRM between NA_Sales and EU_Sales
NA_EU = lm(NA_Sales~EU_Sales, data = sales_prod_sum) 
# Viewing the results.
summary(NA_EU)

#Plot of SLRM between NA_Sales and EU_Sales
NA_EU_plot = plot(NA_Sales~EU_Sales, data = sales_prod_sum) +
  title(main="Linear Regression")
abline(NA_EU)

#SLRM between NA_Sales and Global_Sales
NA_Global = lm(NA_Sales~Global_Sales, data = sales_prod_sum) 
# Viewing the results.
summary(NA_Global)

#Plot of SLRM between NA_Sales and Global_Sales
NA_Global_plot = plot(NA_Sales~Global_Sales, data = sales_prod_sum) +
  title(main="Linear Regression")
abline(NA_Global)

#SLRM between EU_Sales and Global_Sales
EU_Global = lm(EU_Sales~Global_Sales, data = sales_prod_sum) 
# Viewing the results.
summary(EU_Global)

#Plot of SLRM between EU_Sales and Global_Sales
EU_Global_plot = plot(EU_Sales~Global_Sales, data = sales_prod_sum) +
  title(main="Linear Regression")
abline(EU_Global)

##Creating a mutliple linear regression model (MLRM) and visualizing them.

# Installing necessary libraries
install.packages("devtools")
devtools::install_github("cardiomoon/ggiraphExtra")
install.packages("plotrix")
library(plotrix)
install.packages("predict3d")
require(predict3d)
require(rgl)

##  Creating a MLRM
# MLRM of NA_Sales vs EU_Sales + Global_Sales
NA_EU_GL <- lm(NA_Sales ~ EU_Sales + Global_Sales, data=sales_prod_sum)
summary(NA_EU_GL) 

# MLRM of EU_Sales vs NA_Sales + Global_Sales
EU_NA_GL <- lm(EU_Sales ~ NA_Sales + Global_Sales, data=sales_prod_sum)
summary(EU_NA_GL) 

# MLRM of Global_Sales vs EU_Sales + NA_Sales
GL_EU_NA <- lm(Global_Sales ~ EU_Sales + NA_Sales, data=sales_prod_sum)
summary(GL_EU_NA) 

## Predicting global sales based on provided values. 

# Function to calculate predicted Global_Sales_Sum.
Global_predicted <- data.frame(NA_Sales=c(value1), EU_Sales=c(value2))

#use the fitted model to predict the Global Sales
predict(GL_EU_NA, newdata=Global_predicted)

# a) NA_Sales_sum = 34.02 and EU_Sales_sum = 23.80
Global_predicted <- data.frame(NA_Sales=c(34.02), EU_Sales=c(23.8))
predictmodel = predict(GL_EU_NA, newdata=Global_predicted)
predictmodel
# Observed value =67.85, predicted value = 68.06

# b) NA_Sales_sum = 3.93 and EU_Sales_sum = 1.56
Global_predicted <- data.frame(NA_Sales=c(3.93), EU_Sales=c(1.56))
predict(GL_EU_NA, newdata=Global_predicted)
Global_predicted(3.93, 1.56) # observed = 6.04, predicted = 7.36

# c) NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65
Global_predicted <- data.frame(NA_Sales=c(2.73), EU_Sales=c(0.65))
predict(GL_EU_NA, newdata=Global_predicted)
# Observed = 4.32, predicted = 4.908

# d) NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97
Global_predicted <- data.frame(NA_Sales=c(2.26), EU_Sales=c(0.97))
predict(GL_EU_NA, newdata=Global_predicted)
# Observed = 3.53, predicted = 4.76

# e) NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52
Global_predicted <- data.frame(NA_Sales=c(22.08), EU_Sales=c(0.52))
predict(GL_EU_NA, newdata=Global_predicted)
# Observed = 23.21, predicted = 26.626


