## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################


## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library('tidyverse')
library(ggplot2)
# Create insightful summaries of the data set.
library(skimr)
# Create insightful reports on the data set.
library(DataExplorer)


# Import the data set.
sales <- read.csv('turtle_sales.csv', header=T)


# Print the data frame.
view(sales)
as_tibble(sales)
  # Data types look correct

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales_useful = select(sales, -Ranking, -Year, -Genre, -Publisher)
write.csv(sales_useful, "turtle_sales_useful.csv", row.names=FALSE)


# View the data frame.
as_tibble(sales_useful)
view(sales_useful)

# View the descriptive statistics.
summary(sales_useful)
skim(sales_useful)
n_distinct(sales_useful$Product)

################################################################################

# 1a. It could also be useful to be able to compare the region sales side-by-side where region is one of the variables

# Create table of NA_Sales only
sales_NA <- select(sales_useful, -EU_Sales, -Global_Sales)
as_tibble(sales_NA)

# Rename "NA_Sales" column
colnames(sales_NA)[which(names(sales_NA) =="NA_Sales")] <- "Sales"

# Add new column "Region"
sales_NA$Region <- "NA"
as_tibble(sales_NA)

# Repeat for EU and Global
# Create table of EU_Sales only
sales_EU <- select(sales_useful, -NA_Sales, -Global_Sales)
  colnames(sales_EU)[which(names(sales_EU) =="EU_Sales")] <- "Sales"
  sales_EU$Region <- "EU"
  as_tibble(sales_EU)
  
# Create table of Global_Sales only
sales_Global <- select(sales_useful, -NA_Sales, -EU_Sales)
  colnames(sales_Global)[which(names(sales_Global) =="Global_Sales")] <- "Sales"
  sales_Global$Region <- "Global"
  as_tibble(sales_Global)

# Combine the 3 dataframes
sales_byreg <- rbind(sales_Global,sales_NA,sales_EU)
as_tibble(sales_byreg)

sales_byreg$Region <- factor(sales_byreg$Region, levels=c("Global","NA","EU"))


################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(Product, Global_Sales, data=sales_useful)
qplot(Platform, Global_Sales, data=sales_useful,
      geom=c('point', 'jitter'))

qplot(NA_Sales, Global_Sales, data=sales_useful, colour=Platform)
qplot(EU_Sales, Global_Sales, data=sales_useful, colour=Platform)
qplot(EU_Sales, NA_Sales, data=sales_useful, colour=Platform)
  # 1 outlier point with significantly higher sales numbers in all 3 regions

  # 3 products sell significantly more in NA than EU
  which(sales_useful$NA_Sales>15 & sales_useful$NA_Sales<30)
  sales_useful[which(sales_useful$NA_Sales>15 & sales_useful$NA_Sales<30),]
    # product ID = 123, 254, 326
  
  # % of products with EU_Sales > NA_Sales: 
  nrow(sales_useful[which(sales_useful$NA_Sales<sales_useful$EU_Sales),])/
    nrow(sales_useful)*100         
  
  
qplot(Product, Sales, data=sales_byreg, colour=Region)
  
## 2b) Histograms
# Create histograms.
qplot(NA_Sales, data=sales_useful)
qplot(EU_Sales, data=sales_useful)
qplot(Global_Sales, data=sales_useful)

## 2c) Boxplots
# Create boxplots.
qplot(Platform, Global_Sales, data=sales_useful, geom='boxplot')
qplot(Global_Sales, data=sales_useful, geom='boxplot')
qplot(EU_Sales, data=sales_useful, geom='boxplot')
qplot(NA_Sales, data=sales_useful, geom='boxplot')
qplot(Sales, data=sales_byreg, geom='boxplot', color =Region)


###############################################################################

# 3. Observations and insights

## Your observations and insights here ......
  # ???? I have no idea how to use this info
  # 0) Difficult to make any significant insights based on this basic information

  # 1) Brief stats
    #           	  mean      sd   p0   p25       p50     p75   p100 
    # NA_Sales	    2.52    3.41   0    0.478    1.82    3.12   34.0 
    # EU_Sales	    1.64    2.03   0    0.39     1.17    2.16   23.8
    # Global_Sales	5.33    6.26   0.01 1.12     4.32    6.44   67.8

  # 2) Generally: NA Sales > EU Sales (for 65.6% of products)
  # 3) Relative sales for most products are similar across the regions
  #   - Higher sales in NA, higher sales in EU -> higher total sales
  #   * Except for 3 particular products that sell particularly well in NA compared to EU
  # 4) Upper whisker is larger than IQR
  #   - upper 25% of products sell significantly more than the other products

  # 5) Sales spread out among the different products and platforms
  #   - 175 Products
  #   - 22 platforms

  # 6) 1 product has outstanding sales : Product 107 on Wii 
  #   - Will need to evaluate the product's significance on total global sales
  #   - What are the impacts of excluding the outlier?


###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
sales_useful <- read.csv('turtle_sales_useful.csv', header=T)
view(sales_useful)

# Check output: Determine the min, max, and mean values.
# View the descriptive statistics.
summary(sales_useful)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sales_product <- sales_useful %>% group_by(Product) %>% 
    summarise(NA_Sales=sum(NA_Sales),
              EU_Sales=sum(EU_Sales),
              Global_Sales=sum(Global_Sales))

sales_byreg_product <- sales_byreg %>% group_by(Product,Region) %>% 
                                      summarise(Sales=sum(Sales))
  sales_byreg_product$Region <- factor(sales_byreg_product$Region, levels=c("Global","NA","EU"))

# View the data frame.
view(sales_product)
  # Change Product ID into integer
  as.integer(sales_product$Product)
  as_tibble(sales_product)
  as_tibble(sales_byreg_product)
  
# Save the data frame for easier direct access to data
write.csv(sales_product, "turtle_sales_product.csv", row.names=FALSE)
  
# Explore the data frame.
sum(is.na(sales_product))
summary(sales_product)

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
ggplot(sales_product, aes(x=Product, y=Global_Sales)) +
  geom_point(alpha = 0.5, size = 1.5) +
  scale_y_continuous(breaks = seq(0, 70, 5), "Global Sales (in £)") +
  labs(title = "Global Sales of each Product") 
  

ggplot(sales_byreg_product, aes(x=Product, y=Sales, color=Region)) +
  geom_point(alpha = 0.5, size = 1.5) +
  scale_y_continuous(breaks = seq(0, 70, 5), "Sales (in £)")+
  labs(title = "Sales of each Product (by Region)") 

  # ***Not useful since Product ID is a label, not numeric value

# Create histograms.
# Global Sales
ggplot(sales_product, aes(x = Global_Sales)) +
  geom_histogram(binwidth = 2) +
  labs(title = "Sales Distribution of each Product (Global)") 
# NA Sales
ggplot(sales_product, aes(x = NA_Sales)) +
  geom_histogram(binwidth = 2) +
  labs(title = "Sales Distribution of each Product (in NA)") 
# Global Sales
ggplot(sales_product, aes(x = EU_Sales)) +
  geom_histogram(binwidth = 2) +
  labs(title = "Sales Distribution of each Product (in EU)") 


ggplot(sales_byreg_product, aes(x = Sales)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~Region) +
  labs(title = "Sales Distribution of each Product (by Region)") 
  # Confirm that each region has the same number of entries
  table(sales_byreg_product$Region)

# Create boxplots.
ggplot(sales_product, aes(x = Global_Sales)) +
  geom_boxplot() 

ggplot(sales_byreg_product, aes(x = Sales, y = Region, color = Region)) +
  geom_boxplot() 

###############################################################################


# 3. Determine the normality of the data set.
  #Sales by product ID
## 3a) Create Q-Q Plots
# Create Q-Q Plots (for sales by product)
qqnorm(sales_product$Global_Sales)
  qqline(sales_product$Global_Sales)

qqnorm(sales_product$NA_Sales)
  qqline(sales_product$NA_Sales)


  qqnorm(sales_product$EU_Sales)
  qqline(sales_product$EU_Sales)

  # All plots don't fit normal dist 
  # heavy right skew
  

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)
library(BSDA)

# Perform Shapiro-Wilk test.
shapiro.test(sales_product$Global_Sales) # p-value < 2.2e-16
shapiro.test(sales_product$NA_Sales) # p-value < 2.2e-16
shapiro.test(sales_product$EU_Sales) # p-value = 2.987e-16


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(sales_product$Global_Sales) # 3.066769
skewness(sales_product$NA_Sales) # 3.048198 
skewness(sales_product$EU_Sales) # 2.886029 
  # All strong positive skew, assymetric

kurtosis(sales_product$Global_Sales) # 17.79072 
kurtosis(sales_product$NA_Sales) # 15.6026 
kurtosis(sales_product$EU_Sales) # 16.22554 
  # kurosis >>3 : heavy-tailed, strong/many outliers

## 3d) Determine correlation
# Determine correlation.
cor.test(sales_product$Global_Sales,sales_product$NA_Sales) # 0.9162292
cor.test(sales_product$Global_Sales,sales_product$EU_Sales) # 0.8486148
cor.test(sales_product$NA_Sales,sales_product$EU_Sales) # 0.6209317
  # Global sales have stronger correlation with NA_Sales suggesting that NA customer base is has more "control" of the global sales
  # NA & EU sales while correlated, are not as strongly

#######
  # Checking Normality of base dataset 
  # (Where each product-platform pair is considered a separate product)
  # GLOBAL SALES
  qqnorm(sales_useful$Global_Sales)
    qqline(sales_useful$Global_Sales)
  shapiro.test(sales_useful$Global_Sales) # p-value < 2.2e-16
  skewness(sales_useful$Global_Sales) # 4.045582 : strong positive skew
  kurtosis(sales_useful$Global_Sales) # 32.63966 : heavy-tailed

  # NA SALES
  qqnorm(sales_useful$NA_Sales)
    qqline(sales_useful$NA_Sales)
  shapiro.test(sales_useful$NA_Sales) # p-value < 2.2e-16
  skewness(sales_useful$NA_Sales) # 4.30921 : strong positive skew
  kurtosis(sales_useful$NA_Sales) # 31.36852 : heavy-tailed
  
  # EU SALES
  qqnorm(sales_useful$EU_Sales)
  qqline(sales_useful$EU_Sales)
  shapiro.test(sales_useful$EU_Sales) # p-value < 2.2e-16
  skewness(sales_useful$EU_Sales) # 4.818688 : strong positive skew
  kurtosis(sales_useful$EU_Sales) # 44.68924 : heavy-tailed

dftest <- merge(x=sales_byreg_product,y=sales_product,by="Product", all = TRUE)  
dftest <- select(dftest, -NA_Sales,-EU_Sales)
view(dftest)

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# Histogram (by region) to show distribution of sales by product
ggplot(sales_byreg_product, aes(x = Sales)) +
    geom_histogram(binwidth = 2) +
    facet_wrap(~Region) +
    labs(title = "Sales Distribution of each Product (by Region)") 

  
# Scatterplot to indicate relation between Global and NA Sales.
ggplot(data = sales_product,
         mapping = aes(x = NA_Sales, y = Global_Sales)) +
    geom_point(color = 'green', alpha = 0.5, size = 1.5) +
    # Add the line-of-best-fit to the plot.
    geom_smooth(color = 'darkgreen',method = 'lm')

ggplot(data = sales_product,
       mapping = aes(x = NA_Sales, y = Global_Sales)) +
  geom_point(color = 'blue', alpha = 0.5, size = 1.5) +
  # Add the line-of-best-fit to the plot.
  geom_smooth(color = 'darkblue',method = 'lm')

ggplot(data = dftest,
       mapping = aes(x = Global_Sales, y = Sales,color = Region)) +
  geom_point(alpha = 0.5, size = 1.5) +
  # Add the line-of-best-fit to the plot.
  geom_smooth(method = 'lm') +
  labs(title = "Visual Representation of Correlation between Regional & Global Sales \n per product",y = "Regional Sales") 


###############################################################################

# 5. Observations and insights
# Your observations and insights here...

  #1. Distribution of product sales strays far from normal distribution
    # Since most prediction models rely on assumptions of normality, 
    # sales predictions made based on this current sample population data
    # would be unreliable


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
view(sales_useful)


# Determine a summary of the data frame.
summary(sales_useful)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model_G_NA <- lm(Global_Sales~NA_Sales,
             data=sales_useful)
model_G_NA
summary(model_G_NA)

model_G_EU <- lm(Global_Sales~EU_Sales,
                 data=sales_useful)
model_G_EU
summary(model_G_EU)

model_NA_EU <- lm(NA_Sales~EU_Sales,
                 data=sales_useful)
model_NA_EU
summary(model_NA_EU)


## 2b) Create a plot (simple linear regression)
# Basic visualisation.

  #Global-NA
# View residuals on a plot.
plot(model_G_NA$residuals)

# Plot the relationship with base R graphics.
plot(sales_useful$NA_Sales, sales_useful$Global_Sales,
  main= "Linear Regression of Global Sales to NA Sales",
  xlab = "NA Sales", ylab = "Global Sales")
coefficients(model_G_NA)

# Add line-of-best-fit.
abline(coefficients(model_G_NA))

  #Global-EU
plot(sales_useful$EU_Sales, sales_useful$Global_Sales,
     main= "Linear Regression of Global Sales to EU Sales",
     xlab = "NA Sales", ylab = "Global Sales")
coefficients(model_G_EU)
abline(coefficients(model_G_EU))

#NA-EU
plot(sales_useful$EU_Sales, sales_useful$NA_Sales,
     main= "Linear Regression of NA Sales to EU Sales",
     xlab = "NA Sales", ylab = "NA Sales")
coefficients(model_NA_EU)
abline(coefficients(model_NA_EU))

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
sales_cor <- select(sales_useful, -Product, -Platform)
sales_cor

# Multiple linear regression model.
# Install the psych package.
install.packages('psych')

# Import the psych package.
library(psych)

# Use the corPlot() function.
# Specify the data frame (wine) and set 
# character size (cex=2).
corPlot(sales_cor, cex=2)

sales_MLR = lm(Global_Sales~NA_Sales+EU_Sales, data=sales_useful)

# Print the summary statistics.
summary(sales_MLR)
  # all *** good correlation
  # Adjusted R-squared:  0.9685

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
sales_test <- data.frame(NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
                         EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))
sales_test

# Predicting global sales using MLR model 
predictSales = predict(sales_MLR, newdata=sales_test,
                      interval='confidence')

predictSales
  #   Predicted                         Actual Global Sales
  #       fit       lwr       upr
  # 1 71.468572 70.162421 72.774723         67.85   [out of range]
  # 2  6.856083  6.718420  6.993745         6.04    [out of range]
  # 3  4.248367  4.102094  4.394639         4.32    [within range]
  # 4  4.134744  4.009122  4.260365         3.53    [out of range]
  # 5 26.431567 25.413344 27.449791         23.21   [out of range](Pdt: 326)


#######
# 3b. Creating MLR model including Product ID to test if that will improve the model

# when products NOT grouped
sales_MLR_All = lm(Global_Sales~NA_Sales+EU_Sales+Product, data=sales_useful)
# Print the summary statistics.
summary(sales_MLR_All)
  # Product ID has low significance 
  # Adjusted R-squared:  0.9688

# when products ARE grouped
sales_MLR_pdt = lm(Global_Sales~NA_Sales+EU_Sales+Product, data=sales_product)
# Print the summary statistics.
summary(sales_MLR_pdt)
  # Product ID has high significance 
  # Adjusted R-squared:  0.9709


sales_test_All <- data.frame(NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
                             EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52),
                             Product  = c(107,3267,6815,2877,326))
predictSales_pdt = predict(sales_MLR_pdt, newdata=sales_test_All,
                               interval='confidence')
predictSales_pdt
#   Predicted                         Actual Global Sales
#       fit       lwr       upr
# 1 66.358702 64.712584 68.004821         67.85   [within range]
# 2  7.558873  7.307227  7.810518         6.04    [further out of range]
# 3  4.245235  3.873852  4.616618         4.32    [within range]
# 4  5.198279  4.887641  5.508917         3.53    [further out of range]
# 5 26.548792 25.376282 27.721303         23.21   [out of range](Pdt: 326)

predictSales_All = predict(sales_MLR_All, newdata=sales_test_All,
                           interval='confidence')
predictSales_All
  #   Predicted                         Actual Global Sales
  #       fit       lwr       upr
  # 1 71.044457 69.681643 72.407270       67.85   [out of range]
  # 2  6.862144  6.724979  6.999310       6.04    [out of range]
  # 3  4.077969  3.858666  4.297272       4.32    [out of range]
  # 4  4.187925  4.052803  4.323047       3.53    [out of range]
  # 5 26.434135 25.420501 27.447769       23.21   [out of range]

  # While the R-squared is increased (0.9709 vs 0.9685) when Product ID is also considered as a variable in the MLR, it does not seem to significantly change the accuracy of the prediction
  # This is likely since the dataset and initial model is not reliable to begin with, since it does not follow the normal distribution and has significant numbers of outlying data (heavy-tailed)


###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################




