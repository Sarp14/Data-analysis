#Loading the necessary libraries that will be used for the analysis 
library(rjson)
library(corrplot)
library(e1071)
library(plotly)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(lmtest)
library(dplyr)
library(gplots)
library(naniar)
library(ggcorrplot)
library(car)
library(whitestrap)
library(olsrr)
library(viridis)
library(TeachingSampling)

# Data is downloaded from Kaggle: https://www.kaggle.com/datasets/undp/human-development
# For the analysis, two CSV files were used: human_development.csv, which contains data on the basic variables used to measure the HDI index, 
# and inequality_adjusted.csv, which contains data on variables related to economic inequality. 
# The file gdp_per_capita.csv was also used, which contains GDP data by country for a long period.  
# Only the column for the year 2015 was used to align with the data from the previous two CSV files, where all data corresponds to the given year. 
# The GDP file was also downloaded from Kaggle: https://www.kaggle.com/datasets/zgrcemta/world-gdpgdp-gdp-per-capita-and-annual-growths.


# Loading the data itself, for the human development dataset
hdi <- read.csv("/Users/ndamljanovic/Downloads/archive/human_development.csv")

# View of tables and columns
View(hdi)
head(hdi)
str(hdi)

# Converting GNI per capita column into a numeric type
hdi$Gross.National.Income..GNI..per.Capita <- as.numeric(gsub(",", ".", gsub("\\.", "", 
                                                 hdi$Gross.National.Income..GNI..per.Capita)))

str(hdi$Gross.National.Income..GNI..per.Capita)

# Checking and removing missing values for the HDI dataset
sum(is.na(hdi))
hdi_new <- na.omit(hdi)

# Dataset for GDP per capita, we will extract the column related to the year 2015
gdp <- read.csv("/Users/ndamljanovic/Downloads/archive (9)/gdp_per_capita.csv")
str(gdp)

gdp_2015 <- gdp %>%
  select(Country.Name, X2015) %>%
  rename(Country = Country.Name, `GDP_per_cap` = X2015)

View(gdp_2015)

# Removing NA values
gdp_2015 <- na.omit(gdp_2015)

# Loading the data itself, for the inequality dataset
ineq <- read.csv("/Users/ndamljanovic/Downloads/archive/inequality_adjusted.csv")

# View of tables and columns
View(ineq)
head(ineq)
str(ineq)

# Checking and removing missing values for the inequality dataset
sum(is.na(ineq))
ineq_new <- na.omit(ineq)
View(ineq_new)

# We will add a column for GDP per capita for the year 2015 to the HDI dataset based on the column that contains country names
hdi_new <- hdi_new %>%
  left_join(gdp_2015 %>% select(Country, GDP_per_cap), by = "Country")

View(hdi_new)

# Merging the HDI dataset with the new GDP column and the Inequality dataset based on the column that contains country names
merged_data <- hdi_new %>%
  left_join(ineq_new %>% select(Country, Income.Inequality..Quintile.Ratio., 
                                Income.Inequality..Palma.Rati., Income.Inequality..Gini.Coefficient.), by = "Country")
View(merged_data)

# We will assign different names to all columns for better readability
new_column_names <- c("HDI_rank", "Country", "HDI", "Life_expectancy", "Expected_years_of_educ", "Mean_educ", "GNI_per_capita",
                      "GNI_per_cap_minus_HDI_rank", "GDP_per_capita" , "Inequality_quintile_rank", "Inequality_palma_ratio", "Gini_coef")  


colnames(merged_data) <- new_column_names
View(merged_data)
str(merged_data)


# We can see that the new dataset contains 26 missing values
sum(is.na(merged_data))


# Visualization of missing values in the new dataset
gg_miss_var(merged_data) +
  labs(title = "Prikaz nedostajucih vrednosti", 
       x = "Varijable", y = "Broj nedostajucih vrednosti") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5))

# We can see that all 26 missing values are present in the GDP_per_capita column
# When we inspect the dataset using the View() function, we can see that three columns related to inequality
# actually contain missing values, specifically ".." which R, for some reason, does not recognize as NA, i.e., missing values.

# We will replace all cells containing ".." with NA to repeat the visualization and gain insight into the actual number of NA values
merged_data[merged_data == ".."] <- NA
gg_miss_var(merged_data) +
  labs(title = "Prikaz nedostajucih vrednosti", 
       x = "Varijable", y = "Broj nedostajucih vrednosti") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5)) 

# After the replacement, we see that those three columns actually lead in terms of NA values, while GDP_per_capita is in 4th place
# We will replace the NA values in the given columns with the median values of those columns to avoid losing a significant portion of the population,
# since there are over 40 instances of ".." or NA values in those columns.

# We can observe that the last three variables related to economic inequality are of character type, so we will convert them to numeric
# type in order to be able to input the median values.

str(merged_data)

merged_data <- merged_data %>%
  mutate(Inequality_quintile_rank = as.numeric(Inequality_quintile_rank),
         Inequality_palma_ratio = as.numeric(Inequality_palma_ratio),
         Gini_coef = as.numeric(Gini_coef))

# We are inputting the median values for the given 4 columns in place of the missing values
merged_data <- merged_data %>%
  mutate_at(vars(GDP_per_capita, Inequality_quintile_rank, Inequality_palma_ratio, Gini_coef),
            ~ ifelse(is.na(.), median(., na.rm = TRUE), .))

# We can see that there are no longer any missing values in the given dataset
sum(is.na(merged_data))
View(merged_data)

# Visualization of the entire dataset, after merging multiple tables and columns together.
# A choropleth map is displayed using the Plotly package for interactive visualization, showing some of the key human development indicators,
# including the HDI index, GDP per capita, Gini coefficient, and life expectancy. The color range reflects different HDI levels across various countries.
# We can observe a clear difference in the level of the HDI index among different countries and groups of countries, which corresponds with other indicators as well.
map <- plot_ly(data = merged_data, type = "choropleth", locations = ~Country,
               locationmode = "country names", colorscale = "Cividis",
               z = ~HDI,
               text = ~paste("Country: ", Country, "<br>GNI per Capita: ", GNI_per_capita, 
                             "<br>Life Expectancy :", Life_expectancy,
                             "<br>Gini Coef: ", Gini_coef, "<br>HDI: ", HDI),
               hoverinfo = "text",
               colorbar = list(title = "HDI"))

map <- map %>% layout(title = "Prikaz kljucnih HDI indikatora za ceo svet", margin = list(t = 130))

map

# Sampling - we are using the variable gross domestic product per capita (GDP_per_capita)
n <- 50
set.seed(321)

# Selecting the probability of each unit proportional to the GDP per capita variable
res <- S.piPS(n, merged_data$Gini_coef)
# Selected sample
sam <- res[,1]
# Probabilities of including each unit in the sample
Pik.s <- res[,2]
# A new data frame sample_hdi is created, containing units obtained using the PPS method without replacement
uzorak_hdi <- merged_data[sam,]

# Preview of the sample of 50 countries
View(uzorak_hdi)

# Descriptive analysis of our sample

# We can see that the new dataframe obtained from sampling contains 50 rows and 12 columns
# After the changes we made, all columns are quantitative except for the Country column
str(uzorak_hdi)

# Overview of summary statistics for the entire sample
summary(uzorak_hdi[, !(names(uzorak_hdi) %in% c("HDI_rank", "Country"))])

# Distribution of basic descriptive statistics for a few key variables
ggplot(uzorak_hdi, aes(x = "", y = GDP_per_capita)) +
  geom_boxplot(fill = "red", alpha = 0.6, width = 0.6) +
  theme_minimal() +
  ggtitle("BDP po glavi stanovnika") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("BDP")


ggplot(uzorak_hdi, aes(x = "", y = Life_expectancy)) +
  geom_boxplot(fill = "darkgreen", alpha = 0.6, width = 0.6) +
  theme_minimal() +
  ggtitle("Ocekivani zivotni vek") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("Zivotni vek")

ggplot(uzorak_hdi, aes(x = "", y = Mean_educ)) +
  geom_boxplot(fill = "orange", alpha = 0.6, width = 0.6) +
  theme_minimal() +
  ggtitle("Prosecne godine skolovanja") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("Godine skolovanja")


# We can observe a large range of values among certain variables.
# Especially for GNI and GDP per capita, Gini coefficient, Mean_educ, and HDI.

# Bar chart displaying countries by their Gini coefficient
uzorak_hdi %>%
  arrange(Gini_coef) %>%
  plot_ly(x = ~Gini_coef, y = ~seq_along(Country),
          type = "bar", orientation = 'h', marker = list(color = "skyblue"),
          text = ~Country) %>%
  layout(title = "Drzave po Dzini koeficijentu",
         xaxis = list(title = "Dzini koeficijent"),
         yaxis = list(title = ""))


# Scatter plot for countries by GDP per capita
plot <- plot_ly(
  data = uzorak_hdi,
  x = ~Country,
  y = ~GDP_per_capita,
  text = ~paste("Country: ", Country, "<br>GDP Per Capita: $", GDP_per_capita),
  type = "scatter",
  mode = "markers",
  marker = list(size = 12, color = ~GDP_per_capita, colorscale = "Viridis")
) %>% layout(
  title = list(text = "BDP po glavi stanovnika za drzave u uzorku", y = 0.9),  
  xaxis = list(title = "Drzave", 
               tickangle = 45),
  yaxis = list(title = "BDP"),
  margin = list(t = 80)  # Increase top margin
)

plot


# Correlation matrix of numeric variables
kvant_uzorak_hdi <- uzorak_hdi %>%
  select(-Country, -GNI_per_cap_minus_HDI_rank ,-HDI_rank)


cor_matrix_hdi <- cor(kvant_uzorak_hdi)

ggcorrplot(cor_matrix_hdi, 
           title = "Korelaciona matrica",
           ggtheme = theme_minimal(),  
           colors = c("#6D9EC1", "white", "#E46726"),
           hc.order = TRUE,
           lab = TRUE,
           method = "square") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 20)),  
    axis.text.x = element_text(size = 10),  
    axis.text.y = element_text(size = 10),  
    axis.title = element_text(size = 14)  
  )


# Multiple linear model, using GDP per capita and life expectancy 
# as independent variables and the HDI index as the dependent variable
set.seed(123)
model <- lm(HDI ~ GDP_per_capita + Life_expectancy, uzorak_hdi)
summary(model)

# We note that the model is statistically significant. Both independent variables show statistical significance:
# GDP_per_capita   7.837e-07  2.382e-07    3.29   0.0018 ** 
# Life_expectancy  1.386e-02  1.126e-03   12.30   <2e-16 ***

# The entire regression is statistically significant, as seen from the F statistic and its p-value:
# F-statistic:  137.1 on 2 and 52 DF,  p-value: < 2.2e-16


# The values of the coefficient of determination and the adjusted coefficient of determination are:
# Multiple R-squared:  0.8406,   Adjusted R-squared:  0.8345  
# From which we conclude that most of the variability (83%) of the dependent variable can be explained by the given model.

# We will test the key assumptions of the linear model.
# Do the residuals of the model have a normal distribution?

# Visual representation of the distribution of residuals; most residuals are located on the curve itself
plot(model, 2, main = "Vizuelni prikaz raspodele reziduala")

# Formal testing using the Shapiro-Wilk test indicates that the residuals have a normal distribution.
# The p-value in this case is 0.97476, which is greater than 0.05 (0.2982), allowing us to conclude that the
# residuals are normally distributed.
set.seed(123)
shapiro.test(model$residuals)

# Correlation of the two independent variables
cor(uzorak_hdi$GDP_per_capita, uzorak_hdi$Life_expectancy)

# By using the Variance Inflation Factor (VIF), we observe that there is no pronounced multicollinearity between the explanatory variables.
# The VIF value is more indicative of higher multicollinearity, with a high value being over 10, while in this model it is 1.372373.
set.seed(123)
vif(model)

# Testing for homoscedasticity, specifically whether the variance for all random errors is constant. The White test was used.
set.seed(123)
white_test(model)

# Based on the results of the White test: 
# Null hypothesis: Homoscedasticity of the residuals
# Alternative hypothesis: Heteroskedasticity of the residuals
# Test Statistic: 5.47
# P-value: 0.064886
# We conclude that there is no presence of heteroscedasticity in the model.

# Testing for autocorrelation using the Durbin-Watson test
set.seed(123)
durbinWatsonTest(model)

# The DW statistic is close to 2 (1.749609), while the p-value is 0.29, which is greater than 0.05.
# Based on this, we conclude that there is no first-order autocorrelation in the given regression model.
