# Setting up working directory, and opening packages.

library(tinytex)
library(rmarkdown)
library(ggplot2)
library(car)
library(corrplot)
library(tidyr)
library(hablar)

getwd()
setwd("C:/Users/Branko/Desktop/R/Baze/Human development")

# Opening files, human development index, complete fille.

hdi_complete <- read.csv("human_development.csv", stringsAsFactors = F)
str(hdi_complete)
View(hdi_complete)

# Turning the gni per capita column into numeric from a character data type. 
# The commas are also deleted, cause the data in that column is marked by commas.
# Commas are replaced by dots with gsub function. 

gni_per_capita <- as.numeric(gsub(",", ".", gsub("\\.", "",
                                                 hdi_complete$Gross.National.Income..GNI..per.Capita)))
str(gni_per_capita)

# HDI adjusted to inequality, according to gini coefficient. 

hdi_inequality <- read.csv("inequality_adjusted.csv", stringsAsFactors = F)
str(hdi_inequality)
View(hdi_inequality)

# Cleaning the data, changing all columns to numeric  data type, cause they were represented as character. 

char_columns_hdi_ineq <- sapply(hdi_inequality, is.character)
char_columns_hdi_ineq 

hdi_ineq_num <- hdi_inequality                              
hdi_ineq_num[ , char_columns_hdi_ineq] <- as.data.frame(   
  apply(hdi_ineq_num[ , char_columns_hdi_ineq], 2, as.numeric))

View(hdi_ineq_num)


# Gender development 

gender_dev <- read.csv("gender_development.csv",stringsAsFactors = F)
str(gender_dev)
View(gender_dev)

# Doing the same as for the IHDI variable. Applying the process later for other data frames.

char_columns_gen_dev <- sapply(gender_dev, is.character)
char_columns_gen_dev 

gen_dev_num <- gender_dev                              
gen_dev_num[ , char_columns_gen_dev] <- as.data.frame(   
  apply(gen_dev_num[ , char_columns_gen_dev], 2, as.numeric))

View(gen_dev_num)


# Gender inequality

gender_inequality <- read.csv("gender_inequality.csv", stringsAsFactors = F)
str(gender_inequality)
View(gender_inequality)

char_columns_gen_ineq <- sapply(gender_inequality, is.character)
char_columns_gen_ineq 

gen_ineq_num <- gender_inequality                              
gen_ineq_num[ , char_columns_gen_ineq] <- as.data.frame(   
  apply(gen_ineq_num[ , char_columns_gen_ineq], 2, as.numeric))

View(gen_ineq_num)

# Multidimensional poverty 

multidim_poverty <- read.csv("multidimensional_poverty.csv", stringsAsFactors = F)
str(multidim_poverty)
View(multidim_poverty)

# Looking at the summary statistics for HDI and IHDI variables. 
# At the countries which have maximum and minimum hdi value. 

summary(hdi_complete)
hdi_complete[hdi_complete$Human.Development.Index..HDI. == max(hdi_complete$Human.Development.Index..HDI.), ]
hdi_complete[hdi_complete$Human.Development.Index..HDI. == min(hdi_complete$Human.Development.Index..HDI.), ]

summary(hdi_ineq_num$Inequality.adjusted.HDI..IHDI.)

# Making a separate variable with only HDI scores and countries by order of their HDI score. 
# That is done by deleting the first column of the HDI dataset, as well as deleting columns from 4th to 8th in the complete dataset, 
# thus leaving the desired columns with countries and HDI scores. 

hdi_index <- hdi_complete[ , -c(1,4:8)]
str(hdi_index)
summary(hdi_index)

# Extracting the gini coefficient variable, so the correlation analysis could be performed.
 
gini_coefficient <- (hdi_ineq_num$Income.Inequality..Gini.Coefficient.)
summary(gini_coefficient)

# Performing the correlation analysis for two indexes. HDI form the complete variable and 
# Gini coefficient from the adjusted for inequality IHDI variable. The results show weak negative correlation. 
# Which sugests that most developed countries have lower leves of income inequality. 

cor(hdi_index$Human.Development.Index..HDI., gini_coefficient, method = "pearson",
  use = "complete.obs")             
            
# Visualising the correlation between HDI index and Gini coefficient.
# Data frame of Gini coefficients and HDI scores is first made, so that the scatterplot function could procede.

gini_hdi <- data.frame(Countries = (hdi_complete$Country), 
                        Gini = (gini_coefficient), 
                        HDI = (hdi_index$Human.Development.Index..HDI.))
str(gini_hdi)

# Visualising the distribution of the Gini coefficient. 

ggplot(gini_hdi, aes(Gini)) + 
  geom_histogram(binwidth = 1.5, color = "darkblue", fill = "lightblue",
                 linetype = "dashed") +
  labs(title = "Gini distribution", x = "Gini", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))


# Making a separate variable of Gini and HDI indexes for complete world divided by regions.

gini_hdi_regions <- gini_hdi[189:194, ]
gini_hdi_regions

# Visualising the level of HDI by world regions, Gini coefficient is not visualised, 
# because there is no data for gini at the regional level. 

# First, the factor variable for regions is ordered by regional HDI levels. 

gini_hdi_regions$Countries <- factor(gini_hdi_regions$Countries,
                                     levels = c("Europe and Central Asia", "Latin America and the Caribbean",
                                                "East Asia and the Pacific", "Arab States", 
                                                "South Asia", "Sub-Saharan Africa"))


# Then, ggplot visualization is produced, where height of the bars represents HDI level of each region.

ggplot(gini_hdi_regions, aes(x = Countries , y = HDI , fill = HDI)) +
  geom_col(show.legend = FALSE, color = "darkgreen",
           fill = "lightgreen",width = 0.8) + 
  labs(x="Regions", y="Level of HDI",
        title ="Distribution of HDI by world regions") +
  theme(plot.title = element_text(hjust = 0.5))


# Proceedinig to representing the correlation between Gini and HDI indexes. 
# Looking for outliers using the boxplot visualization. 

boxplot(gini_hdi$HDI, main = "HDI", 
        sub = paste("Outler rows: ", boxplot.stats(gini_hdi$HDI)$out))

boxplot(gini_hdi$Gini, main = "Gini", 
        sub = paste("Outler rows: ", boxplot.stats(gini_hdi$Gini)$out))


# Visualizing relationship between HDI and Gini indexes, through scatter plot function. 

scatterplot(gini_coefficient ~ hdi_index$Human.Development.Index..HDI.,
            data = gini_hdi, smooth = FALSE,
            xlab = "Human development index", 
            ylab = "Gini coefficient",
            main = "Correlation of HDI and Gini indexes") 


# Instead of computing correlations of most important indexes one by one, we will make a table 
# of correlations using a corrplot function. First step is extracting the individual indexes.
# Index for gender development.

gender_dev_index <- gen_dev_num$Gender.Development.Index..GDI.
str(gender_dev_index)

# Index for gender inequality
 
gender_ineq_index <- gen_ineq_num$Gender.Inequality.Index..GII.
str(gender_ineq_index)

# And multidimensional poverty index

multi_pov_index <- multidim_poverty$Multidimensional.Poverty.Index..MPI..HDRO.
str(multi_pov_index)

# Then we are making a dataframe which has this couple of indexes as its variables. 

corplot_indexes <- data.frame(Gini = (gini_hdi$Gini), 
                              HDI = (gini_hdi$HDI),
                              Gender_dev =  (gender_dev_index),
                              Gender_ineq = (gender_ineq_index))


# Making a variable of joint correlations 

cor_indexes <- cor(corplot_indexes, method = "pearson", use = "complete.obs") 
cor_indexes

# Visualizing correlations between extracted indexes, by building a correlation matrix with corrplot function. 

corrplot(corr = cor_indexes, title = "Correlations of Human Development indexes", 
         type = "upper",
         method = "color", 
         diag = FALSE,
         outline = TRUE,
         mar=c(0,0,1,0),
         addCoef.col = "black")


# Adding more variables so the corrplot visulization can take a more complex form. 
# Life expectancy at birth

life_expec <- hdi_complete$Life.Expectancy.at.Birth
str(life_expec)

# Expected years of education

expec_education <- hdi_complete$Expected.Years.of.Education
str(expec_education)

# Mean years of education

mean_years_education <- hdi_complete$Mean.Years.of.Education
str(mean_years_education)

# Making a bigger dataframe with additional indexes extracted from the hdi_complete data.frame.

corplot_indexes2 <- data.frame(Gini = (gini_hdi$Gini), 
                               HDI = (gini_hdi$HDI),
                               Gender_dev =  (gender_dev_index),
                               Gender_ineq = (gender_ineq_index),
                               Exp_education = (expec_education),
                               Life_expec_birth = (life_expec),
                               Mean_education = (mean_years_education),
                               GNI_per_capita = (gni_per_capita))

str(corplot_indexes2)

# Performing correlation between all indexes combined. 

cor_indexes2 <- cor(corplot_indexes2, method = "pearson", use = "complete.obs")
cor_indexes2

# Visualising the new correlations using the corrplot function. 

corrplot(corr = cor_indexes2, title = "Correlations of additional HDI indexes", 
         type = "upper",
         method = "color", 
         diag = FALSE,
         order = "hclust",
         col = c("red", "lightblue"),
         outline = TRUE,
         mar=c(0,0,1,0),
         addCoef.col = "black")

# Building a linear regression model. Using the HDI variable as a predictor of expected education. 

linearmod <- lm(corplot_indexes2$Exp_education ~ corplot_indexes2$HDI, data = corplot_indexes2)
linearmod

# Looking at the summary for linear model.

summary(linearmod)

# Considering that the model is significant (p value is less then 0.05) two variables are represented using a plot. 

plot(corplot_indexes2$HDI, corplot_indexes2$Exp_education,
     main = "Linear model for HDI and education",
     xlab = "HDI", ylab = "Expected education",
     col = "blue")

# Adding a linear regression line, so we can see the relationship between two variables.

abline(linearmod)

# The visualization of linear regression model tells us that knowing the HDI level of a country,
# we can with great accuracy predict the years of expected education for each country.