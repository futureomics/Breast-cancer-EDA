library(readr)
library(dplyr)
library(naniar)
library(tidyverse)
library(ggcorrplot) # finding the correlation with variables 
#library(caTools)# splitting data into training set test set 
library(caret)


data_cancer <- read.csv('C:/Users/Lenovo/Downloads/data.csv')
data_cancer

head(data_cancer)
str(data_cancer)

#To visualize all the variable in the data frame
data_1 <- data_cancer %>%
  as.data.frame() %>%
  select_if(is.numeric) %>%
  gather(key = "variable", value = "value")
ggplot(data_1, aes(value)) +
  geom_density() +
  facet_wrap(~variable)

## We have all the data in the numeric form, 
#except diagnosis which is M and B 
## Lets convert this into numeric only

data_cancer$diagnosis <- factor(data_cancer$diagnosis, levels = c("M","B"), labels = c(0,1))

data_cancer$diagnosis <- as.character(data_cancer$diagnosis)
data_cancer$diagnosis <- as.numeric(data_cancer$diagnosis)
str(data_cancer)

data_cancer <- data_cancer %>% relocate(diagnosis,.after= fractal_dimension_worst)

#Visualising the correlation between datasets
r <- cor(data_cancer[,3:32])

round(r,2)

ggcorrplot(r)

#significance level on the correlogram.
#function for computing a matrix of correlation p-value

ggcorrplot(r, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))
