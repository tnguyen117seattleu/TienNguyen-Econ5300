library(tidyverse)
library(fixest)
library (rio)
library(vtable)
library(dplyr)

project1 <- read.csv("~/ECON 5300/Project1/Walmart_Sales.csv")
#Run correlation matrix
numeric_cols<- c('Weekly_Sales','Temperature','CPI','Unemployment')
cor_matrix <- round(cor(project1[ , numeric_cols], use = "complete.obs"),2)
print(cor_matrix)

#Plot figures to pick out good model for model 1
Temperature_fixed_effect <- Project1%>% group_by(Store)%>% Project1$Temperature
project1 <- project1 %>%
  group_by(Store) %>%
  mutate(Temperature_poly = Temperature^2, 
         Temperature_log= log(Temperature), 
         Weekly_sales_log= log(Weekly_sales))

ggplot(project1, aes(x = Temperature_poly, y = Weekly_sales)) +geom_point() +labs(x = "Temperature^2 (fixed effect)", y = "Weekly Sale") 
ggplot(project1, aes(x = Temperature_fixed_effect, y = Weekly_sales)) +geom_point() +labs(x = "Temperature (fixed effect)", y = "Weekly Sale") 
ggplot(project1, aes(x = Temperature_log, y = Weekly_sales)) +geom_point() +labs(x = "log(Temperature) (demeaned by fixed effect)", y = "Weekly Sale") 
ggplot(project1, aes(x = 
Temperature_fixed_effect, y = Weekly_sales_log)) +geom_point() +labs(x = "Temperature (fixed effect)", y = " log(Weekly Sale)") 

#Assigning month for season
project1 <- project1 %>%mutate(Date = dmy(Date),   month = month(Date),
                               season = case_when(month %in% 1:3  ~ "Winter",
                                                  month %in% 4:6  ~ "Spring",
                                                  month %in% 7:9  ~ "Summer",
                                                  month %in% 10:12 ~ "Fall"))

project1 <- project1 %>%mutate(season = factor(season))

#Regresion models

model1<-feols(Weekly_Sales~ Temperature, data= project1 )

model2<-feols(Weekly_Sales~ season+ Temperature+ CPI+ Unemployment | Store, vcov=~Store, data= project1 )

etable(model1,model2)


