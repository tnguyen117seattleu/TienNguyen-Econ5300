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
#Temperature_fixed_effect <- Project1%>% group_by(Store)%>% Project1$Temperature

project1 <- project1 %>%
  group_by(Store) %>%
  mutate(Temperature_poly = Temperature^2, 
         Temperature_log= log(Temperature), 
         Weekly_Sales_log= log(Weekly_Sales),
         Temperature_dm=Temperature- mean(Temperature),
         Temperature_poly_dm=Temperature_poly- mean(Temperature_poly),
         Temperature_log_dm=Temperature_log- mean(Temperature_log),
         Weekly_Sales_log_dm=Weekly_Sales_log-mean(Weekly_Sales_log),
         Weekly_Sales_dm= Weekly_Sales - mean(Weekly_Sales),
         CPI_dm=CPI-mean(CPI),
         Unemployment_dm=Unemployment -mean(Unemployment))
#plot for model 1
ggplot(project1, aes(x = Temperature_poly_dm, y = Weekly_Sales_dm)) +geom_point() +labs(x = "Temperature^2 (fixed effect)", y = "Weekly Sale") 
ggplot(project1, aes(x = Temperature_dm, y = Weekly_Sales_dm)) +geom_point() +labs(x = "Temperature (fixed effect)", y = "Weekly Sale") 
ggplot(project1, aes(x = Temperature_log_dm, y = Weekly_Sales_dm)) +geom_point() +labs(x = "log(Temperature) (demeaned by fixed effect)", y = "Weekly Sale") 
ggplot(project1, aes(x = 
Temperature_dm, y = Weekly_Sales_log_dm)) +geom_point() +labs(x = "Temperature (fixed effect)", y = " log(Weekly Sale)") 
#Plot for model 2
ggplot(project1, aes(x = Temperature_dm+ CPI_dm+ Unemployment_dm, y = Weekly_Sales_log_dm)) +geom_point() +labs(x = "Temperatue,CPI, and Unemployment (fixed effect)", y = " log(Weekly Sale)") 

#Assigning month for season
project1 <- project1 %>%mutate(Date = dmy(Date),   month = month(Date),
                               season = case_when(month %in% 1:3  ~ "Winter",
                                                  month %in% 4:6  ~ "Spring",
                                                  month %in% 7:9  ~ "Summer",
                                                  month %in% 10:12 ~ "Fall"))

project1 <- project1 %>%mutate(season = factor(season))

#Regresion models

model1<-feols(log(Weekly_Sales)~ Temperature|Store,vcov=~Store, data= project1 )

model2<-feols(log(Weekly_Sales)~ season+ Temperature+ CPI+ Unemployment | Store, vcov=~Store, data= project1 )

etable(model1,model2)
summary(model1)
summary(model2)


