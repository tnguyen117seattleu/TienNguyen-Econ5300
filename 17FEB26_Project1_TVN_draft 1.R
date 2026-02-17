library(tidyverse)
library(fixest)
library (rio)
library(vtable)


project1 <- read.csv("~/ECON 5300/Project1/Walmart_Sales.csv")




project1 <- project1 %>%mutate(Date = dmy(Date),   month = month(Date),season = case_when(month %in% 1:3  ~ "Winter",
      month %in% 4:6  ~ "Spring",month %in% 7:9  ~ "Summer",month %in% 10:12 ~ "Fall"))

project1 <- project1 %>%mutate(season = factor(season))

numeric_cols<- c('Weekly_Sales','Temperature','CPI','Unemployment')

cor_matrix <- round(cor(project1[ , numeric_cols], use = "complete.obs"),2)
print(cor_matrix)

model1<-feols(Weekly_Sales~ Temperature, data= project1 )


project1<- project1%>% group_by(Store) %>% 
  mutate(Temperature_mean = mean(Temperature),Weekly_Sales_mean = mean(Weekly_Sales),
         CPI_mean = mean(CPI),Unemployment_mean = mean(Unemployment),
         Temperature_within = Temperature - Temperature_mean,
         Weekly_Sales_within = Weekly_Sales - Weekly_Sales_mean,
         CPI_within = CPI - CPI_mean,
         Unemployment_within = Unemployment - Unemployment_mean )

model2<-feols(Weekly_Sales_within~ season+Temperature_within+CPI_within+Unemployment_within, vcov=~Store, data= project1 )

etable(model1,model2)

