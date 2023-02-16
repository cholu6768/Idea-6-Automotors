# TODOs: 
# - change column names into English
# - check quality on products, family products, store locations - LOOKED GOOD
# - convert currency of Colombian peso into USD - DONE
# - how many customers per store and their avg revenue per customer, min and max sale dates
# - time periods of products and services per store  
# - employees per store - DONE

#############################################
# 0. Import Libraries
#############################################

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(lubridate)

#############################################
# 1. Read Data
#############################################

#import csv file
sales <- as_tibble(read.csv("Data/Ventas.csv", sep = ";"))

#############################################
# 2. Clean and Transform Data
#############################################

#lowercase all column names
names(sales) <- tolower(names(sales))

#drop rows of NAs because they are completely empty
#for this example, we could say there was some data loss
sales <- sales %>% 
  filter_all(all_vars(!is.na(.)))

# I found 5 values of Ventas and 2 of Cantidad that were wrongly entered
# The values had dot and comas, which made them a NA when converted into integers
sales <- sales %>% 
  mutate(ventas = str_replace(ventas, "70.000.001,00", "70000001"),
           ventas = str_replace(ventas, "74.511.448,00", "74511448"),
           ventas = str_replace(ventas, "74.158.398,00", "74158398"), 
           ventas = str_replace(ventas, "65.321.270,00", "65321270"),
           cantidad = str_replace(cantidad, "1,006", "1006"),
           cantidad = str_replace(cantidad, "1,068", "1068")) 

#convert data types, convert ventas to USD currency
sales_tst <- sales %>% 
  mutate(cantidad = as.integer(cantidad),
         ventas = as.numeric(ventas),
         fecha = as.Date(fecha, "%d/%m/%Y"),
         ventas_usd = round((ventas * 0.00023), 2)
         )

#############################################
# 3. Analyze Data
#############################################

#FAMILY PRODUCTS
#LLANTA: 973 unique types
#REENCAUCHE: 278 unique types
#LUBRICANTES: 75 unique types
#SERVICIOS: 130 unique types
#FILTROS: 368 unique types

#AREA and NAME OF STORES
#Bogota: Grandes flotas
#Engativa: Ventas externas & Calle 80
#Suba: Suba
#Usaquen: Santa ana 
#Barrios unidos: Av Chile
#Puente aranda: Calle 13

#TOTAL Revenue in USD
sales_tst %>% 
  summarise(total_revenue = sum(ventas_usd))#17,118,022 USD

#TOTAL PRODUCTS SOLD
sales_tst %>% 
  filter(familia != "SERVICIOS") %>% 
  summarise(items_sold = sum(cantidad))#103,744 Items

#TOTAL REVENUE SERVICES
sales_tst %>% 
  filter(familia == "SERVICIOS") %>% 
  summarise(revenue_services = sum(ventas_usd))#2,591,331 USD

#TOTAL SERVICES OFFERED 
sales_tst %>% 
  filter(familia == "SERVICIOS") %>% 
  summarise(revenue_services = sum(cantidad))#213,395 Services offered

#NUMBER OF STORES
sales_tst %>% 
  summarise(stores = n_distinct(sede))#7 stores

#TOTAL PRODUCT FAMILY REVENUE PER STORE
sales_tst %>% 
  group_by(sede, familia) %>% 
  summarise(revenue = sum(ventas_usd)) %>% 
  pivot_wider(names_from = familia, values_from = revenue)

#REVENUE PER EMPLOYEE
sales_tst %>% 
  group_by(sede,empleado, familia) %>% 
  summarise(revenue = sum(ventas_usd)) %>% 
  pivot_wider(names_from = familia, values_from = revenue)

#Customers per store
cust_store <- sales_tst %>% 
  group_by(sede, familia) %>% 
  summarise(customers = n_distinct(idcliente),
            mean_revenue = mean(ventas_usd),
            median_revenue = median(ventas_usd))

#SUBA seems to be the worst store even though it the second most clients
#Ventas externas and Grandes flotas bring the most revenue to the business
#The familie products that bring the most money are llantas and Reencauche
ggplotly(ggplot(cust_store, aes(fill=familia, y=customers, x=sede)) + 
  geom_bar(position="dodge", stat="identity"))

ggplotly(ggplot(cust_store, aes(fill=familia, y=median_revenue, x=sede)) + 
           geom_bar(position="dodge", stat="identity"))

#Employees per store
#Apart from ventas externas and grandes flotas, SANTA ANA is doing good based on its
#revenues, num of customers and amount of employees.
#SUBA has the second most customers but is also the second with least employees..
sales_tst %>% 
  group_by(sede) %>% 
  summarise(employees = n_distinct(empleado),
            customers = n_distinct(idcliente),
            mean_revenue = mean(ventas_usd),
            median_revenue = median(ventas_usd)) 

#Seasonality
store_season <- sales_tst %>% 
  group_by(sede, year(fecha)) %>% 
  summarise(employees = n_distinct(empleado),
            customers = n_distinct(idcliente),
            mean_revenue = mean(ventas_usd),
            median_revenue = median(ventas_usd))

#SUBA and Calle 80 had the lowest mean revenues across 5 years
ggplotly(ggplot(store_season, aes(x=`year(fecha)`, y=mean_revenue, color=sede)) + 
           geom_line()+
           geom_point())
