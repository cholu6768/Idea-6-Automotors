# TODOs: 
# - change column names into english
# - check quality on products, family products, store locations
# - convert currency of colombian peso into usd - DONE

#############################################
# 0. Import Libraries
#############################################

library(tidyverse)
library(dplyr)

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


