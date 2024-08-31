# loading libraries

library(rvest)
library(tidyverse)
library(tidyr)
library(dplyr)

# loading the data for TCS

url_tcs <- 'https://finance.yahoo.com/quote/TCS.BO/history?period1=1356998400&period2=1696118400&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true'
page <- read_html(url_tcs)
data <- page  %>%  html_table()
tcs <- data[[1]][2:100,]

view(tcs)

# Function To Clean the data

cleaner <- function(comp)
{
  str(comp)
  colnames(comp)[5] = 'Close'
  colnames(comp)[6] = 'Adj_Close'
  
  # Conversion of Date in String to Datetime Object
  
  comp$Date = as.Date(comp$Date,format = '%b %d, %Y')
  
  # Conversion of Stock Prices to Numeric Values
  
  comp$Open <- as.numeric(gsub(',','',comp$Open))
  comp$High <- as.numeric(gsub(',','',comp$High))
  comp$Low <- as.numeric(gsub(',','',comp$Low))
  comp$`Close` <- as.numeric(gsub(',','',comp$Open))
  comp$Adj_Close <- as.numeric(gsub(',','',comp$Adj_Close))
  comp$Volume <-  as.numeric(gsub(',','',comp$Volume))
  
  # Omiting the NA Values
  
  comp <- na.omit(comp)
  
  return(comp)
}

# Cleaning Data USing Function Cleaner

tcs <- cleaner(tcs)
view(tcs)
str(tcs)


# Now repeating the above process for the remaining companies and commodities

# JSW Steel

url_jsw <- 'https://finance.yahoo.com/quote/JSWSTEEL.BO/history?period1=1470009600&period2=1696204800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true'
page <- read_html(url_jsw)
data <- page %>% html_table()
jsw <- data[[1]][2:100,]

jsw <- cleaner(jsw)
view(jsw)

# Infosys

url_infosys <- 'https://finance.yahoo.com/quote/INFY.BO/history?period1=1470009600&period2=1696204800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true'
page <- read_html(url_infosys)
data <- page %>% html_table()
infosys <- data[[1]][2:100,]

infosys <- cleaner(infosys)
view(infosys)


# HDFC Bank

url_hdfc <- 'https://finance.yahoo.com/quote/HDFCBANK.BO/history?period1=1470009600&period2=1696204800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true'
page <- read_html(url_hdfc)
data <- page %>% html_table()
hdfc <- data[[1]][2:100,]

hdfc <- cleaner(hdfc)
view(hdfc)


# Bse Sensex

url_bse <- 'https://finance.yahoo.com/quote/%5EBSESN/history?period1=1470009600&period2=1696204800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true'
page <- read_html(url_bse)
data <- page %>% html_table()
bse <- data[[1]][2:100,]

bse <- cleaner(bse)
view(bse)


# Now the Data is Cleaned

# Appending Features And Some Financial Metrics

change <- function(fin)
{
  fin <- arrange(fin,Date)
  fin$Change = numeric(length=86)
  fin$Change[1] = 0
  for(i in 2:86)
  {
    fin$Change[i] = (fin$Close[i]-fin$Close[i-1])/(fin$Close[i-1])*100
  }
  return(fin)
}

tcs <- change(tcs)
jsw <- change(jsw)
hdfc <- change(hdfc)
infosys <- change(infosys)
bse <- change(bse)

# Some Financial Metrics

infosys_fm <- c(22.9,2.48,15.92)
names(infosys_fm) <- c('P/E','Div_Yield','NPR')

jsw_fm <- c(20.2,0.46,6.19)
names(jsw_fm) <- c('P/E','Div_Yield','NPR')

hdfc_fm <- c(22.9,2.48,26.68)
names(hdfc_fm) <- c('P/E','Div_Yield','NPR')

tcs_fm <- c(27.56,1.49,19)
names(tcs_fm) <- c('P/E','Div_Yield','NPR')

tcs <- list(tcs,tcs_fm)
infosys <- list(infosys,infosys_fm)
jsw <- list(jsw,jsw_fm)
hdfc <- list(hdfc,hdfc_fm)


# Storing Data as R Object 
save(tcs,file='../Data_Files/TCS')
save(jsw,file='../Data_Files/JSW')
save(hdfc,file='../Data_Files/HDFC')
save(infosys,file='../Data_Files/INFOSYS')
save(bse,file='../Data_Files/BSE')




