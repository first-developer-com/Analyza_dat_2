library(dplyr)
#library(lubridate)
#library(rlist)
library(ggplot2)
#library(timeperiodsR)


getwd()
setwd('/home/pinguin/Documents/Unicorn2020/Analyza dat - semestralka1/Analyza_dat_2/data')

oct <- read.table('2019-Oct.csv', sep=',', dec='.', na.strings=c(" ", ""), header = T, encoding = "UTF-8") 
nov <- read.table('2019-Nov.csv', sep=',', dec='.', na.strings=c(" ", ""), header = T, encoding = "UTF-8") 
dec <- read.table('2019-Dec.csv', sep=',', dec='.', na.strings=c(" ", ""), header = T, encoding = "UTF-8") 
jan <- read.table('2020-Jan.csv', sep=',', dec='.', na.strings=c(" ", ""), header = T, encoding = "UTF-8") 
feb <- read.table('2020-Feb.csv', sep=',', dec='.', na.strings=c(" ", ""), header = T, encoding = "UTF-8") 



data <- rbind(oct, nov, dec, jan, feb)

rm(oct,nov,dec,jan,feb)

data <- na_delete(data, colnames(data)[-c(5,6)])
data <- priprava_dat(data)






na_delete <- function(df, cols){
  return(df[complete.cases(df[cols]), ]) # vyloucim pozorovani, ktere maji na hodnoty krome category_code a brand
}

priprava_dat <- function(df){
  # prevod event_time do formatu datumu
  df$event_time <- as.Date(df$event_time)
  
  # prevod category_id do citelniho formatu
  df$category_id <- as.character(df$category_id)
  
  # pro vyber si zalozim stloupce day, month a week
  df <- df %>% mutate(month = cut.Date(event_time, breaks = "1 month", labels = FALSE)) %>% arrange(event_time)
  df <- df %>% mutate(week = cut.Date(event_time, breaks = "1 week", labels = FALSE)) %>% arrange(event_time)
  df <- df %>% mutate(day = cut.Date(event_time, breaks = "1 day", labels = FALSE)) %>% arrange(event_time)
  
  return(df)
}


count_mounth_statistics <- function(df){
  objednane_produkty = df %>% filter(event_type == 'purchase')
  
  prijem <- sum(objednane_produkty$price)
  
  objednavky <- objednane_produkty %>% group_by(user_session) %>% summarize(sum = sum(price), .groups='drop') 
  prumer_objednavky <- mean(objednavky$sum) 
  objednavky_count <- nrow(objednavky)
  rm(objednavky)
  visits <- df %>% group_by(user_session) %>% summarize(user_id = first(user_id), .groups='drop') %>% nrow()
  konverze_visit_buy <- objednavky_count*100/visits
  
  produkty <- rle(sort(objednane_produkty$product_id))
  prodej_produktu <- data.frame(prod_id=produkty$values, n_buy=produkty$lengths)
  rm(produkty)
  
  zobrazene_produkty <- df %>% filter(event_type == 'view')
  produkty <- rle(sort(zobrazene_produkty$product_id))
  zobrazeni_produktu <- data.frame(prod_id=produkty$values, n_show=produkty$lengths)
  
  
  konverze_produktu <- left_join(zobrazeni_produktu, prodej_produktu, by='prod_id')
  
  konverze_produktu[is.na(konverze_produktu)] <- 0
  #konverze_produktu$konverze <-konverze_produktu$n_buy*100/konverze_produktu$n_show
  brand_prodej <- objednane_produkty %>% group_by(brand) %>% summarise(brand = first(brand), prijem = sum(price), .groups='drop')
  
  kategorie_prodej <- objednane_produkty %>% group_by(category_id) %>% summarise(category_id = first(category_id), prijem = sum(price), .groups='drop')
  
  results <- list(prijem = prijem, prumer_objednavky = prumer_objednavky, objednavky_count = objednavky_count, konverze_visit_buy = konverze_visit_buy, konverze_produktu = konverze_produktu, brand_prodej = brand_prodej, kategorie_prodej = kategorie_prodej)
  return(results)
}

count_week_statistics <- function(df){
  objednane_produkty = df %>% filter(event_type == 'purchase')

  prijem <- sum(objednane_produkty$price)
  
  objednavky <- objednane_produkty %>% group_by(user_session) %>% summarize(sum = sum(price), .groups='drop') 
  prumer_objednavky <- mean(objednavky$sum) 
  objednavky_count <- nrow(objednavky)
  rm(objednavky)
  visits <- df %>% group_by(user_session) %>% summarize(user_id = first(user_id), .groups='drop') %>% nrow()
  konverze_visit_buy <- objednavky_count*100/visits
  results <- list(prijem = prijem, prumer_objednavky = prumer_objednavky, objednavky_count = objednavky_count, konverze_visit_buy = konverze_visit_buy)
  return(results)
}

count_day_statistics <- function(df){
  objednane_produkty = df %>% filter(event_type == 'purchase')
  
  prijem <- sum(objednane_produkty$price)
  
  objednavky <- objednane_produkty %>% group_by(user_session) %>% summarize(sum = sum(price), .groups='drop') 
  prumer_objednavky <- mean(objednavky$sum) 
  objednavky_count <- nrow(objednavky)
  results <- list(prijem = prijem, prumer_objednavky = prumer_objednavky, objednavky_count = objednavky_count)
  return(results)
}


count_statistics <- function(data){
  
}

mounth_statistics <- list()
for(a in 1:5){
  month <- data %>% filter(month == a) %>% count_mounth_statistics()
  list_name = paste0('month_', a)
  mounth_statistics[[list_name]] <- month
  rm(month)
}

week_statistics <- list()
for(a in 1:18){
  week <- data %>% filter(week == a) %>% count_week_statistics()
  list_name = paste0('week_', a)
  week_statistics[[list_name]] <- week
}

day_statistics <- list()
for(a in 1:123){
  day <- data %>% filter(day == a) %>% count_day_statistics()
  list_name = paste0('day', a)
  day_statistics[[list_name]] <- day
}

final_result <- list()

final_result[['month_prijmy']] <- data.frame(month=c('1','2','3','4','5'), prijem=c(NA, NA, NA, NA, NA))
final_result[['month_objednavka_mean']] <- data.frame(month=c('1','2','3','4','5'), prumer=c(NA, NA, NA, NA, NA))
final_result[['month_objednavky_count']] <- data.frame(month=c('1','2','3','4','5'), count=c(NA, NA, NA, NA, NA))
final_result[['month_konverze_visit_buy']] <- data.frame(month=c('1','2','3','4','5'), value=c(NA, NA, NA, NA, NA))
final_result[['month_sales_products']] <- data.frame(month=c('1','2','3','4','5'), min_count=c(NA, NA, NA, NA, NA), max_count=c(NA, NA, NA, NA, NA))
final_result[['month_popular_products']] <- data.frame(month=c('1','2','3','4','5'), min_count=c(NA, NA, NA, NA, NA), max_count=c(NA, NA, NA, NA, NA))

brands <-unique(data$brand)
brands[is.na(brands)] <- 'No brand'

final_result[['month_barand_sales']] <- data.frame(brand=brands)
rm(brands)







for(page in 1:5){
  final_result[['month_prijmy']][page, 2] <-mounth_statistics[[page]]$prijem
  final_result[['month_objednavka_mean']][page, 2] <- mounth_statistics[[page]]$prumer_objednavky
  final_result[['month_objednavky_count']][page, 2] <- mounth_statistics[[page]]$objednavky_count
  final_result[['month_konverze_visit_buy']][page, 2] <- mounth_statistics[[page]]$konverze_visit_buy
  kp <- mounth_statistics[[1]]$konverze_produktu
  final_result[['month_sales_products']][page, 2] <- filter(kp, n_buy < 1)
  final_result[['month_sales_products']][page, 3] <- filter(kp, n_buy > 10)
  final_result[['month_popular_products']][page, 2] <- filter(kp, n_show < 10)
  final_result[['month_popular_products']][page, 3] <- filter(kp, n_show > 850)
  rm(kp)
  
  bp <- mounth_statistics[[page]]$brand_prodej
  bp$brand[is.na(bp$brand)] <- 'No brand'
  bp$prijem <- bp$prijem*100/mounth_statistics[[page]]$prijem
  colnames(bp) <- c('brand', paste0('prijem_', page))
  final_result[['month_barand_sales']] <-right_join(final_result[['month_barand_sales']], bp, by='brand')
  
  rm(bp)
}


final_result[['month_barand_sales']]$index2_to_1 <- final_result[['month_barand_sales']]$prijem_2/final_result[['month_barand_sales']]$prijem_1*100-100
final_result[['month_barand_sales']]$index3_to_2 <- final_result[['month_barand_sales']]$prijem_3/final_result[['month_barand_sales']]$prijem_2*100-100
final_result[['month_barand_sales']]$index4_to_3 <- final_result[['month_barand_sales']]$prijem_4/final_result[['month_barand_sales']]$prijem_3*100-100
final_result[['month_barand_sales']]$index5_to_4 <- final_result[['month_barand_sales']]$prijem_5/final_result[['month_barand_sales']]$prijem_4*100-100


















# grafy
ggplot(data=final_result[['month_prijmy']], aes(x=month, y=prijem, group = 1))+geom_line()+geom_line(aes(y=mean(prijem))) # to funguje
ggplot(data=final_result[['month_objednavka_mean']], aes(x=month, y=prumer, group = 1))+geom_line()+geom_line(aes(y=mean(prumer))) # to funguje
ggplot(data=final_result[['month_objednavky_count']], aes(x=month, y=count, group = 1))+geom_line()+geom_line(aes(y=mean(count))) # to funguje
ggplot(data=final_result[['month_konverze_visit_buy']], aes(x=month, y=value, group = 1))+geom_line()+geom_line(aes(y=mean(value))) # to funguje





