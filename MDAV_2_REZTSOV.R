library(dplyr)
library(lubridate)
#library(timeperiodsR)


getwd()
setwd('/home/pinguin/Documents/Unicorn2020/Analyza dat - semestralka1/Analyza_dat_2/data')

# dal prepsat na cyklus
jan <- read.table('2020-Jan.csv', sep=',', dec='.', na.strings=c("NA","NaN", " ", ""), header = T, encoding = "UTF-8") 


jan <- na_delete(jan, colnames(jan)[-c(5,6)])

jan <- priprava_dat(jan)


na_delete <- function(df, cols){
  return(jan[complete.cases(jan[cols]), ]) # vyloucim pozorovani, ktere maji na hodnoty krome category_code a brand
}

priprava_dat <- function(df){
  # prevod event_time do formatu datumu
  jan$event_time <- as.Date(jan$event_time)
  
  # prevod category_id do citelniho formatu
  jan$category_id <- as.character(jan$category_id)
  
  # pro vyber si zalozim stloupce day a week
  jan <- jan %>% mutate(week = cut.Date(event_time, breaks = "1 week", labels = FALSE)) %>% arrange(event_time)
  jan <- jan %>% mutate(day = cut.Date(event_time, breaks = "1 day", labels = FALSE)) %>% arrange(event_time)
  
  return(jan)
}



count_mounth_statistics <- function(df){
    
}


jan2 <- jan
objednane_produkty = jan2 %>% filter(event_type == 'purchase')

prijem <- sum(objednane_produkty$price)

objednavky <- objednane_produkty %>% group_by(user_session) %>% summarize(sum = sum(price), .groups='drop') 
prumer_objednavky <- mean(objednavky$sum) 
objednavky_count <- nrow(objednavky)
rm(objednavky)
visits <- jan2 %>% group_by(user_session) %>% summarize(user_id = first(user_id), .groups='drop') %>% nrow()
konverze_visit_buy <- objednavky_count*100/visits



produkty <- rle(sort(objednane_produkty$product_id))
prodej_produktu <- data.frame(prod_id=produkty$values, n_buy=produkty$lengths)
rm(produkty)

zobrazene_produkty <- jan2 %>% filter(event_type == 'view')
produkty <- rle(sort(zobrazene_produkty$product_id))
zobrazeni_produktu <- data.frame(prod_id=produkty$values, n_show=produkty$lengths)


konverze_produktu <- left_join(zobrazeni_produktu, prodej_produktu, by='prod_id')

konverze_produktu[is.na(konverze_produktu)] <- 0
konverze_produktu$konverze <-konverze_produktu$n_buy*100/konverze_produktu$n_show

clients <- jan2$user_id[!(jan2$user_id %in% jan2$user_id[duplicated(jan2$user_id)])]

results <- list(prijem = prijem, prumer_objednavky = prumer_objednavky, objednavky_count = objednavky_count, konverze_visit_buy = konverze_visit_buy, konverze_produktu = konverze_produktu, clients = clients)
