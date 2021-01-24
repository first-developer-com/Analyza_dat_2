library(dplyr)
library(ggplot2)
library(openxlsx)

# definice cesty k cvs souboram
getwd()
setwd('/home/pinguin/Documents/Unicorn2020/Analyza dat - semestralka1/Analyza_dat_2/data')

# načítání souborů
oct <- read.table('2019-Oct.csv', sep=',', dec='.', na.strings=c(" ", ""), header = T, encoding = "UTF-8") 
nov <- read.table('2019-Nov.csv', sep=',', dec='.', na.strings=c(" ", ""), header = T, encoding = "UTF-8") 
dec <- read.table('2019-Dec.csv', sep=',', dec='.', na.strings=c(" ", ""), header = T, encoding = "UTF-8") 
jan <- read.table('2020-Jan.csv', sep=',', dec='.', na.strings=c(" ", ""), header = T, encoding = "UTF-8") 
feb <- read.table('2020-Feb.csv', sep=',', dec='.', na.strings=c(" ", ""), header = T, encoding = "UTF-8") 


# spojení vstupních dat do jedneho data.fame. Dělám to abych se dalo zpracovávat podle týdnů. Jinak musel bych resitspojeni týdnů na hranice měsíců. 
data <- rbind(oct, nov, dec, jan, feb)

# mazání dat, které už se nebudou používat. Pro šetření operacky
rm(oct,nov,dec,jan,feb)


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

count_mounth_statistics <- function(df){ # funkce zpracovává 1 měsíc
  objednane_produkty = df %>% filter(event_type == 'purchase') # filtrování jen objednané zboží
  
  prijem <- sum(objednane_produkty$price) # příjem
  
  objednavky <- objednane_produkty %>% group_by(user_session) %>% summarize(sum = sum(price), .groups='drop') # tím získám objednávky. Jeden uživatel může objednat více zboží za jeden krát. Abych to identifikovat, udělám group_by session_id

  prumer_objednavky <- mean(objednavky$sum) # průměrná hodnota objednávky 
  objednavky_count <- nrow(objednavky) # počet objednávek
  rm(objednavky) # mazání dat, které už se nebudou používat. Pro šetření operacky
  visits <- df %>% group_by(user_session) %>% summarize(user_id = first(user_id), .groups='drop') %>% nrow() # tím získám počet návštěvníků. Grupuju podle seesion_id abych vyhodit duplicity
  konverze_visit_buy <- objednavky_count*100/visits # konverze
  
  brand_prodej <- objednane_produkty %>% group_by(brand) %>% summarise(brand = first(brand), prijem = sum(price), .groups='drop') # prodeje podle brandů
  
  kategorie_prodej <- objednane_produkty %>% group_by(category_id) %>% summarise(category_id = first(category_id), prijem = sum(price), .groups='drop') # prodeje podle kategorie
  
  # ukládám výsledky do listu
  results <- list(prijem = prijem, prumer_objednavky = prumer_objednavky, objednavky_count = objednavky_count, konverze_visit_buy = konverze_visit_buy, brand_prodej = brand_prodej, kategorie_prodej = kategorie_prodej) 
  return(results)
}

count_week_statistics <- function(df){ #funkce zpracovává 1 tyden
  objednane_produkty = df %>% filter(event_type == 'purchase') # filtrování jen objednané zboží

  prijem <- sum(objednane_produkty$price) # příjem
  
  objednavky <- objednane_produkty %>% group_by(user_session) %>% summarize(sum = sum(price), .groups='drop') # tím získám objednávky. Jeden uživatel může objednat více zboží za jeden krát. Abych to identifikovat, udělám group_by session_id
  prumer_objednavky <- mean(objednavky$sum) # průměrná hodnota objednávky 
  objednavky_count <- nrow(objednavky)# počet objednávek
  rm(objednavky) # mazání dat, které už se nebudou používat. Pro šetření operacky
  visits <- df %>% group_by(user_session) %>% summarize(user_id = first(user_id), .groups='drop') %>% nrow() # tím získám počet návštěvníků. Grupuju podle seesion_id abych vyhodit duplicity
  konverze_visit_buy <- objednavky_count*100/visits # konverze
  results <- list(prijem = prijem, prumer_objednavky = prumer_objednavky, objednavky_count = objednavky_count, konverze_visit_buy = konverze_visit_buy)
  # ukládám výsledky do listu
  return(results)
}

count_day_statistics <- function(df){ #funkce zpracovává 1 den
  objednane_produkty = df %>% filter(event_type == 'purchase')
  
  prijem <- sum(objednane_produkty$price) # příjem
  
  # ukládám výsledky do listu
  results <- list(prijem = prijem)
  return(results)
}

count_statistics <- function(data, month=T, week=T, day=T){ # hlavní funkce, která postupně prochází měsíce, týdne a dny a ukládá výsledky výpočtů
  final_result <- list() # definice proměny z vysledkama
  if(month){ 
    final_result[['month_prijmy']] <- data.frame(month=c('1','2','3','4','5'), prijem=c(NA, NA, NA, NA, NA)) # zakládání data.frame pro příjem
    final_result[['month_objednavka_mean']] <- data.frame(month=c('1','2','3','4','5'), prumer=c(NA, NA, NA, NA, NA)) #zakládání data.frame pro průměrnou hodnotu objednávky
    final_result[['month_objednavky_count']] <- data.frame(month=c('1','2','3','4','5'), count=c(NA, NA, NA, NA, NA)) #zakládání data.frame pro počet objednávek
    final_result[['month_konverze_visit_buy']] <- data.frame(month=c('1','2','3','4','5'), value=c(NA, NA, NA, NA, NA)) #zakládání data.frame pro konverze
    
    brands <-unique(data$brand) # získání seznamu brandů
    brands[is.na(brands)] <- 'No brand' # přidání No brand
    
    final_result[['month_brand_sales']] <- data.frame(brand=brands) # zakládání data.frame pro brandy
    rm(brands) # mazání dat, které už se nebudou používat. Pro šetření operacky
    
    categories <-unique(data$category_id)
    categories[is.na(categories)] <- 'No category'
    
    final_result[['month_category_sales']] <- data.frame(category_id=categories)
    rm(categories) # mazání dat, které už se nebudou používat. Pro šetření operacky
    
    
    
    mounth_statistics <- list() # definice proměně pro měsíční statistiky
    for(a in 1:5){ # proházím cyklusem 5 měsíců
      month <- data %>% filter(month == a) %>% count_mounth_statistics() # výběr dat za 1 měsíc a spuštění funkce count_mounth_statistics pro ten měsíc
      list_name = paste0('month_', a) # definice názvu listu v výsledkách 
      mounth_statistics[[list_name]] <- month # ukládání výsledků
      rm(month) # mazání dat, které už se nebudou používat. Pro šetření operacky
    }
    
    for(page in 1:5){ # ukládání výsledků pro všechny měsíce do jedneho data.frame
      final_result[['month_prijmy']][page, 2] <-mounth_statistics[[page]]$prijem
      final_result[['month_objednavka_mean']][page, 2] <- mounth_statistics[[page]]$prumer_objednavky
      final_result[['month_objednavky_count']][page, 2] <- mounth_statistics[[page]]$objednavky_count
      final_result[['month_konverze_visit_buy']][page, 2] <- mounth_statistics[[page]]$konverze_visit_buy
      
      # výpočet podílu prodeje brandu na celkovim
      bp <- mounth_statistics[[page]]$brand_prodej 
      bp$brand[is.na(bp$brand)] <- 'No brand'
      bp$prijem <- bp$prijem*100/mounth_statistics[[page]]$prijem
      colnames(bp) <- c('brand', paste0('prijem_', page))
      final_result[['month_brand_sales']] <-right_join(final_result[['month_brand_sales']], bp, by='brand')
      rm(bp) # mazání dat, které už se nebudou používat. Pro šetření operacky
      
      # výpočet podílu prodeje kategorie na celkovim
      catp <- mounth_statistics[[page]]$kategorie_prodej
      catp$category_id[is.na(catp$category_id)] <- 'No category'
      catp$prijem <- catp$prijem*100/mounth_statistics[[page]]$prijem
      colnames(catp) <- c('category_id', paste0('prijem_', page))
      
      final_result[['month_category_sales']] <-right_join(final_result[['month_category_sales']], catp, by='category_id')
      rm(catp) # mazání dat, které už se nebudou používat. Pro šetření operacky
    }
    
    # výpočet indexu změny prodeje podle minulého měsíce
    final_result[['month_brand_sales']]$index2_to_1 <- final_result[['month_brand_sales']]$prijem_2/final_result[['month_brand_sales']]$prijem_1*100-100
    final_result[['month_brand_sales']]$index3_to_2 <- final_result[['month_brand_sales']]$prijem_3/final_result[['month_brand_sales']]$prijem_2*100-100
    final_result[['month_brand_sales']]$index4_to_3 <- final_result[['month_brand_sales']]$prijem_4/final_result[['month_brand_sales']]$prijem_3*100-100
    final_result[['month_brand_sales']]$index5_to_4 <- final_result[['month_brand_sales']]$prijem_5/final_result[['month_brand_sales']]$prijem_4*100-100
    final_result[['month_brand_sales']]$prumer <- rowMeans(final_result[['month_brand_sales']][,c('prijem_1','prijem_2','prijem_3','prijem_4','prijem_5')])
    final_result[['month_brand_sales']][is.na(final_result[['month_brand_sales']] )] <- 0 
    
    # výpočet indexu změny prodeje podle minulého měsíce
    final_result[['month_category_sales']]$index2_to_1 <- final_result[['month_category_sales']]$prijem_2/final_result[['month_category_sales']]$prijem_1*100-100
    final_result[['month_category_sales']]$index3_to_2 <- final_result[['month_category_sales']]$prijem_3/final_result[['month_category_sales']]$prijem_2*100-100
    final_result[['month_category_sales']]$index4_to_3 <- final_result[['month_category_sales']]$prijem_4/final_result[['month_category_sales']]$prijem_3*100-100
    final_result[['month_category_sales']]$index5_to_4 <- final_result[['month_category_sales']]$prijem_5/final_result[['month_category_sales']]$prijem_4*100-100
    final_result[['month_category_sales']]$prumer <- rowMeans(final_result[['month_category_sales']][,c('prijem_1','prijem_2','prijem_3','prijem_4','prijem_5')])
    final_result[['month_category_sales']][is.na(final_result[['month_category_sales']] )] <- 0 
    
  }
  
  if(week){
    week_statistics <- list() # definice proměně pro týdenní statistiky
    for(a in 1:22){ # proházím cyklusem 22 týdnů
      week <- data %>% filter(week == a) %>% count_week_statistics() # výběr dat za 1 týden a spuštění funkce count_week_statistics
      list_name = paste0('week_', a) # definice názvu listu v výsledkách 
      week_statistics[[list_name]] <- week # ukládání výsledků
    }
    
    # zakládání data.frame stejně jako pro měsíce
    final_result[['week_prijmy']] <- data.frame(week=seq(1,22), prijem=rep(NA, 22)) 
    final_result[['week_objednavka_mean']] <- data.frame(week=seq(1,22), prumer=rep(NA, 22))
    final_result[['week_objednavky_count']] <- data.frame(week=seq(1,22), count=rep(NA, 22))
    final_result[['week_konverze_visit_buy']] <- data.frame(week=seq(1,22), value=rep(NA, 22))
    
    
    for(page in 1:22){ # ukládání výsledků pro všechny týdny do jedneho data.frame
      final_result[['week_prijmy']][page, 2] <-week_statistics[[page]]$prijem
      final_result[['week_objednavka_mean']][page, 2] <- week_statistics[[page]]$prumer_objednavky
      final_result[['week_objednavky_count']][page, 2] <- week_statistics[[page]]$objednavky_count
      final_result[['week_konverze_visit_buy']][page, 2] <- week_statistics[[page]]$konverze_visit_buy
      
    }
  }
  
  if(day){
    day_statistics <- list() # definice proměně pro denní statistiky
    for(a in 1:152){ # proházím cyklusem 152 dnů
      day <- data %>% filter(day == a) %>% count_day_statistics() # výběr dat za 1 den a spuštění funkce count_day_statistics
      list_name = paste0('day', a) # definice názvu listu v výsledkách 
      day_statistics[[list_name]] <- day # ukládání výsledků
    }
    
    #final_result <- list()
    final_result[['day_prijmy']] <- data.frame(day=seq(1,152), prijem=rep(NA, 152)) # zakládání data.frame pro příjem
    
    
    for(page in 1:152){ # ukládání výsledků pro všechny dny do jedneho data.frame
      final_result[['day_prijmy']][page, 2] <-day_statistics[[page]]$prijem
    }
  }
  return(final_result)
}





# zpuštění výpočtu
data <- na_delete(data, colnames(data)[-c(5,6)])
data <- priprava_dat(data)


result <- count_statistics(data, T, T, T)


# kreslení grafů
ggplot(data=result[['month_prijmy']], aes(x=month, y=prijem, group = 1))+geom_line()+geom_line(aes(y=mean(prijem)))+geom_point()+labs(x='Měsíce', y='Příjmy', title="Graf příjmů podle měsíce")
ggplot(data=result[['month_objednavka_mean']], aes(x=month, y=prumer, group = 1))+geom_line()+geom_line(aes(y=mean(prumer))) + geom_point()+labs(x='Měsíce', y='Průměrná hodnota', title="Průměr objednávky podle měsíce")
ggplot(data=result[['month_objednavky_count']], aes(x=month, y=count, group = 1))+geom_line()+geom_line(aes(y=mean(count)))  + geom_point()+labs(x='Měsíce', y='Počet', title="Počet objednávek podle měsíce")
ggplot(data=result[['month_konverze_visit_buy']], aes(x=month, y=value, group = 1))+geom_line()+geom_line(aes(y=mean(value))) + geom_point()+labs(x='Měsíce', y='Procento konverze', title="Graf konverze podle měsíce")
    
grapg_brands_data <- result[['month_brand_sales']]
grapg_brands_data[is.na(grapg_brands_data)] <- 0
grapg_brands_data <- grapg_brands_data[grapg_brands_data$prumer>1,]
    
    
ggplot(grapg_brands_data, aes(x = "", y=prumer, fill = brand)) + geom_bar(width = 1, stat = "identity") + theme(axis.line = element_blank(), plot.title = element_text (hjust=0.5)) + labs(fill="class", x= NULL, y= NULL, title="Graf podílu brandu na celkových příjmech") + coord_polar(theta = "y", start=0)
  

 
ggplot(data=result[['week_prijmy']], aes(x=week, y=prijem, group = 1))+geom_line()+geom_line(aes(y=mean(prijem)))+geom_point()+labs(x='Týdny', y='Příjmy', title="Graf příjmů podle týdnů")
ggplot(data=result[['week_objednavka_mean']], aes(x=week, y=prumer, group = 1))+geom_line()+geom_line(aes(y=mean(prumer)))+geom_point()+labs(x='Týdny', y='Průměrná hodnota', title="Průměr objednávky podle týdnů")
ggplot(data=result[['week_objednavky_count']], aes(x=week, y=count, group = 1))+geom_line()+geom_line(aes(y=mean(count)))+geom_point()+labs(x='Týdny', y='Počet', title="Počet objednávek podle týdnů")
ggplot(data=result[['week_konverze_visit_buy']], aes(x=week, y=value, group = 1))+geom_line()+geom_line(aes(y=mean(value)))+geom_point()+labs(x='Týdny', y='Procento konverze', title="Graf konverze podle týdnů")


ggplot(data=result[['day_prijmy']], aes(x=day, y=prijem, group = 1))+geom_line()+geom_line(aes(y=mean(prijem)))+geom_point()+labs(x='Dny', y='Příjmy', title="Graf příjmů podle dnů")
  
# generování tabulek pro přílohy
write.xlsx(result[['month_brand_sales']], 'month_brand_sales.xlsx')
write.xlsx(result[['month_category_sales']], 'month_category_sales.xlsx')






