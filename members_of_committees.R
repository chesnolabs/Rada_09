library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(httr)
library(magrittr)
library (data.table)
library(readr)

Sys.setlocale("LC_ALL", "Ukrainian")

# Функція для скачування даних
get_komitet <- function(){
  # Унікальні ID комітетів
  comit_id <- c(3002, 3003, 3004, 3005, 3006, 3007, 3008, 3009, 3010, 3293, 3012, 3013, 
                 3014,3015,3016,3017,3018,3019,3020,3021, 3022,3023, 3024)
  
  for(i in comit_id) {
    
    url <- paste0("http://w1.c1.rada.gov.ua/pls/site2/p_komity_list?pidid=", i)
  
    url_table <- read_html(url, encoding = "Windows-1251")%>%
      html_table(fill = TRUE)
    
    # На основній сторінці нам треба таблиця №1
    results_2019 <- url_table[[1]]
    
    # Збираємо назву Комітету над табличкою
    name <- read_html(url, encoding = "Windows-1251") %>%
      html_node(".information_block h3") %>%
      html_text(trim = TRUE)
    
    # Додаємо нову колонку з назвою комітету в основну таблицю
    results_2019$name <- name
    
    # Зберігаємо дані в таблицю
    write.table(results_2019,
                paste0("members_komit_09_",  Sys.Date(),".csv"), 
                append = TRUE, col.names = FALSE, 
                row.names = FALSE, sep = ";")
    message(paste(i, '_downloaded'))
    
    Sys.sleep(0.3)
    }
}

komitety <- get_komitet()

# Імпортуємо завантажені дані в R
# Увага: у нас не завжди кіклькість нардепів серед членів комітетів буде дорівнювати кількості чинних нардепів
# Оскільки є нардепи, які не входять в жоден комітетів 
# Насамперед йдеться про керівництво ВРУ, але є також і рядові нардепи, яких обрали пізніше, але ВРУ ще не направила їх в комітети 
membership_k <- read.csv("members_komit_09_2021-06-04.csv", header = FALSE)
