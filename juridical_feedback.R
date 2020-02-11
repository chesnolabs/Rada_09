library(tidyr)
library(dplyr)
library(rvest)
library(xml2)
library(data.table)
library(stringi)
library(RCurl)
library(stringr)

##### Негативні висновки Головного юридичного управління (ГЮУ) ####

##### Детальний опис законодавчої активності ####
bills_main_skl9 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_main-skl9.csv", 
                            fileEncoding = "UTF-8")
                            
##### Головні виконавці законів ####
bills_executives <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_executives-skl9.csv", 
                             fileEncoding = "UTF-8" ) %>%
  filter(type=="mainExecutive") %>% select(bill_id, department)

##### Чинні закони і інформація про них з виконавцями ####
bills_acts09 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_acts-skl9.csv", 
                         fileEncoding = "UTF-8" )%>%
  left_join(bills_main_skl9, by=c("bill_id"="bill_id"))%>%
  left_join(bills_executives, by=c("bill_id"="bill_id"))
  
##### Законодавчі ініціативи, які мають зауваження ГЮУ ####

bills_documents_gyur <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_documents-skl9.csv", 
                                 fileEncoding = "UTF-8" )%>%
  filter(document_type=="Зауваження Головного юридичного управління")

##### Відокремлення  ####
bi_gyur <- bills_documents_gyur%>%
  mutate(n_id = stri_extract_last_regex(document_url, "\\d{6}")) # Відокремлюємо останні шість символів в посиланні
  
# Поєднання зауважень із законодавчою активністю
bi_gyur_z <- bi_gyur%>%
  #left_join(bills_acts09, by=c("bill_id"="bill_id"))%>%
  #filter(!is.na(act_number))%>%
  left_join(bills_main_skl9, by=c("bill_id"="bill_id"))%>%
  filter(is.na(n_id))
  
##### Скачування висновків  ####  

# Робимо список з необхідних айдішників
bi_gyur_list <- as.list(bi_gyur_z$n_id)

# Завантажуємо дані

get_data <- function(){
 
  for(i in bi_gyur_list) {
    
    url <- paste0("http://w1.c1.rada.gov.ua/pls/zweb2/webproc77_1?pf35401=", i)

    page <- read_html(url)

    if(url.exists(url) == TRUE){
      
    name <- page%>%
      html_nodes("p")%>% html_text()%>%
      data.frame()
    
    df <- data.frame(name, url)
    }
    
    message(paste( 'завантажено'))
    
    write.table(df, 'concl_jurid_10_02_2020_full.csv', # Change date
                append = TRUE, col.names = FALSE, row.names = FALSE, sep = ';')
    
    Sys.sleep(0.6)
    
  }
}

##### Функція для активації скачування   ####  
results_juridicials <- get_data()

##### Завантаження скачаних висновків  ####  
library(readr)

# Відкрити скачаний файл
conclusion_read <- read_delim("concl_jurid_10_02_2020_full.csv", # Змінюємо назву відповідно до 70 рядка
                             ";", escape_double = FALSE, col_names = FALSE, 
                              locale = locale(encoding = "Windows-1251"), trim_ws = TRUE)
                              
# Вилучитит в колонці n_id_extracted останні шість символів 
conclusion_fixed_id <- conclusion_read %>%
  mutate(n_id_extracted = stri_extract_last_regex(X2, "\\d{6}"))
  
# Приєднати до новоствореної колонки з 6-ма символами датафрейм bi_gyur_z (зауваження із законодавчою активністю)
conclusion_fixed <- conclusion_fixed_id %>%
  left_join(bi_gyur_z, by=c("n_id_extracted"="n_id"))%>%
  separate(X1, c("text", "status_conc"), sep = ":")%>%
  mutate(status=str_trim(status_conc))

# Розширюємо файл
conclusion_extended <- conclusion_fixed %>%
  left_join(bills_main_skl9, by=c("bill_id"="bill_id"))%>%
  left_join(bills_executives, by=c("bill_id"="bill_id"))

# Записуємо файл розширених висновців 
write.xlsx(as.data.frame(conclusion_extended),  
           file=paste0("висновки_ГЮУ_фулл", "06_02_2020", ".xlsx"), #Change date
           sheetName="conclusion_extended", row.names=FALSE, append = FALSE)

#### Проходження ЗП, IX скл ####
bills_passings_reasons <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_passings-skl9.csv", 
                            fileEncoding = "UTF-8" )%>%
  group_by(passing_title)%>%
  summarise(n=n())

# Список законодавчих ініціатив, де "Вручено подання Комітету про відхилення"
bills_passings1 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_passings-skl9.csv", 
                           fileEncoding = "UTF-8" )%>%
  filter(passing_title=="Вручено подання Комітету про відхилення")

# Список законодавчих ініціатив, де "Проект відкликано"
bills_passings2 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_passings-skl9.csv", 
                            fileEncoding = "UTF-8" )%>%
  filter(passing_title=="Проект відкликано")%>%
  left_join(bills_main_skl9, by=c("bill_id"="bill_id"))%>%
  left_join(bills_executives, by=c("bill_id"="bill_id"))

# ЗП з ініціаторами і гол.виконавцями, де "Проект відкликано"
bills_passings2_init <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_passings-skl9.csv", 
                            fileEncoding = "UTF-8" )%>%
  filter(passing_title=="Проект відкликано")%>%
  left_join(bills_main_skl9, by=c("bill_id"="bill_id"))%>%
  left_join(bills_initiators, by=c("bill_id"="bill_id"))%>%
  left_join(factions_09, by=c("person"="fullname"))%>%
  left_join(bills_executives, by=c("bill_id"="bill_id"))
  
# Записуємо файл ЗП з ініціаторами і гол.виконавцями, де "Проект відкликано"
write.xlsx(as.data.frame(bills_passings3),  
           file=paste0("проект_відкликано_", "03_02_2020", ".xlsx"), # Змінити дату
           sheetName="bills_passings2_init", row.names=FALSE, append = FALSE) # Аркуш має таку саму назву як і датафрейм, який записуємо

# ЗП, де "Проект відхилено"
bills_passings3 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_passings-skl9.csv", 
                            fileEncoding = "UTF-8" )%>%
  filter(passing_title=="Проект відхилено")%>%
  left_join(bills_main_skl9, by=c("bill_id"="bill_id"))%>%
  left_join(bills_documents_gyur, by=c("bill_id"="bill_id"))#%>%
  left_join(bills_executives, by=c("bill_id"="bill_id"))
  
# Записуємо файл ЗП з ініціаторами і гол.виконавцями, де "Проект відхилено"
write.xlsx(as.data.frame(bills_passings3),  
           file=paste0("проект_відхилено_", "03_02_2020", ".xlsx"), # Змінити дату
           sheetName="bills_passings3", row.names=FALSE, append = FALSE)

# ЗП з ініціаторами і гол.виконавцями, де "Проект відхилено"
bills_passings3_init <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_passings-skl9.csv", 
                            fileEncoding = "UTF-8" )%>%
  filter(passing_title=="Проект відхилено")%>%
  left_join(bills_main_skl9, by=c("bill_id"="bill_id"))%>%
  left_join(bills_documents_gyur, by=c("bill_id"="bill_id"))#%>%
  left_join(bills_executives, by=c("bill_id"="bill_id"))%>%
  left_join(bills_initiators, by=c("bill_id"="bill_id"))%>%
  left_join(factions_09, by=c("person"="fullname"))
  
# Записуємо файл ЗП з ініціаторами і гол.виконавцями, де "Проект відхилено"
write.xlsx(as.data.frame(bills_passings3_init),  
           file=paste0("проект_відхилено_", "03_02_2020", ".xlsx"), # Змінити дату
           sheetName="bills_passings3_init", row.names=FALSE, append = FALSE)



