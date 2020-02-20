# rm(ls=list())
# Негативні висновки Головного науково-експертного управління (ГНЕУ) до законопроектів ####
library(rvest)
library(xml2)
library(stringi)
library(readr)

#  Існуючі висновки до законопроектів 9 скликання
bills_documents_gneu <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_documents-skl9.csv", 
                            fileEncoding = "UTF-8" )%>%
  filter(document_type=="Висновок Головного науково-експертного управління")

# Відокремлення ID  ####
bi_gneu <- bills_documents_gneu%>%
  mutate(n_id = stri_extract_last_regex(document_url, "\\d{6}"))

# Поєднання зауважень із законодавчою активністю ####
bi_gneu_z <- bi_gneu%>%
  #left_join(bills_acts09, by=c("bill_id"="bill_id"))%>%   # Лише чинні акти
  #filter(!is.na(act_number))%>%
  left_join(bills_main_skl9, by=c("bill_id"="bill_id"))%>% # Усі висновки до всіх існуючих ЗП
  filter(is.na(n_id))

# Робимо список з необхідних айдішників ####

bi_gneu_list <- as.list(bi_gneu_z$n_id)

# Завантажуємо дані ####

get_short_conclusions_gneu <- function(){
  
  for(i in bi_gneu_list) {
    
    url <- paste0("https://w1.c1.rada.gov.ua/pls/zweb2/webproc77_1?pf35401=", i)
    
    page <- read_html(url)
    
    if(url.exists(url) == TRUE){
      
      name <- page%>%
        html_nodes("p")%>% 
        html_text()%>%
        data.frame()
      
      df <- data.frame(name, url)
    }
    
 message(paste(i, '_downloaded'))
 
      # Write data 
      write.table(df, fileEncoding = "UTF-8", 
                  paste0("GNEU_concl_", "20_02_2020",  #Change date
                         ".csv"), 
                  append = TRUE, col.names = FALSE, 
                  row.names = FALSE, sep = ";")  
      
    }
    Sys.sleep(0.4)
    
}

# Функція для активації скачування   ####  

short_conclusions_gneu <- get_short_conclusions_gneu()

# Завантаження скачаних висновків  ####  

# Відкрити завантажений файл

GNEU_conclusion_read <- read_delim("GNEU_concl_20_02_2020.csv", 
                                    ";", escape_double = FALSE, col_names = FALSE, 
                                   #locale = locale(encoding = "Windows-1251"),
                                    trim_ws = TRUE)
View(GNEU_concl_20_02_2020)

# Вилучити в колонці n_id_extracted останні шість символів 

gneu_conclusion_fixed_id <- GNEU_conclusion_read %>%
  mutate(n_id_extracted = stri_extract_last_regex(X2, "\\d{6}"))

# Таблиця з висновками ГНЕУ ####
# Приєднати до новоствореної колонки з 6-ма символами датафрейм bi_gneu_z (зауваження із законодавчою активністю)

gneu_conclusion_fixed <- gneu_conclusion_fixed_id %>%
  left_join(bi_gneu_z, by=c("n_id_extracted"="n_id"))%>%
  separate(X1, c("text", "status_conc"), sep = ":")%>%
  mutate(status=str_trim(status_conc))

# Перевірити дублікати номерів законопроектів. 
duplicated(gneu_conclusion_fixed$bill_id)

# Записуємо файл із висновками ####
write.xlsx(as.data.frame(gneu_conclusion_fixed),  
           file=paste0("висновки_ГНЕУ_фулл_", "20_02_2020", ".xlsx"), #Change date
           sheetName="gneu_conclusion_fixed", row.names=FALSE, append = FALSE)
