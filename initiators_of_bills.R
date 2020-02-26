#  https://data.rada.gov.ua/open/data/drafts
#  Законопроекти

# Закононодавство ####

# Фракції нардепів ####
get_factions_open <- function(){
  posts <- read_csv("https://data.rada.gov.ua/ogd/mps/skl9/mp-posts_ids.csv")
  posts_ids <- read_tsv("https://data.rada.gov.ua/ogd/mps/skl9/mp-posts_unit.txt", 
                        locale(encoding = "windows-1251"), col_names = F, col_types = NULL) %>% 
    rename(unit_id = X1, unit = X2)
  mps <- read_csv("https://data.rada.gov.ua/ogd/mps/skl9/mps09-data.csv")
  
  factions_full <- posts %>% 
    left_join(mps[, c("rada_id" ,"id", "full_name")], by = c("mp_id" = "id")) %>% 
    left_join(posts_ids) %>% 
    filter(unit_type %in% c("grp", "fra")) %>% 
    select(mp_id, full_name, unit)
  
  factions_df <-  mps %>% 
    filter(is.na(resignation_text)) %>% 
    select(rada_id, id, full_name) %>% 
    left_join(factions_full, by = c("id" = "mp_id", "full_name")) %>% 
    mutate(unit = ifelse(is.na(unit), "Позафракційні", unit)) %>% 
    rename(factions = unit, fullname = full_name) %>% 
    mutate(factions = recode(factions,
                             `Фракція ПОЛІТИЧНОЇ ПАРТІЇ "СЛУГА НАРОДУ"` = "Слуга Народу",
                             `Фракція Політичної Партії "ГОЛОС" у Верховній Раді України дев'ятого скликання` = "ГОЛОС",
                             `Фракція ПОЛІТИЧНОЇ ПАРТІЇ "ЄВРОПЕЙСЬКА СОЛІДАРНІСТЬ"` = "ЄС",
                             `Фракція Політичної партії "ОПОЗИЦІЙНА ПЛАТФОРМА - ЗА ЖИТТЯ" у Верховній Раді України` = "ОПЗЖ",
                             `Фракція політичної партії Всеукраїнське об'єднання "Батьківщина" у Верховній Раді України дев'ятого скликання` = "Батьківщина",
                             `Група "За майбутнє" у Верховній Раді України` = "За майбутнє",
                             `Група "ДОВІРА"`= "ДОВІРА"))
  return(factions_df)
}
# Функція для завантаження фракцій
factions_09 <- get_factions_open()

# Законопроекти, що зареєстровано у Верховній Раді 9 скл
# https://data.rada.gov.ua/open/data/bills_main-skl9
bills_main_skl9 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_main-skl9.csv", 
                            fileEncoding = "UTF-8")

# Головні виконавці ####
bills_executives <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_executives-skl9.csv", 
                             fileEncoding = "UTF-8" ) %>%
  filter(type=="mainExecutive") %>%
  select(bill_id, department)

# Чинні закони  ####
bills_acts09 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_acts-skl9.csv", 
                         fileEncoding = "UTF-8" )%>%
  left_join(bills_main_skl9, by=c("bill_id"="bill_id"))%>%
  left_join(bills_executives, by=c("bill_id"="bill_id"))

# Ініціатори законопроектів ####
bills_initiators <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_initiators-skl9.csv",
                             fileEncoding = "UTF-8")%>%
  left_join(factions_09, by=c("person"="fullname"))     # Приєднуємо чинні фракції нардепів

# Ініціатори законопроектів.Розширення з к-стю ініціаторів ####
bills_acts09_init <- bills_acts09%>%
  left_join(bills_initiators, by=c("bill_id"="bill_id"))%>% 
  group_by(bill_id) %>% 
  mutate(order = seq_along(bill_id))%>% # Кількість ініціаторів до кожного законопроекту 
  ungroup()

# Запис у файл ####

dir.create("output_cumulative")

write.xlsx(as.data.frame(bills_acts09), 
           file=paste0("output_cumulative/Ініціатори_законів", ".xlsx"),
           sheetName="Чинні_акти", row.names=FALSE, append = FALSE)

write.xlsx(as.data.frame(bills_acts09_init), 
           file=paste0("output_cumulative/Ініціатори_законів", ".xlsx"),
           sheetName="Ініціатори_законів", row.names=FALSE, append = TRUE)

