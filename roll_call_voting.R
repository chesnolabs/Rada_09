library(dplyr)
library(tidyverse)
library(stringr)
library(jsonlite)
library(extrafont)
library(extrafontdb)
library(forcats)
library(ggplot2)
library(readxl)
library(xlsx)
library(splitstackshape) # https://stackoverflow.com/questions/15097162/splitting-strings-into-multiple-rows-in-r
library(purrr)
library(rJava)

#### Крок 1. Завантажуємо законодавчу частину ####

# Законопроекти, що зареєстровано у Верховній Раді 9 скликання
# https://data.rada.gov.ua/open/data/bills_main-skl9

bills_main_skl9 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_main-skl9.csv", 
                            fileEncoding = "UTF-8")%>%
  select(bill_id, number,type, rubric, subject, currentPhase_title)

# Головні та інші виконавці законопроектів IX скликання
#https://data.rada.gov.ua/open/data/bills_executives-skl9

bills_executives <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_executives-skl9.csv", 
                                  fileEncoding = "UTF-8")%>%
  filter(type=="mainExecutive")%>%
  select(-convocation, -person_id, -organization, -post)

#### Крок 2. Завантажуємо інформація про нардепів, фракції та адженду ####

#### Крок 2.1 ####
# ПІБ нардепів, стать та Ідентифікаціний код, пофіксовані ПІБ з апостофами 
# MPs, Sex and ID for personal voting, NAMES FIXED
mps09 <- function() {
  
  mps09 <- fromJSON(readLines(file("http://w1.c1.rada.gov.ua/pls/radan_gs09/od_data_dep?type_data=j")))
  pers_vote <- mps09$mp
  
  # Fix names according to names of MPs in main mps_09 DF and factions_09 DF
  
  pers_vote$name[pers_vote$name == "Ар’єв Володимир Ігорович"] <- "Ар'єв Володимир Ігорович"
  pers_vote$name[pers_vote$name == "Безугла Мар’яна Володимирівна"] <- "Безугла Мар'яна Володимирівна"
  pers_vote$name[pers_vote$name ==  "Володіна Дар’я Артемівна"] <- "Володіна Дар'я Артемівна"
  pers_vote$name[pers_vote$name ==  "Джемілєв Мустафа  "] <- "Джемілєв Мустафа" 
  pers_vote$name[pers_vote$name ==  "Заблоцький Мар’ян Богданович"] <- "Заблоцький Мар'ян Богданович" 
  pers_vote$name[pers_vote$name ==  "Кожем’якін Андрій Анатолійович"] <- "Кожем'якін Андрій Анатолійович"   
  pers_vote$name[pers_vote$name ==  "М’ялик Віктор Ничипорович"] <- "М'ялик Віктор Ничипорович" 
  pers_vote$name[pers_vote$name ==  "Медяник В’ячеслав Анатолійович"] <- "Медяник В'ячеслав Анатолійович"
  pers_vote$name[pers_vote$name ==  "Салійчук Олександр В’ячеславович"] <- "Салійчук Олександр В'ячеславович"   
  pers_vote$name[pers_vote$name ==  "Циба Тетьяна Вікторівна"] <- "Циба Тетяна Вікторівна" 
  pers_vote$name[pers_vote$name ==  "Красносільська Анастасія Олегівна"] <- "Радіна Анастасія Олегівна" # Changed surname in March 2020
  
  return(pers_vote)
}
mps09 <- mps09()  

#### Крок 2.2. ####
mps_09 <- read.csv("https://data.rada.gov.ua/ogd/mps/skl9/mps09-data.csv", fileEncoding = "UTF-8")%>%
  select(rada_id, full_name, region_name, date_end)


#### Крок 2.3. ####
# The Factions IDs' to decrypt the roll-call voting

factions <- function() {
  
  factions <- fromJSON(readLines(file("http://w1.c1.rada.gov.ua/pls/radan_gs09/od_data_frac?type_data=j")))
  
  factions <- factions$faction%>%
    mutate(name = recode(name,
                         `Фракція політичної партії "СЛУГА НАРОДУ"` = "Слуга Народу",
                         `Фракція політичної партії "ГОЛОС"` = "ГОЛОС",
                         `Фракція політичної партії "Європейська солідарність"` = "ЄС",
                         `Фракція політичної партії "ОПОЗИЦІЙНА ПЛАТФОРМА-ЗА ЖИТТЯ"` = "ОПЗЖ",
                         `Фракція політичної партії Всеукраїнське об’єднання "Батьківщина"` = "Батьківщина",
                         `Депутатська група "За майбутнє"` = "За майбутнє",
                         `Депутатська група "ДОВІРА"`= "ДОВІРА"))%>%
    select(-type)
  
  return(factions)
}

factions <- factions()

#### Крок 2.4. ####
# The Factions from the Open Data Portral 

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
                             `Група "ДОВІРА"`= "ДОВІРА"))%>%
    mutate(rada_id=as.character(rada_id))
  return(factions_df)
}

factions_09 <- get_factions_open()

#### Крок 2.5. Аджена ####
# Agenda: id_question, name_question

agenda <- function(){
  
  agenda <- fromJSON(readLines(file("http://w1.c1.rada.gov.ua/pls/radan_gs09/od_data_pd?n_skl=9&type_data=j")))
  
  questions <- agenda$agenda$question
  
  quest <- reduce(questions, bind_rows) # More efficient way
  
  quest <- quest%>%
    select(-reporter_question)
  
  return(quest)
  
}

agenda <- agenda()

#### Крок 2.6. Події у ВРУ з таймінгом ####
event_question <- read.csv("https://data.rada.gov.ua/ogd/zal/ppz/skl9/plenary_event_question-skl9.csv", 
                           fileEncoding = "UTF-8")%>%
  select(-date_agenda, -id_question, -date_question)

#####  Крок 2.7. Результати поіменного голосування депутатів 9 скл ####
# https://data.rada.gov.ua/open/data/plenary_vote_results-skl9

personal_vote <- read.delim("https://data.rada.gov.ua/ogd/zal/ppz/skl9/plenary_vote_results-skl9.tsv")%>%
  mutate(id_question=as.character(id_question))%>%
  left_join(event_question, by=c("id_event"="id_event"))

#### Крок 2.8. Приєднуємо усі законодавчі активності ####   

zp_names <- personal_vote%>%
  left_join(agenda, by=c("id_question"="id_question"))%>%
  left_join(bills_main_skl9, by=c("number_question"="number"))%>%
  left_join(bills_executives, by=c("bill_id"="bill_id"))

#### Крок 2.9. reporter_question
# https://data.rada.gov.ua/open/data/plenary_reporter_question-skl9

reporter_question <- read.csv("https://data.rada.gov.ua/ogd/zal/ppz/skl9/plenary_reporter_question-skl9.csv", 
                              fileEncoding = "UTF-8")%>%
  separate(name_reporter, c("surname", "name", "parent"), sep = " ")%>%
  mutate(surname = str_to_title(tolower(surname)))%>%
  unite(fullname, c("surname", "name", "parent"), sep = " ")%>%
  left_join(factions_09, by=c("fullname"="fullname"))%>%
  select(-rada_id, -id)%>%
  left_join(zp_names, by=c("number_question"="number_question"))

#### Крок 3. Поіменка ####

# Крок 3.1 Personal voting into rows
# https://stackoverflow.com/questions/15097162/splitting-strings-into-multiple-rows-in-r

out <- cSplit(zp_names, "results", sep="|", "long")%>%
  separate(results, c("mps_id", "faction", "vote_status"), ":")%>%
  mutate(faction = recode(faction,
                          `0` = "Позафракційні",
                          `1` = "Слуга Народу",
                          `2` = "ОПЗЖ",
                          `3` = "Батьківщина",
                          `4` = "ЄС",
                          `5` = "ГОЛОС",
                          `6` = "За майбутнє",
                          `7`="ДОВІРА"))%>%
  mutate(vote_status = recode(vote_status,
                              `0` = "Відсутній",
                              `1` = "За",
                              `2` = "Проти",
                              `3` = "Утримався",
                              `4` = "Не голосував",
                              `5` = "Присутній"))%>%
  #left_join(mps_09, by=c("mps_id"="id_mp"))%>%
  left_join(factions_09, by=c("mps_id"="rada_id"))%>%
  mutate(id_question=as.character(id_question))%>%
  mutate(mps_id=as.integer(mps_id))


#### Крок 4. Групування і розшифровка  ####

# 4.1 По фракціям у відсотках

out_factions_perc <- out%>%
  left_join(mps_09, by=c("name"="full_name"))%>%  # ДФ з колонкою для філтрування date_end
  filter(date_end=="")%>%                         # Прибираємо вибулих депутатів
  group_by(factions)%>%
  summarise(   
    # У відсотках
    vote_for_perc = round(mean(vote_status == "За")*100, 1), 
    vote_abstain_perc = round(mean(vote_status == "Утримався")*100, 1),
    vote_against_perc = round(mean(vote_status == "Проти")*100, 1), 
    vote_present_perc = round(mean(vote_status == "Присутній")*100, 1),
    vote_not_voting_perc = round(mean(vote_status == "Не голосував")*100, 1),
    vote_absent_perc = round(mean(vote_status == "Відсутній")*100, 1)) %>% 
  mutate(factions=as.factor(factions))%>%
  ungroup()%>%
  mutate(uchast=vote_for_perc+vote_abstain_perc+vote_against_perc)%>%
  arrange(uchast, factions)

# 4.1.a) По фракціям у відсотках, long

out_factions_perc_long <- out_factions_perc%>%
  mutate(factions = fct_reorder(factions, levels(out_factions_perc$factions))) %>% 
  gather(status, n_vote, vote_for_perc:vote_absent_perc, factor_key = TRUE)%>%
  filter(!status=="vote_present_perc") # Фільтрує невідомий критерій з нулями.

# 4.2 По нардепам у кількості

out_mps_n <- out %>%
  #mutate(mps_id=as.integer(mps_id))%>%
  group_by(fullname, mps_id, factions)%>%
  summarise(vote_for = sum(vote_status == "За"), 
            vote_abstain = sum(vote_status == "Утримався"),
            vote_against_ = sum(vote_status == "Проти"), 
            vote_present = sum(vote_status == "Присутній"),
            vote_not_voting = sum(vote_status == "Не голосував"),
            vote_absent = sum(vote_status == "Відсутній")) %>% 
  arrange(vote_for, fullname)%>%
  left_join(mps_09, by=c("fullname"="full_name"))%>%
  #left_join(factions_09, by=c("mps_id"="rada_id"))%>%
  filter(date_end=="")%>%
  select( -rada_id)

# 4.3 По нардепам у  відсотках

out_mps_perc <- out %>%
  group_by(fullname, mps_id, factions)%>%
  summarise(
    vote_for_perc = round(mean(vote_status == "За")*100, 0), 
    vote_abstain_perc = round(mean(vote_status == "Утримався")*100, 0),
    vote_against_perc = round(mean(vote_status == "Проти")*100, 0), 
    vote_present_perc = round(mean(vote_status == "Присутній")*100, 0),
    vote_not_voting_perc = round(mean(vote_status == "Не голосував")*100, 0),
    vote_absent_perc = round(mean(vote_status == "Відсутній")*100, 0))%>%
  arrange(vote_for_perc, fullname)%>%
  left_join(mps_09, by=c("fullname"="full_name"))%>%
  #left_join(factions_09, by=c("mps_id"="rada_id"))%>%
  filter(date_end=="")%>%
  select( -rada_id)

# # 4.3.a) По нардепам у  відсотках, long

out_mps_perc_long <- out_mps_perc%>%
  gather(vote_status, n_vote, vote_for_perc:vote_absent_perc)%>%
  filter(!vote_status=="vote_present_perc")


#### Крок 5. Writa data ####

dir.create("output_cumulative")

write.xlsx(as.data.frame(personal_vote), 
           file=paste0("output_cumulative/personal_vote", ".xlsx"),
           sheetName="personal_vote", row.names=FALSE, append = FALSE)

write.xlsx(as.data.frame(zp_names), 
           file=paste0("output_cumulative/personal_voting", ".xlsx"),
           sheetName="Голосування з назвами законопроектів наростаюче", row.names=FALSE, append = TRUE)

write.xlsx(as.data.frame(out_mps_n), 
           file=paste0("output_cumulative/personal_voting", ".xlsx"),
           sheetName="Нардепи наростаюче за увесь час", row.names=FALSE, append = TRUE)

write.xlsx(as.data.frame(out_mps_perc), 
           file=paste0("output_cumulative/personal_voting", ".xlsx"),
           sheetName="За нардепами наростаюче голосування у %", row.names=FALSE, append = TRUE)

write.xlsx(as.data.frame(out_factions_perc), 
           file=paste0("output_cumulative/personal_voting", ".xlsx"),
           sheetName="За фракціями наростаюче голосування у %", row.names=FALSE, append = TRUE)
