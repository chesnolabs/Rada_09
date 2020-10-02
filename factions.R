library(dplyr)
library(tidyverse)

get_factions_open <- function(){
  posts <- read_csv("https://data.rada.gov.ua/ogd/mps/skl9/mp-posts_ids.csv")
  posts_ids <- read_tsv("https://data.rada.gov.ua/ogd/mps/skl9/mp-posts_unit.txt", 
                        locale(encoding = "windows-1251"), col_names = F, col_types = NULL) %>% 
    dplyr::rename(unit_id = X1, unit = X2)
  mps <- read_csv("https://data.rada.gov.ua/ogd/mps/skl9/mps09-data.csv")
  
  factions_full <- posts %>% 
    left_join(mps[, c("rada_id" ,"id", "full_name", "date_end", "region_name", "gender")], by = c("mp_id" = "id")) %>% 
    left_join(posts_ids) %>% 
    filter(unit_type %in% c("grp", "fra")) %>% 
    select(mp_id, full_name, unit)
  
  factions_df <-  mps %>% 
    #filter(is.na(resignation_text)) %>% 
    select(rada_id, id, full_name, region_name, date_end, gender) %>% 
    left_join(factions_full, by = c("id" = "mp_id", "full_name")) %>% 
    mutate(unit = ifelse(is.na(unit), "Позафракційні", unit)) %>% 
    dplyr::rename(factions = unit, fullname = full_name) %>% 
    mutate(factions = recode(factions, 
                             `Фракція ПОЛІТИЧНОЇ ПАРТІЇ "СЛУГА НАРОДУ"` = "Слуга Народу",
                             `Фракція Політичної Партії "ГОЛОС" у Верховній Раді України дев'ятого скликання` = "ГОЛОС",
                             `Фракція ПОЛІТИЧНОЇ ПАРТІЇ "ЄВРОПЕЙСЬКА СОЛІДАРНІСТЬ"` = "ЄС",
                             `Фракція Політичної партії "ОПОЗИЦІЙНА ПЛАТФОРМА - ЗА ЖИТТЯ" у Верховній Раді України` = "ОПЗЖ",
                             `Фракція політичної партії Всеукраїнське об'єднання "Батьківщина" у Верховній Раді України дев'ятого скликання` = "Батьківщина",
                             `Група "Партія "За майбутнє"` = "За майбутнє",
                             `Група "ДОВІРА"`= "ДОВІРА"))%>%
    mutate(rada_id=as.character(rada_id)) %>% 
    mutate(rada_id=recode(rada_id, 
                          "208"="438"))%>% # Можливо тимчасово, бо не встигли змінити айдішник Радіної
    mutate(date_end = ifelse(is.na(date_end), "", date_end))%>% # Replace NA with a blank
    mutate(region_name = ifelse(is.na(region_name), "", region_name)) # Replace NA with a blank
  
  return(factions_df)
}

factions_09 <- get_factions_open()

