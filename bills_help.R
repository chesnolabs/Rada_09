# Для роботи з цим файлом необхідно завантажити zakon_act.R


# Необхідна допомога інших фракцій та груп ####

minus_226 <- function(){

minus_226 <- cSplit(zakon_act_2nd_read, "results", sep="|", "long")%>%
  separate(results, c("mps_id", "faction", "vote_status"), ":")%>%
  mutate(faction = recode(faction,
                          `0` = "Позафракційні",
                          `1` = "Слуга Народу",
                          `2` = "ОПЗЖ",
                          `3` = "Батьківщина",
                          `4` = "ЄС",
                          `5` = "ГОЛОС",
                          `6` = "За майбутнє",
                          `7` = "ДОВІРА",
                          `8` = "За майбутнє"))%>%
  mutate(vote_status = recode(vote_status,
                              `0` = "Відсутній",
                              `1` = "За",
                              `2` = "Проти",
                              `3` = "Утримався",
                              `4` = "Не голосував",
                              `5` = "Присутній"))

minus_226$mps_id[minus_226$mps_id ==  "208"] <- "438" # Changed surname in March 2020


minus_226 <- minus_226%>%
  left_join(factions_09, by=c("mps_id"="rada_id"))%>%
  #filter(date_end=="")%>%
  mutate(id_question=as.character(id_question))%>%
  mutate(mps_id=as.integer(mps_id))


}

minus_226 <- minus_226()

# minus_226 ####
# Кількість голосів, які дала кожна фракція чи група за той чи інший законопроект

faction_votes <- minus_226 %>%
  group_by(faction, id_event,name_event, number_question, init_question, date_agenda, for., department)%>%
  summarise(vote_for = sum(vote_status == "За"), 
            vote_abstain = sum(vote_status == "Утримався"),
            vote_against_ = sum(vote_status == "Проти"), 
            vote_present = sum(vote_status == "Присутній"),
            vote_not_voting = sum(vote_status == "Не голосував"),
            vote_absent = sum(vote_status == "Відсутній")) 

faction_votes$date_agenda <- as.Date(faction_votes$date_agenda)

# Split df into several according to the factions' names

split_FV <- split(faction_votes, with(faction_votes, interaction(faction)), drop = TRUE)

# Save split_FV into different dataframes and spreadsheets

# create style

hs <- createStyle(textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize=12,
                  fontName="Arial", fgFill = "#4F80BD")
# create workbook
wb <- createWorkbook()

# Save a list into different spreadsheets in one Excel file   
Map(function(data, nameofsheet){     
  
  addWorksheet(wb, nameofsheet)
  
  writeData(wb, nameofsheet, data, headerStyle=hs,  borders = "surrounding")
  
}, split_FV, names(split_FV))

# Save a workbook to an excel file 
now_date <- "17_12_2020"

saveWorkbook(wb, file=paste0("output_cumulative/data/недостача_голосів", 
                             now_date, ".xlsx"), overwrite = TRUE)

