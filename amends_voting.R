# Персональне голосування нардепів лише за поправки
# Перед тим, як працювати з кодом далі, треба прогнати код з файлу **roll_call_voting.R**

#### Крок 1. Усі події ВРУ  ####
event_question <- read.csv("https://data.rada.gov.ua/ogd/zal/ppz/skl9/plenary_event_question-skl9.csv", 
                           fileEncoding = "UTF-8")

#### Крок 2. Лише поправки  ####

amendments_voting <- event_question%>%
  filter(grepl("Поіменне голосування про поправку", name_event))%>%
  left_join(personal_vote, by=c("id_event"="id_event"))%>%
  select(-date_agenda.y,id_question.y, -total, -presence)%>%
  mutate(id_question.x=as.character(id_question.x))%>%
  left_join(agenda, by=c("id_question.x"="id_question"))%>%
  # Download bills in bills_09.R
  left_join(bills_main_skl9, by=c("number_question"="number"))%>%
  left_join(bills_executives, by=c("bill_id"="bill_id"))


#### Крок 3. Розшифровка голосування за поправки ####

out_amends <- cSplit(amendments_voting, "results", sep="|", "long")%>%
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
  left_join(mps09, by=c("mps_id"="id_mp"))%>%
  left_join(factions_09, by=c("mps_id"="rada_id"))%>%
  mutate(id_question.x=as.character(id_question.x))


####Крок 4. По фракціям у % ####

out_amends_factions_perc <- out_amends%>%
  left_join(mps_09, by=c("name"="full_name"))%>%  # ДФ з колонкою для філтрування date_end
  filter(date_end=="")%>%                         # Прибираємо не депутатів
  #filter(is.na(faction))%>%
  #left_join(factions_09, by=c("mps_id"="rada_id"))%>%
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
  arrange(vote_for_perc, factions)

####Крок 4.1. По фракціям, %, Long ####
# Перевести в довгий формат дані по фракціям у відсотках

out_amends_factions_perc_long <- out_amends_factions_perc%>%
  mutate(factions = fct_reorder(factions, levels(out_amends_factions_perc$factions))) %>% 
  gather(status, n_vote, vote_for_perc:vote_absent_perc, factor_key = TRUE)%>%
  filter(!status=="vote_present_perc") # Фільтрує невідомий критерій з нулями.

####Крок 5. По фракціям і головних комітетах, % ####
 
amends_faction_depart_perc <- out_amends%>%
  left_join(mps_09, by=c("name"="full_name"))%>%  # ДФ з колонкою для філтрування date_end
  filter(date_end=="")%>%                         # Прибираємо не депутатів
  group_by(factions, department)%>%
  summarise(
    # У відсотках
    vote_for_perc = round(mean(vote_status == "За")*100, 1), 
    vote_abstain_perc = round(mean(vote_status == "Утримався")*100, 1),
    vote_against_perc = round(mean(vote_status == "Проти")*100, 1), 
    vote_present_perc = round(mean(vote_status == "Присутній")*100, 1),
    vote_not_voting_perc = round(mean(vote_status == "Не голосував")*100, 1),
    vote_absent_perc = round(mean(vote_status == "Відсутній")*100, 1)) %>% 
  arrange(vote_for_perc, factions)

####Крок 5.1. По фракціям і головних комітетах, %, Long ####
amends_faction_depart_perc_long <- amends_faction_depart_perc%>%
  gather(status, n_vote, vote_for_perc:vote_absent_perc, factor_key = TRUE)%>%
  filter(!status=="vote_present_perc") # Фільтрує невідомий критерій з нулями.

#### Крок 6. По нардепах, абс.числа ####

out_amends_mps_n <- out_amends %>%
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
  filter(date_end=="")%>%
  select( -rada_id, -mps_id)


#### Крок 7. По нардепах, % ####

out_amends_mps_perc <- out_amends %>%
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
  filter(date_end=="")%>%
  select( -rada_id)

#### Крок 8. Запис у файл ####

dir.create("output_cumulative")

write.xlsx(as.data.frame(amendments_voting), 
           file=paste0("output_cumulative/amends_voting", ".xlsx"),
           sheetName="Голосування за поправки", row.names=FALSE, append = FALSE)

write.xlsx(as.data.frame(out_amends_factions_perc), 
           file=paste0("output_cumulative/amends_voting", ".xlsx"),
           sheetName="Фракції за поправки у відсотках", row.names=FALSE, append = TRUE)

write.xlsx(as.data.frame(amends_faction_depart_perc), 
           file=paste0("output_cumulative/amends_voting", ".xlsx"),
           sheetName="Фракції за поправки+комітет у %", row.names=FALSE, append = TRUE)

write.xlsx(as.data.frame(out_amends_mps_n), 
           file=paste0("output_cumulative/amends_voting", ".xlsx"),
           sheetName="Нардепи за поправки у числах", row.names=FALSE, append = TRUE)

write.xlsx(as.data.frame(out_amends_mps_perc), 
           file=paste0("output_cumulative/amends_voting", ".xlsx"),
           sheetName="Нардепи за поправки у відсотках", row.names=FALSE, append = TRUE)
