# Продовження роботи, для початку прогнати файл з персональним голосуванням roll_call_voting.R

# Продовження роботи, для початку прогнати файл з персональним голосуванням roll_call_voting.R

# У датафреймі з усіма законоавдчими активностями відфільтровуємо голосування і залишаємо 
#### Проект, Проект Закону, Пропозиції Президента, Проект Постанови ####
zp <- zp_names%>%
  #left_join(bills_main_skl9, by=c("number_question"="number"))%>%
  filter(!init_question=="0")    # Проект, Проект Закону, Пропозиції Президента, Проект Постанови             


#### Розшифровуємо ####

zp_out <- cSplit(zp, "results", sep="|", "long")%>%
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
  left_join(factions_09, by=c("mps_id"="rada_id"))%>%
  filter(date_end=="")%>%
  mutate(id_question=as.character(id_question))%>%
  mutate(mps_id=as.integer(mps_id))

#### По фракціям у %  ####

zp_out_factions_perc <- zp_out%>%
  #left_join(mps_09, by=c("name"="full_name"))%>%  # ДФ з колонкою для філтрування date_end
  #filter(date_end=="")%>%    
  group_by(factions)%>%
  summarise(
    
    # У відсотках
    vote_for = round(mean(vote_status == "За")*100, 1), 
    vote_abstain = round(mean(vote_status == "Утримався")*100, 1),
    vote_against = round(mean(vote_status == "Проти")*100, 1), 
   
    vote_present = round(mean(vote_status == "Присутній")*100, 1),
    vote_not_voting = round(mean(vote_status == "Не голосував")*100, 1),
    vote_absent = round(mean(vote_status == "Відсутній")*100, 1)) %>% 
  ungroup()%>%
  mutate(uchast=vote_for+vote_abstain+vote_against)%>%
  mutate(factions=as.factor(factions))%>%
  arrange(vote_for, factions)

#### По фракціям, %, Long####
zp_out_factions_perc_long <- zp_out_factions_perc%>%
  gather(status, n_vote, vote_for:vote_absent, factor_key = TRUE)

#### По нардепам, абсол.числа ####
zp_out_mps_n <- zp_out %>%
  group_by(fullname, mps_id, factions)%>%
  summarise(vote_for = sum(vote_status == "За"), 
            vote_against_ = sum(vote_status == "Проти"), 
            vote_abstain = sum(vote_status == "Утримався"),
            vote_present = sum(vote_status == "Присутній"),
            vote_not_voting = sum(vote_status == "Не голосував"),
            vote_absent = sum(vote_status == "Відсутній")) %>% 
  arrange(vote_for, fullname)

#### По нардепам, % ####

zp_out_mps_perc <- zp_out %>%
  group_by(fullname, mps_id, factions)%>%
  summarise(
    vote_for_perc = round(mean(vote_status == "За")*100, 1), 
    vote_abstain_perc = round(mean(vote_status == "Утримався")*100, 1),
    vote_against_perc = round(mean(vote_status == "Проти")*100, 1), 
    vote_present_perc = round(mean(vote_status == "Присутній")*100, 1),
    vote_not_voting_perc = round(mean(vote_status == "Не голосував")*100, 1),
    vote_absent_perc = round(mean(vote_status == "Відсутній")*100, 1)
    ) %>% 
  arrange(vote_for_perc, fullname)

#### По нардепам, %, long ####

# Long table from wide
zp_out_mps_perc_long <- zp_out_mps_perc%>%
  gather(status, n_vote, vote_for_perc:vote_absent_perc, factor_key = TRUE)

### II. Проекти Закону ####

zakon <- zp%>%
  filter(type.x=="Проект Закону")

# Як голосуютю по рубрикам ####

zakon_rubrics <- zakon%>%
  group_by(rubric)%>%
  summarise(n=n())%>%
  ungroup()

# Як голосують по головним комітетам ####

zakon_depart<- zakon%>%
  group_by(department)%>%
  summarise(n=n())%>%
  ungroup()

# Як голосують по головним комітетам і рубрикам ####

zakon_dual<- zakon%>%
  group_by(department, rubric)%>%
  summarise(n=n())%>%
  ungroup()

# Голосування лише за "проекти законів" ####
zakon_out <- cSplit(zakon, "results", sep="|", "long")%>%
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
  left_join(factions_09, by=c("mps_id"="rada_id"))%>%
  filter(date_end=="")%>%
  mutate(id_question=as.character(id_question))%>%
  mutate(mps_id=as.integer(mps_id))

# ЗП по нардепам, абсол.числа  ####

z_mps_n <- zakon_out %>%
  group_by(fullname, mps_id, factions)%>%
  summarise(vote_for = sum(vote_status == "За"), 
           vote_abstain = sum(vote_status == "Утримався"),
            vote_against_ = sum(vote_status == "Проти"), 
            vote_present = sum(vote_status == "Присутній"),
            vote_not_voting = sum(vote_status == "Не голосував"),
            vote_absent = sum(vote_status == "Відсутній")) %>% 
  arrange(vote_for, fullname)

# ЗП по нардепах, % #### 

z_mps_perc <- zakon_out %>%
  group_by(fullname, mps_id, factions)%>%
  summarise(
    # У відсотках
    vote_for = round(mean(vote_status == "За")*100, 0), 
    vote_abstain = round(mean(vote_status == "Утримався")*100, 0),
    vote_against = round(mean(vote_status == "Проти")*100, 0), 
    vote_present = round(mean(vote_status == "Присутній")*100, 0),
    vote_not_voting = round(mean(vote_status == "Не голосував")*100, 0),
    vote_absent = round(mean(vote_status == "Відсутній")*100, 0)) %>% 
  arrange(vote_for, fullname)

# Те саме, але з розбивкою по рубрикам, у відсотках

z_mps_perc_rubric <- zakon_out %>%
  group_by(fullname, factions, rubric)%>%
  summarise(
    # У відсотках
    vote_for = round(mean(vote_status == "За")*100, 0), 
    vote_abstain = round(mean(vote_status == "Утримався")*100, 0),
    vote_against = round(mean(vote_status == "Проти")*100, 0), 
    vote_present = round(mean(vote_status == "Присутній")*100, 0),
    vote_not_voting = round(mean(vote_status == "Не голосував")*100, 0),
    vote_absent = round(mean(vote_status == "Відсутній")*100, 0)) %>% 
  arrange(vote_for, fullname)

# ЗП по нардепах, %, Long ####

z_mps_perc_rubric_long <- z_mps_perc_rubric%>%
  gather(status, n_vote, vote_for:vote_absent, factor_key = TRUE)%>%
  filter(!status=="vote_present")

# ЗП по фракціях і головним комітетам, % ####
z_faction_rubric_perc <- zakon_out%>%
  group_by(factions, rubric)%>%
  summarise(
    # У відсотках
    vote_for = round(mean(vote_status == "За")*100, 1), 
    vote_abstain = round(mean(vote_status == "Утримався")*100, 1),
    vote_against = round(mean(vote_status == "Проти")*100, 1),
    vote_present = round(mean(vote_status == "Присутній")*100, 1),
    vote_not_voting = round(mean(vote_status == "Не голосував")*100, 1),
    vote_absent = round(mean(vote_status == "Відсутній")*100, 1)) %>% 
  arrange(vote_for, factions)

# ЗП по фракціях і головним комітетам, %, long ####
z_faction_rubric_perc_long <- z_faction_rubric_perc%>%  
  gather(status, n_vote, vote_for:vote_absent, factor_key = TRUE)%>%
  filter(!status=="vote_present")

# ЗП По фракціям, % ####

z_factions_perc <- zakon_out%>%
  group_by(factions)%>%
  summarise(
    # У відсотках
    vote_for = round(mean(vote_status == "За")*100, 1), 
    vote_abstain = round(mean(vote_status == "Утримався")*100, 1),
    vote_against = round(mean(vote_status == "Проти")*100, 1), 
    vote_present = round(mean(vote_status == "Присутній")*100, 1),
    vote_not_voting = round(mean(vote_status == "Не голосував")*100, 1),
    vote_absent = round(mean(vote_status == "Відсутній")*100, 1)) %>% 
  mutate(factions=as.factor(factions))%>%
  arrange(vote_for, factions)

# ЗП По фракціям, %, long ####

z_factions_perc_long <- z_factions_perc%>%
  mutate(factions = fct_reorder(factions, levels(z_factions_perc$factions))) %>% 
  gather(status, n_vote, vote_for:vote_absent, factor_key = TRUE)%>%
  filter(!status=="vote_present")

# ЗП по фракціям і Комітетами, % ####

z_faction_depart_perc <- zakon_out%>%
  group_by(factions, department)%>%
  summarise(
    # У відсотках
    vote_for = round(mean(vote_status == "За")*100, 1), 
    vote_abstain = round(mean(vote_status == "Утримався")*100, 1),
    vote_against = round(mean(vote_status == "Проти")*100, 1), 
    vote_present = round(mean(vote_status == "Присутній")*100, 1),
    vote_not_voting = round(mean(vote_status == "Не голосував")*100, 1),
    vote_absent = round(mean(vote_status == "Відсутній")*100, 1)) %>% 
  arrange(vote_for, factions)

# ЗП по фракціям і Комітетами, %, long ####

z_faction_depart_perc_long <- z_faction_depart_perc%>%
  gather(status, n_vote, vote_for:vote_absent, factor_key = TRUE)%>%
  filter(!status=="vote_present")

# ЗП по ПІБ, фракції і Комітетам, % ####

z_mps_depart_perc <- zakon_out%>%
  group_by(fullname, factions, department)%>%
  summarise(
    # У відсотках
    vote_for = round(mean(vote_status == "За")*100, 1), 
    vote_abstain = round(mean(vote_status == "Утримався")*100, 1),
    vote_against = round(mean(vote_status == "Проти")*100, 1), 
    vote_present = round(mean(vote_status == "Присутній")*100, 1),
    vote_not_voting = round(mean(vote_status == "Не голосував")*100, 1),
    vote_absent = round(mean(vote_status == "Відсутній")*100, 1)) %>% 
  arrange(vote_for, factions)

# ЗП по ПІБ, фракції і Комітетам, абсолютне число ####

z_mps_depart_n <- zakon_out%>%
  group_by(fullname, factions, department)%>%
  summarise(
    # У абсолютних числах
    vote_for = sum(vote_status == "За"), 
    vote_abstain = sum(vote_status == "Утримався"),
    vote_against_ = sum(vote_status == "Проти"), 
    vote_present = sum(vote_status == "Присутній"),
    vote_not_voting = sum(vote_status == "Не голосував"),
    vote_absent = sum(vote_status == "Відсутній"))%>%
  arrange(vote_for, factions)


# III. ЗП без поправок ####
# ZP without amendments = zp_wo_amends

zp_wo_amends <- zp%>%
  filter(!grepl("Поіменне голосування про поправку", name_event))

# Як голосують по рубрикам ####

zp_wo_rubrics <- zp_wo_amends%>%
  group_by(rubric)%>%
  summarise(n=n())%>%
  ungroup()

# Як голосують по головним комітетам ####

zp_wo_depart<- zp_wo_amends%>%
  group_by(department)%>%
  summarise(n=n())%>%
  ungroup()

# Як голосують по головним комітетам і рубрикам ####

zp_wo_dual<- zp_wo_amends%>%
  group_by(department, rubric)%>%
  summarise(n=n())%>%
  ungroup()

# Голосування лише за "проекти законів" ####
zp_wo_out <- cSplit(zp_wo_amends, "results", sep="|", "long")%>%
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
  left_join(factions_09, by=c("mps_id"="rada_id"))%>%
  filter(date_end=="")%>%
  mutate(id_question=as.character(id_question))%>%
  mutate(mps_id=as.integer(mps_id))

# ЗП по нардепам, абсол.числа  ####

zp_wo_mps_n <- zp_wo_out %>%
  group_by(fullname, factions)%>%
  summarise(vote_for = sum(vote_status == "За"), 
            vote_abstain = sum(vote_status == "Утримався"),
            vote_against_ = sum(vote_status == "Проти"), 
            vote_present = sum(vote_status == "Присутній"),
            vote_not_voting = sum(vote_status == "Не голосував"),
            vote_absent = sum(vote_status == "Відсутній")) %>% 
  arrange(vote_for, fullname)

# ЗП по нардепах, % #### 

zp_wo_mps_perc <- zp_wo_out %>%
  group_by(fullname, factions)%>%
  summarise(
    # У відсотках
    vote_for = round(mean(vote_status == "За")*100, 0), 
    vote_abstain = round(mean(vote_status == "Утримався")*100, 0),
    vote_against = round(mean(vote_status == "Проти")*100, 0), 
    vote_present = round(mean(vote_status == "Присутній")*100, 0),
    vote_not_voting = round(mean(vote_status == "Не голосував")*100, 0),
    vote_absent = round(mean(vote_status == "Відсутній")*100, 0)) %>% 
  arrange(vote_for, fullname)

# Те саме, але з розбивкою по рубрикам, у відсотках

zp_wo_mps_perc_rubric <- zp_wo_out %>%
  group_by(fullname, factions, rubric)%>%
  summarise(
    # У відсотках
    vote_for = round(mean(vote_status == "За")*100, 0), 
    vote_abstain = round(mean(vote_status == "Утримався")*100, 0),
    vote_against = round(mean(vote_status == "Проти")*100, 0), 
    vote_present = round(mean(vote_status == "Присутній")*100, 0),
    vote_not_voting = round(mean(vote_status == "Не голосував")*100, 0),
    vote_absent = round(mean(vote_status == "Відсутній")*100, 0)) %>% 
  arrange(vote_for, fullname)

# ЗП по нардепах, %, Long ####

zp_wo_mps_perc_rubric_long <- zp_wo_mps_perc_rubric%>%
  gather(status, n_vote, vote_for:vote_absent, factor_key = TRUE)%>%
  filter(!status=="vote_present")

# ЗП по фракціях і головним комітетам, % ####
zp_wo_faction_rubric_perc <- zp_wo_out%>%
  group_by(factions, rubric)%>%
  summarise(
    # У відсотках
    vote_for = round(mean(vote_status == "За")*100, 1), 
    vote_abstain = round(mean(vote_status == "Утримався")*100, 1),
    vote_against = round(mean(vote_status == "Проти")*100, 1),
    vote_present = round(mean(vote_status == "Присутній")*100, 1),
    vote_not_voting = round(mean(vote_status == "Не голосував")*100, 1),
    vote_absent = round(mean(vote_status == "Відсутній")*100, 1)) %>% 
  arrange(vote_for, factions)

# ЗП по фракціях і головним комітетам, %, long ####
zp_wo_faction_rubric_perc_long <- zp_wo_faction_rubric_perc%>%  
  gather(status, n_vote, vote_for:vote_absent, factor_key = TRUE)%>%
  filter(!status=="vote_present")

# ЗП По фракціям, % ####

zp_wo_factions_perc <- zp_wo_out%>%
  group_by(factions)%>%
  summarise(
    # У відсотках
    vote_for = round(mean(vote_status == "За")*100, 1), 
    vote_abstain = round(mean(vote_status == "Утримався")*100, 1),
    vote_against = round(mean(vote_status == "Проти")*100, 1), 
    vote_present = round(mean(vote_status == "Присутній")*100, 1),
    vote_not_voting = round(mean(vote_status == "Не голосував")*100, 1),
    vote_absent = round(mean(vote_status == "Відсутній")*100, 1)) %>% 
  mutate(factions=as.factor(factions))%>%
  arrange(vote_for, factions)

# ЗП По фракціям, %, long ####

zp_wo_factions_perc_long <- zp_wo_factions_perc%>%
  mutate(factions = fct_reorder(factions, levels(zp_wo_factions_perc$factions))) %>% 
  gather(status, n_vote, vote_for:vote_absent, factor_key = TRUE)%>%
  filter(!status=="vote_present")

# ЗП по фракціям і Комітетами, % ####

zp_wo_faction_depart_perc <- zp_wo_out%>%
  group_by(factions, department)%>%
  summarise(
    # У відсотках
    vote_for = round(mean(vote_status == "За")*100, 1), 
    vote_abstain = round(mean(vote_status == "Утримався")*100, 1),
    vote_against = round(mean(vote_status == "Проти")*100, 1), 
    vote_present = round(mean(vote_status == "Присутній")*100, 1),
    vote_not_voting = round(mean(vote_status == "Не голосував")*100, 1),
    vote_absent = round(mean(vote_status == "Відсутній")*100, 1)) %>% 
  arrange(vote_for, factions)

# ЗП по фракціям і Комітетами, %, long ####

zp_wo_faction_depart_perc_long <- zp_wo_faction_depart_perc%>%
  gather(status, n_vote, vote_for:vote_absent, factor_key = TRUE)%>%
  filter(!status=="vote_present")

# ЗП по ПІБ, фракції і Комітетам, % ####

zp_wo_mps_depart_perc <- zp_wo_out%>%
  group_by(fullname, factions, department)%>%
  summarise(
    # У відсотках
    vote_for = round(mean(vote_status == "За")*100, 1), 
    vote_abstain = round(mean(vote_status == "Утримався")*100, 1),
    vote_against = round(mean(vote_status == "Проти")*100, 1), 
    vote_present = round(mean(vote_status == "Присутній")*100, 1),
    vote_not_voting = round(mean(vote_status == "Не голосував")*100, 1),
    vote_absent = round(mean(vote_status == "Відсутній")*100, 1)) %>% 
  arrange(vote_for, factions)

# ЗП по ПІБ, фракції і Комітетам, абсолютне число ####

zp_wo_mps_depart_n <- zp_wo_out%>%
  group_by(fullname, factions, department)%>%
  summarise(
    # У абсолютних числах
    vote_for = sum(vote_status == "За"), 
    vote_abstain = sum(vote_status == "Утримався"),
    vote_against_ = sum(vote_status == "Проти"), 
    vote_present = sum(vote_status == "Присутній"),
    vote_not_voting = sum(vote_status == "Не голосував"),
    vote_absent = sum(vote_status == "Відсутній"))%>%
  arrange(vote_for, factions)


