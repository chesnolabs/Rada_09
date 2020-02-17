library(scales)

# Початок роботи у файлі bills_roll_call.R 
# https://github.com/chesnolabs/Rada_09/blob/master/bills_roll_call.R 

# Підписані Закони ####

zakon_act <- zakon%>%
  filter(currentPhase_title=="Закон підписано")

# Як голосують по головним комітетам ####
zakon_act_depart <- zakon_act%>%
  group_by(department)%>%
  summarise(n=n())

# Як голосують по головним комітетам і рубрикам ####
zakon_act_dual <- zakon_act%>%
  group_by(rubric, department)%>%
  summarise(n=n())

# Як голосуютю по рубрикам ####
zakon_act_rubric <- zakon_act%>%
  group_by(rubric)%>%
  summarise(n=n())

# Поіменка за підписані закони ####

zakon_act_out <- cSplit(zakon_act, "results", sep="|", "long")%>%
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
  mutate(id_question=as.character(id_question))

# По фракціям, % ####

z_act_out_faction <- zakon_act_out%>%
  left_join(mps_09, by=c("name"="full_name"))%>%  # ДФ з колонкою для філтрування date_end
  filter(date_end=="")%>%                         # Прибираємо не депутатів
  group_by(factions)%>%
  summarise(
    # У відсотках
    vote_for_perc = round(mean(vote_status == "За")*100, 1), 
    vote_abstain_perc = round(mean(vote_status == "Утримався")*100, 1),
    vote_against_perc = round(mean(vote_status == "Проти")*100, 1), 
    vote_present_perc = round(mean(vote_status == "Присутній")*100, 1),
    vote_not_voting_perc = round(mean(vote_status == "Не голосував")*100, 1),
    vote_absent_perc = round(mean(vote_status == "Відсутній")*100, 1)) %>% 
  arrange(vote_for_perc, factions)

# По фракціям, %, long ####

z_act_out_faction_long <- z_act_out_faction%>%
  mutate(factions = fct_reorder(factions, levels(z_act_out_faction$factions))) %>% 
  gather(status, n_vote, vote_for_perc:vote_absent_perc, factor_key = TRUE)%>%
  filter(!status=="vote_present_perc") # Фільтрує невідомий критерій з нулями.
  
# По фракціям і Комітетам, % ####

z_act_out_faction_depart <- zakon_act_out%>%
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

# По фракціям і Комітетами, %, long ####

z_act_out_faction_depart_long <- z_act_out_faction_depart%>%
  gather(status, n_vote, vote_for_perc:vote_absent_perc, factor_key = TRUE)%>%
  filter(!status=="vote_present_perc") # Фільтрує невідомий критерій з нулями.

# По ПІБ, фракціям, гол.комітетам % #### 

z_act_out_mps_depart_perc <- zakon_act_out %>%
  group_by(fullname, factions, department)%>%
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

# По ПІБ і фракціям, %, ####

z_act_out_mps_perc <- zakon_act_out %>%
  group_by(fullname, factions)%>%
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

# Скорочення назв гол.комітетів ####

fixed <- z_act_out_faction_depart_long %>%  # 
  mutate(department = recode(department, 
                             "Комітет Верховної Ради України з питань прав людини, деокупації та реінтеграції тимчасово окупованих територій у Донецькій, Луганській областях та Автономної Республіки Крим, міста Севастополя, національних меншин і міжнаціональних відносин" = "З прав людини, деокупації",
                             "Комітет з питань організації державної влади, місцевого самоврядування, регіонального розвитку та містобудування" = "Держвлади і самоврядування",
                             "Комітет з питань Регламенту, депутатської етики та організації роботи Верховної Ради України" = "Регламентний комітет",
                             "Комітет з питань соціальної політики та захисту прав ветеранів" = "Соцполітика і захист ветеранів",
                             "Комітет з питань інтеграції України з Європейським Союзом" = "Інтеграція з ЄС",
                             "Комітет з питань зовнішньої політики та міжпарламентського співробітництва"="Зовнішня політика",
                             "Комітет з питань енергетики та житлово-комунальних послуг"="Енергетика і ЖКГ",
                             "Комітет з питань здоров'я нації, медичної допомоги та медичного страхування"="Медичний комітет",
                             "Комітет з питань національної безпеки, оборони та розвідки"="Нацбезпеки і оборони",
                             "Комітет з питань екологічної політики та природокористування"="Екологія і природа",
                             "Комітет з питань антикорупційної політики"="Антикорупційний комітет",
                             "Комітет з питань гуманітарної та інформаційної політики"="Гуманітарна і інформполітика",
                             "Комітет з питань транспорту та інфраструктури"="Транспорт і інфраструктура",
                             "Комітет з питань освіти, науки та інновацій"="Освіта і наука",
                             "Комітет з питань аграрної та земельної політики"="Аграрний комітет",
                             "Комітет з питань фінансів, податкової та митної політики"="Фінанси і податки",
                             "Комітет з питань правоохоронної діяльності"="Правоохоронний",
                             "Комітет з питань економічного розвитку"="Економрозвиток",
                             "Комітет з питань бюджету"="Бюджетний комітет",
                             "Комітет з питань правової політики"="Правова політика",
                             "Комітет з питань цифрової трансформації"="Цифрова трансформація",
                             "Комітет з питань молоді і спорту"="Молоді та спорту")
  )

# 1-а графіка, пофракційно і за головними комітетами, % ####

ggplot(fixed, aes(x=factions,y=n_vote,fill=status))+
  geom_col(position = position_stack(reverse = TRUE))+
  coord_flip()+
  facet_wrap(.~department)+
  #theme_minimal() +
  ggtitle("Розподіл голосувань за підписані законопроекти,
Пофракційно і за головними комітетами, % ")+
  scale_fill_manual(labels = c("За", "Утрималися", "Проти", "Не голосували", "Відсутні"),
                    values = c("vote_for_perc" = "#452571", 
                               "vote_abstain_perc" = "#e5e500",
                               "vote_against_perc" = "firebrick", 
                               "vote_not_voting_perc" = "#e8e8e8",
                               "vote_absent_perc" = "#333333"))+
  theme(text = element_text(family = "PFDinTextCompPro-Bold", size=30, colour = "firebrick" ),
        plot.caption = element_text(hjust = 1, face = "italic", color="black", size=18, family = "PFDinTextCompPro-Light"),
        panel.grid.major.x = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),#Прибрала сітку грідс
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.title=element_blank(),
        legend.text = element_text(family = "PFDinTextCompPro-Regular", size = 18, colour = "black"),
        legend.background = element_blank(),
        strip.background=element_blank(),
        strip.text.x =element_text(size = 22),  # Facets names
        axis.text.x = element_text(size = 14,  family = "PFDinTextCompPro-Light", colour = "black"), #цифри внизу#201d41
        axis.text.y = element_text(size = 18,  family = "PFDinTextCompPro-Regular", colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background =  element_rect(fill = "white"),
        panel.background= element_rect(fill = "white", color = "white"))+
  labs(caption="Згідно з даними участі депутатів у голосуваннях станом на 21 грудня 2019 року.
       Інформація з офіційного сайту та з порталу відкритих даних Верховної Ради України")

ggsave(filename = paste0("output_cumulative/Підписані_закони1", "22_12_2019", ".png"), 
       width = 1400, height = 900, dpi = 300, device = png, limitsize = F)

# 2-а графіка, пофракційно ####

ggplot(z_act_out_faction_long, 
       aes(x=factions,y=n_vote, fill=status))+
  geom_col(position = position_stack(reverse = TRUE))+
  coord_flip()+
  ggtitle("Розподіл голосів «за», «проти» і «утрималися»
За підписані закони Верховної Ради 9 скл., пофракційно, %")+
  scale_fill_manual(labels = c("За", "Утрималися", "Проти", "Не голосували", "Відсутні"),
                    values = c("vote_for_perc" = "#452571", 
                               "vote_abstain_perc" = "#e5e500",
                               "vote_against_perc" = "firebrick", 
                               "vote_not_voting_perc" = "#e8e8e8",
                               "vote_absent_perc" = "#333333"))+
  scale_y_continuous(breaks = c(0,15,20,25,30,35,40,50,70,85),
                     labels=c("0"="0%",
                              "15"="15",
                              "20"="20",
                              "25"="25",
                              "30"="30",
                              "35"="35",
                              "40"="40",
                              "50"="50",
                              "70"="70",
                              "85"="85"))+
  theme(text = element_text(family = "PFDinTextCompPro-Bold", size=18),
        plot.caption = element_text(hjust = 1,face = "italic", size=14,colour = "black",
                                    family = "PFDinTextCompPro-Light"),
        panel.grid.major.x = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),#Прибрала сітку грідс
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.title=element_blank(),
        legend.text = element_text( family = "PFDinTextCompPro-Light"),
        legend.background = element_blank(),
        strip.background=element_blank(),
        axis.text.x = element_text(size = 12, colour = "black", family = "PFDinTextCompPro-Light"), #цифри внизу #201d41
        axis.text.y = element_text(size = 20, colour = "black", family = "PFDinTextCompPro-Regular"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background =  element_rect(fill = "white"),
        panel.background= element_rect(fill = "white", color = "white"))+
  labs(caption="Згідно з даними участі депутатів у голосуваннях станом на 21 грудня 2019 року. #Change 
       Інформація з офіційного сайту та з порталу відкритих даних Верховної Ради України")

ggsave(filename = paste0("output_cumulative/Фракції_підписані_закони", "22_12_2019", ".png"), #Change
       width = 850, height = 450, dpi = 300, device = png, limitsize = F)

# Запис у файл ####

dir.create("output_cumulative")

write.xlsx(as.data.frame(bills_acts09), 
           file=paste0("output_cumulative/Прийняті_закони", ".xlsx"),
           sheetName="Прийняті_законініціати", row.names=FALSE, append = FALSE)

write.xlsx(as.data.frame(zakon_act), 
           file=paste0("output_cumulative/Прийняті_закони", ".xlsx"),
           sheetName="Прийняті_законопроекти", row.names=FALSE, append = TRUE)

write.xlsx(as.data.frame(zakon_act_depart), 
           file=paste0("output_cumulative/Прийняті_закони", ".xlsx"),
           sheetName="Закони_гол_комітет", row.names=FALSE, append = TRUE)

write.xlsx(as.data.frame(z_act_out_faction), 
           file=paste0("output_cumulative/Прийняті_закони", ".xlsx"),
           sheetName="За фракціями, у %", row.names=FALSE, append = TRUE)

write.xlsx(as.data.frame(z_act_out_faction_depart), 
           file=paste0("output_cumulative/Прийняті_закони", ".xlsx"),
           sheetName="За фракціями і гол.комітетом %", row.names=FALSE, append = TRUE)

write.xlsx(as.data.frame(z_act_out_mps_perc), 
           file=paste0("output_cumulative/Прийняті_закони", ".xlsx"),
           sheetName="За нардепами і фракціями, %", row.names=FALSE, append = TRUE)

write.xlsx(as.data.frame(z_act_out_mps_depart_perc), 
           file=paste0("output_cumulative/Прийняті_закони", ".xlsx"),
           sheetName="За нардепами наростаюче голосування у %", row.names=FALSE, append = TRUE)

