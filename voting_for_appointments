# Голосування про призначення на посаду #
# Переконатися в тому, що в робоче середовище вже завантажені файли персонального голосування і законодавча активність

#  *********** Постанови про призначення  ####

work_position_voting <- read.csv("https://data.rada.gov.ua/ogd/zal/ppz/skl9/plenary_event_question-skl9.csv", 
                                 fileEncoding = "UTF-8")%>%
  select( -date_event, -type_event,  -id_question, -date_agenda, -name_event) %>%
  left_join(personal_vote, by=c("id_event"="id_event"))%>%
  filter(grepl("проект Постанови про призначення|Поіменне голосування про призначення", name_event))%>%
  mutate(id_question=as.character(id_question))%>%
  left_join(agenda, by=c("id_question"="id_question"))%>%
  # Download bills in bills_09.R
  left_join(bills_main_skl9, by=c("number_question"="number"))%>%
  left_join(bills_executives, by=c("bill_id"="bill_id")) %>% 
  filter(!id_event %in% c(6776, 3809))#  Підтягується зайва інформація, необзідно слідкувати і відфільтровувати
  
  
  
# ***********Поіменка ####

out_work <- function() {
  
  out_work <- cSplit(work_position_voting, "results", sep="|", "long")%>%
    separate(results, c("mps_id", "faction", "vote_status"), ":")%>%
    mutate(faction = recode(faction,
                            `0` = "Позафракційні",
                            `1` = "Слуга Народу",
                            `2` = "ОПЗЖ",
                            `3` = "Батьківщина",
                            `4` = "ЄС",
                            `5` = "ГОЛОС",
                            `6` = "За майбутнє",
                            `7`="ДОВІРА",
                            `8` = "За майбутнє"))%>%
    mutate(vote_status = recode(vote_status,
                                `0` = "Відсутній",
                                `1` = "За",
                                `2` = "Проти",
                                `3` = "Утримався",
                                `4` = "Не голосував",
                                `5` = "Присутній"))
  
  
  out_work$mps_id[out_work$mps_id ==  "208"] <- "438" # Radina-Krasnosilska changed the surname in March 2020
  
  out_work <- out_work%>%
    left_join(factions_09, by=c("mps_id"="rada_id"))%>%
    filter(date_end=="")%>%
    mutate(id_question=as.character(id_question))%>%
    mutate(mps_id=as.integer(mps_id))
  
  return(out_work)
}

out_work <- out_work()

# Кількість голосувань за призначення 
nrow(work_position_voting)


# 4. *********** Групування і розшифровка  ####

# 4.1 По фракціям у відсотках ####

positions_by_factions <- out_work%>%
  group_by(factions)%>%
  summarise(   
    # У відсотках
    vote_for_perc = round(mean(vote_status == "За")*100, 1), 
    vote_abstain_perc = round(mean(vote_status == "Утримався")*100, 1),
    vote_against_perc = round(mean(vote_status == "Проти")*100, 1), 
    vote_present_perc = round(mean(vote_status == "Присутній")*100, 1),
    vote_not_voting_perc = round(mean(vote_status == "Не голосував")*100, 1),
    vote_absent_perc = round(mean(vote_status == "Відсутній")*100, 1)) %>% 
  ungroup()%>%
  #mutate(uchast=vote_for_perc+vote_abstain_perc+vote_against_perc)%>%
  mutate(factions=as.factor(factions))%>%
  arrange(vote_for_perc, factions)
  
  
# 4.1.a) По фракціям у відсотках, long

positions_by_factions_long <- positions_by_factions%>%
  mutate(factions = fct_reorder(factions, levels(positions_by_factions$factions))) %>% 
  gather(status, n_vote, vote_for_perc:vote_absent_perc, factor_key = TRUE)%>%
  filter(!status=="vote_present_perc") # Фільтрує невідомий критерій з нулями.
  
  
  # Як нардепи за посади голосують ####

ggplot(positions_by_mps, aes(factions, vote_for_perc))+
  #geom_point( position = "jitter")+
  geom_jitter(width = 0.1, size=3, color="#452571")+
  #geom_boxplot()+
  coord_flip()+
  ggtitle("Голосування нардепів за призначення на посади
47 голосувань, %")+
  theme(text = element_text(family = "PFDinTextCompPro-Bold", 
                            size=30, 
                            colour = "black" ),
        plot.caption = element_text(hjust = 1, 
                                    #face = "italic", 
                                    color="grey", size=18, 
                                    family = "PFDinTextCompPro-Regular"),
        panel.grid.major.x = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
        panel.grid.major.y = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
        panel.grid.minor.x = element_blank(),#Прибрала сітку грідс
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.title=element_blank(),
        legend.text = element_text(family = "PFDinTextCompPro-Regular", size = 18, colour = "black"),
        legend.background = element_blank(),
        strip.background=element_blank(),
        strip.text.x =element_text(size = 22),  # Facets names
        axis.text.x = element_text(size = 16,  family = "PFDinTextCompPro-Light", colour = "black"), #цифри внизу#201d41
        axis.text.y = element_text(size = 30,  family = "PFDinTextCompPro-Regular", colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background =  element_rect(fill = "white"),
        panel.background= element_rect(fill = "white", color = "white"))+
  labs(caption="Згідно з даними участі депутатів у голосуваннях станом на 18 грудня 2020 року
      Кожна крапка позначає кількість голосувань «за» нардепа за призначення на посади
       Інформація з офіційного сайту та з порталу відкритих даних Верховної Ради України")


# Запис у файл
ggsave(filename = paste0("output_cumulative/призначення_на_посаду", "18_12_2020", ".png"), 
       width = 1050, height = 550, dpi = 300, 
       device = png, limitsize = F)
       
       

# Пофракційно ####

ggplot(positions_by_factions_long, aes(factions, n_vote, fill=status))+
  #geom_point( position = "jitter")+
  geom_col(position = position_stack(reverse = TRUE))+
  #geom_boxplot()+
  coord_flip()+
  ggtitle("Голосування нардепів за призначення на посади
Усього 47 голосувань, %")+
  scale_fill_manual(labels = c("За", "Утрималися", "Проти", "Не голосували", "Відсутні"),
                    values = c("vote_for_perc" = "#452571", # #583479 #452571
                               "vote_abstain_perc" = "#e5e500", # #f39200  #e5e500 #E5E5E5
                               "vote_against_perc" = "#FC3F1B", #firebrick 
                               "vote_not_voting_perc" = "#E5E5E5", #
                               "vote_absent_perc" = "#333333"))+ # #333333
  theme(text = element_text(family = "PFDinTextCompPro-Bold", 
                            size=30, 
                            colour = "black" ),
        plot.caption = element_text(hjust = 1, 
                                    #face = "italic", 
                                    color="grey", size=18, 
                                    family = "PFDinTextCompPro-Regular"),
        panel.grid.major.x = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
        panel.grid.major.y = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
        panel.grid.minor.x = element_blank(),#Прибрала сітку грідс
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.title=element_blank(),
        legend.text = element_text(family = "PFDinTextCompPro-Regular", size = 18, colour = "black"),
        legend.background = element_blank(),
        strip.background=element_blank(),
        strip.text.x =element_text(size = 22),  # Facets names
        axis.text.x = element_text(size = 16,  family = "PFDinTextCompPro-Light", colour = "black"), #цифри внизу#201d41
        axis.text.y = element_text(size = 30,  family = "PFDinTextCompPro-Regular", colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background =  element_rect(fill = "white"),
        panel.background= element_rect(fill = "white", color = "white"))+
  labs(caption="Згідно з даними участі депутатів у голосуваннях станом на 18 грудня 2020 року
       Інформація з офіційного сайту та з порталу відкритих даних Верховної Ради України")

# Запис у файл
ggsave(filename = paste0("output_cumulative/призначення_на_посаду_пофракційно_", "18_12_2020", ".png"), 
       width = 1050, height = 550, dpi = 300, 
       device = png, limitsize = F)
       


