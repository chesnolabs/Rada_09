#### Графіка ####
# Спочатку прогнати код з файлу **amends_voting.R** 

####Скорочуємо назви комітетів ####

amends_faction_depart_perc_long <- amends_faction_depart_perc_long%>%
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

#### 1-а графіка, пофракційно і за головними комітетами, % ####

ggplot(amends_faction_depart_perc_long, aes(x=factions,y=n_vote,fill=status))+
  geom_col(position = position_stack(reverse = TRUE))+
  coord_flip()+
  facet_wrap(.~department)+
  #theme_minimal() +
  ggtitle("Розподіл голосувань за поправки до законопроектів,
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

# Запис у файл
ggsave(filename = paste0("output_cumulative/Поправки_пофракційно_комітет", "22_12_2019", ".png"), 
       width = 1400, height = 900, dpi = 300, 
       device = png, limitsize = F)

#### 2-а графіка, пофракційно, % ####

ggplot(out_amends_factions_perc_long, aes(x=factions,y=n_vote,fill=status))+
  geom_col(position = position_stack(reverse = TRUE))+
  coord_flip()+
  ggtitle("Розподіл голосувань за поправки до законопроектів,
Пофракційно, % ")+
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

# Запис у файл
ggsave(filename = paste0("output_cumulative/Поправки_пофракційно", "22_12_2019", ".png"), 
       width = 850, height = 450, dpi = 300, 
       device = png, limitsize = F)

#### Графіка для 225+1 голосів ####

#### 1-а графіка 225+1, пофракційно ####

ggplot(out_act_amends_factions_perc_long, aes(x=faction,y=n_vote,fill=status))+
  geom_col(position = position_stack(reverse = TRUE))+
  coord_flip()+
  ggtitle("Розподіл голосувань за поправки до законопроектів,
Які отримали 225+1 голос, пофракційно, % ")+
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

# Запис у файл
ggsave(filename = paste0("output_cumulative/226_поправки_фракційно", "22_12_2019", ".png"), 
       width = 850, height = 450, dpi = 300, device = png, limitsize = F)

#### 2-а графіка: пофракційно + гол.комітет ####

ggplot(act_amends_faction_depart_perc_long, aes(x=faction.y,y=n_vote,fill=status))+
  geom_col(position = position_stack(reverse = TRUE))+
  coord_flip()+
  ggtitle("Розподіл голосувань за поправки, які отримали 225+1 голос
За головним комітетом законопроекту, до якого відноситься поправка, % ")+
  facet_wrap(.~department)+
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

# Запис у файл
ggsave(filename = paste0("output_cumulative/Поправки226_пофракційно_комітет", "22_12_2019", ".png"), 
       width = 1400, height = 900, dpi = 300, 
       device = png, limitsize = F)
