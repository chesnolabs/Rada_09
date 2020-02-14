# Для початку проженіть файл roll_call_voting.R

library(extrafont)
library(extrafontdb)
library(ggplot2)
windowsFonts()
font_import() # takes a few minutes

# If you can't download the fonts, you can do it manually
windowsFonts("PFDinTextCompPro-Light" = windowsFont("PFDinTextCompPro-Light"))
windowsFonts("PFDinTextCompPro-XBlack" = windowsFont("PFDinTextCompPro-XBlack"))
windowsFonts("FiraSans-Eight" = windowsFont("FiraSans-Eight"))
windowsFonts("PFDinTextCompPro-XThin" = windowsFont("PFDinTextCompPro-XThin"))
windowsFonts("PFDinTextCompPro-Regular" = windowsFont("PFDinTextCompPro-Regular"))
windowsFonts("PFDinTextCompPro-Bold" = windowsFont("PFDinTextCompPro-Bold"))

#### Гістограма голоси "за" #####
# Уся законодавча активність

ggplot(zp_names, aes(zp_names$for.))+
  geom_histogram(bins = 100, binwidth=2, fill="#452571")+
  #geom_vline(xintercept = mean(personal_vote$for.), color="red", linetype="dashed", size=0.5)+
  theme_minimal() +
  labs(caption = "Згідно з даними участі депутатів у голосуваннях.
       Інформація з офіційного сайту та з порталу відкритих даних Верховної Ради України")+
  ggtitle("Розподіл голосів за")+
  scale_x_continuous(breaks = c(0,50,100,150,200,250,300,350, 370),
                     labels=c("0"="0",
                              "50"="50",
                              "100"="100",
                              "150"="150",
                              "200"="200",
                              "250"="250",
                              "300"="300",
                              "350"="350",
                              "370"="370"
                     ))+
  theme(text = element_text(family = "Fira Sans Extra Condensed"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16))


#### geom_col за, проти, утримався за увесь час ВРУ 9 #### 

graph_4 <- out_factions_perc_long%>%
  filter(status %in% c('vote_against_perc', 'vote_abstain_perc', 'vote_for_perc'))

# Упорядкований від найбільшого до найменшого
ggplot(graph_4, 
       aes(x=factions,y=n_vote, fill=status))+
  geom_col(position = position_stack(reverse = TRUE))+
  coord_flip()+
  ggtitle("Розподіл голосів «за», «проти» і «утрималися»
Усі голосування Верховної Ради 9 скл., пофракційно, %")+
  scale_fill_manual(labels = c("За",  "Утрималися", "Проти"),
                    values = c("vote_for_perc" = "#452571", 
                               "vote_against_perc" = "#FC3F1B", 
                               "vote_abstain_perc" = "#E5E5E5"))+
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
        panel.grid.minor.x = element_blank(),     # Прибирає сітку
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.title=element_blank(),
        legend.text = element_text( family = "PFDinTextCompPro-Light"),
        legend.background = element_blank(),
        strip.background=element_blank(),
        axis.text.x = element_text(size = 12, colour = "black", family = "PFDinTextCompPro-Light"), #цифри внизу#201d41
        axis.text.y = element_text(size = 20, colour = "black", family = "PFDinTextCompPro-Regular"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background =  element_rect(fill = "white"),
        panel.background= element_rect(fill = "white", color = "white"))+
  labs(caption="Згідно з даними участі депутатів у голосуваннях станом на 21 грудня 2019 року. # Змінити дату
       Інформація з офіційного сайту та з порталу відкритих даних Верховної Ради України")

# Запис у файл
ggsave(filename = paste0("output_cumulative/Усі_голосування","_21_12_2019", ".png"), # Змінити дату
       width = 850, height = 450, dpi = 400, device = png, limitsize = F)

