# Final Fancy shit: Party logos und objekt zuweisen

# install.packages("ggimage")
library(ggimage)

gg1 <- #objekt zuweisen
  ggplot(data = gles_data,
         aes(x = zweitstimme,
             y = (..count..)/sum(..count..), 
             fill = zweitstimme)) + 
  geom_bar(stat = "count")+ 
  geom_text(aes(y=..count../sum(..count..)+0.02,
                label=paste0(round(..count../sum(..count..)*100,1),"%"),
                color = zweitstimme), 
            stat="count")+
  scale_fill_manual(values = c("afd" = "blue",
                               "fdp" = "yellow",
                               "gruene" = "green",
                               "andere" = "grey",
                               "linke" = "purple",
                               "spd" = "red",
                               "union" = "black"),
                    name = "Zweitstimme",
                    labels = c("AfD","Andere","FDP","Grüne","Linke","SPD","Union"))+
  scale_color_manual(values = c("afd" = "blue",
                                "fdp" = "yellow",
                                "gruene" = "green",
                                "andere" = "grey",
                                "linke" = "purple",
                                "spd" = "red",
                                "union" = "black"),
                     name = "Zweitstimme",
                     labels = c("AfD","Andere","FDP","Grüne","Linke","SPD","Union"))+
  labs(x = "Partei", 
       y = "Anteil Zweitstimmen",
       title = "Titel",
       subtitle = "Untertitel",
       caption = "whatever") + 
  theme_ipsum(grid = "Y")+ #Theme mit schöner Schrit und mit horizontalen Grid-Linien
  geom_image(aes(x = 1,y = 0.15,image = "images/AFD.png"),size = 0.1)+
  geom_image(aes(x = 3,y = 0.15,image = "images/FDP.png"),size = 0.1)+  
  geom_image(aes(x = 4,y = 0.15,image = "images/GRUENE.png"),size = 0.1)+
  geom_image(aes(x = 5,y = 0.15,image = "images/LINKE.png"),size = 0.1)+
  geom_image(aes(x = 6,y = 0.30,image = "images/SPD.png"),size = 0.1)+
  geom_image(aes(x = 7,y = 0.45,image = "images/CDU.png"),size = 0.1)


gg1 #print

# Entweder pdf
ggsave(filename = "results/gles_barplot_logos.pdf",
       plot = gg1,
       device = "pdf",
       width = 10,
       height = 6,
       dpi = 1000)
