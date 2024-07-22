#Libraries
library(tidyverse)
library(ggbump)
library(ggtext)
library(showtext)

#Data
df_raw <- read.csv("TDF2024.csv")

#Data processing
df_1 <- df_raw|>
  pivot_longer(cols = starts_with("Stage"), values_to = "Ranking", names_to = "Stage_name")|>
  separate(Stage_name, into = c("Stage_prefix", "Stages"))|>
  select(-Stage_prefix)

df_1$Stages <- as.integer(df_1$Stages)
df_1$Ranking <- as.integer(df_1$Ranking)
df_1$Highlight <- (df_1$Stages == 1) | (df_1$Stages == 10) | (df_1$Stages == 21)
df_1$Team <- as.character(df_1$Team)

selected_teams <- c("UAD","IWA","EFE","SOQ")

#Fonts
font_add_google("Roboto Condensed")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")
showtext_auto()

#Define annotations

plot_title <- "UAE Team Emirates' Dominance at Tour de France 2024"
plot_subtitle <- "Team rankings over the 21 gruelling stages across Tour de France 2024 and winning teams of the<br><span style='color:#FAE64B'>**general classification**</span>, 
<span style='color:#B2DDC6'>**points classification**</span>, <span style='color:#D54000'>**mountain classification**</span>, and <span style='color:#FFFFFF'>**young rider classification**</span>."
plot_caption <- "Data Visualisation: Nien Xiang Tou | Nienxiangtou.com | <span style='font-family:fb;'>&#xf09b; </span>Nxrunning | <span style='font-family:fb;'>&#xf099; </span>Nxrunning"
  
#Data visualisation
p<-ggplot(df_1, aes(x = Stages, y = Ranking, group = Team_Code))+
  geom_bump(linewidth = 0.6, color = "#505050", smooth = 6)+
  geom_bump(aes(color = Team_Code), linewidth = 0.6, smooth = 6,
            data = ~.|>filter(Team_Code %in% selected_teams))+
  geom_point(data = subset(df_1, Highlight), aes(x = Stages, y = Ranking), 
             color = "#505050", size = 2)+
  geom_point(data = subset(df_1, Highlight), aes(x = Stages, y = Ranking), 
             color = "#B9B9B9", size = 1)+
  geom_point(data = subset(df_1, Highlight)|>filter(Team_Code %in% selected_teams), 
             aes(x = Stages, y = Ranking, color = Team_Code), 
             size = 1)+
  geom_text(data = df_1|>filter(Stages == 21)|>filter(Team_Code %in% selected_teams == FALSE), 
            aes(x = 22, label = Team), 
            hjust = 0, color = "#B9B9B9", family = "Roboto Condensed", size = 6)+
  geom_text(data = df_1|>filter(Stages == 21)|>filter(Team_Code %in% selected_teams), 
            aes(x = 22, label = Team, colour = Team_Code), 
            hjust = 0, family = "Roboto Condensed", fontface = "bold", size = 6)+
  labs(title = plot_title,
       subtitle = plot_subtitle,
       y = "General Rankings",
       caption = plot_caption)+
  scale_x_continuous(limits = c(1, 27), breaks = c(1,5,10,15,20))+
  scale_y_reverse(breaks = c(1,5,10,15,20))+
  scale_color_manual(values = c("#D54000","#B2DDC6","#FFFFFF","#FAE46B"))+
  theme(
    plot.background = element_rect(fill = "#262626", color = "#262626"),
    panel.background = element_rect(fill = "#262626", color = "#262626"),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.title = element_text(color = "#FFFFFF", size = 18),
    axis.text = element_text(color = "#FFFFFF", size = 15),
    plot.title = element_textbox_simple(color = "#FDB000", face="bold",
                                        family = "Roboto Condensed", size = 35),
    plot.subtitle = element_textbox_simple(color = "#B9B9B9", size = 18,
                                           family = "Roboto Condensed", lineheight = 0.5,
                                           margin = margin(t = 1, b = 0.5, unit = "mm")),
    plot.caption = element_textbox_simple(color = "#FFFFFF", size = 15,
                                          family = "Roboto Condensed", halign = 0.5,
                                          margin = margin(t = 1, b = 0.5, unit = "mm"))
  )

#Save plot
ggsave(p, width = 112.4, height = 120, units = "mm",filename = "TDF2024.jpeg")
