# Install packages
# devtools::install_github("zmeers/ggparliament")

# Libraries
library(tidyverse)
library(ggparliament)
library(ggtext)
library(showtext)

# Data
sg <- data.frame(
  party_name = c("PAP", "WP", "New Seats"),
  seats = c(83, 10, 4)
)

# Transformation of data for ggparliament plot
# Specify the data, type of plot, number of rows, and numeric variable defining the number of seats
sg_parliament<- parliament_data(election_data = sg, 
                                parl_rows = 5,
                                type = 'semicircle',
                                party_seats = sg$seats)

# Fonts
# To use the icons, download the otf file into your working directory
font_add_google("Roboto Condensed")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")
showtext_auto()

# Define annotations
plot_title <- "GE2025: Seats Up For Grabs"
plot_subtitle <- "The current parliament is made up of elected members from the <span style='color:#EC2D29'>**People’s Action Party**</span> and <span style='color:#93B3D4'>**The Workers’ Party**</span>, with four <span style='color:#848484'>**new seats**</span> to be contested in the upcoming election."
plot_caption <- "Data Visualisation: Nien Xiang Tou | Nienxiangtou.com | <span style='font-family:fb;'>&#xf09b; </span>Nxrunning | <span style='font-family:fb;'>&#xf099; </span>Nxrunning"

# Visualisation

p<-ggplot(sg_parliament, aes(x, y, colour = party_name)) +
  geom_parliament_seats(size = 3) +
  annotate('curve', x = -1.6, y = 1.8, xend = -1.35, yend = 1.65, 
           arrow = arrow(length = unit(0.03, "npc")), curvature = -0.2)+
  annotate('text', x = -2, y = 1.95, label = "People's Action Party", 
           size = 4, hjust = 0)+
  annotate('curve', x = 2.1, y = 1, xend = 2.0, yend = 0.8, 
           arrow = arrow(length = unit(0.03, "npc")), curvature = -0.2)+
  annotate('text', x = 2.2, y = 1.1, label = "The Workers'Party", 
           size = 4, hjust = 0.5)+
  annotate('curve', x = 2.3, y = 0.2, xend = 1.8, yend = -0.15, 
           arrow = arrow(length = unit(0.03, "npc")), curvature = -0.7)+
  annotate('text', x = 2.1, y = 0.3, label = "Additional seats", 
           size = 4, hjust = 0)+
  annotate('text', x = 0, y = 0.2, label = "97", 
           size = 15, hjust = 0.5, fontface = "bold", colour = "#8B2A4C")+
  annotate('text', x = 0, y = -0.1, label = "Total Seats", 
           size = 5, hjust = 0.5, fontface = "bold", colour = "#8B2A4C")+
  labs(title = plot_title,
       subtitle = plot_subtitle,
       caption = plot_caption) +
  xlim(-2, 2.4)+
  ylim(-0.2, 2.2)+
  scale_color_manual(values = c("#848484","#EC2D29","#93B3D4"))+
  theme(
    plot.background = element_rect(fill = "grey95", color = "grey95"),
    panel.background = element_rect(fill = "grey95", color = "grey95"),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_textbox_simple(color = "#8B2A4C", face="bold",
                                        family = "Roboto Condensed", size = 35),
    plot.subtitle = element_textbox_simple(color = "#000000", size = 18,
                                           family = "Roboto Condensed", lineheight = 0.5,
                                           margin = margin(t = 1, b = 0.5, unit = "mm")),
    plot.caption = element_textbox_simple(color = "#8B2A4C", size = 15,
                                          family = "Roboto Condensed", halign = 0.5,
                                          margin = margin(t = 1.5, b = 0.5, unit = "mm"))
  )


#Save plot
ggsave(p, width = 80, height = 50, units = "mm",filename = "GE2025.jpeg")
