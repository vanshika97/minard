library(xlsx)
library(dplyr)
library(ggrepel)
library(tidyverse)
library(lubridate)
library(ggmap)
library(gridExtra)
library(pander)

df <- read.xlsx("C:/Users/vansh/TCD MAI/Data Visualisation/minard/minard-data.xlsx", sheetName = "Sheet1")
cities <- df[names(df) %in% c("LONC", "LATC", "CITY")]
temperature <- df[names(df) %in% c("LONT", "TEMP", "DAYS", "MON", "DAY")]
survivors <- df[names(df) %in% c("LONP", "LATP", "SURV", "DIR", "DIV")]
cities <- cities %>% filter_all(any_vars(!is.na(.)))
temperature <- temperature %>% filter_all(any_vars(!is.na(.)))
survivors <- survivors %>% filter_all(any_vars(!is.na(.)))

Minard.temp2 <- data.frame(long=rev(temperature$LONT),
                           lat=c(54.7,54.3,54.4,54.3,54.4,54.6,54.8,55.2,55.7))
troops <- ggplot()+
  geom_segment(data=Minard.temp2,aes(x=long,y=lat,xend=long,yend=53.2),size=0.2)+
  geom_path(data=survivors, aes(x = LONP, y = LATP, group = DIV,
                                color = DIR, size = SURV), lineend = "round") +
  geom_point(data = cities, aes(x = LONC, y = LATC),
             color = "#DC5B44") +
  geom_text_repel(data = cities, aes(x = LONC, y = LATC, label = CITY),
                  color = "#DC5B44") +
  scale_size(range = c(0.5, 15))+ 
  scale_colour_manual(values = c("#DFC17D", "#252524"))+ 
  guides(color = FALSE, size = FALSE) +
  theme_nothing()+
  theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))

pretty_temp <- temperature %>%
  mutate(nice.label = paste0(TEMP, "°"))
temp <- ggplot(data = pretty_temp, aes(x = LONT, y = TEMP)) +
  geom_line() +
  geom_segment(aes(xend=LONT,yend=0),size=0.2)+
  geom_text_repel(aes(label = nice.label))+
  labs(y="° Celsius")+
  scale_x_continuous(limits = ggplot_build(troops)$layout$panel_scales_x[[1]]$range$range) +
  scale_y_continuous(position = "right") +
  theme_bw(base_family = "Open Sans Condensed Light") +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks = element_blank(),
        panel.border = element_blank(),
        plot.margin=unit(c(-0.5,1,1,1), "cm"))

plot.both <- rbind(ggplotGrob(troops),
                   ggplotGrob(temp))

panels <-plot.both$layout$t[grep("panel", plot.both$layout$name)]
plot.both$heights[panels] <- unit(c(3, 1), "null")
grid::grid.draw(plot.both)

