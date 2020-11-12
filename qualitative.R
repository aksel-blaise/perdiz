# qualitative data for Perdiz points
library(here)
library(tidyverse)
library(wesanderson)
library(ggpubr)

# read attribute data
data <- read.csv("perdiz.csv", header = TRUE, as.is = TRUE)

raw <- data$raw.mat # raw material
con <- data$context # burial context
temp <- data$temporal # temporal period
site <- data$trinomial # site

# temporal attributes
pal.1 <- wes_palette("Moonrise2", 5, type = "continuous")

#####
# barplot of raw material count by burial context
raw.con <- ggplot(data, aes(raw)) +
  geom_bar(aes(fill = con))+
  scale_fill_manual(values = pal.1) +
  labs(x = 'Raw Material', y = 'Count')

# barplot of raw material count by temporal
raw.temp <- ggplot(data, aes(raw)) +
  geom_bar(aes(fill = temp))+
  scale_fill_manual(values = pal.1) +
  labs(x = 'Raw Material', y = 'Count')

# barplot of raw material count by site
raw.site <- ggplot(data, aes(trinomial)) +
  geom_bar(aes(fill = raw))+
  scale_fill_manual(values = pal.1) +
  labs(x = 'Raw Material', y = 'Count')

# render figure
raw.figure <- ggarrange(raw.site,
                        ggarrange(raw.con,raw.temp,
                                  ncol = 2, 
                                  labels = c("b","c")),
                        nrow = 2,
                        labels = c("a")
                        )

# plot figure
raw.figure

#####
# temporal attributes
pal <- wes_palette("Moonrise2", 12, type = "continuous")
# gantt chart of relative dates for perdiz arrow points
temp<-data.frame(Site = c('Tuck Carpenter (41CP5)', 'Johns (41CP12)', 'BJ Horton (41CP20)','Keering (41CP21)','Kitchen Branch (41CP220)','Pine Tree Mound (41HS15)', 'Washington Square Mound (41NA49)', 'Spradley (41NA206)', 'Jack Walton (41SA135)', 'Redwine (41SM193)', 'Old Timers (41SY43)', 'Sybs Site (41SY280)'),
                 Date_Range_CE = c(1430,1430,1500,1430,1400,1300,1238,1680,1200,1300,1400,1400), # in years CE
                 end = c(1500,1600,1550,1550,1500,1650,1445,1725,1400,1454,1680,1575) # in years CE
)
# reorder types by beginning of relative date range
temp$Site <- factor(temp$Site, levels = temp$Site[order(temp$Date_Range_CE)])
# arrange figure
type.time <- ggplot(temp, 
                    aes(x = Date_Range_CE, 
                        xend = end, 
                        y = factor(Site,
                                   levels = rev(levels(factor(Site)))), 
                        yend = Site, 
                        color = Site)) +
  geom_segment(size = 2.5) +
  scale_colour_manual(values = pal) +
  theme(legend.position = "none") +
  labs(y = "Site", x = "Date Range CE")
# render figure
type.time
