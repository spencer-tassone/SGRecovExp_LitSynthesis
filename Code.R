library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(emmeans)
library(matrixTests)
library(tidyverse)

rm(list = ls())
dev.off()

setwd("D:/School/SeagrassRecovery/Data")

dat <- read.csv('StemDensity.csv')

dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y", tz = "America/Jamaica")
dat$StemDensity_Meter <- dat$StemDensity_QrtMeter * 4
dat$LocTreat <- paste(dat$MeadowLocation, dat$Treatment)
dat$SiteTreat <- paste(dat$Site_no, dat$Treatment)

dat %>%
  group_by(Date, MeadowLocation, Treatment, Site_no) %>%
  summarise(mean_shoots = mean(StemDensity_Meter, na.rm = T),
            sd_shoots = sd(StemDensity_Meter, na.rm = T))

# Just control sites
control <- dat %>%
  select(Date,MeadowLocation,Treatment,Site_no,StemDensity_Meter) %>%
  filter(Treatment == 'Control') %>%
  select(!Treatment) %>%
  mutate(stemdensity_control = StemDensity_Meter,
         Date = as.factor(Date),
         Site_no = as.factor(Site_no),
         MeadowLocation = as.factor(MeadowLocation)) %>%
  select(!StemDensity_Meter) %>%
  na.omit()

# Just treatment sites
treatment <- dat %>%
  select(Date,MeadowLocation,Treatment,Site_no,StemDensity_Meter) %>%
  filter(Treatment == 'Treatment') %>%
  select(!Treatment) %>%
  mutate(stemdensity_treatment = StemDensity_Meter,
         Date = as.factor(Date),
         Site_no = as.factor(Site_no),
         MeadowLocation = as.factor(MeadowLocation)) %>%
  select(!StemDensity_Meter) %>%
  na.omit()

# visually compare control site shoot density

sample_dates <- unique(as.Date(control$Date, tz = 'America/Jamaica'))
sample_dates <- format(sample_dates, '%b-%Y')

control %>%
  ggplot(aes(x = Date, y = stemdensity_control, fill = Site_no)) +
  stat_boxplot() +
  labs(x = "",
       y = expression(paste("Control Site Shoot Density ( ", m^-2,")")),
       fill = "Site Number") +
  scale_x_discrete(labels = substr(sample_dates,1,3))+
  scale_fill_manual(values = c("#fee0d2", "#fc9272", "#de2d26", "#deebf7", "#9ecae1", "#3182bd")) +
  theme_bw() +
  theme(plot.margin = unit(c(1, 1, 2, 1), "lines"),
        panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        legend.title.align = 1.5,
        legend.direction = "vertical",
        legend.margin = margin(),
        legend.background = element_blank(),
        legend.position = c(0.28,0.85),
        panel.border = element_blank()) +
  guides(fill = guide_legend(nrow = 2)) +
  coord_cartesian(clip = 'off', ylim = c(0, 650)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate(geom = "text", x = c(3,8.5,15), y = -130, label = c(2020,2021,2022), size = 6) +
  annotate('rect',
           xmin = c(5.35, 11.35),
           xmax = c(5.65, 11.65),
           ymin = -50, ymax = -15, fill = 'white') +
  annotate('segment',
           x = c(5.35, 5.65, 11.35, 11.65),
           xend = c(5.35, 5.65, 11.35, 11.65), y = -50, yend = -20) +
  guides(fill=guide_legend(ncol=2,byrow=F))

# visually compare treatment site shoot density
treatment %>%
  ggplot(aes(x = Date, y = stemdensity_treatment, fill = Site_no)) +
  stat_boxplot() +
  labs(x = "",
       y = expression(paste("Treatment Site Shoot Density ( ", m^-2,")")),
       fill = "Site Number") +
  scale_x_discrete(labels = substr(sample_dates,1,3))+
  scale_fill_manual(values = c("#fee0d2", "#fc9272", "#de2d26", "#deebf7", "#9ecae1", "#3182bd")) +
  theme_bw() +
  theme(plot.margin = unit(c(1, 1, 2, 1), "lines"),
        panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.margin = margin(),
        legend.background = element_blank(),
        legend.position = c(0.28,0.85),
        panel.border = element_blank()) +
  guides(fill = guide_legend(nrow = 2)) +
  coord_cartesian(clip = 'off', ylim = c(0, 650)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate(geom = "text", x = c(3,8.5,15), y = -130, label = c(2020,2021,2022), size = 6) +
  annotate('rect',
           xmin = c(5.35, 11.35),
           xmax = c(5.65, 11.65),
           ymin = -50, ymax = -15, fill = 'white') +
  annotate('segment',
           x = c(5.35, 5.65, 11.35, 11.65),
           xend = c(5.35, 5.65, 11.35, 11.65), y = -50, yend = -20) +
  guides(fill=guide_legend(ncol=2,byrow=F))

# three-way anova with interaction to check for differences in shoot density at
# each meadow location (central vs. edge) and sampling date
aov_control <- lm(stemdensity_control ~ Site_no * MeadowLocation * Date, data = control)
aov_treat <- lm(stemdensity_treatment ~ Site_no * MeadowLocation * Date, data = treatment)
emm_control <- emmeans(aov_control, pairwise ~ Site_no | MeadowLocation | Date)
emm_treat <- emmeans(aov_treat, pairwise ~ Site_no | MeadowLocation | Date)
emm_control$contrasts # 3 statistically significant differences out of 108 comparisons
emm_treat$contrasts # 0 statistically significant differences out of 108 comparisons
# 18 sampling dates * 2 locations (central & edge) * 3 sites per location = 108 tests

test <- dat %>%
  group_by(Date,LocTreat) %>%
  summarise(MeanStemDensity = round(mean(StemDensity_Meter, na.rm = T)),
            SD = round(sd(StemDensity_Meter, na.rm = T)))

test$lower <- test$MeanStemDensity - test$SD
test$upper <- test$MeanStemDensity + test$SD

round((1-test[6,3]/test[5,3])*100) # 93% removal efficiency at central treatment sites
round((1-test[8,3]/test[7,3])*100) # 84% removal efficiency at northern treatment sites

test_year1 <- test[1:20,]
test_year2 <- test[21:44,]
test_year3 <- test[45:NROW(test),]

test_year1 <- test_year1 %>%
  mutate(Month = month(Date),
         Year = year(Date),
         date = zoo::as.yearmon(paste(Year, Month), "%Y %m"))
test_year2 <- test_year2 %>%
  mutate(Month = month(Date),
         Year = year(Date),
         date = zoo::as.yearmon(paste(Year, Month), "%Y %m"))
test_year3 <- test_year3 %>%
  mutate(Month = month(Date),
         Year = year(Date),
         date = zoo::as.yearmon(paste(Year, Month), "%Y %m"))

tt <- rbind(test_year1, test_year2, test_year3)

sample_dates <- unique(tt$date)

# Figure 3: timeseries of absolute shoot density ----
### saved as width = 800 height = 600
cols <- c("Central Control" = "#ca0020", "Central Treatment" = "#f48282",
          "Northern Control" = "#0571b0", "Northern Treatment" = "#92c5de")

ggplot(data = tt, aes(x = factor(date), y = MeanStemDensity, color = LocTreat, group = paste(Year,LocTreat))) +
  geom_line() +
  geom_errorbar(ymin = tt$lower, ymax = tt$upper,
                color = "black", width = 0.2, alpha = 0.4) +
  geom_point(size = 3, aes(fill = LocTreat), color = "black", shape = 21) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  scale_x_discrete(labels = substr(sample_dates,1,3))+
  scale_y_continuous(breaks=seq(0,650,50)) +
  labs(x = "",
       y = expression(paste("Mean (± SD) Shoot Density (ind. ", m^-2,")"))) +
  theme_bw() +
  theme(plot.margin = unit(c(1, 1, 2, 1), "lines"),
        panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.margin = margin(),
        legend.background = element_blank(),
        legend.position = c(0.28,0.93),
        panel.border = element_blank()) +
  guides(fill = guide_legend(nrow = 2)) +
  coord_cartesian(clip = 'off', ylim = c(0, 650)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate(geom = "text", x = c(3,8.5,15), y = -130, label = unique(tt$Year), size = 6) +
  annotate('rect',
           xmin = c(5.35, 11.35),
           xmax = c(5.65, 11.65),
           ymin = -50, ymax = -15, fill = 'white') +
  annotate('segment',
           x = c(5.35, 5.65, 11.35, 11.65),
           xend = c(5.35, 5.65, 11.35, 11.65), y = -50, yend = -20)

test_nt <- test[test$LocTreat == "Northern Treatment",]
y_lab <-"Northern Treatment<br>Shoot Density Standard Deviation (ind. m<sup> -2</sup>)"
max(test_nt$SD) # +/- 107 shoots m-2

# Timeseries of northern treatment site shoot standard deviation ----
ggplot(test_nt, aes(x = Date, y = SD)) +
  geom_point() +
  labs(x = "",
       y = y_lab) +
  scale_x_date(breaks = seq(as.Date("2020-06-01"), as.Date("2022-11-01"), by = "month"),
               limits = as.Date(c("2020-06-01","2022-11-01")),
               date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0,120),
                     breaks=seq(0,120,20)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = ggtext::element_textbox_simple(orientation = "left-rotated", halign = 0.5, width = unit(12, "cm")))

june20_dat <- dat[1:36,]
july20_dat <- dat[37:72,]
aug20_dat <- dat[73:108,]
sept20_dat <- dat[109:144,]
oct20_dat <- dat[145:180,]
may21_dat <- dat[181:216,]
june21_dat <- dat[217:252,]
july21_dat <- dat[253:288,]
aug21_dat <- dat[289:324,]
sept21_dat <- dat[325:360,]
oct21_dat <- dat[361:396,]
apr22_dat <- dat[397:432,]
may22_dat <- dat[433:468,]
june22_dat <- dat[469:504,]
july22_dat <- dat[505:540,]
aug22_dat <- dat[541:576,]
sept22_dat <- dat[577:612,]
oct22_dat <- dat[613:NROW(dat),]

june20_table <- june20_dat %>%
  group_by(MeadowLocation,Treatment) %>%
  summarise(MeanStem = mean(StemDensity_Meter))
june20_table$MeanStem <- round(june20_table$MeanStem, digits = 0)

july20_table <- july20_dat %>%
  group_by(MeadowLocation,Treatment) %>%
  summarise(MeanStem = mean(StemDensity_Meter))
july20_table$MeanStem <- round(july20_table$MeanStem, digits = 0)

aug20_table <- aug20_dat %>%
  group_by(MeadowLocation,Treatment) %>%
  summarise(MeanStem = mean(StemDensity_Meter))
aug20_table$MeanStem <- round(aug20_table$MeanStem, digits = 0)

sept20_table <- sept20_dat %>%
  group_by(MeadowLocation,Treatment) %>%
  summarise(MeanStem = mean(StemDensity_Meter))
sept20_table$MeanStem <- round(sept20_table$MeanStem, digits = 0)

oct20_table <- oct20_dat %>%
  group_by(MeadowLocation,Treatment) %>%
  summarise(MeanStem = mean(StemDensity_Meter))
oct20_table$MeanStem <- round(oct20_table$MeanStem, digits = 0)

may21_table <- may21_dat %>%
  group_by(MeadowLocation,Treatment) %>%
  summarise(MeanStem = mean(StemDensity_Meter))
may21_table$MeanStem <- round(may21_table$MeanStem, digits = 0)

june21_table <- june21_dat %>%
  group_by(MeadowLocation,Treatment) %>%
  summarise(MeanStem = mean(StemDensity_Meter))
june21_table$MeanStem <- round(june21_table$MeanStem, digits = 0)

july21_table <- july21_dat %>%
  group_by(MeadowLocation,Treatment) %>%
  summarise(MeanStem = mean(StemDensity_Meter))
july21_table$MeanStem <- round(july21_table$MeanStem, digits = 0)

aug21_table <- aug21_dat %>%
  group_by(MeadowLocation,Treatment) %>%
  summarise(MeanStem = mean(StemDensity_Meter))
aug21_table$MeanStem <- round(aug21_table$MeanStem, digits = 0)

sept21_table <- sept21_dat %>%
  group_by(MeadowLocation,Treatment) %>%
  summarise(MeanStem = mean(StemDensity_Meter))
sept21_table$MeanStem <- round(sept21_table$MeanStem, digits = 0)

oct21_table <- oct21_dat %>%
  group_by(MeadowLocation,Treatment) %>%
  summarise(MeanStem = mean(StemDensity_Meter))
oct21_table$MeanStem <- round(oct21_table$MeanStem, digits = 0)

apr22_table <- apr22_dat %>%
  group_by(MeadowLocation,Treatment) %>%
  summarise(MeanStem = mean(StemDensity_Meter))
apr22_table$MeanStem <- round(apr22_table$MeanStem, digits = 0)

may22_table <- may22_dat %>%
  group_by(MeadowLocation,Treatment) %>%
  summarise(MeanStem = mean(StemDensity_Meter))
may22_table$MeanStem <- round(may22_table$MeanStem, digits = 0)

june22_table <- june22_dat %>%
  group_by(MeadowLocation,Treatment) %>%
  summarise(MeanStem = mean(StemDensity_Meter))
june22_table$MeanStem <- round(june22_table$MeanStem, digits = 0)

july22_table <- july22_dat %>%
  group_by(MeadowLocation,Treatment) %>%
  summarise(MeanStem = mean(StemDensity_Meter))
july22_table$MeanStem <- round(july22_table$MeanStem, digits = 0)

aug22_table <- aug22_dat %>%
  group_by(MeadowLocation,Treatment) %>%
  summarise(MeanStem = mean(StemDensity_Meter))
aug22_table$MeanStem <- round(aug22_table$MeanStem, digits = 0)

sept22_table <- sept22_dat %>%
  group_by(MeadowLocation,Treatment) %>%
  summarise(MeanStem = mean(StemDensity_Meter, na.rm = T))
sept22_table$MeanStem <- round(sept22_table$MeanStem, digits = 0)

oct22_table <- oct22_dat %>%
  group_by(MeadowLocation,Treatment) %>%
  summarise(MeanStem = mean(StemDensity_Meter, na.rm = T))
oct22_table$MeanStem <- round(oct22_table$MeanStem, digits = 0)

june20_spread <- june20_table %>%
  spread(key = Treatment,
         value = MeanStem)
july20_spread <- july20_table %>%
  spread(key = Treatment,
         value = MeanStem)
aug20_spread <- aug20_table %>%
  spread(key = Treatment,
         value = MeanStem)
sept20_spread <- sept20_table %>%
  spread(key = Treatment,
         value = MeanStem)
oct20_spread <- oct20_table %>%
  spread(key = Treatment,
         value = MeanStem)
may21_spread <- may21_table %>%
  spread(key = Treatment,
         value = MeanStem)
june21_spread <- june21_table %>%
  spread(key = Treatment,
         value = MeanStem)
july21_spread <- july21_table %>%
  spread(key = Treatment,
         value = MeanStem)
aug21_spread <- aug21_table %>%
  spread(key = Treatment,
         value = MeanStem)
sept21_spread <- sept21_table %>%
  spread(key = Treatment,
         value = MeanStem)
oct21_spread <- oct21_table %>%
  spread(key = Treatment,
         value = MeanStem)
apr22_spread <- apr22_table %>%
  spread(key = Treatment,
         value = MeanStem)
may22_spread <- may22_table %>%
  spread(key = Treatment,
         value = MeanStem)
june22_spread <- june22_table %>%
  spread(key = Treatment,
         value = MeanStem)
july22_spread <- july22_table %>%
  spread(key = Treatment,
         value = MeanStem)
aug22_spread <- aug22_table %>%
  spread(key = Treatment,
         value = MeanStem)
sept22_spread <- sept22_table %>%
  spread(key = Treatment,
         value = MeanStem)
oct22_spread <- oct22_table %>%
  spread(key = Treatment,
         value = MeanStem)

june20_spread$fraction = june20_spread$Treatment/june20_spread$Control
june20_spread$fraction = round(june20_spread$fraction*100, digits = 1)
june20_spread$Date = as.Date(unique(june20_dat$Date))
june20_spread <- june20_spread[,c(5,1:4)]
july20_spread$fraction = july20_spread$Treatment/july20_spread$Control
july20_spread$fraction = round(july20_spread$fraction*100, digits = 1)
july20_spread$Date = as.Date(unique(july20_dat$Date))
july20_spread <- july20_spread[,c(5,1:4)]
aug20_spread$fraction = aug20_spread$Treatment/aug20_spread$Control
aug20_spread$fraction = round(aug20_spread$fraction*100, digits = 1)
aug20_spread$Date = as.Date(unique(aug20_dat$Date))
aug20_spread <- aug20_spread[,c(5,1:4)]
sept20_spread$fraction = sept20_spread$Treatment/sept20_spread$Control
sept20_spread$fraction = round(sept20_spread$fraction*100, digits = 1)
sept20_spread$Date = as.Date(unique(sept20_dat$Date))
sept20_spread <- sept20_spread[,c(5,1:4)]
oct20_spread$fraction = oct20_spread$Treatment/oct20_spread$Control
oct20_spread$fraction = round(oct20_spread$fraction*100, digits = 1)
oct20_spread$Date = as.Date(unique(oct20_dat$Date))
oct20_spread <- oct20_spread[,c(5,1:4)]
may21_spread$fraction = may21_spread$Treatment/may21_spread$Control
may21_spread$fraction = round(may21_spread$fraction*100, digits = 1)
may21_spread$Date = as.Date(unique(may21_dat$Date))
may21_spread <- may21_spread[,c(5,1:4)]
june21_spread$fraction = june21_spread$Treatment/june21_spread$Control
june21_spread$fraction = round(june21_spread$fraction*100, digits = 1)
june21_spread$Date = as.Date(unique(june21_dat$Date))
june21_spread <- june21_spread[,c(5,1:4)]
july21_spread$fraction = july21_spread$Treatment/july21_spread$Control
july21_spread$fraction = round(july21_spread$fraction*100, digits = 1)
july21_spread$Date = as.Date(unique(july21_dat$Date))
july21_spread <- july21_spread[,c(5,1:4)]
aug21_spread$fraction = aug21_spread$Treatment/aug21_spread$Control
aug21_spread$fraction = round(aug21_spread$fraction*100, digits = 1)
aug21_spread$Date = as.Date(unique(aug21_dat$Date))
aug21_spread <- aug21_spread[,c(5,1:4)]
sept21_spread$fraction = sept21_spread$Treatment/sept21_spread$Control
sept21_spread$fraction = round(sept21_spread$fraction*100, digits = 1)
sept21_spread$Date = as.Date(unique(sept21_dat$Date))
sept21_spread <- sept21_spread[,c(5,1:4)]
oct21_spread$fraction = oct21_spread$Treatment/oct21_spread$Control
oct21_spread$fraction = round(oct21_spread$fraction*100, digits = 1)
oct21_spread$Date = as.Date(unique(oct21_dat$Date))
oct21_spread <- oct21_spread[,c(5,1:4)]
apr22_spread$fraction = apr22_spread$Treatment/apr22_spread$Control
apr22_spread$fraction = round(apr22_spread$fraction*100, digits = 1)
apr22_spread$Date = as.Date(unique(apr22_dat$Date))
apr22_spread <- apr22_spread[,c(5,1:4)]
may22_spread$fraction = may22_spread$Treatment/may22_spread$Control
may22_spread$fraction = round(may22_spread$fraction*100, digits = 1)
may22_spread$Date = as.Date(unique(may22_dat$Date))
may22_spread <- may22_spread[,c(5,1:4)]
june22_spread$fraction = june22_spread$Treatment/june22_spread$Control
june22_spread$fraction = round(june22_spread$fraction*100, digits = 1)
june22_spread$Date = as.Date(unique(june22_dat$Date))
june22_spread <- june22_spread[,c(5,1:4)]
july22_spread$fraction = july22_spread$Treatment/july22_spread$Control
july22_spread$fraction = round(july22_spread$fraction*100, digits = 1)
july22_spread$Date = as.Date(unique(july22_dat$Date))
july22_spread <- july22_spread[,c(5,1:4)]
aug22_spread$fraction = aug22_spread$Treatment/aug22_spread$Control
aug22_spread$fraction = round(aug22_spread$fraction*100, digits = 1)
aug22_spread$Date = as.Date(unique(aug22_dat$Date))
aug22_spread <- aug22_spread[,c(5,1:4)]
sept22_spread$fraction = sept22_spread$Treatment/sept22_spread$Control
sept22_spread$fraction = round(sept22_spread$fraction*100, digits = 1)
sept22_spread$Date = as.Date(unique(sept22_dat$Date))
sept22_spread <- sept22_spread[,c(5,1:4)]
oct22_spread$fraction = oct22_spread$Treatment/oct22_spread$Control
oct22_spread$fraction = round(oct22_spread$fraction*100, digits = 1)
oct22_spread$Date = as.Date(unique(oct22_dat$Date))
oct22_spread <- oct22_spread[,c(5,1:4)]

recov_spread <- rbind(june20_spread, july20_spread,
                      aug20_spread, sept20_spread,
                      oct20_spread,may21_spread,
                      june21_spread,july21_spread,
                      aug21_spread,sept21_spread,
                      oct21_spread,apr22_spread,
                      may22_spread,june22_spread,
                      july22_spread,aug22_spread,
                      sept22_spread,oct22_spread)
recov_spread <- as.data.frame(recov_spread)

recov_spread$ExpAge <- c(0,0,1,1,2,2,3,3,
                         4,4,11,11,12,12,
                         13,13,14,14,15,15,
                         16,16,22,22,23,23,
                         24,24,25,25,26,26,
                         27,27,28,28)

recov_spread <- recov_spread[3:NROW(recov_spread),]
recov_spread$logResponseRatio <- log(recov_spread$Treatment/recov_spread$Control)
recov_spread[5,7] <- NA

recov_spread <- recov_spread %>%
  mutate(Month = month(Date),
         Year = year(Date),
         date = zoo::as.yearmon(paste(Year, Month), "%Y %m"))

recov_spread %>%
  group_by(Year,MeadowLocation) %>%
  summarise(MaxFrac = round(max(fraction, na.rm = T)))

dat3 <- dat[-c(577:594),c(1:4,9)]

### Welch t-test to determine if there are significant differences
### between control and treatment site shoot density for each date at each location
table1 <- dat3 %>% 
  group_nest(Date,ExpAge,MeadowLocation,Treatment) %>% 
  pivot_wider(names_from = Treatment,values_from = data) %>% 
  mutate(
    test = map2(
      .x = Control,
      .y = Treatment,
      .f = ~col_t_welch(.x,.y, conf.level = 0.95) # Welch used for unequal variances
    )
  ) %>% 
  unnest(test) %>%
  select(Date,ExpAge,MeadowLocation,pvalue,mean.x,mean.y,mean.diff,stderr,obs.x,obs.y,conf.low,conf.high)

table1 <- as.data.frame(table1)
table1 <- table1 %>%
  mutate(pvalue = round(p.adjust(pvalue, method = "BH"), 3),
         mean.diff = round(mean.diff),
         sd = stderr * sqrt(obs.x),
         lower = mean.diff - sd,
         upper = mean.diff + sd,
         sig = ifelse(pvalue < 0.05, "Sig", "Not Sig"),
         Month = month(Date),
         Year = year(Date),
         date = zoo::as.yearmon(paste(Year, Month), "%Y %m"))

# Figure 5: Shoot density difference ----
### Exported as width = 1000 height = 650
table1 %>%
  ggplot(aes(y = mean.diff, x = factor(date))) +
  geom_linerange(ymin = table1$conf.low, ymax = table1$conf.high,
                 color = "black", size = 3.5, alpha = 0.35) +
  geom_point(size = 3) +
  scale_y_continuous(breaks = seq(-100,450,50)) +
  scale_x_discrete(labels=substr(sample_dates,1,3))+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = expression(paste("Mean (± 95% CI) Shoot Density Difference (ind. ", m^-2,")")),
       x = "") +
  theme_bw() +
  theme(plot.margin = unit(c(1, 1, 2, 1), "lines"),
        panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        panel.border = element_blank(),
        strip.background = element_rect(fill="white"),
        strip.text.x = element_text(size = 14, color = "black"),) +
  coord_cartesian(clip = 'off', ylim = c(-100, 455)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate(geom = "text", x = c(3), y = -200, label = 2020, size = 6) +
  annotate(geom = "text", x = c(8.5), y = -200, label = 2021, size = 6) +
  annotate(geom = "text", x = c(15), y = -200, label = 2022, size = 6) +
  annotate('rect', xmin = c(5.4, 11.4), xmax = c(5.6, 11.6),
           ymin = -135, ymax = -120, fill = 'white') +
  annotate('segment', x = c(5.4, 5.6, 11.4, 11.6),
           xend = c(5.4, 5.6, 11.4, 11.6), y = -135, yend = -120) +
  facet_wrap(~MeadowLocation)

# Shoot density log response ratio ----
cols2 <- c("Central" = "black", "Northern" = "white")

ggplot(data = recov_spread, aes(x = Date, y = logResponseRatio, group = MeadowLocation)) +
  geom_line(aes(fill = MeadowLocation)) +
  geom_point(aes(fill = MeadowLocation), size = 3, shape = 21) +
  scale_fill_manual(values = cols2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(-5,00),
                     breaks=seq(-5,0,1)) +
  labs(x = "",
       y = expression(ln~(~frac(italic(bar(x))[t],italic(bar(x))[c])))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"),
        legend.position = c(0.15,0.9),
        legend.key = element_blank(),
        legend.background = element_blank())

central_lm_dat <- recov_spread[recov_spread$MeadowLocation == "Central",]
northern_lm_dat <- recov_spread[recov_spread$MeadowLocation == "Northern",]
central_lm_model <- lm(fraction~Date, data = central_lm_dat)
northern_lm_model <- lm(fraction~Date, data = northern_lm_dat)
central_lm_model_ExpAge <- lm(fraction~ExpAge, data = central_lm_dat)
northern_lm_model_ExpAge <- lm(fraction~ExpAge, data = northern_lm_dat)
summary(central_lm_model)
summary(northern_lm_model)
summary(central_lm_model_ExpAge)
summary(northern_lm_model_ExpAge)

central_lm_dat2021 <- central_lm_dat[5:7,]
northern_lm_dat2021 <- northern_lm_dat[5:7,]
summary(lm(fraction~ExpAge, data = central_lm_dat2021))
summary(lm(fraction~ExpAge, data = northern_lm_dat2021))

round(max(central_lm_dat$fraction, na.rm = T)) # Maximum recovery 114%
round(summary(central_lm_model)$coefficients[2],2) # slope = 0.13
round(summary(central_lm_model)$coefficients[4],2) # slope standard error = 0.01
round(100/summary(central_lm_model)$coefficients[2]/30.417,) # Relative recovery estimated to take 26 months using linear regression. Need to divide by 30.417 to convert from number of days til 100% RR to number of months
central_slopeCI <- confint(central_lm_model, 'Date', level = 0.95) # 95% confidence interval
round(100/central_slopeCI[1]/30.417,) # Relative recovery estimated 95% CI to take between 22 & 30 months using linear regression. 
round(100/central_slopeCI[2]/30.417,) # Relative recovery estimated 95% CI to take between 22 & 30 months using linear regression. 
central_rr_lab <- expression(paste(RR[max] == 114,"%"))
central_slope_lab <- expression(paste(Slope == 0.13," ± ",0.01)) # comes from Experiment Age
central_rr100_lab <- expression(paste(RR[100] == 26," months" ))
round(max(northern_lm_dat$fraction)) # Maximum recovery 43%
round(summary(northern_lm_model)$coefficients[2],3) # slope = 0.021
round(summary(northern_lm_model)$coefficients[4],2) # slope standard error = 0.01
round((100/summary(northern_lm_model)$coefficients[2])/30.417,) # Relative recovery estimated to take 158 months using linear regression. Need to divide by 30.417 to convert from number of days til 100% RR to number of months
northern_slopeCI <- confint(northern_lm_model, 'Date', level = 0.95) # 95% confidence interval
round(100/northern_slopeCI[1]/30.417,) # Relative recovery estimated 95% CI to take between 80 & 9035 months using linear regression. 
round(100/northern_slopeCI[2]/30.417,) # Relative recovery estimated 95% CI to take between 80 & 9035 months using linear regression. 
northern_rr_lab <- expression(paste(RR[max] == 43,"%"))
northern_slope_lab <- expression(paste(Slope == 0.021," ± ",0.01)) # comes from Experiment Age
northern_rr100_lab <- expression(paste(RR[100] == 158," months"))

recov_spread$sample <- rep(1:17, each = 2)

central_dummylm_dat <- recov_spread[recov_spread$MeadowLocation == "Central",]
northern_dummylm_dat <- recov_spread[recov_spread$MeadowLocation == "Northern",]
central_dummylm_model <- lm(fraction~central_dummylm_dat$sample, data = central_lm_dat)
northern_dummylm_model <- lm(fraction~northern_dummylm_dat$sample, data = northern_lm_dat)

# Figure 4: Relative recovery of shoot density ----
### saved as width = 800 height = 600
sample_dates2 <- unique(recov_spread$date)

ggplot(data = recov_spread, aes(x = factor(date), y = fraction, group = MeadowLocation)) +
  geom_point(aes(fill = MeadowLocation), size = 3, shape = 21, color = "black") +
  scale_fill_manual(values = cols2) +
  geom_abline(slope = coef(central_dummylm_model)[[2]],
              intercept = coef(central_dummylm_model)[[1]],
              linetype = 'solid') +
  geom_abline(slope = coef(northern_dummylm_model)[[2]],
              intercept = coef(northern_dummylm_model)[[1]],
              linetype = 'longdash') +
  geom_segment(aes(x = 1.2, xend = 2.0, y = 114.5, yend = 114.5)) +
  geom_segment(aes(x = 1.2, xend = 2.0, y = 108, yend = 108), linetype = 'dashed') +
  scale_x_discrete(labels = substr(sample_dates2,1,3))+
  scale_y_continuous(breaks=seq(0,120,20)) +
  labs(x = "",
       y = "Relative Recovery (%)") +
  ggplot2::annotate(geom = 'text', x = 7.5, y = 90, label = central_rr_lab, parse = TRUE, size = 5) +
  ggplot2::annotate(geom = 'text', x = 7.5, y = 82, label = central_slope_lab, parse = TRUE, size = 5) +
  ggplot2::annotate(geom = 'text', x = 7.5, y = 74, label = central_rr100_lab, parse = TRUE, size = 5) +
  ggplot2::annotate(geom = 'text', x = 7.5, y = 16, label = northern_rr_lab, parse = TRUE, size = 5) +
  ggplot2::annotate(geom = 'text', x = 7.5, y = 8, label = northern_slope_lab, parse = TRUE, size = 5) +
  ggplot2::annotate(geom = 'text', x = 7.5, y = 0, label = northern_rr100_lab, parse = TRUE, size = 5) +
  theme_bw() +
  theme(plot.margin = unit(c(1, 1, 2, 1), "lines"),
        panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"),
        legend.position = c(0.135,0.9),
        legend.key = element_blank(),
        legend.background = element_blank(),
        panel.border = element_blank()) +
  guides(fill = guide_legend(nrow = 2)) +
  coord_cartesian(clip = 'off', ylim = c(0, 120)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate(geom = "text", x = 2.5, y = -25, label = 2020, size = 6) +
  annotate(geom = "text", x = 7.5, y = -25, label = 2021, size = 6) +
  annotate(geom = "text", x = 14, y = -25, label = 2022, size = 6) +
  annotate('rect', xmin = c(4.4, 10.4), xmax = c(4.6, 10.6),
           ymin = -10, ymax = -3, fill = 'white') +
  annotate('rect', xmin = 4.3, xmax = 4.7,
           ymin = 20, ymax = 28, fill = 'white') +
  annotate('rect', xmin = 10.3, xmax = 10.7,
           ymin = c(25,60), ymax = c(35,70), fill = 'white') +
  annotate('segment', x = c(4.4, 4.6, 10.4, 10.6),
           xend = c(4.4, 4.6, 10.4, 10.6), y = -10, yend = -3)

test1 <- dat %>%
  group_by(MeadowLocation,ExpAge,EdgeDistance) %>%
  summarise(MeanStem = mean(StemDensity_Meter, na.rm = T))

dat2 <- dat[dat$Treatment == "Treatment",]
dat2$EdgeDistance <- as.factor(dat2$EdgeDistance)

### Exported as width = 900 height = 650
# legend_title <- "Edge \nDistance (m)"
# cols3 <- c("0.5" = "#fc8d59", "1.5" = "#ffffbf","2.5" = "#91bfdb")
# 
# ggplot(data = dat2, aes(x = factor(ExpAge), y = StemDensity_Meter, fill = EdgeDistance)) +
#   geom_boxplot() +
# labs(x = "Experiment Age (months)",
#      y = expression(paste("Seagrass Shoot Density ( ", m^-2,")"))) +
#   scale_fill_manual(legend_title, values = cols3) +
#   scale_y_continuous(breaks = seq(0,700,50)) +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         text = element_text(size = 16),
#         axis.text.x = element_text(size = 14, color = "black"),
#         axis.text.y = element_text(size = 14, color = "black"),
#         legend.text = element_text(size = 14, color = "black"),
#         legend.title = element_text(size = 14, color = "black"),
#         legend.position = c(0.08,0.88),
#         legend.background = element_blank(),
#         strip.background = element_rect(fill="white"),
#         panel.border = element_blank()) +
#   coord_cartesian(clip = 'off', ylim = c(0, 700)) +
#   annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
#   annotate('rect', xmin = c(5.4, 11.4), xmax = c(5.6, 11.6),
#            ymin = -50, ymax = -20, fill = 'white') +
#   annotate('segment', x = c(5.4, 5.6, 11.4, 11.6),
#            xend = c(5.4, 5.6, 11.4, 11.6), y = -50, yend = -20) +
#   facet_wrap(~MeadowLocation, labeller = as_labeller(c(
#     "Central" = "Central - Treatment",
#     "Northern" = "Northern - Treatment")))

dat2 <- dat2 %>%
  mutate(Month = month(Date),
         Year = year(Date),
         date = zoo::as.yearmon(paste(Year, Month), "%Y %m"))

# Figure 7: Shoot density difference between plot edge and interior ----
### Exported as width = 1000 height = 650
legend_title <- "Edge \nDistance (m)"
cols3 <- c("0.5" = "#fc8d59", "1.5" = "#ffffbf","2.5" = "#91bfdb")

ggplot(data = dat2, aes(x = factor(date), y = StemDensity_Meter, fill = EdgeDistance)) +
  geom_boxplot() +
  labs(x = "",
       y = expression(paste("Seagrass Shoot Density ( ", m^-2,")"))) +
  scale_fill_manual(legend_title, values = cols3) +
  scale_y_continuous(breaks = seq(0,700,50)) +
  scale_x_discrete(labels=substr(sample_dates,1,3))+
  theme_bw() +
  theme(plot.margin = unit(c(1, 1, 2, 1), "lines"),
        panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        legend.position = c(0.08,0.88),
        legend.background = element_blank(),
        strip.background = element_rect(fill="white"),
        strip.text.x = element_text(size = 14, color = "black"),
        panel.border = element_blank()) +
  coord_cartesian(clip = 'off', ylim = c(0, 700)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate(geom = "text", x = c(3), y = -130, label = 2020, size = 6) +
  annotate(geom = "text", x = c(8.5), y = -130, label = 2021, size = 6) +
  annotate(geom = "text", x = c(15), y = -130, label = 2022, size = 6) +
  annotate('rect', xmin = c(5.4, 11.4), xmax = c(5.6, 11.6),
           ymin = -50, ymax = -20, fill = 'white') +
  annotate('segment', x = c(5.4, 5.6, 11.4, 11.6),
           xend = c(5.4, 5.6, 11.4, 11.6), y = -50, yend = -20) +
  facet_wrap(~MeadowLocation, labeller = as_labeller(c(
    "Central" = "Central - Treatment",
    "Northern" = "Northern - Treatment")))

# Attempt to back out the relative clonal contribution to recovery:
# assuming 1) everything in the interior in year 1 is seedling recruitment
# and 2) seedling recruitment was uniform across the plot,
# then subtracting out the interior shoot density from the exterior shoot density
# should leave clonal shoot contribution 
# clonal_recov <- dat %>%
#   filter(ExpAge > 5 & ExpAge < 17) %>%
#   group_by(ExpAge,MeadowLocation) %>%
#   summarise(Mean_Exterior = mean(StemDensity_Meter[EdgeDistance == 0.5]),
#             Mean_Interior = mean(StemDensity_Meter[EdgeDistance %in% c(1.5,2.5)]),
#             Mean_Diff = Mean_Exterior - Mean_Interior, 
#             Perc_Clonal = round(((Mean_Diff/Mean_Exterior)*100)),
#             .groups = 'drop')
# View(clonal_recov)

### three-way anova with interaction to check for differences in shoot density at
### each known edge distance for each location and sampling date
df_edgedist <- dat[,c(1,3:4,6:7,9)]
df_edgedist <- df_edgedist[df_edgedist$Treatment == "Treatment",]
df_edgedist$Date <- as.factor(df_edgedist$Date)
df_edgedist$EdgeDistance <- as.factor(df_edgedist$EdgeDistance)
aov_edgedist <- lm(StemDensity_Meter ~ EdgeDistance*MeadowLocation*Date, data = df_edgedist)
summary(aov_edgedist)
car::Anova(aov_edgedist)
# plot(aov_edgedist)
emm_edgedist <- emmeans(aov_edgedist, pairwise ~ EdgeDistance | MeadowLocation | Date )
emm_edgedist$contrasts

### How many more shoots on average were there at the 0.5 edge distance (exterior)
### than at the 1.5 m and 2.5 m (interior) plots?
df_edgedist <- dat[,c(1,3:4,6:7,9)]
df_edgedist <- df_edgedist[df_edgedist$Treatment == "Treatment",]
test <- df_edgedist %>%
  mutate(Year = year(Date),
         distance = ifelse(EdgeDistance < 1, 'exterior','interior')) %>%
  group_by(distance,MeadowLocation,Year) %>%
  summarise(MeanShootDensity = mean(StemDensity_Meter, na.rm = T)) %>%
  ungroup() %>%
  group_by(MeadowLocation)

test <- test %>%
  arrange(-desc(Year),MeadowLocation) %>%
  mutate(diff = round(MeanShootDensity - lag(MeanShootDensity)),
         diff = ifelse(row_number()%%2==1,NA,diff)) %>%
  filter(!Year == 2020)
test # note that in 2022 the exterior shoot density was greater than interior shoot density

# https://en.wikipedia.org/wiki/Transient_response
# https://en.wikipedia.org/wiki/Damping
