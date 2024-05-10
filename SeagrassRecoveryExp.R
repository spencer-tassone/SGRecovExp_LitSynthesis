library(tidyverse)
library(data.table)
library(RColorBrewer)
library(emmeans)
library(matrixTests)

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

# three-way anova with interaction to check for differences in shoot density at
# each meadow location (central vs. edge) and sampling date
aov_control <- lm(stemdensity_control ~ Site_no * MeadowLocation * Date, data = control)
aov_treat <- lm(stemdensity_treatment ~ Site_no * MeadowLocation * Date, data = treatment)
emm_control <- emmeans(aov_control, pairwise ~ Site_no | MeadowLocation | Date)
emm_treat <- emmeans(aov_treat, pairwise ~ Site_no | MeadowLocation | Date)
emm_control$contrasts # 3 statistically significant differences out of 108 comparisons
emm_treat$contrasts # 0 statistically significant differences out of 108 comparisons
# 18 sampling dates * 2 locations (central & edge) * 3 sites per location = 108 tests

# Treatment density differences by meadow location and date ----
aov_test <- lm(stemdensity_treatment ~ MeadowLocation * Date, data = treatment)
emm_test <- emmeans(aov_test, pairwise ~ MeadowLocation | Date)
emm_test$contrasts

# Removal Efficiency ----
dat %>%
  filter(Date %in% as.Date('2020-07-15')) %>%
  select(Date,Site_no,Treatment,StemDensity_Meter) %>%
  group_by(Site_no,Treatment) %>%
  summarise(Mean_StemDensity_Meter = round(mean(StemDensity_Meter))) %>%
  spread(key = Treatment, value = Mean_StemDensity_Meter) %>%
  mutate(Frac = round(100-((Treatment/Control)*100)),
         MeadowLocation = ifelse(Site_no <= 3, 'Central','Edge')) %>%
  group_by(MeadowLocation) %>%
  summarise(RemovalEfficiency = round(mean(Frac)),
            SD_RemovalEfficiency = round(sd(Frac)))

# Removal efficiency at central = 93 +/- 1 and at edge = 85 +/- 6
# Is this difference in removal efficiency statistically significant?
# Conduct two-sample independent t-test
aa <- dat %>%
  filter(Date %in% as.Date('2020-07-15')) %>%
  select(Date,Site_no,Treatment,StemDensity_Meter) %>%
  group_by(Site_no,Treatment) %>%
  summarise(Mean_StemDensity_Meter = round(mean(StemDensity_Meter))) %>%
  spread(key = Treatment, value = Mean_StemDensity_Meter) %>%
  mutate(Frac = round(100-((Treatment/Control)*100)),
         MeadowLocation = ifelse(Site_no <= 3, 'Central','Edge'))

t.test(Frac ~ MeadowLocation, data = aa, var.equal = TRUE)

# p-value = 0.08 which is greater than alpha value of 0.05, so we cannot reject the null hypothesis that there is no difference between groups
bb <- dat %>%
  group_by(Date,LocTreat) %>%
  summarise(MeanStemDensity = round(mean(StemDensity_Meter, na.rm = T)),
            SD = round(sd(StemDensity_Meter, na.rm = T))) %>%
  mutate(upper = MeanStemDensity + SD,
         lower = MeanStemDensity - SD,
         Month = month(Date),
         Year = year(Date),
         date = zoo::as.yearmon(paste(Year, Month), "%Y %m"),
         LocTreat = ifelse(LocTreat == "Northern Control","Edge Control",LocTreat),
         LocTreat = ifelse(LocTreat == "Northern Treatment","Edge Treatment",LocTreat))

sample_dates <- unique(bb$date)

# Figure 3: timeseries of absolute shoot density ----
### saved as width = 800 height = 600
# For plotting purposes, need to adjust sampling dates of Oct 2020 & Oct 2021 (difficult to see on discontinuous axis)
bb <- bb %>%
  mutate(Date = if_else(Date == as.Date('2020-10-14'), as.Date('2020-10-07'), Date)) %>%
  mutate(Date = if_else(Date == as.Date('2021-10-20'), as.Date('2021-10-07'), Date))

cols <- c("Central Control" = "#ca0020", "Central Treatment" = "#f48282",
          "Edge Control" = "#0571b0", "Edge Treatment" = "#92c5de")

ggplot(data = bb, aes(x = factor(date), y = MeanStemDensity, color = LocTreat, group = paste(Year,LocTreat))) +
  annotate(geom = 'rect', fill = 'gray75',
           xmin = 13.2, xmax = 13.4, # 30 days in June so 6/30 = 0.2 and 12/30 = 0.4
           ymin = -Inf, ymax = Inf) + # add storm event (May 6-12, 2022)
  geom_line(aes(y = MeanStemDensity, x = as.numeric(factor(Date)) + (day(Date)-1)/ days_in_month(Date))) +
  geom_errorbar(aes(ymin = bb$lower, ymax = bb$upper), alpha = 0) +
  geom_errorbar(aes(ymin = bb$lower, ymax = bb$upper, x = as.numeric(factor(Date)) + (day(Date)-1)/ days_in_month(Date)),
                color = "black", width = 0.2, alpha = 0.4) +
  geom_point(aes(fill = LocTreat, shape = LocTreat), alpha = 0) + # hidden, to get axis labels
  geom_point(aes(fill = LocTreat, shape = LocTreat, x = as.numeric(factor(Date)) + (day(Date)-1)/ days_in_month(Date)),
             size = 3, color = "black", alpha = 0.7) +
  # geom_point(size = 3, aes(fill = LocTreat, shape = LocTreat), color = "black", alpha = 0.7) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  scale_shape_manual(values = c(`Central Control` = 21, `Central Treatment` = 21,
                                `Edge Control` = 24, `Edge Treatment` = 24)) +
  scale_x_discrete(labels = substr(sample_dates,1,3))+
  scale_y_continuous(breaks=seq(0,650,50)) +
  labs(x = "",
       y = expression(paste("Mean (± SD) Shoot Density (ind. ", ~ m^-2,")"))) +
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
  annotate(geom = "text", x = c(3,8.5,15), y = -130, label = unique(bb$Year), size = 6) +
  annotate('rect',
           xmin = c(5.35, 11.35),
           xmax = c(5.65, 11.65),
           ymin = -50, ymax = -15, fill = 'white') +
  annotate('segment',
           x = c(5.35, 5.65, 11.35, 11.65),
           xend = c(5.35, 5.65, 11.35, 11.65), y = -50, yend = -20)

bb %>%
  group_by(LocTreat) %>%
  summarise(MaxSD = max(SD)) %>%
  filter(LocTreat == 'Edge Treatment') # +/- 107 shoots m-2

y_lab <-"Edge Treatment<br>Shoot Density Standard Deviation (ind. m<sup> -2</sup>)"

# Timeseries of edge treatment site shoot standard deviation ----
bb %>%
  filter(LocTreat %in% 'Edge Treatment') %>%
  ggplot(aes(x = Date, y = SD)) +
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

# Maximum relative recovery for each location and year ----
cc <- dat %>%
  group_by(Date, MeadowLocation, Treatment) %>%
  summarise(MeanStem = round(mean(StemDensity_Meter, na.rm = T))) %>%
  spread(key = Treatment, value = MeanStem) %>%
  mutate(fraction = round((Treatment / Control) * 100, 1)) %>%
  data.frame() %>%
  mutate(ExpAge = c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15,
                    16, 16, 22, 22, 23, 23, 24, 24, 25, 25, 26, 26, 27, 27, 28, 28)) %>%
  filter(!ExpAge == 0) %>%
  mutate(Month = month(Date),
         Year = year(Date),
         date = zoo::as.yearmon(paste(Year, Month), "%Y %m"))

cc %>%
  group_by(Year,MeadowLocation) %>%
  summarise(MaxFrac = round(max(fraction, na.rm = T)))

# Welch t-test for shoot density at each location and date ----
# Welch's used bc of unequal variances. Used to determine if there are significant 
# differences between control and treatment site shoot density for each date at each location

# removing Sept 2022 central sites due to missing data (poor weather prevented sampling)
dd <- dat %>%
  filter(!c(Date == as.Date('2022-09-12') & MeadowLocation == 'Central')) %>%
  select(Date,ExpAge,MeadowLocation,Treatment,StemDensity_Meter)

table1 <- dd %>% 
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
  select(Date,ExpAge,MeadowLocation,pvalue,mean.x,mean.y,mean.diff,stderr,obs.x,obs.y,conf.low,conf.high) %>%
  data.frame() %>%
  mutate(pvalue = round(p.adjust(pvalue, method = "BH"), 3),
         mean.diff = round(mean.diff),
         sd = stderr * sqrt(obs.x),
         lower = mean.diff - sd,
         upper = mean.diff + sd,
         sig = ifelse(pvalue < 0.05, "Sig", "Not Sig"),
         Month = month(Date),
         Year = year(Date),
         date = zoo::as.yearmon(paste(Year, Month), "%Y %m"),
         MeadowLocation = ifelse(MeadowLocation == 'Northern', 'Edge', MeadowLocation))

View(table1)

# Shoot density difference ----
### Exported as width = 1000 height = 650
table1 %>%
  ggplot(aes(y = mean.diff, x = factor(date))) +
  geom_linerange(ymin = table1$conf.low, ymax = table1$conf.high,
                 color = "black", linewidth = 3.5, alpha = 0.35) +
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

central_lm_dat <- cc[cc$MeadowLocation == "Central",]
northern_lm_dat <- cc[cc$MeadowLocation == "Northern",]
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
round(100/central_slopeCI[1]/30.417,) # Relative recovery estimated 95% CI to take up to 30 months using linear regression. 
round(100/central_slopeCI[2]/30.417,) # Relative recovery estimated 95% CI to take as few as 22 months using linear regression. 
central_rr_lab <- expression(paste(RR[max] == 114,"%"))
central_slope_lab <- expression(paste(Slope == 0.13," ± ",0.01)) # comes from Experiment Age
central_rr100_lab <- expression(paste(RR[100] == 26," months" ))
round(max(northern_lm_dat$fraction)) # Maximum recovery 43%
round(summary(northern_lm_model)$coefficients[2],3) # slope = 0.021
round(summary(northern_lm_model)$coefficients[4],2) # slope standard error = 0.01
round((100/summary(northern_lm_model)$coefficients[2])/30.417,) # Relative recovery estimated to take 158 months using linear regression. Need to divide by 30.417 to convert from number of days til 100% RR to number of months
northern_slopeCI <- confint(northern_lm_model, 'Date', level = 0.95) # 95% confidence interval
round(100/northern_slopeCI[1]/30.417,) # Relative recovery estimated 95% CI to take up to 9035 months using linear regression. 
round(100/northern_slopeCI[2]/30.417,) # Relative recovery estimated 95% CI to take as few as 80 months using linear regression. 
northern_rr_lab <- expression(paste(RR[max] == 43,"%"))
northern_slope_lab <- expression(paste(Slope == 0.021," ± ",0.01)) # comes from Experiment Age
northern_rr100_lab <- expression(paste(RR[100] == 158," months"))

cc$sample <- rep(1:17, each = 2)

central_dummylm_dat <- cc[cc$MeadowLocation == "Central",]
northern_dummylm_dat <- cc[cc$MeadowLocation == "Northern",]
central_dummylm_model <- lm(fraction~central_dummylm_dat$sample, data = central_lm_dat)
northern_dummylm_model <- lm(fraction~northern_dummylm_dat$sample, data = northern_lm_dat)

# Figure 4: Relative recovery of shoot density ----
### saved as width = 800 height = 600
# For plotting purposes, need to adjust sampling dates of Oct 2020 & Oct 2021 (difficult to see on discontinous axis)
cc <- cc %>%
  mutate(Date = if_else(Date == as.Date('2020-10-14'), as.Date('2020-10-07'), Date)) %>%
  mutate(Date = if_else(Date == as.Date('2021-10-20'), as.Date('2021-10-07'), Date))

sample_dates2 <- unique(cc$date)

cc$MeadowLocation <- ifelse(cc$MeadowLocation == 'Northern','Edge',cc$MeadowLocation)
cols2 <- c("Central" = "black", "Edge" = "white")

ggplot(data = cc, aes(x = factor(date), y = fraction, group = MeadowLocation)) +
  annotate(geom = 'rect', fill = 'gray75',
           xmin = 12.2, xmax = 12.4, # 30 days in June so 6/30 = 0.2 and 12/30 = 0.4
           ymin = -Inf, ymax = Inf) + # add storm event (May 6-12, 2022)
  geom_abline(slope = coef(central_dummylm_model)[[2]],
              intercept = coef(central_dummylm_model)[[1]],
              linetype = 'solid') +
  geom_abline(slope = coef(northern_dummylm_model)[[2]],
              intercept = coef(northern_dummylm_model)[[1]],
              linetype = 'longdash') +
  geom_point(aes(fill = MeadowLocation), alpha = 0) + # hidden, to get axis labels
  geom_point(aes(fill = MeadowLocation,
                 x = as.numeric(factor(Date)) + (day(Date)-1)/ days_in_month(Date)),
             size = 3, shape = 21, color = "black") +
  scale_fill_manual(values = cols2) +
  geom_segment(aes(x = 1.35, xend = 2.15, y = 114.5, yend = 114.5)) +
  geom_segment(aes(x = 1.35, xend = 2.15, y = 108, yend = 108), linetype = 'dashed') +
  scale_x_discrete(labels = substr(sample_dates2,1,3))+
  scale_y_continuous(breaks=seq(0,120,20)) +
  labs(x = "",
       y = "Relative Recovery (%)") +
  annotate(geom = 'text', x = 7.5, y = 90, label = central_rr_lab, parse = TRUE, size = 5) +
  annotate(geom = 'text', x = 7.5, y = 82, label = central_slope_lab, parse = TRUE, size = 5) +
  annotate(geom = 'text', x = 7.5, y = 74, label = central_rr100_lab, parse = TRUE, size = 5) +
  annotate(geom = 'text', x = 7.5, y = 16, label = northern_rr_lab, parse = TRUE, size = 5) +
  annotate(geom = 'text', x = 7.5, y = 8, label = northern_slope_lab, parse = TRUE, size = 5) +
  annotate(geom = 'text', x = 7.5, y = 0, label = northern_rr100_lab, parse = TRUE, size = 5) +
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

# Figure 7: Shoot density difference between plot edge and interior ----
### Exported as width = 1000 height = 650

ee <- dat %>%
  filter(Treatment == 'Treatment') %>%
  mutate(EdgeDistance = as.factor(EdgeDistance),
         Month = month(Date),
         Year = year(Date),
         date = zoo::as.yearmon(paste(Year, Month), "%Y %m"))

legend_title <- "Edge \nDistance (m)"
cols3 <- c("0.5" = "#fc8d59", "1.5" = "#ffffbf","2.5" = "#91bfdb")

ee$MeadowLocation <- ifelse(ee$MeadowLocation == 'Northern','Edge',ee$MeadowLocation)

ggplot(data = ee, aes(x = factor(date), y = StemDensity_Meter, fill = EdgeDistance)) +
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
    "Edge" = "Edge - Treatment")))

### three-way anova with interaction to check for differences in shoot density at
### each known edge distance for each location and sampling date

df_edgedist <- dat %>%
  filter(Treatment == 'Treatment') %>%
  select(Date, MeadowLocation, Treatment, Site_name, EdgeDistance, StemDensity_Meter) %>%
  mutate(Date = as.factor(Date),
         EdgeDistance = as.factor(EdgeDistance))

aov_edgedist <- lm(StemDensity_Meter ~ EdgeDistance*MeadowLocation*Date, data = df_edgedist)
summary(aov_edgedist)
car::Anova(aov_edgedist)
# plot(aov_edgedist)
emm_edgedist <- emmeans(aov_edgedist, pairwise ~ EdgeDistance | MeadowLocation | Date )
emm_edgedist$contrasts

### How many more shoots on average were there at the 0.5 edge distance (exterior)
### than at the 1.5 m and 2.5 m (interior) plots?

dat %>%
  filter(Treatment == 'Treatment') %>%
  select(Date, MeadowLocation, Treatment, Site_name, EdgeDistance, StemDensity_Meter) %>%
  mutate(Year = year(Date),
         distance = ifelse(EdgeDistance < 1, 'exterior','interior')) %>%
  group_by(distance,MeadowLocation,Year) %>%
  summarise(MeanShootDensity = mean(StemDensity_Meter, na.rm = T)) %>%
  ungroup() %>%
  group_by(MeadowLocation) %>%
  arrange(-desc(Year),MeadowLocation) %>%
  mutate(diff = round(MeanShootDensity - lag(MeanShootDensity)),
         diff = ifelse(row_number()%%2==1,NA,diff)) %>%
  filter(!Year == 2020) # note that in 2022 the exterior shoot density (0.5 meter) was, on average, greater than the combined interior shoot density (1.5 m and 2.5 m)
