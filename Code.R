library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

rm(list = ls())
dev.off()

setwd("F:/School/SeagrassRecovery/Data")

dat <- fread('StemDensity.csv')

dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
dat$StemDensity_Meter <- dat$StemDensity_QrtMeter * 4
dat$LocTreat <- paste(dat$MeadowLocation, dat$Treatment)
dat$SiteTreat <- paste(dat$Site_no, dat$Treatment)

test <- dat %>%
  group_by(ExpAge,LocTreat) %>%
  summarise(MeanStemDensity = round(mean(StemDensity_Meter),0),
            SD = round(sd(StemDensity_Meter),0))

test$lower <- test$MeanStemDensity - test$SD
test$upper <- test$MeanStemDensity + test$SD

test_year1 <- test[1:20,]
test_year2 <- test[21:44,]

cols <- c("Central Control" = "#d7191c", "Central Treatment" = "#fdae61",
          "Northern Control" = "#2c7bb6", "Northern Treatment" = "#7bccc4")

### saved as width = 800 height = 600
# ggplot(data = test, aes(x = ExpAge, y = MeanStemDensity, color = LocTreat)) +
#   geom_errorbar(ymin = test$lower, ymax = test$upper,
#                 color = "black", width = 0.2, alpha = 0.2) +
#   geom_line() +
#   geom_point(size = 3) +
#   scale_color_manual(values = cols) +
#   scale_x_continuous(limits = c(0,16.5),
#                      breaks=seq(0,16.5,1)) +
#   scale_y_continuous(limits = c(0,600),
#                      breaks=seq(0,600,100)) +
#   labs(x = "Experiment Age (months)",
#        y = expression(paste("Mean Stem Density ( ", m^-2,")"))) +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         text = element_text(size = 16),
#         axis.text.x = element_text(size = 14, color = "black"),
#         axis.text.y = element_text(size = 14, color = "black"),
#         legend.title = element_blank(),
#         legend.direction = "horizontal",
#         legend.margin = margin(),
#         legend.position = c(0.5,0.95))

### saved as width = 800 height = 600
ggplot(data = test_year1, aes(x = ExpAge, y = MeanStemDensity, color = LocTreat)) +
  geom_errorbar(ymin = test_year1$lower, ymax = test_year1$upper,
                color = "black", width = 0.2, alpha = 0.2) +
  geom_line() +
  geom_point(size = 3) +
  geom_line(data = test_year2, aes(x = ExpAge, y = MeanStemDensity, color = LocTreat)) +
  geom_errorbar(data = test_year2, aes(x = ExpAge, y = MeanStemDensity),
                ymin = test_year2$lower, ymax = test_year2$upper,
                color = "black", width = 0.2, alpha = 0.2) +
  geom_point(data = test_year2, aes(x = ExpAge, y = MeanStemDensity), size = 3) +
  scale_color_manual(values = cols) +
  scale_x_continuous(limits = c(0,16.5),
                     breaks=seq(0,16.5,1)) +
  scale_y_continuous(limits = c(0,600),
                     breaks=seq(0,600,100)) +
  labs(x = "Experiment Age (months)",
       y = expression(paste("Mean Shoot Density ( ", m^-2,")"))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.margin = margin(),
        legend.position = c(0.5,0.95))

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

recov_spread <- rbind(june20_spread, july20_spread,
                      aug20_spread, sept20_spread,
                      oct20_spread,may21_spread,
                      june21_spread,july21_spread,
                      aug21_spread,sept21_spread,oct21_spread)
recov_spread <- as.data.frame(recov_spread)

recov_spread$ExpAge <- c(0,0,1,1,2,2,3,3,
                         4,4,11,11,12,12,
                         13,13,14,14,15,15,16,16)

recov_spread <- recov_spread[3:22,]
recov_spread$logResponseRatio <- log(recov_spread$Treatment/recov_spread$Control)
recov_spread[5,7] <- NA

cols2 <- c("Central" = "black", "Northern" = "white")

ggplot(data = recov_spread, aes(x = Date, y = logResponseRatio, group = MeadowLocation)) +
  geom_line(aes(fill = MeadowLocation)) +
  geom_point(aes(fill = MeadowLocation), size = 3, shape = 21) +
  scale_fill_manual(values = cols2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(-5,00),
                     breaks=seq(-5,0,1)) +
  labs(x = "",
       y = expression(ln~(~frac(italic(bar(x))[T],italic(bar(x))[C])))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 14, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"),
        legend.position = c(0.15,0.9),
        legend.key = element_blank(),
        legend.background = element_blank())

central_lm_dat <- recov_spread[recov_spread$MeadowLocation == "Central",]
northern_lm_dat <- recov_spread[recov_spread$MeadowLocation == "Northern",]
central_lm_dat <- central_lm_dat[2:7,]
northern_lm_dat <- northern_lm_dat[2:7,]
central_lm_model <- lm(fraction~ExpAge, data = central_lm_dat)
northern_lm_model <- lm(fraction~ExpAge, data = northern_lm_dat)
summary(central_lm_model)
summary(northern_lm_model)
round(100/summary(central_lm_model)$coefficients[2],1)
round(100/summary(northern_lm_model)$coefficients[2],1)

central_rr_lab <- expression(paste(RR[max] == 54,"%"))
central_slope_lab <- expression(paste(Slope == 5.1," ± ",0.3))
central_rr100_lab <- expression(paste(RR[100] == 19.5," months"))
northern_rr_lab <- expression(paste(RR[max] == 43,"%"))
northern_slope_lab <- expression(paste(Slope == 2.9," ± ",0.3))
northern_rr100_lab <- expression(paste(RR[100] == 34.3," months"))

### saved as height = 700 width = 600

ggplot(data = recov_spread, aes(x = ExpAge, y = fraction, group = MeadowLocation)) +
  geom_point(aes(fill = MeadowLocation), size = 3, shape = 21) +
  # geom_point(size = 3, shape = 21, color = "black") +
  scale_fill_manual(values = cols2) +
  geom_abline(slope = coef(central_lm_model)[[2]],
              intercept = coef(central_lm_model)[[1]],
              linetype = 'solid') +
  geom_abline(slope = coef(northern_lm_model)[[2]],
              intercept = coef(northern_lm_model)[[1]],
              linetype = 'longdash') +
  geom_segment(aes(x = 1.2, xend = 1.9, y = 95.25, yend = 95.25)) +
  geom_segment(aes(x = 1.2, xend = 1.9, y = 90.7, yend = 90.7), linetype = 'dashed') +
  scale_x_continuous(limits = c(1,16),
                     breaks=seq(1,16,1)) +
  scale_y_continuous(limits = c(0,100),
                     breaks=seq(0,100,10)) +
  labs(x = "Experiment Age (months)",
       y = "Relative Recovery (%)") +
  ggplot2::annotate(geom = 'text', x = 12, y = 75, label = central_rr_lab, parse = TRUE, size = 5) +
  ggplot2::annotate(geom = 'text', x = 12, y = 70, label = central_slope_lab, parse = TRUE, size = 5) +
  ggplot2::annotate(geom = 'text', x = 12, y = 65, label = central_rr100_lab, parse = TRUE, size = 5) +
  ggplot2::annotate(geom = 'text', x = 12, y = 25, label = northern_rr_lab, parse = TRUE, size = 5) +
  ggplot2::annotate(geom = 'text', x = 12, y = 20, label = northern_slope_lab, parse = TRUE, size = 5) +
  ggplot2::annotate(geom = 'text', x = 12, y = 15, label = northern_rr100_lab, parse = TRUE, size = 5) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black"),
        legend.position = c(0.15,0.9),
        legend.key = element_blank(),
        legend.background = element_blank())

test1 <- dat %>%
  group_by(MeadowLocation,ExpAge,EdgeDistance) %>%
  summarise(MeanStem = mean(StemDensity_Meter))

dat2 <- dat[37:396,]
# dat2$MeadowLocation <- as.factor(dat2$MeadowLocation)
# dat2$Treatment <- as.factor(dat2$Treatment)
# dat2$ExpAge <- as.factor(dat2$ExpAge)
# dat2$LocTreat <- as.factor(dat2$LocTreat)
# m1 <- aov(StemDensity_Meter ~ LocTreat * ExpAge, data = dat2)
# m1 <- aov(StemDensity_Meter ~ MeadowLocation * ExpAge * EdgeDistance, data = dat2)
# summary(m1)
# TukeyHSD(m1)

dat3 <- dat2[dat2$Treatment == "Treatment",]
dat3$EdgeDistance <- as.factor(dat3$EdgeDistance)

### Exported as width = 800 height = 600
legend_title <- "Edge Distance (m)"
cols3 <- c("0.5" = "#fc8d59", "1.5" = "#ffffbf","2.5" = "#91bfdb")

ggplot(data = dat3, aes(x = factor(ExpAge), y = StemDensity_Meter, fill = EdgeDistance)) +
  geom_boxplot() +
labs(x = "Experiment Age (months)",
     y = expression(paste("Seagrass Shoot Density ( ", m^-2,")"))) +
scale_fill_manual(legend_title, values = cols3) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 16, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        legend.position = c(0.13,0.86),
        strip.background =element_rect(fill="white")) +
  facet_wrap(~MeadowLocation, labeller = as_labeller(c(
    "Central" = "Central - Treatment",
    "Northern" = "Northern - Treatment")))

# Attempt to back out the relative clonal contribution to recovery
test2 <- dat3[73:180,c(2:3,6:7,9)]
test2 %>%
  group_by(ExpAge,MeadowLocation) %>%
  summarise(Mean_Ext = mean(StemDensity_Meter[EdgeDistance == 0.5]),
            Mean_Int = mean(StemDensity_Meter[EdgeDistance %in% c(1.5,2.5)]),
            Mean_Diff = Mean_Ext - Mean_Int,
            Perc_Clonal = round(((Mean_Diff/Mean_Ext)*100),),
            .groups = 'drop')

# library(emmeans)
# 
# dat2 <- dat
# 
# dat2$ExpAge <- factor(dat2$ExpAge)
# dat2$Treatment <- factor(dat2$Treatment)
# dat2$MeadowLocation <- factor(dat2$MeadowLocation)
# m1 <- lm(StemDensity_Meter ~ ExpAge * Treatment * MeadowLocation, data = dat)
# emm <- emmeans(m1,  ~ Treatment | ExpAge, by = "MeadowLocation")
# emm <- emmeans(m1,  ~ pairwise | Treatment, by = "MeadowLocation")
# emm_pairs <- pairs(emm, by = c("ExpAge","MeadowLocation"))
# emm_pairs
# 
# my_labeller <- as_labeller(function(x){
#   return(paste0("", x))
# })
# 
# plot(emm_pairs, by = "MeadowLocation", CIs = TRUE) +
#   labs(x = "Mean Stem Density Difference (C-T)",
#        y = "Experiment Age (months)") +
#   geom_vline(xintercept = 0, linetype = "dashed") +
#   scale_y_discrete(labels = c("0","1","2","3","4",
#                               "11","12","13","14")) +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         text = element_text(size = 16),
#         axis.text.x = element_text(size = 14, color = "black"),
#         axis.text.y = element_text(size = 14, color = "black"),
#         strip.background =element_rect(fill="white")) +
#   facet_wrap(~MeadowLocation, labeller = my_labeller)

library(matrixTests)
library(tidyverse)

dat4 <- dat[,c(2:4,9)]
colnames(dat4)[1] <- "Age"
colnames(dat4)[2] <- "Location"
colnames(dat4)[4] <- "Value"

table1 <- dat4 %>% 
  group_nest(Age,Location,Treatment) %>% 
  pivot_wider(names_from = Treatment,values_from = data) %>% 
  mutate(
    test = map2(
      .x = Control,
      .y = Treatment,
      .f = ~col_t_welch(.x,.y)
    )
  ) %>% 
  unnest(test) %>% 
  select(Age,Location,pvalue,mean.diff,stderr)

table1 <- as.data.frame(table1)
table1$pval_corrected <- p.adjust(table1$pvalue, method = "BH")
table1$lower <- table1$mean.diff - table1$stderr
table1$upper <- table1$mean.diff + table1$stderr

table1 %>%
  ggplot(aes(y = mean.diff, x = factor(Age))) +
  geom_linerange(ymin = table1$lower, ymax = table1$upper,
                 color = "black", size = 3.5, alpha = 0.35) +
  geom_point(size = 4) +
  scale_y_continuous(breaks = seq(-50,500,100),
                     limits = c(-50,500)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Mean Shoot Density Difference (± SE)",
       x = "Experiment Age (months)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        strip.background =element_rect(fill="white")) +
  facet_wrap(~Location)
