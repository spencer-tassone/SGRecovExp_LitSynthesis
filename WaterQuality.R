library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggpubr)
library(Tides)
library(emmeans)
library(kableExtra)

rm(list = ls())
dev.off()

setwd("D:/School/SeagrassRecovery/Data")
dat <- read.csv('WaterQuality_forR.csv')
dat_benthic <- read.csv('CHL_benthic.csv')
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y", tz = "America/Jamaica")
dat_benthic$Date <- as.Date(dat_benthic$Date, format = "%m/%d/%Y", tz = "America/Jamaica")
dat$LocTreat <- paste(dat$Location, dat$Treatment)
dat$SiteTreat <- paste(dat$Site, dat$Treatment)

setwd("D:/School/SeagrassRecovery/Data/HOBO/Temperature")
dat_temp <- read.csv('ALLTEMP.csv')
dat_temp$DateTime <- as.POSIXct(dat_temp$DateTime, tz = "America/Jamaica", "%m/%d/%Y %H:%M")
colnames(dat_temp)[2:17] <- c("1C","1T","2C","2C_SED","2T","2T_SED","3C","3T",
                              "4C","4T","5C","5C_SED","5T","5T_SED","6C","6T")

dat_temp_long <- dat_temp %>%
  pivot_longer(!DateTime, names_to = "Site", values_to = "Temperature_C") %>%
  arrange(Site) %>%
  mutate(Location = ifelse(Site %in% c("1C","1T","2C","2C_SED","2T","2T_SED","3C","3T"),"Central","Northern"))

setwd("D:/School/SeagrassRecovery/Data/HOBO/WaterLevel")
dat_wlevel <- read.csv('ALLWLEVEL.csv')
colnames(dat_wlevel)[2:6] <- c("1C","3T","4C","5T","6T")
dat_wlevel$DateTime <- as.POSIXct(dat_wlevel$DateTime, tz = "America/Jamaica", "%Y-%m-%d %H:%M")
dat_wlevel_long <- dat_wlevel %>%
  pivot_longer(!DateTime, names_to = "Site", values_to = "waterlevel_m") %>%
  arrange(Site) %>%
  mutate(Location = ifelse(Site %in% c("1C","3T"),"Central","Northern"))

depth_table <- dat_wlevel_long %>%
  group_by(Location) %>%
  summarise(meandepth_m = round(mean(waterlevel_m),1))

tide1C <- dat_wlevel_long[dat_wlevel_long$Site == '1C',]
tide3T <- dat_wlevel_long[dat_wlevel_long$Site == '3T',]
tide4C <- dat_wlevel_long[dat_wlevel_long$Site == '4C',]
tide5T <- dat_wlevel_long[dat_wlevel_long$Site == '5T',]
tide6T <- dat_wlevel_long[dat_wlevel_long$Site == '6T',]
tide1C <- tide1C[9:2541,c(1,3)]
tide3T <- tide3T[9:2541,c(1,3)]
tide4C <- tide4C[9:2540,c(1,3)]
tide5T <- tide5T[9:2541,c(1,3)]
tide6T <- tide6T[9:2540,c(1,3)]
colnames(tide1C)[1:2] <- c("time","h")
colnames(tide3T)[1:2] <- c("time","h")
colnames(tide4C)[1:2] <- c("time","h")
colnames(tide5T)[1:2] <- c("time","h")
colnames(tide6T)[1:2] <- c("time","h")

tc1C <- TidalCharacteristics(tide1C, h0 = 0.001, hoffset = 0, dtMax = 15, unit = "mins")
tc3T <- TidalCharacteristics(tide3T, h0 = 0.001, hoffset = 0, dtMax = 15, unit = "mins")
tc4C <- TidalCharacteristics(tide4C, h0 = 0.001, hoffset = 0, dtMax = 15, unit = "mins")
tc5T <- TidalCharacteristics(tide5T, h0 = 0.001, hoffset = 0, dtMax = 15, unit = "mins")
tc6T <- TidalCharacteristics(tide6T, h0 = 0.001, hoffset = 0, dtMax = 15, unit = "mins")
# plot(tc1C)
# plot(tc3T)
# plot(tc4C)
# plot(tc5T)
# plot(tc6T)

tc3T <- data.frame(tc3T$HL)
tc3T <- tc3T[c(1:57,60,63:NROW(tc3T)),]
test <- diff(tc3T$time, lag = 1)

tide_table <- data.frame(matrix(ncol = 3, nrow = 5))
colnames(tide_table)[1:3] <- c("Site","MeanDepth_m","MeanTidalRange_m")
tide_table$Site <- c("1C","3T","4C","5T","6T")
tide_table[1,2] <- round(mean(tide1C$h),2)
tide_table[1,3] <- round(mean(abs(diff(tc1C$HL$h, lag = 1))),2)
tide_table[2,2] <- round(mean(tide3T$h),2)
tide_table[2,3] <- round(mean(abs(diff(tc3T$h, lag = 1))),2)
tide_table[3,2] <- round(mean(tide4C$h),2)
tide_table[3,3] <- round(mean(abs(diff(tc4C$HL$h, lag = 1))),2)
tide_table[4,2] <- round(mean(tide5T$h),2)
tide_table[4,3] <- round(mean(abs(diff(tc5T$HL$h, lag = 1))),2)
tide_table[5,2] <- round(mean(tide6T$h),2)
tide_table[5,3] <- round(mean(abs(diff(tc6T$HL$h, lag = 1))),2)

tiderange1C <- data.frame(round(abs(diff(tc1C$HL$h, lag = 1)),2))
tiderange1C$Site <- "1C"
colnames(tiderange1C)[1] <- "tiderange_m"
tiderange3T <- data.frame(round(abs(diff(tc3T$h, lag = 1)),2))
tiderange3T$Site <- "3T"
colnames(tiderange3T)[1] <- "tiderange_m"
central_tiderange <- bind_rows(tiderange1C,tiderange3T)
depth_table$meantidalrange_m <- NA
depth_table[1,3] <- round(mean(central_tiderange$tiderange_m),1)

tiderange4C <- data.frame(round(abs(diff(tc4C$HL$h, lag = 1)),2))
tiderange4C$Site <- "4C"
colnames(tiderange4C)[1] <- "tiderange_m"
tiderange5T <- data.frame(round(abs(diff(tc5T$HL$h, lag = 1)),2))
tiderange5T$Site <- "5T"
colnames(tiderange5T)[1] <- "tiderange_m"
tiderange6T <- data.frame(round(abs(diff(tc6T$HL$h, lag = 1)),2))
tiderange6T$Site <- "6T"
colnames(tiderange6T)[1] <- "tiderange_m"
northern_tiderange <- bind_rows(tiderange4C,tiderange5T,tiderange6T)
depth_table[2,3] <- round(mean(northern_tiderange$tiderange_m),1)

wq_table <- dat %>%
  group_by(Location) %>%
  summarise(MeanDO = round(mean(DO_pct, na.rm = T),1),
            SD_DO = round(sd(DO_pct, na.rm = T),1),
            MeanSalinity = round(mean(Salinity, na.rm = T),1),
            SD_Salinity = round(sd(Salinity, na.rm = T),1),
            MeanTurbidity = round(mean(Turbidity, na.rm = T),1),
            SD_Turbidity = round(sd(Turbidity, na.rm = T),1),
            MeanTSS = round(mean(TSS, na.rm = T),1),
            SD_TSS = round(sd(TSS, na.rm = T),1),
            MeanCHL_pelagic = round(mean(CHL_pelagic, na.rm = T),1),
            SD_CHL_pelagic = round(sd(CHL_pelagic, na.rm = T),1))

benthic_table <- dat_benthic %>%
  group_by(Location) %>%
  summarise(MeanCHL_benthic = round(mean(CHL_benthic, na.rm = T),1),
            SD_CHL_benthic = round(sd(CHL_benthic, na.rm = T),1))

dat_wtemp <- dat_temp_long %>%
  subset(!Site %in% c("2C_SED", "2T_SED", "5C_SED", "5T_SED"))

wtemp_table <- dat_temp_long %>%
  subset(!Site %in% c("2C_SED", "2T_SED", "5C_SED", "5T_SED")) %>%
  group_by(Location) %>%
  summarise(MeanWTemp = round(mean(Temperature_C, na.rm = T),1),
            SD_WTemp = round(sd(Temperature_C, na.rm = T),1))

sedtemp_table <- dat_temp_long %>%
  subset(Site %in% c("2C_SED", "2T_SED", "5C_SED", "5T_SED")) %>%
  group_by(Location) %>%
  summarise(MeanSedTemp = round(mean(Temperature_C, na.rm = T),1),
            SD_SedTemp = round(sd(Temperature_C, na.rm = T),1))

wq_table <- merge(wq_table, benthic_table, by = 'Location', all = TRUE)
wq_table <- merge(wq_table, wtemp_table, by = 'Location', all = TRUE)

df <- data.frame(matrix(ncol = 4, nrow = 8))
colnames(df)[1:4] <- c("Variable","Central","Edge","p-value")
df$Variable <- c("Salinity (g kg<sup> -1</sup>)",
                 "Depth (m)<sup> 1</sup>",
                 "Water Temperature (<sup> o</sup>C )<sup> 2</sup>",
                 "Dissolved Oxygen saturation (%)",
                 "Turbidity (NTU)",
                 "Total Suspended Solids (mg L<sup> -1</sup>)",
                 "Pelagic Chlorophyll-a (µg L<sup> -1</sup>)",
                 "Benthic Chlorophyll-a (mg m<sup> -2</sup>)")
df$Central <- c(paste(wq_table[1,4], wq_table[1,5], sep = " ± "),
                paste(depth_table[1,2], depth_table[1,3], sep = " ± "),
                paste(wq_table[1,14], wq_table[1,15], sep = " ± "),
                paste(wq_table[1,2], wq_table[1,3], sep = " ± "),
                paste(wq_table[1,6], wq_table[1,7], sep = " ± "),
                paste(wq_table[1,8], wq_table[1,9], sep = " ± "),
                paste(wq_table[1,10], wq_table[1,11], sep = " ± "),
                paste(wq_table[1,12], wq_table[1,13], sep = " ± "))
df$Edge <- c(paste(wq_table[2,4], wq_table[2,5], sep = " ± "),
                 paste(depth_table[2,2], depth_table[2,3], sep = " ± "),
                 paste(wq_table[2,14], wq_table[2,15], sep = " ± "),
                 paste(wq_table[2,2], wq_table[2,3], sep = " ± "),
                paste(wq_table[2,6], wq_table[2,7], sep = " ± "),
                paste(wq_table[2,8], wq_table[2,9], sep = " ± "),
                paste(wq_table[2,10], wq_table[2,11], sep = " ± "),
                paste(wq_table[2,12], wq_table[2,13], sep = " ± "))
df$`p-value` <- c(round(t.test(Salinity ~ Location, data = dat)$p.value,3),
                  round(t.test(waterlevel_m ~ Location, data = dat_wlevel_long)$p.value,3),
                  round(t.test(Temperature_C ~ Location, data = dat_wtemp)$p.value,3),
                  round(t.test(DO_pct ~ Location, data = dat)$p.value,3),
                  round(t.test(Turbidity ~ Location, data = dat)$p.value,3),
                  round(t.test(TSS ~ Location, data = dat)$p.value,3),
                  round(t.test(CHL_pelagic ~ Location, data = dat)$p.value,3),
                  round(t.test(CHL_benthic ~ Location, data = dat_benthic)$p.value,3))
df$`p-value` <- ifelse(df$`p-value` < 0.001, "< 0.001",df$`p-value`)

# Table 1: Water quality metrics ----
df %>%
  kbl(escape =  F) %>%
  kable_styling(full_width = F, html_font = "Times New Roman", position = 'center') %>%
  kable_classic(html_font = "Cambria") %>%
  column_spec(1:4, background = 'white') %>%
  footnote(number = c("Mean depth ± mean tidal range for Oct. 14, 2020 – Nov. 10, 2020;",
                      "Water temp. from continuous ≤ 1-hour measurements at 20 cm above sediment surface between Jun. 2020 - Oct. 2022"))
setwd("D:/School/SeagrassRecovery/Data")
df %>%
  kbl(escape =  F) %>%
  kable_styling(full_width = F, html_font = "Times New Roman", position = 'center') %>%
  kable_classic(html_font = "Cambria") %>%
  column_spec(1:4, background = 'white') %>%
  footnote(number = c("Mean depth ± mean tidal range for Oct. 14, 2020 – Nov. 10, 2020;",
                      "Water temp. from continuous ≤ 1-hour measurements at 20 cm above sediment surface between Jun. 2020 - Oct. 2022")) %>%
  save_kable(file = 'Table1.html', self_contained = T)

wtemp_table <- dat_temp_long %>%
  subset(!Site %in% c("2C_SED", "2T_SED", "5C_SED", "5T_SED")) %>%
  group_by(Location, DateTime) %>%
  summarise(MeanWTemp = round(mean(Temperature_C, na.rm = T),1),
            SD_WTemp = round(sd(Temperature_C, na.rm = T),1))
round((sum(wtemp_table$SD_WTemp <= 0.1, na.rm = T)/NROW(wtemp_table$SD_WTemp))*100) #88% of SD values <= 0.1

test <- wtemp_table[,1:3]
wtemp_table_wide <- test %>%
  pivot_wider(names_from = "Location", values_from = MeanWTemp) %>%
  mutate(Diff = Northern-Central)

ggplot(wtemp_table_wide, aes(x = Central, y = Northern)) +
  geom_point(alpha = 0.1) +
  geom_abline(slope = 1, intercept = 0, linetype = 'longdash', color = 'red') +
  stat_smooth(method = 'lm') +
  scale_x_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  scale_y_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  labs(x = expression(Mean~Central~Water~Temperature~(degree*C)),
       y = expression(Mean~Northern~Water~Temperature~(degree*C))) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = 1, label.y = 35, size = 4) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 1, label.y = 33, size = 4) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = 'black'))

max(wtemp_table_wide$Diff, na.rm = T) # 4.2 C
min(wtemp_table_wide$Diff, na.rm = T) # -8.9 C
round(mean(wtemp_table_wide$Diff, na.rm = T),2) # -0.48 C

# Figure 2: Water temperature difference between central and northern ----
# width = 900 height = 650

lims <- as.POSIXct(strptime(c("2020-05-15 00:00","2022-10-15 23:45"), format = "%Y-%m-%d %H:%M")) 

ggplot(wtemp_table_wide, aes(x = DateTime, y = Diff)) +
  scale_y_continuous(breaks = seq(-9,4,1)) +
  scale_x_datetime(date_labels = "%b",
                   date_breaks = "1 month",
                   limits = lims,
                   expand = expansion(add = c(0,0))) +
  labs(x = "",
       y = expression(Water~Temperature~Difference~(degree*C))) +
  annotate("rect", fill = "gray93",  
           xmin = as.POSIXct("2020-06-01", "%Y-%m-%d"),
           xmax = as.POSIXct("2020-07-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray93", 
           xmin = as.POSIXct("2020-08-01", "%Y-%m-%d"),
           xmax = as.POSIXct("2020-09-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray93", 
           xmin = as.POSIXct("2020-10-01", "%Y-%m-%d"),
           xmax = as.POSIXct("2020-11-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray93", 
           xmin = as.POSIXct("2020-12-01", "%Y-%m-%d"),
           xmax = as.POSIXct("2021-01-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray93", 
           xmin = as.POSIXct("2021-02-01", "%Y-%m-%d"),
           xmax = as.POSIXct("2021-03-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray93", 
           xmin = as.POSIXct("2021-04-01", "%Y-%m-%d"),
           xmax = as.POSIXct("2021-05-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray93", 
           xmin = as.POSIXct("2021-06-01", "%Y-%m-%d"),
           xmax = as.POSIXct("2021-07-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray93", 
           xmin = as.POSIXct("2021-08-01", "%Y-%m-%d"),
           xmax = as.POSIXct("2021-09-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray93", 
           xmin = as.POSIXct("2021-10-01", "%Y-%m-%d"),
           xmax = as.POSIXct("2021-11-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray93", 
           xmin = as.POSIXct("2021-12-01", "%Y-%m-%d"),
           xmax = as.POSIXct("2022-01-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray93", 
           xmin = as.POSIXct("2022-02-01", "%Y-%m-%d"),
           xmax = as.POSIXct("2022-03-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray93", 
           xmin = as.POSIXct("2022-04-01", "%Y-%m-%d"),
           xmax = as.POSIXct("2022-05-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray93", 
           xmin = as.POSIXct("2022-06-01", "%Y-%m-%d"),
           xmax = as.POSIXct("2022-07-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray93", 
           xmin = as.POSIXct("2022-08-01", "%Y-%m-%d"),
           xmax = as.POSIXct("2022-09-01", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray93", 
           xmin = as.POSIXct("2022-10-01", "%Y-%m-%d"),
           xmax = as.POSIXct("2022-10-15", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate('rect', fill = "white", color = "black",
           xmin = as.POSIXct("2020-6-20", "%Y-%m-%d"),
           xmax = as.POSIXct("2021-04-10", "%Y-%m-%d"),
           ymin = -7.7, ymax = -6.1) +
  geom_line(color = "gray30") +
  geom_hline(yintercept = 0, linetype = 'solid', size = 0.75, color = 'black') +
  geom_hline(yintercept = mean(wtemp_table_wide$Diff, na.rm = T), linetype = 'longdash', size = 0.75, color = 'red') +
  annotate("text", x = as.POSIXct("2020-07-01", "%Y-%m-%d"), y = -6.5,
           label = 'Positive = Northern > Central',
           size = 5, fontface = 1, hjust = 0) +
  annotate("text", x = as.POSIXct("2020-07-01", "%Y-%m-%d"), y = -7.25,
           label = "Negative = Central > Northern",
           size = 5, fontface = 1, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 16, color = 'black')) +
  coord_cartesian(clip = 'off', ylim = c(-9, 4)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate(geom = "text", x = as.POSIXct("2020-08-15", "%Y-%m-%d"), y = -11.3, label = 2020, size = 6) +
  annotate(geom = "text", x = as.POSIXct("2021-06-01", "%Y-%m-%d"), y = -11.3, label = 2021, size = 6) +
  annotate(geom = "text", x = as.POSIXct("2022-05-01", "%Y-%m-%d"), y = -11.3, label = 2022, size = 6)

wtemp_table_wide[which.min(wtemp_table_wide$Diff),]

test <- wtemp_table_wide
test <- test %>%
  mutate(date = date(DateTime))
test2 <- test[test$date > "2022-07-18" &
                test$date < "2022-08-08",]
lims <- as.POSIXct(strptime(c("2022-07-19 00:00","2022-08-07 23:45"), format = "%Y-%m-%d %H:%M"))

# max water temperature difference between northern and central sites ----
# width = 1100 height = 650
ggplot(test2, aes(x = DateTime, y = Central)) +
  geom_line(color = "black", size = 0.7) +
  geom_line(aes(x = DateTime, y = Northern), color = 'red', size = 0.7) +
  labs(x = '2022',
       y = expression(Water~Temperature~(degree*C))) +
  scale_y_continuous(breaks = seq(21,33,2)) +
  scale_x_datetime(date_labels = "%b %d",
                   date_breaks = "1 day",
                   limits = lims,
                   expand = expansion(add = c(0,0))) +
  annotate("text", x = as.POSIXct("2022-07-21 09:00", "%Y-%m-%d"), y = 22,
           label = 'Northern',
           size = 6, fontface = 1, hjust = 0) +
  annotate("text", x = as.POSIXct("2022-07-21 09:00", "%Y-%m-%d"), y = 21,
           label = "Central",
           size = 6, fontface = 1, hjust = 0) +
  annotate("segment", color = "red", size = 1,
           x = as.POSIXct("2022-07-20", "%Y-%m-%d"),
           xend = as.POSIXct("2022-07-21", "%Y-%m-%d"),
           y = 22, yend = 22) +
  annotate("segment", color = "black", size = 1,
           x = as.POSIXct("2022-07-20", "%Y-%m-%d"),
           xend = as.POSIXct("2022-07-21", "%Y-%m-%d"),
           y = 21, yend = 21) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 16, color = 'black'))

# Same as figure above but with air temperature included [air temp data http://vcr.uvadcos.io/ shiny app from vcr website]
# test2 <- test2 %>%
#   mutate(hour = hour(DateTime))
# 
# setwd("D:/School/SeagrassRecovery/Data")
# air_temp <- read.csv('july2022_airtemp.csv')
# air_temp$Index <- as.POSIXct(air_temp$Index, tz = "America/Jamaica", "%m/%d/%Y %H:%M")
# 
# air_temp <- air_temp %>%
#   mutate(date = date(Index),
#          hour = hour(Index))
# air_temp <- air_temp[,c(1,3)]
# 
# test3 <- merge(test2, air_temp, by = c('date','hour'))
# View(test3)
# 
# lims <- as.POSIXct(strptime(c("2022-07-19 00:00","2022-08-07 23:45"), format = "%Y-%m-%d %H:%M"))
# # max water temperature difference between northern and central sites
# # width = 1100 height = 650
# ggplot(test3, aes(x = DateTime, y = Central)) +
#   geom_line(color = "black", size = 0.7) +
#   geom_line(aes(x = DateTime, y = Northern), color = 'red', size = 0.7) +
#   geom_line(aes(x = DateTime, y = t_celcius), color = 'green3', size = 0.7) +
#   labs(x = '2022',
#        y = expression(Water~Temperature~(degree*C))) +
#   scale_y_continuous(breaks = seq(21,33,2)) +
#   scale_x_datetime(date_labels = "%b %d",
#                    date_breaks = "1 day",
#                    limits = lims,
#                    expand = expansion(add = c(0,0))) +
#   annotate("text", x = as.POSIXct("2022-07-21 09:00", "%Y-%m-%d"), y = 22,
#            label = 'Northern',
#            size = 6, fontface = 1, hjust = 0) +
#   annotate("text", x = as.POSIXct("2022-07-21 09:00", "%Y-%m-%d"), y = 21,
#            label = "Central",
#            size = 6, fontface = 1, hjust = 0) +
#   annotate("segment", color = "red", size = 1,
#            x = as.POSIXct("2022-07-20", "%Y-%m-%d"),
#            xend = as.POSIXct("2022-07-21", "%Y-%m-%d"),
#            y = 22, yend = 22) +
#   annotate("segment", color = "black", size = 1,
#            x = as.POSIXct("2022-07-20", "%Y-%m-%d"),
#            xend = as.POSIXct("2022-07-21", "%Y-%m-%d"),
#            y = 21, yend = 21) +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         text = element_text(size = 16, color = "black"),
#         axis.text.x = element_text(size = 16, color = "black", angle = 90, vjust = 0.5, hjust = 1),
#         axis.text.y = element_text(size = 16, color = 'black'))

# Turbidity: control and treatment for each location ----

ggplot(dat, aes(x = factor(format(Date, "%b %Y"),level = c('Jul 2020','Aug 2020','Sep 2020','Oct 2020',
                                                           'May 2021','Jun 2021','Jul 2021','Aug 2021','Sep 2021','Oct 2021',
                                                           'Apr 2022','May 2022','Jun 2022','Jul 2022','Aug 2022')),
                y = Turbidity, fill = Treatment, group = interaction(Date, Treatment))) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,70,10)) +
  scale_fill_manual(values = c('Control' = 'darkorange2',
                               'Treatment' = 'royalblue1')) +
  labs(x = "",
       y = "Turbidity (NTU)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        strip.background = element_rect(fill="white"),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.margin = margin(),
        legend.position = c(0.08,0.93),
        panel.border = element_blank()) +
  facet_wrap(~Location) +
  coord_cartesian(clip = 'off', ylim = c(0, 70)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate('rect', xmin = c(4.4, 10.4), xmax = c(4.6, 10.6),
           ymin = -5, ymax = -2, fill = 'white') +
  annotate('segment', x = c(4.4, 4.6, 10.4, 10.6),
           xend = c(4.4, 4.6, 10.4, 10.6), y = -5, yend = -2)

# Turbidity: between locations ----
ggplot(dat, aes(x = factor(format(Date, "%b %Y"),level = c('Jul 2020','Aug 2020','Sep 2020','Oct 2020',
                                                           'May 2021','Jun 2021','Jul 2021','Aug 2021','Sep 2021','Oct 2021',
                                                           'Apr 2022','May 2022','Jun 2022','Jul 2022','Aug 2022')),
                y = Turbidity, fill = Location, group = interaction(Date, Location))) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,35,5)) +
  scale_fill_manual(values = c('Central' = 'darkorange2',
                               'Northern' = 'royalblue1')) +
  labs(x = "",
       y = "Turbidity (NTU)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        strip.background = element_rect(fill="white"),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.margin = margin(),
        legend.position = c(0.08,0.93),
        panel.border = element_blank()) +
  coord_cartesian(clip = 'off', ylim = c(0, 35)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate('rect', xmin = c(4.4, 10.4), xmax = c(4.6, 10.6),
           ymin = -2.5, ymax = -1, fill = 'white') +
  annotate('segment', x = c(4.4, 4.6, 10.4, 10.6),
           xend = c(4.4, 4.6, 10.4, 10.6), y = -2.5, yend = -1)

# two-way anova with interaction
df_turbidity <- dat[,c(1:4,10)]
df_turbidity$Date <- as.factor(df_turbidity$Date)
aov_turbidity <- lm(Turbidity ~ Date*Location, data = df_turbidity, subset = c(-163,-165,-175,-180))
summary(aov_turbidity)
car::Anova(aov_turbidity)
plot(aov_turbidity)
emm_turbidity <- emmeans(aov_turbidity, pairwise ~ Location | Date)
emm_turbidity$contrasts

##### TSS: control and treatment for each location ----

ggplot(dat, aes(x = factor(format(Date, "%b %Y"),level = c('Jul 2020','Aug 2020','Sep 2020','Oct 2020',
                                                           'May 2021','Jun 2021','Jul 2021','Aug 2021','Sep 2021','Oct 2021',
                                                           'Apr 2022','May 2022','Jun 2022','Jul 2022','Aug 2022')),
                y = TSS, fill = Treatment, group = interaction(Date, Treatment))) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,90,10)) +
  scale_fill_manual(values = c('Control' = 'darkorange2',
                               'Treatment' = 'royalblue1')) +
  labs(x = "",
       y = expression(paste("Total Suspended Solids (mg   ",  L^-1,")"))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        strip.background = element_rect(fill="white"),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.margin = margin(),
        legend.position = c(0.08,0.93),
        panel.border = element_blank()) +
  facet_wrap(~Location) +
  coord_cartesian(clip = 'off', ylim = c(0, 90)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate('rect', xmin = c(4.4, 10.4), xmax = c(4.6, 10.6),
           ymin = -6, ymax = -3, fill = 'white') +
  annotate('segment', x = c(4.4, 4.6, 10.4, 10.6),
           xend = c(4.4, 4.6, 10.4, 10.6), y = -6, yend = -3)

# TSS: between locations ----

ggplot(dat, aes(x = factor(format(Date, "%b %Y"),level = c('Jul 2020','Aug 2020','Sep 2020','Oct 2020',
                                                           'May 2021','Jun 2021','Jul 2021','Aug 2021','Sep 2021','Oct 2021',
                                                           'Apr 2022','May 2022','Jun 2022','Jul 2022','Aug 2022')),
                y = TSS, fill = Location, group = interaction(Date, Location))) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,90,10)) +
  scale_fill_manual(values = c('Central' = 'darkorange2',
                               'Northern' = 'royalblue1')) +
  labs(x = "",
       y = expression(paste("Total Suspended Solids (mg   ",  L^-1,")"))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        strip.background = element_rect(fill="white"),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.margin = margin(),
        legend.position = c(0.08,0.93),
        panel.border = element_blank()) +
  coord_cartesian(clip = 'off', ylim = c(0, 90)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate('rect', xmin = c(4.4, 10.4), xmax = c(4.6, 10.6),
           ymin = -6, ymax = -3, fill = 'white') +
  annotate('segment', x = c(4.4, 4.6, 10.4, 10.6),
           xend = c(4.4, 4.6, 10.4, 10.6), y = -6, yend = -3)

# two-way anova with interaction
df_tss <- dat[,c(1:4,11)]
df_tss$Date <- as.factor(df_tss$Date)
aov_tss <- lm(TSS ~ Date*Location, data = df_tss, subset = c(-85,-93,-95,-165))
summary(aov_tss)
car::Anova(aov_tss)
plot(aov_tss)
emm_tss <- emmeans(aov_tss, pairwise ~ Location | Date) 
emm_tss$contrasts

# Dissolved Oxygen: control and treatment for each location ----

ggplot(dat, aes(x = factor(format(Date, "%b %Y"),level = c('Jul 2020','Aug 2020','Sep 2020','Oct 2020',
                                                           'May 2021','Jun 2021','Jul 2021','Aug 2021','Sep 2021','Oct 2021',
                                                           'Apr 2022','May 2022','Jun 2022','Jul 2022','Aug 2022')),
                y = DO_mgl, fill = Treatment, group = interaction(Date, Treatment))) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,16,2)) +
  scale_fill_manual(values = c('Control' = 'darkorange2',
                               'Treatment' = 'royalblue1')) +
  labs(x = "",
       y = expression(paste("Dissolved Oxygen (mg   ",  L^-1,")"))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        strip.background = element_rect(fill="white"),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.margin = margin(),
        legend.position = c(0.08,0.93),
        panel.border = element_blank()) +
  facet_wrap(~Location) +
  coord_cartesian(clip = 'off', ylim = c(0, 16)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate('rect', xmin = c(4.4, 10.4), xmax = c(4.6, 10.6),
           ymin = -1.2, ymax = -0.5, fill = 'white') +
  annotate('segment', x = c(4.4, 4.6, 10.4, 10.6),
           xend = c(4.4, 4.6, 10.4, 10.6), y = -1.2, yend = -0.5)

# Dissolved Oxygen: between locations ----
 
ggplot(dat, aes(x = factor(format(Date, "%b %Y"),level = c('Jul 2020','Aug 2020','Sep 2020','Oct 2020',
                                                           'May 2021','Jun 2021','Jul 2021','Aug 2021','Sep 2021','Oct 2021',
                                                           'Apr 2022','May 2022','Jun 2022','Jul 2022','Aug 2022')),
                y = DO_mgl, fill = Location, group = interaction(Date, Location))) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,16,2)) +
  scale_fill_manual(values = c('Central' = 'darkorange2',
                               'Northern' = 'royalblue1')) +
  labs(x = "",
       y = expression(paste("Dissolved Oxygen (mg   ",  L^-1,")"))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        strip.background = element_rect(fill="white"),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.margin = margin(),
        legend.position = c(0.08,0.93),
        panel.border = element_blank()) +
  coord_cartesian(clip = 'off', ylim = c(0, 16)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate('rect', xmin = c(4.4, 10.4), xmax = c(4.6, 10.6),
           ymin = -1.2, ymax = -0.5, fill = 'white') +
  annotate('segment', x = c(4.4, 4.6, 10.4, 10.6),
           xend = c(4.4, 4.6, 10.4, 10.6), y = -1.2, yend = -0.5)

# two-way anova with interaction
df_do <- dat[,c(1:4,7)]
df_do$Date <- as.factor(df_do$Date)
aov_do <- lm(DO_mgl ~ Date*Location, data = df_do)
summary(aov_do)
car::Anova(aov_do)
plot(aov_do)
emm_do <- emmeans(aov_do, pairwise ~ Location | Date) 
emm_do$contrasts

# Salinity: control and treatment for each location ----

ggplot(dat, aes(x = factor(format(Date, "%b %Y"),level = c('Jul 2020','Aug 2020','Sep 2020','Oct 2020',
                                                           'May 2021','Jun 2021','Jul 2021','Aug 2021','Sep 2021','Oct 2021',
                                                           'Apr 2022','May 2022','Jun 2022','Jul 2022','Aug 2022')),
                y = Salinity, fill = Treatment, group = interaction(Date, Treatment))) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,35,5)) +
  scale_fill_manual(values = c('Control' = 'darkorange2',
                               'Treatment' = 'royalblue1')) +
  labs(x = "",
       y = expression(paste("Salinity (g  ",  kg^-1,")"))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        strip.background = element_rect(fill="white"),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.margin = margin(),
        legend.position = c(0.08,0.15),
        panel.border = element_blank()) +
  facet_wrap(~Location) +
  coord_cartesian(clip = 'off', ylim = c(0, 35)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate('rect', xmin = c(4.4, 10.4), xmax = c(4.6, 10.6),
           ymin = -2.5, ymax = -1, fill = 'white') +
  annotate('segment', x = c(4.4, 4.6, 10.4, 10.6),
           xend = c(4.4, 4.6, 10.4, 10.6), y = -2.5, yend = -1)

# Salinity: between locations ----

ggplot(dat, aes(x = factor(format(Date, "%b %Y"),level = c('Jul 2020','Aug 2020','Sep 2020','Oct 2020',
                                                           'May 2021','Jun 2021','Jul 2021','Aug 2021','Sep 2021','Oct 2021',
                                                           'Apr 2022','May 2022','Jun 2022','Jul 2022','Aug 2022')),
                y = Salinity, fill = Location, group = interaction(Date, Location))) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,35,5)) +
  scale_fill_manual(values = c('Central' = 'darkorange2',
                               'Northern' = 'royalblue1')) +
  labs(x = "",
       y = expression(paste("Salinity (g  ",  kg^-1,")"))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        strip.background = element_rect(fill="white"),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.margin = margin(),
        legend.position = c(0.08,0.15),
        panel.border = element_blank()) +
  coord_cartesian(clip = 'off', ylim = c(0, 35)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate('rect', xmin = c(4.4, 10.4), xmax = c(4.6, 10.6),
           ymin = -2.5, ymax = 1, fill = 'white') +
  annotate('segment', x = c(4.4, 4.6, 10.4, 10.6),
           xend = c(4.4, 4.6, 10.4, 10.6), y = -2.5, yend = -1)

# two-way anova with interaction
df_salinity <- dat[,c(1:4,8)]
df_salinity$Date <- as.factor(df_salinity$Date)
aov_salinity <- lm(Salinity ~ Date*Location, data = df_salinity)
summary(aov_salinity)
car::Anova(aov_salinity)
plot(aov_salinity)
emm_salinity <- emmeans(aov_salinity, pairwise ~ Location | Date) 
emm_salinity$contrasts

# Pelagic CHLa: control and treatment for each location ----

ggplot(dat, aes(x = factor(format(Date, "%b %Y"),level = c('Jul 2020','Aug 2020','Sep 2020','Oct 2020',
                                                           'May 2021','Jun 2021','Jul 2021','Aug 2021','Sep 2021','Oct 2021',
                                                           'Apr 2022','May 2022','Jun 2022','Jul 2022','Aug 2022')),
                y = CHL_pelagic, fill = Treatment, group = interaction(Date, Treatment))) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,20,2)) +
  scale_fill_manual(values = c('Control' = 'darkorange2',
                               'Treatment' = 'royalblue1')) +
  labs(x = "",
       y = expression(paste("Pelagic CHLa (μg   ",  L^-1,")"))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        strip.background = element_rect(fill="white"),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.margin = margin(),
        legend.position = c(0.08,0.93),
        panel.border = element_blank()) +
  facet_wrap(~Location) +
  coord_cartesian(clip = 'off', ylim = c(0, 20)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate('rect', xmin = c(4.4, 10.4), xmax = c(4.6, 10.6),
           ymin = -1.5, ymax = -0.5, fill = 'white') +
  annotate('segment', x = c(4.4, 4.6, 10.4, 10.6),
           xend = c(4.4, 4.6, 10.4, 10.6), y = -1.5, yend = -0.5)

# Pelagic CHLa: between locations ----

ggplot(dat, aes(x = factor(format(Date, "%b %Y"),level = c('Jul 2020','Aug 2020','Sep 2020','Oct 2020',
                                                           'May 2021','Jun 2021','Jul 2021','Aug 2021','Sep 2021','Oct 2021',
                                                           'Apr 2022','May 2022','Jun 2022','Jul 2022','Aug 2022')),
                y = CHL_pelagic, fill = Location, group = interaction(Date, Location))) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,20,2)) +
  scale_fill_manual(values = c('Central' = 'darkorange2',
                               'Northern' = 'royalblue1')) +
  labs(x = "",
       y = expression(paste("Pelagic CHLa (μg   ",  L^-1,")"))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        strip.background = element_rect(fill="white"),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.margin = margin(),
        legend.position = c(0.08,0.93),
        panel.border = element_blank()) +
  coord_cartesian(clip = 'off', ylim = c(0, 20)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate('rect', xmin = c(4.4, 10.4), xmax = c(4.6, 10.6),
           ymin = -1.5, ymax = -0.5, fill = 'white') +
  annotate('segment', x = c(4.4, 4.6, 10.4, 10.6),
           xend = c(4.4, 4.6, 10.4, 10.6), y = -1.5, yend = -0.5)

# two-way anova with interaction
df_pelagic <- dat[,c(1:4,14)]
df_pelagic$Date <- as.factor(df_pelagic$Date)
aov_pelagic <- lm(CHLa_pelagic ~ Date*Location, data = df_pelagic, subset = -165)
summary(aov_pelagic)
car::Anova(aov_pelagic)
plot(aov_pelagic)
emm_pelagic <- emmeans(aov_pelagic, pairwise ~ Location | Date) 
emm_pelagic$contrasts

# Benthic CHLa: control and treatment for each location ----

ggplot(dat_benthic, aes(x = factor(format(Date, "%b %Y"),level = c('Jul 2020','Aug 2020','Sep 2020','Oct 2020',
                                                           'May 2021','Jun 2021','Jul 2021','Aug 2021','Sep 2021','Oct 2021',
                                                           'Apr 2022','May 2022','Jun 2022','Jul 2022','Aug 2022')),
                y = CHL_benthic, fill = Treatment, group = interaction(Date, Treatment))) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,250,25)) +
  scale_fill_manual(values = c('Control' = 'darkorange2',
                               'Treatment' = 'royalblue1')) +
  labs(x = "",
       y = expression(paste("Benthic CHLa (mg   ",  m^-2,")"))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        strip.background = element_rect(fill="white"),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.margin = margin(),
        legend.position = c(0.08,0.93),
        panel.border = element_blank()) +
  facet_wrap(~Location) +
  coord_cartesian(clip = 'off', ylim = c(0, 250)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate('rect', xmin = c(4.4, 10.4), xmax = c(4.6, 10.6),
           ymin = -9, ymax = -3, fill = 'white') +
  annotate('segment', x = c(4.4, 4.6, 10.4, 10.6),
           xend = c(4.4, 4.6, 10.4, 10.6), y = -9, yend = -3)

# Benthic CHLa: between locations ----

ggplot(dat_benthic, aes(x = factor(format(Date, "%b %Y"),level = c('Jul 2020','Aug 2020','Sep 2020','Oct 2020',
                                                           'May 2021','Jun 2021','Jul 2021','Aug 2021','Sep 2021','Oct 2021',
                                                           'Apr 2022','May 2022','Jun 2022','Jul 2022','Aug 2022')),
                y = CHL_benthic, fill = Location, group = interaction(Date, Location))) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,250,25)) +
  scale_fill_manual(values = c('Central' = 'darkorange2',
                               'Northern' = 'royalblue1')) +
  labs(x = "",
       y = expression(paste("Benthic CHLa (mg   ",  m^-2,")"))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        strip.background = element_rect(fill="white"),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.margin = margin(),
        legend.position = c(0.08,0.93),
        panel.border = element_blank()) +
  coord_cartesian(clip = 'off', ylim = c(0, 250)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate('rect', xmin = c(4.4, 10.4), xmax = c(4.6, 10.6),
           ymin = -9, ymax = -3, fill = 'white') +
  annotate('segment', x = c(4.4, 4.6, 10.4, 10.6),
           xend = c(4.4, 4.6, 10.4, 10.6), y = -9, yend = -3)

# two-way anova with interaction
df_benthic <- dat_benthic
df_benthic$Date <- as.factor(df_benthic$Date)
aov_benthic <- lm(CHL_benthic ~ Date*Location, data = df_benthic)
summary(aov_benthic)
car::Anova(aov_benthic)
plot(aov_benthic)
emm_benthic <- emmeans(aov_benthic, pairwise ~ Location | Date) 
emm_benthic$contrasts
plot(emm_benthic$contrasts)
