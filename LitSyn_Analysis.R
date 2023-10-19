library(tidyverse)
library(ggspatial)
library(sf)
library(rnaturalearth)
library(rstatix)
library(ggsignif)
library(ggpmisc)
library(patchwork)
library(ggridges)

rm(list = ls())
dev.off()

setwd("D:/School/SeagrassRecovery/LitSurvey")
dat <- read.csv("LitSurvey_Data.csv")

# Map sites
lat_long <- dat %>%
  group_by(Order,Country,StudyType) %>%
  summarise(Lat = mean(Lat),
            Long = mean(Long))

lat_long <- lat_long %>%
  group_by(Order) %>%
  mutate(StudyType = if(all(c("Experiment", "Observation") %in% StudyType)) 
    "Both" else StudyType) %>% 
  ungroup %>% 
  distinct

lat_long$col <- ifelse(lat_long$StudyType == 'Experiment',"#FCDB2D",
                       ifelse(lat_long$StudyType == 'Observation',"dodgerblue2","darkorchid2"))

# need to jitter these points due to overlap with another study in similar area
lat_long <- lat_long %>%
  mutate(Long = ifelse(Order == 19, -123.5, Long)) %>%
  mutate(Long = ifelse(Order == 51, -125.5, Long)) %>%
  mutate(Lat = ifelse(Order == 2, 35.5, Lat)) %>%
  mutate(Lat = ifelse(Order == 23, 35.5, Lat)) %>%
  mutate(Lat = ifelse(Order == 38, 36, Lat)) %>%
  mutate(Lat = ifelse(Order == 66, 36, Lat)) %>%
  mutate(Lat = ifelse(Order == 76, 35.5, Lat))

world <- rnaturalearth::ne_countries(scale = "medium",
                                     returnclass = "sf")

# If having issues downloading lake data take steps below
## remove.packages("rnaturalearth")
## # restart R
## devtools::install_github("ropensci/rnaturalearth")
## require("rnaturalearth")

lakes <- rnaturalearth::ne_download(scale = 110,
                                    type = 'lakes',
                                    category = 'physical') %>%
  sf::st_as_sf(lakes110, crs = 4269)

sf_all <- st_as_sf(lat_long, 
                   coords = c("Long", "Lat"),
                   crs = 4269)

map <- ggplot() +
  geom_sf(data = world,
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "gray95") +
  geom_sf(data = lakes,
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "white") +
  geom_sf(data = sf_all,
          color = "black",
          fill = sf_all$col,
          shape = 21,
          size = 3.2,
          alpha = 0.8) +
  coord_sf(ylim = c(-90, 90),
           xlim = c(-180, 180),
           expand = FALSE) +
  labs(x = "Longitude",
       y = "Latitude") +
  scale_y_continuous(breaks = c(seq(-75,75,25))) +
  scale_x_continuous(breaks = c(seq(-150,150,25))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", linewidth = 0.5),
        panel.background = element_rect(fill = "white"))

dat2 <- dat %>%
  group_by(Order,Country,StudyType) %>%
  summarise(count = n())

dat2 <- dat2 %>%
  group_by(Order) %>%
  mutate(StudyType = if(all(c("Experiment", "Observation") %in% StudyType)) 
    "Both" else StudyType) %>% 
  ungroup() %>% 
  distinct()

dat2 <- dat2 %>%
  filter(!row_number() %in% 15) %>%  # need to remove a duplicate
  mutate(count = ifelse(row_number() == 44, 1, count))  # this site had one experiment & one observation = one 'both'

dat2 <- dat2 %>%
  group_by(Country, StudyType) %>%
  summarise(count = n())

freq_plot <- ggplot(data = dat2, aes(x = factor(Country, levels = c('USA','Australia','France','China','Portugal','South Korea','Denmark','Philippines','Spain','Brazil','Indonesia','Mauritania','Netherlands','New Zealand','South Africa','Thailand')),
                                     y = count,
                                     fill = factor(StudyType, levels = c('Both','Observation','Experiment')))) +
  geom_bar(position = 'stack',
           stat = "identity",
           color = "black") +
  labs(x = "",
       y = "Num. of Studies (n = 60)") +
  scale_y_continuous(breaks = seq(0,20,2), limits = c(0,20)) +
  scale_fill_manual(values = c('darkorchid2','dodgerblue2','#FCDB2D')) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.title = element_text(size = 11, color = "black"),
        axis.title.x =  element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.85,0.8),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.margin = grid::unit(c(0,0,2,2), "mm"))

setwd("D:/School/SeagrassRecovery/LitSurvey/Figures/ForManuscript")
# Figure 8: Map ----
# width = 1200 height = 900
map + annotation_custom(ggplotGrob(freq_plot),
                        xmin = -180, xmax = -55,
                        ymin = -90, ymax = 5)
# Analyses
dat$Region <- ifelse(dat$Lat < 23.5 & dat$Lat > -23.5, "Tropics",
                     ifelse(dat$Lat > 23.5 & dat$Lat < 35, "Subtropics",
                            ifelse(dat$Lat < -23.5 & dat$Lat > -35, "Subtropics",
                                   ifelse(dat$Lat > 35, "Temperate",
                                          ifelse(dat$Lat < 35, "Temperate", NA)))))

dat <- dat[,c(1:7,35,8:34)]
table(dat$Region)
test <- dat[!is.na(dat$Recovery_months),]
table(test$Region)
dat2 <- dat[!duplicated(dat$Order),]
table(dat2$Region)
table(dat2$StudyType)
dat2 %>%
  group_by(StudyType, NumSpecies) %>%
  summarise(count = n())
round((14/27)*100) # 52% of experimental studies done in monoculture meadows
round((20/33)*100) # 61% of observational studies done in monoculture meadows

dat2 %>%
  group_by(StudyType) %>%
  summarise(median = median(DisturbanceArea_m2, na.rm = T),
            min = min(DisturbanceArea_m2, na.rm = T),
            max = max(DisturbanceArea_m2, na.rm = T))

dat2 %>%
  summarise(OrderMagnitudeDiff = round(log10(max(DisturbanceArea_m2, na.rm = T)-log10(min(DisturbanceArea_m2, na.rm = T)))))

species <- dat2 %>% 
  separate_rows(Species, sep = ",\\s+") %>%
  count(Species, name = 'Freq') %>%
  mutate(genus = stringr::word(Species, 1))

genus <- species %>%
  group_by(genus) %>%
  summarise(Freq = sum(Freq))

species_fig <- species %>%
  ggplot(aes(y = forcats::fct_reorder(Species, Freq), x = Freq)) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "gray75") +
  labs(y = "",
       x = "Number of Studies") +
  scale_x_continuous(breaks = seq(0,20,2), limits = c(0,20)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black", face = "italic"),
        axis.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 16, color = "black"),
        strip.background = element_rect(fill = "white"))

genus_fig <- genus %>%
  ggplot(aes(y = forcats::fct_reorder(genus, Freq), x = Freq)) +
  geom_bar(stat = "identity",
           color = "black",
           fill = "gray75") +
  labs(y = "",
       x = "") +
  scale_x_continuous(breaks = seq(0,40,5), limits = c(0,40)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black", face = "italic"),
        axis.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 16, color = "black"),
        strip.background = element_rect(fill = "white"),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))

# Figure 9: Genus species ---- 
# width = 1000 height = 800
species_fig + annotation_custom(ggplotGrob(genus_fig),
                                xmin = 3.1, xmax = 20.5,
                                ymin = 0.01, ymax = 23)

dat %>%
  group_by(Region) %>%
  summarise(Median = median(Recovery_months, na.rm = T),
                                       SD = sd(Recovery_months, na.rm = T),
                                       MIN = min(Recovery_months, na.rm = T),
                                       MAX = max(Recovery_months, na.rm = T))

dat_exp <- dat %>%
  filter(StudyType == 'Experiment')

dat_obs <- dat %>%
  filter(StudyType == 'Observation') %>%
  mutate(DisturbanceArea_km2 = DisturbanceArea_m2/1000000)

dat_mono <- dat %>%
  filter(Mixed_Mono == 'Mono')

dat_mono_table <- as.data.frame(table(unlist(strsplit(dat_mono$Species,', '))))
dat_species_table <- as.data.frame(table(unlist(strsplit(dat$Species,', '))))

# SI Figure 6: Disturbance area as a function of experiment or observation ----

p1 <- ggplot(data = dat_exp, aes(x = StudyType, y = DisturbanceArea_m2)) +
  geom_boxplot(outlier.color = "black") +
  scale_y_continuous(breaks = seq(0,14,2),
                     limits = c(0,14)) +
  labs(x = "",y = expression(paste("Disturbance Area ( ", m^2,")"))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        axis.title.x = element_text(vjust = -0.5),
        panel.grid = element_blank(),
        panel.background = element_blank())

p2 <- ggplot(data = dat_obs, aes(x = StudyType, y = DisturbanceArea_km2)) +
  geom_boxplot(outlier.color = "black") +
  # scale_y_continuous(breaks = seq(0,5,1),
  #                    limits = c(0,5)) +
  labs(x = "",y = expression(paste("Disturbance Area ( ", km^2,")"))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        axis.title.x = element_text(vjust = -0.5),
        panel.grid = element_blank(),
        panel.background = element_blank())

### width = 800 height = 600
p1 + p2 + plot_layout(ncol = 2)
  
current_lrg_disArea = round(max(dat_exp$DisturbanceArea_m2),1)
our_study_distArea = 3.14*(3*3)
round((our_study_distArea/current_lrg_disArea),2)

# SI Figure 2: Disturbance type ----
### width = 800 height = 600
dat2 %>%
  arrange(-desc(DistType)) %>%
  mutate(DistType = factor(DistType, levels = c("Physical","Chemical","Light",
                                                "NA","Climate","Disease","Natural"))) %>%
  ggplot(aes(DistType)) +
  geom_bar(color = "black",
           fill = "gray75") +
  labs(x = "Disturbance Type",
       y = "Number of Studies") +
  scale_y_continuous(breaks = seq(0,35,5), limits = c(0,35)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black", angle = 30, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 16, color = "black"))

# SI Figure 4: Disturbance shape ----
### width = 800 height = 600

table(dat2$StudyType)
adj_studyType <- dat2 %>%
  mutate(StudyType = ifelse(StudyType == 'Experiment', 'Experiment (n = 27)', 'Observation (n = 33)'))

adj_studyType %>%
  mutate(DistShape = factor(DistShape, levels = c("Irregular","Square","Circle","Rectagle","Ellipse","NA"))) %>%
ggplot(aes(DistShape)) +
  geom_bar(color = "black",
           fill = "gray75") +
  labs(x = "Disturbance Shape",
       y = "Number of Studies") +
  scale_y_continuous(breaks = seq(0,24,2), limits = c(0,24)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black", angle = 30, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 16, color = "black"),
        strip.background = element_rect(fill = "white")) +
  facet_wrap(~StudyType)

# SI Figure 3: Recovery trajectory shape----
### width = 800 height = 600
ggplot(data = dat2, aes(Recovery_Trajectory)) +
  geom_bar(color = "black",
           fill = "gray75") +
  scale_y_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  labs(x = "Recovery Trajectory",
       y = "Number of Studies") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank())

# SI Figure 8: Recovery mechanism ----
### width = 800 height = 600
adj_studyType %>%
  mutate(Recovery_Mechanism = factor(Recovery_Mechanism, levels = c("Clonal","Seed","Both"))) %>%
ggplot(aes(Recovery_Mechanism)) +
  geom_bar(color = "black",
           fill = "gray75") +
  labs(x = "Recovery Mechanism",
       y = "Number of Studies") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        strip.text.x = element_text(size = 16, color = "black"),
        panel.background = element_blank(),
        strip.background = element_rect(fill = "white")) +
  facet_wrap(~StudyType)

# SI Figure 5: Species included in experimental and observations studies ----
### width = 800 height = 600
ggplot(data = adj_studyType, aes(NumSpecies)) +
  geom_bar(color = "black",
           fill = "gray75") +
  scale_y_continuous(breaks = seq(0,20,2), limits = c(0,20)) +
  scale_x_continuous(breaks = seq(1,7,1)) +
  labs(x = "Number of Species",
       y = "Number of Studies") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 16, color = "black"),
        strip.background = element_rect(fill = "white")) +
  facet_wrap(~StudyType)

round(mean(dat_exp$StudyDuration_months, na.rm = T))
round(mean(dat_obs$StudyDuration_months, na.rm = T))

dat <- dat %>%
  mutate(PerimAreaRatio = DisturbancePerimeter_m/DisturbanceArea_m2,
         MeadowDistRatio = DisturbanceArea_m2/MeadowSizeStart_m2,
         Perc_MeadowDist <- MeadowDistRatio*100,
         abs_lat = abs(Lat))

# Figure 10: Recovery time as a function of disturbance area ----
# width = 800 height = 600
library(ggpubr)

cols <- c("Tropics" = "#000000", "Subtropics" = "#969696", "Temperate" = "#ffffff")

library(scales)

test <- dat %>%
  select(StudyType, DisturbanceArea_m2, Recovery_months, Perc_RecoveryDuringStudy) %>%
  filter(Perc_RecoveryDuringStudy >= 90) %>%
  drop_na(DisturbanceArea_m2) %>%
  mutate(log_x = log10(DisturbanceArea_m2),
         log_y = log10(Recovery_months))
lm_mod <- lm(log_y ~ log_x, data = test)
poly_mod <- lm(log10(Recovery_months) ~ poly(log10(DisturbanceArea_m2),2), data = test)
summary(lm_mod)
summary(poly_mod)

cor.test(test$log_x,test$log_y, method = 'pearson')

myexp <- data.frame(StudyType = c('This Study','This Study'),
                    DisturbanceArea_m2 = c(28.3,28.3),
                    Recovery_months = c(24,158),
                    Perc_RecoveryDuringStudy = c(100,43),
                    log_x = c(1.45,1.45),
                    log_y = c(1.38,2.19))

aa <- rbind(test,myexp)

aa %>%
  ggplot(aes(x = log_x, y = log_y)) +
  geom_point(aes(fill=factor(StudyType),
                 shape = factor(StudyType)),
             color = "black", size = 4, alpha = 0.7) +
  scale_x_continuous(breaks = seq(-2,6,1),
                     limits = c(-2,6)) +
  scale_y_continuous(breaks = seq(-0.5,2.5,0.5),
                     limits = c(-0.5,2.5)) +
  scale_shape_manual(values = c(21, 21, 24),
                     name = "Study Type") +
  scale_fill_manual(values = c('#FCDB2D','dodgerblue2','#FCDB2D'),
                    name = "Study Type") +
  labs(x = expression(log[10]~"[Disturbance Area"~ (m^-2) ~"]"),
       y = expression(log[10]~"[Recovery Time (months)]")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        legend.title = element_text(size = 16, color = "black"),
        legend.title.align = 0.5,
        legend.text = element_text(size = 16, color = "black"),
        legend.position = c(0.15,0.88),
        panel.grid = element_blank(),
        panel.background = element_blank()) +
  stat_cor(aes(label = paste(after_stat(r.label), after_stat(p.label), sep = "~`,`~")),
           label.x = 3, label.y = 0, size = 6,
           r.accuracy = 0.01,
           p.accuracy = 0.001)

summary(lm(aa$log_y~poly(aa$log_x,2)))

formula <- aa$log_y~poly(aa$log_x,2)

aa %>%
  ggplot(aes(x = log_x, y = log_y)) +
  geom_point(aes(fill=factor(StudyType),
                 shape = factor(StudyType)),
             color = "black", size = 4, alpha = 0.7) +
  scale_x_continuous(breaks = seq(-2,6,1),
                     limits = c(-2,6)) +
  scale_y_continuous(breaks = seq(-0.5,2.5,0.5),
                     limits = c(-0.5,2.5)) +
  scale_shape_manual(values = c(21, 21, 24),
                     name = "Study Type") +
  scale_fill_manual(values = c('#FCDB2D','dodgerblue2','#FCDB2D'),
                    name = "Study Type") +
  labs(x = expression(log[10]~"[Disturbance Area"~ (m^-2) ~"]"),
       y = expression(log[10]~"[Recovery Time (months)]")) +
  stat_smooth(method = 'lm',
              formula = y~poly(x,2),
              se = F,
              color = 'red') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        legend.title = element_text(size = 16, color = "black"),
        legend.title.align = 0.5,
        legend.text = element_text(size = 16, color = "black"),
        legend.position = c(0.15,0.88),
        panel.grid = element_blank(),
        panel.background = element_blank()) +
  stat_regline_equation(formula = formula,
                        aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~`,`~")),
                        label.x = 1.8, label.y = 0.25, size = 6) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("p-value = ", signif(..p.value.., digits = 3), sep = "")),
                  label.x = 3, label.y = 0, size = 6)

# Recovery time as a function of perimeter:area ratio ----
# width = 800 height = 600
test2 %>%
  ggplot(aes(x = PerimAreaRatio, y = log_y)) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = seq(0,40,5),
                     limits = c(0,42)) +
  scale_y_continuous(breaks = seq(-0.5,2.0,0.5),
                     limits = c(-0.5,2.1)) +
  labs(x = expression("Disturbance Perimeter:Area Ratio"~ (m^-1)),
       y = expression(log[10]~"[Recovery Time (months)]")) +
  stat_smooth(method = 'lm',
              se = F,
              color = 'red') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank()) +
  stat_cor(aes(label = paste(after_stat(r.label), after_stat(p.label), sep = "~`,`~")),
           label.x = 20, label.y = 1.7, size = 6,
           r.accuracy = 0.01,
           p.accuracy = 0.001) +
  stat_regline_equation(formula = test2$log_y ~ test2$PerimAreaRatio,
                        aes(label = ..eq.label..),
                        label.x = 20, label.y = 2, size = 6)

# SI Figure 7: Recovery rate normalized to disturbance area by region ----
# width = 800 height = 600
zz <- dat %>%
  filter(Perc_RecoveryDuringStudy >= 90,
         Perc_AboveGroundReduction >= 80) %>%
  mutate(recov_area = DisturbanceArea_m2/Recovery_months,
         recov_area_log = log10(recov_area)) %>%
  drop_na(recov_area)

num_region2 <- c("Tropics (n = 12)", "Subtropics (n = 17)", "Temperate (n = 20)")
my_comparisons <- list(c("Tropics","Subtropics"),
                       c("Subtropics","Temperate"),
                       c("Tropics","Temperate"))

zz %>%
  mutate(Region = factor(Region, levels = c("Tropics","Subtropics","Temperate"))) %>%
  filter(Perc_RecoveryDuringStudy >= 90) %>%
  ggplot(aes(x = Region, y = recov_area_log)) +
  scale_x_discrete(labels = num_region2) +
  geom_boxplot(outlier.colour="white", outlier.fill = "white", outlier.shape = 1, outlier.size = 0) +
  geom_jitter(shape=1, position=position_jitter(0.2), color = "black", fill = "white", size = 2) +
  labs(x = "Latitudinal Regions",
       y = expression(log[10]~'['~Recovery~Rate~(m^2~month^-1)~']')) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        axis.title.x = element_text(vjust = -0.5),
        panel.grid = element_blank(),
        panel.background = element_blank())

zz %>%
  group_by(Region) %>%
  summarise(Median = round(median(recov_area, na.rm = T),2))

summary(aov(recov_area~Region, zz))
