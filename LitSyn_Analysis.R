library(ggplot2)
library(ggspatial)
library(sf)
library(rnaturalearth)
library(tidyr)
library(dplyr)
library(rstatix)
library(ggsignif)
library(ggpmisc)

rm(list = ls())
dev.off()

setwd("D:/School/SeagrassRecovery/LitSurvey")
dat <- read.csv("LitSurvey_Data.csv")

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
lat_long[c(15,41),5] <- c(-123.5,-125.5) # need to jitter these points due to overlap with another study in similar area
lat_long[c(1,18,30,51,59),4] <- c(35.5,33.5,36,36,33.5) # need to jitter these points due to overlap with another study in similar area

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
          alpha = 1) +
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
  ungroup %>% 
  distinct()

dat2 <- dat2[c(1:14,16:NROW(dat2)),] # need to remove a duplicate
dat2[44,4] <- 1 # this site had one experiment & one observation = one 'both'

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

setwd("D:/School/SeagrassRecovery/LitSurvey/Figures")
# Figure 8: Map ----
# width = 1200 height = 900
map + annotation_custom(ggplotGrob(freq_plot),
                        xmin = -180, xmax = -55,
                        ymin = -90, ymax = 5)

library(wordcloud)
library(wordcloud2)
library(tm)

text <- dat[!duplicated(dat$Order),]
text <- text[,2]
docs <- Corpus(VectorSource(text))

docs <- docs %>%
  tm_map(removePunctuation)

docs <- docs %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

# docs <- tm_map(docs, content_transformer(tolower))
# docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix), decreasing = TRUE)
df <- data.frame(word = names(words), freq=words)

set.seed(1234)

dff <- df[df$freq >= 2,]

# wordcloud ----
wordcloud(words = df$word, freq = df$freq, min.freq = 2,
          max.words = 200, random.order = FALSE, rot.per = 0,
          colors = brewer.pal(5, "Dark2"))

wordcloud2(data = dff, backgroundColor = "black",
           shape = "diamond", color=rep_len(c("#253494","#2c7fb8","#41b6c4", "#a1dab4", "#ffffcc"), nrow(df)),
           rotateRatio = 0, minSize = 2, shuffle = TRUE,
           fontWeight = 'normal')

###########

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

# SI Figure 2: Genus species ---- 
# width = 1000 height = 800
species_fig + annotation_custom(ggplotGrob(genus_fig),
                                xmin = 3.1, xmax = 20.5,
                                ymin = 0.01, ymax = 23)

dat %>%
  group_by(Region) %>%
  drop_na(Recovery_months) %>%
  summarise(n = n(),
            median = round(median(Recovery_months, na.rm = T)))

num_region <- c("Tropics (n = 22)", "Subtropics (n = 36)", "Temperate (n = 42)")
my_comparisons <- list(c("Tropics","Temperate"),
                       c("Subtropics","Temperate"))

# Figure 9: Recovery Time as a function of region ----
# width = 800 height = 600
dat %>%
  mutate(Region = factor(Region, levels = c("Tropics","Subtropics","Temperate"))) %>%
  ggplot(aes(x = Region, y = Recovery_months)) +
  geom_signif(textsize = 4,
              y_position = log10(c(2000, 1400)),
              comparisons = my_comparisons,
              map_signif_level = function(x) paste("p =", scales::pvalue(x))) +
  scale_y_log10() +
  scale_x_discrete(labels = num_region) +
  geom_boxplot(outlier.colour="white", outlier.fill = "white", outlier.shape = 1, outlier.size = 0) +
  geom_jitter(shape=1, position=position_jitter(0.2), color = "black", fill = "white", size = 2) +
  labs(x = "Latitudinal Regions",
       y = "Recovery Time (months)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        axis.title.x = element_text(vjust = -0.5),
        panel.grid = element_blank(),
        panel.background = element_blank())

dat %>%
  group_by(Region) %>%
  summarise(Median = median(Recovery_months, na.rm = T),
                                       SD = sd(Recovery_months, na.rm = T),
                                       MIN = min(Recovery_months, na.rm = T),
                                       MAX = max(Recovery_months, na.rm = T))

trop <- na.omit(dat[dat$Region == "Tropics",c(8,29)])
subtrop <- na.omit(dat[dat$Region == "Subtropics",c(8,29)])
temp <- na.omit(dat[dat$Region == "Temperate",c(8,29)])
zone <- rbind(trop,subtrop)
zone <- rbind(zone,temp)

# ECDF ----
# width = 800 height = 600
zone %>%
  mutate(Region = forcats::fct_relevel(Region, c('Tropics','Subtropics','Temperate'))) %>%
  group_by(Region) %>%
  ggplot(aes(Recovery_months, color = Region)) +
  stat_ecdf(geom = 'line', pad = F, size = 1) +
  scale_y_continuous(breaks = seq(0,1,0.1),
                     limits = c(0,1)) +
  scale_x_continuous(breaks = seq(0,1200,100),
                     limits = c(0,1200)) +
  labs(x = "Recovery Length (months)",
       y = "Empirical Cumulative Distribution") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        axis.title.x = element_text(vjust = -0.5),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.position = c(0.85,0.15),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank())

zone %>%
  mutate(Region = forcats::fct_relevel(Region, c('Tropics','Subtropics','Temperate'))) %>%
  group_by(Region) %>%
  ggplot(aes(log10(Recovery_months), color = Region)) +
  stat_ecdf(geom = 'line', pad = F, size = 1) +
  scale_y_continuous(breaks = seq(0,1,0.1),
                     limits = c(0,1)) +
  labs(x = expression(log[10]~"[Recovery Length (months)]"),
       y = "Empirical Cumulative Distribution") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        axis.title.x = element_text(vjust = -0.5),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.position = c(0.85,0.15),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank())

# quantile(test$Recovery_months, probs = seq(.1,.9, by = .1), na.rm = T)
# f <- ecdf(test$Recovery_months)
# round(ecdf(test$Recovery_months)(24),3)
# mean(test$Recovery_months < 24, na.rm = T)
# mean(test$Recovery_months < 124, na.rm = T)

dat2 %>%
  summarise(OrderMagnitudeDiff = round(log10(max(Recovery_months, na.rm = T)-log10(min(Recovery_months, na.rm = T)))))

# dat_100 <- dat[dat$Perc_RecoveryDuringStudy >= 95,]
# dat_100 <- dat_100[!is.na(dat_100$Perc_RecoveryDuringStudy),]
# 
# dat_100 %>%
#   mutate(Region = factor(Region, levels = c("Tropics","Subtropics","Temperate"))) %>%
#   ggplot(aes(x = Region, y = Recovery_months)) +
#   scale_y_log10() +
#   geom_boxplot(outlier.colour="white", outlier.fill = "white", outlier.shape = 1, outlier.size = 0) +
#   geom_jitter(shape=1, position=position_jitter(0.2), color = "black", fill = "white", size = 2) +
#   labs(x = "Latitudinal Regions",
#        y = "Recovery Time (months)") +
#   theme_bw() +
#   theme(axis.text.x = element_text(size = 16, color = "black"),
#         axis.text.y = element_text(size = 16, color = "black"),
#         axis.title = element_text(size = 16, color = "black"),
#         axis.title.x = element_text(vjust = -0.5),
#         panel.grid = element_blank(),
#         panel.background = element_blank())
# 
# dat_100 %>% group_by(Region) %>% summarise(Median = median(Recovery_months, na.rm = T))

dat_exp <- dat2[dat2$StudyType == "Experiment",]
dat_obs <- dat2[dat2$StudyType == "Observation",]
dat_obs$DisturbanceArea_km2 <- dat_obs$DisturbanceArea_m2/1000000
dat_mono <- dat2[dat2$Mixed_Mono == "Mono",]
dat_mono_table <- as.data.frame(table(unlist(strsplit(dat_mono$Species,', '))))
dat_species_table <- as.data.frame(table(unlist(strsplit(dat$Species,', '))))
dat_nonlin <- dat2[dat2$Recovery_Trajectory == "Nonlinear",]
dat_lin <- dat2[dat2$Recovery_Trajectory == "Linear",]

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

library(patchwork)

# SI Figure 6: Disturbance area as a function of experiment or observation ----
### width = 800 height = 600
p1 + p2 + plot_layout(ncol = 2)
  
current_lrg_disArea = round(max(dat_exp$DisturbanceArea_m2),1)
our_study_distArea = 3.14*(3*3)
round((our_study_distArea/current_lrg_disArea),2)

table(dat2$StudyType)

adj_studyType <- dat2
adj_studyType[adj_studyType == "Experiment"] <- "Experiment (n = 27)"
adj_studyType[adj_studyType == "Observation"] <- "Observation (n = 33)"

# SI Figure 3: Disturbance shape ----
### width = 800 height = 600
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

# SI Figure 1: Disturbance type ----
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

# Recovery shape ----
### width = 800 height = 600
ggplot(data = dat2, aes(Recovery_Trajectory)) +
  geom_bar(color = "black",
           fill = "gray75") +
  scale_y_continuous(breaks = seq(0,35,5),
                     limits = c(0,35)) +
  labs(x = "Recovery Shape",
       y = "Number of Studies") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank())

# Figure 12: Recovery mechanism ----
### width = 800 height = 600
dat2 %>%
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
ggplot(data = dat2, aes(NumSpecies)) +
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

dat_100 <- dat[dat$Perc_RecoveryDuringStudy >= 90,]
dat_100 <- dat_100[!is.na(dat_100$Perc_RecoveryDuringStudy),]
dat_100$Recovery_months <- as.numeric(dat_100$Recovery_months)
dat_100$DisturbanceArea_m2 <- as.numeric(dat_100$DisturbanceArea_m2)
dat_100$MeadowSizeStart_m2 <- as.numeric(dat_100$MeadowSizeStart_m2)
dat_100$MeadowSizeEnd_m2 <- as.numeric(dat_100$MeadowSizeEnd_m2)
dat_100$Perimeter <- as.numeric(dat_100$DisturbancePerimeter_m)
dat_100$PerimAreaRatio <- dat_100$Perimeter/dat_100$DisturbanceArea_m2
dat_100_small <- dat_100[dat_100$DistArea_m2 <= 100,]
dat_100_small <- dat_100_small[!is.na(dat_100_small$Recov.),]

dat2$StudyDuration_years <- dat2$StudyDuration_months/12
dat$StudyDuration_years <- dat$StudyDuration_months/12
dat_exp <- dat[dat$StudyType == "Experiment",]
dat_obs <- dat[dat$StudyType == "Observation",]

round(mean(dat_exp$StudyDuration_months, na.rm = T))
round(mean(dat_obs$StudyDuration_months, na.rm = T))

dat$DisturbanceArea_m2 <- as.numeric(dat$DisturbanceArea_m2)
dat$Recovery_months <- as.numeric(dat$Recovery_months)
dat$DisturbancePerimeter_m <- as.numeric(dat$DisturbancePerimeter_m)
dat$PerimRatio <- dat$DisturbancePerimeter_m/dat$DisturbanceArea_m2
dat$Perc_AboveGroundReduction <- as.numeric(dat$Perc_AboveGroundReduction)
dat$MeadowSizeStart_m2 <- as.numeric(dat$MeadowSizeStart_m2)
dat$MeadowDistRatio <- dat$DisturbanceArea_m2/dat$MeadowSizeStart_m2
dat$Perc_MeadowDist <- dat$MeadowDistRatio*100
dat$abs_lat <- abs(dat$Lat)

library(ggpubr)

cols <- c("Tropics" = "#000000", "Subtropics" = "#969696", "Temperate" = "#ffffff")

library(scales)

test <- dat[c(3,23,29:30)]
test <- test %>%
  filter(Perc_RecoveryDuringStudy > 90) %>%
  drop_na(DisturbanceArea_m2) %>%
  mutate(log_x = log10(DisturbanceArea_m2),
         log_y = log10(Recovery_months))
lm_mod <- lm(log_y ~ log_x, data = test)
poly_mod <- lm(log10(Recovery_months) ~ poly(log10(DisturbanceArea_m2),2), data = test)
summary(lm_mod)
summary(poly_mod)

# Figure 11: Recovery time as a function of disturbance area ----
# width = 800 height = 600

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
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
           label.x = 3, label.y = 0, size = 6,
           r.accuracy = 0.01,
           p.accuracy = 0.001)

test2 <- test
test2 <- test2[c(1:49,51:61),]

summary(lm(test$log_y~poly(test$log_x,2)))
summary(lm(test2$log_y~poly(test2$log_x,2)))

formula <- test$log_y~poly(test$log_x,2)

test %>%
  ggplot(aes(x = log_x, y = log_y)) +
  geom_point(color = "black", size = 4, shape = 21, alpha = 0.7, aes(fill=factor(StudyType))) +
  scale_x_continuous(breaks = seq(-2,6,1),
                     limits = c(-2,6)) +
  scale_y_continuous(breaks = seq(-0.5,2.5,0.5),
                     limits = c(-0.5,2.5)) +
  scale_fill_manual(values = c('#FCDB2D','dodgerblue2'),
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

## linear
# test %>%
#   ggplot(aes(x = log_x, y = log_y)) +
#   geom_point(size = 2) +
#   geom_point(color = "black", size = 2, shape = 1) +
#   scale_x_continuous(breaks = seq(-2,6,1),
#                      limits = c(-2,6)) +
#   scale_y_continuous(breaks = seq(-0.5,2.5,0.5),
#                      limits = c(-0.5,2.5)) +
#   labs(x = expression(log[10]~"[Disturbance Area"~ (m^-2) ~"]"),
#        y = expression(log[10]~"[Recovery Time (months)]")) +
#   stat_smooth(method = 'lm',
#               se = F,
#               color = 'red') +
#   theme_bw() +
#   theme(axis.text.x = element_text(size = 16, color = "black"),
#         axis.text.y = element_text(size = 16, color = "black"),
#         axis.title = element_text(size = 16, color = "black"),
#         panel.grid = element_blank(),
#         panel.background = element_blank()) +
#   stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#            label.x = 3, label.y = 0, size = 6,
#            r.accuracy = 0.01,
#            p.accuracy = 0.001) +
#   stat_regline_equation(formula = test$log_y ~ test$log_x,
#                         aes(label = ..eq.label..),
#                         label.x = 3, label.y = 0.25, size = 6)

test2 <- dat[c(29:30,37)]

test2 <- test2 %>%
  filter(Perc_RecoveryDuringStudy > 90) %>%
  drop_na(Recovery_months) %>%
  mutate(log_y = log10(Recovery_months))

lm_mod2 <- lm(test2$log_y ~ test2$PerimRatio)
summary(lm_mod2)
# see link below for interpretation of log-transformed linear models
# https://data.library.virginia.edu/interpreting-log-transformations-in-a-linear-model/
(exp(coef(lm_mod2)["test2$PerimRatio"]) - 1) * 100 # for every 1 unit increase in  7.6%

# Recovery time as a function of perimeter:area ratio ----
# width = 800 height = 600

test2 %>%
  ggplot(aes(x = PerimRatio, y = log_y)) +
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
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 20, label.y = 1.7, size = 6,
           r.accuracy = 0.01,
           p.accuracy = 0.001) +
  stat_regline_equation(formula = test2$log_y ~ test2$PerimRatio,
                        aes(label = ..eq.label..),
                        label.x = 20, label.y = 2, size = 6)

# Figure 10: Recovery time as a function of region and species ----
### width = 800 height = 600
dat %>%
  mutate(Region = factor(Region, levels = c("Tropics","Subtropics","Temperate"))) %>%
ggplot(aes(x = factor(NumSpecies), y = Recovery_months)) +
  geom_boxplot(outlier.colour = "white", outlier.fill = "white") +
  geom_jitter(shape=1, position=position_jitter(0.2), color = "black", fill = "white", size = 2) +
  scale_y_log10() +
  scale_x_discrete(limits= factor(1:6)) +
  labs(x = "Number of Species",
       y = "Recovery Time (months)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 16, color = "black"),
        strip.background = element_rect(fill = "white")) +
  facet_wrap(~Region)

library(MASS)

lm_testset <- dat[,c(10,23:25,29,34,37,40)]
lm_testset <- na.omit(lm_testset)
NROW(lm_testset)
lm_testset$log_recov <- log10(lm_testset$Recovery_months)
lm_testset <- subset(lm_testset, select = -c(Recovery_months)) # not appropriate to use Recovery_months as covariate for log-recovery months

full.model <- lm(log_recov~., data = lm_testset)

step.model <- stepAIC(full.model, direction = "backward",
                      trace = F)
summary(step.model)

# test <- dat[dat$NumSpecies <= 3, ]
# lm_testset_3species <- test[,c(10,25,29,34,37,40)]
# lm_testset_3species <- na.omit(lm_testset_3species)
# NROW(lm_testset_3species)
# lm_testset_3species$log_recov <- log10(lm_testset_3species$Recovery_months)
# lm_testset_3species <- subset(lm_testset_3species, select = -c(Recovery_months)) # not appropriate to use Recovery_months as covariate for log-recovery months
# 
# full.model_3species <- lm(log_recov~., data = na.omit(lm_testset_3species))
# summary(full.model_3species)
# 
# step.model_3species <- stepAIC(full.model_3species, direction = "backward",
#                                trace = F)
# summary(step.model_3species)

library(corrplot)

dv <- subset(lm_testset_3species, select = -c(log_recov))
testRes <- cor.mtest(dv, conf.level = 0.95)
M <- cor(dv)
corrplot(M, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag=FALSE)

# round(summary(step.model)$coefficients[1],3)
# round(summary(step.model)$coefficients[2],3)
# round(summary(step.model)$coefficients[3],3)
# round(summary(step.model)$coefficients[4],3)
# round(summary(step.model)$coefficients[5],3)
# round(summary(step.model)$coefficients[6],3)
