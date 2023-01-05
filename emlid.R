library(ggplot2)
library(dplyr)
library(ggh4x)

rm(list = ls())
dev.off()

setwd("D:/School/SeagrassRecovery/Data")

dat <- read.csv('emlid_Oct2022.csv')
dat$distance <- rep(c(-9,-8,-7,-6,-5,-4,-3.5,-3,-2.5,-2,-1,
                      0,1,2,2.5,3,3.5,4,5,6,7,8,9),12)
dat$treatment <- ifelse(dat$Site %in% c("1C","2C","3C","4C","5C","6C"), "Control","Treatment")
dat$location <- ifelse(dat$Site %in% c("1C","1T","2C","2T","3C","3T"), "Central","Northern")
dat$InsideOut <- ifelse(dat$distance %in% c(-3,-2.5,-2,-1,0,1,2,2.5,3),"inside","outside")

outside_mean <- dat %>%
  group_by(Site,InsideOut) %>%
  summarise(MeanOutside = mean(Elevation)) %>%
  filter(!InsideOut == 'inside')
outside_mean <- outside_mean[,c(1,3)]

dat <- left_join(dat,outside_mean, by = "Site")

dat$deltaElevation_cm <- round((dat$Elevation - dat$MeanOutside) * 100, digits = 2)

# dat <- dat[!dat$Name == 12,]
# dat2 <- dat[!dat$Solution.status == "FLOAT",]
# dat2 <- dat2[!dat2$Site == "4C",]

dat %>%
  group_by(Site) %>%
  summarise(MinElevation_m = min(Elevation),
            MaxElevation_m = max(Elevation),
            ElavationDiff_cm = (max(Elevation)-min(Elevation))*100)

dat %>%
  mutate(elevation_cm = Elevation * 100) %>%
  group_by(location) %>%
  summarise(mean_elevation_m = mean(Elevation),
            SD_m = sd(Elevation),
            SD_cm = sd(elevation_cm))

# a = 3.14*(300*300)
# d = 16.5
# dd = 18.7
# round((a*d)/1000000,1) # 4.7 m3 of sediment loss from 6T
# round((a*dd)/1000000,1) # 5.3 m3 of sediment loss from 5T

dat <- within(dat, Site <- factor(Site, levels = c("1C","1T","4C","4T","2C","2T",
                                                   "5C","5T","3C","3T","6C","6T")))

wrapper <- function(x, ...) paste(strwrap(x, ...), collapse = "\n")

# Create blank headers for a dummy variable that we will use for rows
outer_rect <- list(element_blank(), element_blank(), element_blank())
outer_text <- list(element_blank(), element_blank(), element_blank())

# Create black headers for first row of location strips, blank otherwise
loc_rect <- list(element_rect(fill = "black"),
                 element_rect(fill = "black"),
                 element_blank(), element_blank(), element_blank(), element_blank())
loc_text <- list(element_text(colour = "white", size = 16),
                 element_text(colour = "white", size = 16),
                 element_blank(), element_blank(), element_blank(), element_blank())

# Create 12 normal strips for the letter headers
final_rect <- elem_list_rect(fill = rep("white", 12))
final_text <- elem_list_text(colour = rep("black", 12))

dat$rownum <- NA
dat[dat$Site %in% c('1C','1T','4C','4T'),35] <- 1
dat[dat$Site %in% c('2C','2T','5C','5T'),35] <- 2
dat[dat$Site %in% c('3C','3T','6C','6T'),35] <- 3

# Figure 6: Bed elevation ----
# width = 1200 height = 800
dat %>%
  ggplot(aes(x = distance, y = Elevation)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(-8,8,2),
                     limits = c(-9,9)) +
  scale_y_continuous(breaks = seq(-1.7,-1,0.1),
                     limits = c(-1.7,-1)) +
  labs(x = "Distance (m)",
       y = "Elevation (m)") +
  annotate("rect", xmin = -3, xmax = 3, ymin = -Inf, ymax = Inf,
           alpha = .1,fill = "blue") +
  annotate("text", x = -6, y = -1.05, label = wrapper("Outside (South)", width = 7)) +
  annotate("text", x = 0, y = -1.01, label = "Inside") +
  annotate("text", x = 6, y = -1.05, label = wrapper("Outside (North)", width = 7)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 16, color = "black"),
        strip.background = element_rect(fill = "white")) +
  facet_nested_wrap(rownum~location + Site, nrow = 3,
                    strip = strip_nested(
                      background_x = c(outer_rect, loc_rect, final_rect),
                      text_x = c(outer_text, loc_text, final_text))) +
  theme(panel.spacing.y = unit(-15, "mm"))

ggplot(data = dat, aes(x = distance, y = deltaElevation_cm)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "longdash") +
  scale_x_continuous(breaks = seq(-8,8,2),
                     limits = c(-9,9)) +
  scale_y_continuous(breaks = seq(-18,4,4),
                     limits = c(-18,4)) +
  labs(x = "Distance (m)",
       y = "Outside Mean Removed - Elevation (cm)") +
  annotate("rect", xmin = -3, xmax = 3, ymin = -Inf, ymax = Inf,
           alpha = .1,fill = "blue") +
  annotate("text", x = -6, y = -14, label = wrapper("Outside (South)", width = 7)) +
  annotate("text", x = 0, y = 2, label = "Inside") +
  annotate("text", x = 6, y = -14, label = wrapper("Outside (North)", width = 7)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 16, color = "black"),
        strip.background = element_rect(fill = "white")) +
  facet_wrap(~Site)

dat %>%
  group_by(Site, InsideOut) %>%
  summarise(Min = min(deltaElevation_cm)) %>%
  filter(!InsideOut == 'outside')
