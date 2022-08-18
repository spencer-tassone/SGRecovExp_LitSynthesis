library(ggplot2)
library(dplyr)

rm(list = ls())
dev.off()

setwd("D:/School/SeagrassRecovery/Data")
dat <- read.csv('WaterQuality_forR.csv')

dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
dat$LocTreat <- paste(dat$Location, dat$Treatment)
dat$SiteTreat <- paste(dat$Site, dat$Treatment)
dat$TSS[dat$TSS > 100] <- NA

#### Turbidity

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
           ymin = -6, ymax = -3, fill = 'white') +
  annotate('segment', x = c(4.4, 4.6, 10.4, 10.6),
           xend = c(4.4, 4.6, 10.4, 10.6), y = -6, yend = -3)

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

##### TSS

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

#### Dissolved Oxygen

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

ggplot(dat, aes(x = factor(format(Date, "%b %Y"),level = c('Jul 2020','Aug 2020','Sep 2020','Oct 2020',
                                                           'May 2021','Jun 2021','Jul 2021','Aug 2021','Sep 2021','Oct 2021',
                                                           'Apr 2022','May 2022','Jun 2022','Jul 2022','Aug 2022')),
                y = DO_mgl, fill = Location, group = interaction(Date, Location))) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,16,2)) +
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
  coord_cartesian(clip = 'off', ylim = c(0, 16)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate('rect', xmin = c(4.4, 10.4), xmax = c(4.6, 10.6),
           ymin = -1.2, ymax = -0.5, fill = 'white') +
  annotate('segment', x = c(4.4, 4.6, 10.4, 10.6),
           xend = c(4.4, 4.6, 10.4, 10.6), y = -1.2, yend = -0.5)

#### Salinity

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

#### Pelagic CHLa

ggplot(dat, aes(x = factor(format(Date, "%b %Y"),level = c('Jul 2020','Aug 2020','Sep 2020','Oct 2020',
                                                           'May 2021','Jun 2021','Jul 2021','Aug 2021','Sep 2021','Oct 2021',
                                                           'Apr 2022','May 2022','Jun 2022','Jul 2022','Aug 2022')),
                y = CHLa_pelagic, fill = Treatment, group = interaction(Date, Treatment))) +
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

ggplot(dat, aes(x = factor(format(Date, "%b %Y"),level = c('Jul 2020','Aug 2020','Sep 2020','Oct 2020',
                                                           'May 2021','Jun 2021','Jul 2021','Aug 2021','Sep 2021','Oct 2021',
                                                           'Apr 2022','May 2022','Jun 2022','Jul 2022','Aug 2022')),
                y = CHLa_pelagic, fill = Location, group = interaction(Date, Location))) +
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

#### Benthic CHLa

ggplot(dat, aes(x = factor(format(Date, "%b %Y"),level = c('Jul 2020','Aug 2020','Sep 2020','Oct 2020',
                                                           'May 2021','Jun 2021','Jul 2021','Aug 2021','Sep 2021','Oct 2021',
                                                           'Apr 2022','May 2022','Jun 2022','Jul 2022','Aug 2022')),
                y = CHL_benthic, fill = Treatment, group = interaction(Date, Treatment))) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,120,20)) +
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
  coord_cartesian(clip = 'off', ylim = c(0, 120)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate('rect', xmin = c(4.4, 10.4), xmax = c(4.6, 10.6),
           ymin = -9, ymax = -3, fill = 'white') +
  annotate('segment', x = c(4.4, 4.6, 10.4, 10.6),
           xend = c(4.4, 4.6, 10.4, 10.6), y = -9, yend = -3)

ggplot(dat, aes(x = factor(format(Date, "%b %Y"),level = c('Jul 2020','Aug 2020','Sep 2020','Oct 2020',
                                                           'May 2021','Jun 2021','Jul 2021','Aug 2021','Sep 2021','Oct 2021',
                                                           'Apr 2022','May 2022','Jun 2022','Jul 2022','Aug 2022')),
                y = CHL_benthic, fill = Location, group = interaction(Date, Location))) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,120,20)) +
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
  coord_cartesian(clip = 'off', ylim = c(0, 120)) +
  annotation_custom(grid::rectGrob(gp = grid::gpar(fill = NA))) +
  annotate('rect', xmin = c(4.4, 10.4), xmax = c(4.6, 10.6),
           ymin = -9, ymax = -3, fill = 'white') +
  annotate('segment', x = c(4.4, 4.6, 10.4, 10.6),
           xend = c(4.4, 4.6, 10.4, 10.6), y = -9, yend = -3)
