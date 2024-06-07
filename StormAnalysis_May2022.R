library(tidyverse)
library(data.table)
library(openair)
library(grid)
library(gridExtra)

rm(list = ls())
dev.off()

# Quantifying South Bay Storm May 2022 ----
# There was a strong storm in early May 2022 that is believed to have eroded the edge treatment sites in South Bay.
# NOAA maintains a tide monitoring location in Wachapreague, VA that also collects hourly meteorology. Meterology data from 2022 was downloaded from: https://tidesandcurrents.noaa.gov/met.html?id=8631044
# Beaufort Wind Force Scale - see p. 31 (https://web.archive.org/web/20170505114755/https://library.wmo.int/pmb_ged/wmo_558_en-v1.pdf)

setwd("D:/School/SeagrassRecovery/Data/Wachapreague/Meteorology")

met_dat <- read.csv('CO-OPS_8631044_met_2022.csv')

met_dat <- met_dat %>%
  rename(date = Date,
         time_gmt = Time..GMT.,
         windspeed_ms = Wind.Speed..m.s.,
         windDir_deg = Wind.Dir..deg.,
         windgust_ms = Wind.Gust..m.s.,
         atemp_c = Air.Temp...C.,
         press_mb = Baro..mb.,
         humidity = Humidity....,
         visb_km = Visibility..km.) %>%
  mutate(date = as.Date(date, format = "%Y/%m/%d"),
         datetime = as.POSIXct(paste(date, time_gmt), format = "%Y-%m-%d %H:%M", tz = 'EST'),
         year = year(date),
         across(where(is.character), ~na_if(., "-")),
         windspeed_ms = as.numeric(windspeed_ms),
         windDir_deg = as.numeric(windDir_deg),
         windgust_ms = as.numeric(windgust_ms),
         atemp_c = as.numeric(atemp_c),
         press_mb = as.numeric(press_mb),
         humidity = as.numeric(humidity),
         visb_km = as.numeric(visb_km))

sum(is.na(met_dat$windgust_ms)) # 61
sum(is.na(met_dat$press_mb)) # 56

# Linear interpolate for data gaps less than or equal to 2 hours ----
library(zoo)

interpolate <- function(df){
  df$windgust_int <- na.approx(df$windgust_ms, maxgap = 2, na.rm = T)
  df$press_int <- na.approx(df$press_mb, maxgap = 2, na.rm = T)
  return(df)
}

met_dat <- interpolate(met_dat)

met_dat <- met_dat %>%
  mutate(windgust_ms = ifelse(is.na(windgust_ms),windgust_int,windgust_ms),
         press_mb = ifelse(is.na(press_mb),press_int,press_mb)) %>%
  select(!c(windgust_int,press_int))

sum(is.na(met_dat$windgust_ms)) # 25
sum(is.na(met_dat$press_mb)) # 20

# Beaufort Wind Force Scale ----

beaufort <- function(df) {
  df$beaufort_scale <- ifelse(df$windgust_ms < 0.2, 'Calm',
                              ifelse(df$windgust_ms >= 0.2 & df$windgust_ms < 1.5, 'LightAir',
                                     ifelse(df$windgust_ms >= 1.5 & df$windgust_ms < 3.3, 'LightBreeze',
                                            ifelse(df$windgust_ms >= 3.3 & df$windgust_ms < 5.4, 'GentleBreeze',
                                                   ifelse(df$windgust_ms >= 5.4 & df$windgust_ms < 7.9, 'ModerateBreeze',
                                                          ifelse(df$windgust_ms >= 7.9 & df$windgust_ms < 10.7, 'FreshBreeze',
                                                                 ifelse(df$windgust_ms >= 10.7 & df$windgust_ms < 13.8, 'StrongBreeze',
                                                                        ifelse(df$windgust_ms >= 13.8 & df$windgust_ms < 17.1, 'NearGale',
                                                                               ifelse(df$windgust_ms >= 17.1 & df$windgust_ms < 20.7, 'Gale',
                                                                                      ifelse(df$windgust_ms >= 20.7 & df$windgust_ms < 24.4, 'StrongGale',
                                                                                             ifelse(df$windgust_ms >= 24.4 & df$windgust_ms < 28.4, 'Storm',
                                                                                                    ifelse(df$windgust_ms >= 28.4 & df$windgust_ms < 32.6, 'ViolentStorm',
                                                                                                           ifelse(df$windgust_ms >= 32.6, 'Hurricane', NA)
                                                                                                    ))))))))))))
return(df)
}

met_dat <- met_dat %>%
  beaufort()

aa <- met_dat %>%
  filter(date >= as.POSIXct('2022-04-20 00:00:00') & date <= as.POSIXct('2022-05-18 23:00:00')) %>%
  select(datetime, windDir_deg, windgust_ms, press_mb)

aa$press_mb <- scales::rescale(aa$press_mb, from = c(990, 1040), to = c(0, 21))

# Time series of wind gust, barometric pressure, and beaufort scale ----
# width = 800 height = 600
p1 <- ggplot(data = aa, aes(x = datetime, y = windgust_ms)) +
  annotate("rect",
           xmin = as.POSIXct("2022-04-20 00:00:00"),
           xmax = as.POSIXct("2022-05-18 23:00:00"),
           ymin = -Inf, ymax = 0.2,
           fill = 'gray95', alpha = 0.5) +
  annotate("rect",
           xmin = as.POSIXct("2022-04-20 00:00:00"),
           xmax = as.POSIXct("2022-05-18 23:00:00"),
           ymin = 0.2, ymax = 1.5,
           fill = '#ffffcc', alpha = 0.5) +
  annotate("rect",
           xmin = as.POSIXct("2022-04-20 00:00:00"),
           xmax = as.POSIXct("2022-05-18 23:00:00"),
           ymin = 1.5, ymax = 3.3,
           fill = '#ffeda0', alpha = 0.5) +
  annotate("rect",
           xmin = as.POSIXct("2022-04-20 00:00:00"),
           xmax = as.POSIXct("2022-05-18 23:00:00"),
           ymin = 3.3, ymax = 5.4,
           fill = '#fed976', alpha = 0.5) +
  annotate("rect",
           xmin = as.POSIXct("2022-04-20 00:00:00"),
           xmax = as.POSIXct("2022-05-18 23:00:00"),
           ymin = 5.4, ymax = 7.9,
           fill = '#feb24c', alpha = 0.5) +
  annotate("rect",
           xmin = as.POSIXct("2022-04-20 00:00:00"),
           xmax = as.POSIXct("2022-05-18 23:00:00"),
           ymin = 7.9, ymax = 10.7,
           fill = '#fd8d3c', alpha = 0.5) +
  annotate("rect",
           xmin = as.POSIXct("2022-04-20 00:00:00"),
           xmax = as.POSIXct("2022-05-18 23:00:00"),
           ymin = 10.7, ymax = 13.8,
           fill = '#fc4e2a', alpha = 0.5) +
  annotate("rect",
           xmin = as.POSIXct("2022-04-20 00:00:00"),
           xmax = as.POSIXct("2022-05-18 23:00:00"),
           ymin = 13.8, ymax = 17.1,
           fill = '#e31a1c', alpha = 0.5) +
  annotate("text", x = as.POSIXct("2022-05-14 00:00:00"), y = 14.5, label = "Near Gale", hjust = 0, size = 5) +
  annotate("rect",
           xmin = as.POSIXct("2022-04-20 00:00:00"),
           xmax = as.POSIXct("2022-05-18 23:00:00"),
           ymin = 17.1, ymax = 21,
           fill = '#b10026', alpha = 0.5) +
  annotate("text", x = as.POSIXct("2022-05-14 00:00:00"), y = 17.8, label = "Gale", hjust = 0, size = 5) +
  geom_line(linewidth = 0.5) +
  geom_line(data = aa, aes(x = datetime, y = press_mb), color = 'blue', linewidth = 0.8, linetype = 'longdash') +
  scale_x_datetime(limits = c(as.POSIXct("2022-04-20 00:00:00"),as.POSIXct("2022-05-18 23:00:00")),
                   expand = c(0,0), date_breaks = "4 days",date_labels = "%b %d") +
  scale_y_continuous(
    name = expression(Hourly~Maximum~Wind~Speed~(m~s^-1)),
    breaks = seq(0, 20, 2),
    limits = c(0, 21),
    expand = c(0, 0),
    sec.axis = sec_axis(
      trans = ~ scales::rescale(.x, from = c(0, 21), to = c(990, 1040)),
      name = "Barometric Pressure (mb)")) +
  labs(x = "2022") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 14, color = 'black'),
        axis.text.y.right = element_text(size = 14, color = 'blue'),
        axis.title.y.right = element_text(size = 14, color = 'blue'))

bb <- met_dat %>%
  group_by(beaufort_scale) %>%
  summarise(Total_2022 = n()) %>%
  arrange(factor(beaufort_scale, levels = c('Calm', 'LightAir', 'LightBreeze',
                                            'GentleBreeze', 'ModerateBreeze', 'FreshBreeze',
                                            'StrongBreeze', 'NearGale', 'Gale')))

cc <- aa %>%
  beaufort() %>%
  group_by(beaufort_scale) %>%
  summarise(Total_AprMay2022 = n()) %>%
  arrange(factor(beaufort_scale, levels = c('Calm', 'LightAir', 'LightBreeze',
                                            'GentleBreeze', 'ModerateBreeze', 'FreshBreeze',
                                            'StrongBreeze', 'NearGale', 'Gale')))

bb <- left_join(bb,cc, by = 'beaufort_scale') %>%
  mutate(Frac = round((Total_AprMay2022/Total_2022)*100))
View(bb)

# Results ----
# Comparing the total number of gale measurements between the April-May 2022 sampling period,
# there were 17 observations (see bb). In 2022, there were a total of 29 gale observations,
# suggesting that this one wind event in early-May 2009 made up 59% of the total gale observations
# for 2022. Similarly, this one event made up 42 out of 88 (i.e., 48%) of the 2022 near gale observations. 

# Wind Rose ----
# width = 800 height = 600
# First, need at make some substitutions to the windRose function

body(windRose)[[3]] <- substitute(if (length(cols) == 1 && cols == "greyscale") {
                                    trellis.par.set(list(strip.background = list(col = "white")))
                                    calm.col <- "black"
                                  } else {
                                    calm.col <- "white"
                                  })

body(windRose)[[15]] <- substitute(if (is.character(statistic)) {
  ok.stat <- c("prop.count", "prop.mean", "abs.count", "frequency")
  if (!is.character(statistic) || !statistic[1] %in% ok.stat) {
    warning("In windRose(...):\n  statistic unrecognised", 
            "\n  enforcing statistic = 'prop.count'", call. = FALSE)
    statistic <- "prop.count"
  }
  if (statistic == "prop.count") {
    stat.fun <- length
    stat.unit <- "%"
    stat.scale <- "all"
    stat.lab <- NULL
    stat.fun2 <- function(x) format(mean(x, na.rm = TRUE), 
                                    digits = dig.lab)
    stat.lab2 <- "mean"
    stat.labcalm <- function(x) round(x, 1)
  }
  if (statistic == "prop.mean") {
    stat.fun <- function(x) sum(x, na.rm = TRUE)
    stat.unit <- "%"
    stat.scale <- "panel"
    stat.lab <- "Proportion contribution to the mean (%)"
    stat.fun2 <- function(x) format(mean(x, na.rm = TRUE), 
                                    digits = 5)
    stat.lab2 <- "mean"
    stat.labcalm <- function(x) round(x, 1)
  }
  if (statistic == "abs.count" | statistic == "frequency") {
    stat.fun <- length
    stat.unit <- ""
    stat.scale <- "none"
    stat.lab <- "Count by wind direction"
    stat.fun2 <- function(x) round(length(x), 0)
    stat.lab2 <- "count"
    stat.labcalm <- function(x) round(x, 0)
  }
})

p2 <- windRose(aa, ws = 'windgust_ms', wd = 'windDir_deg',
         breaks = c(0,1,2,3,5,8,11,14,17,21),
         cols = c('gray95','#ffffcc','#ffeda0','#fed976','#feb24c',
                  '#fd8d3c','#fc4e2a','#e31a1c','#b10026'),
         annotate = T,
         paddle = F,
         key.position = 'right',
         key.header = expression(Wind~Speed~(m~s^-1)),
         key.footer = NULL,
         offset = 5,
         angle.scale = 235,
         fontsize = 24,
         par.settings = list(axis.line = list(col = 'white')),
         key = list(labels = c("0 to 1 (Calm)",
                               "1 to 2 (Light Air)",
                               "2 to 3 (Light Breeze)",
                               "3 to 5 (Gentle Breeze)",
                               "5 to 8 (Moderate Breeze)", 
                               "8 to 11 (Fresh Breeze)",
                               "11 to 14 (Strong Breeze)",
                               "14 to 17 (Near Gale)",
                               "17 to 21 (Gale)")))

# Combine the ggplot and the Openair plot using gridExtra::grid.arrange
openair_grob <- grid.grabExpr(print(p2), wrap.grobs = TRUE)

grid.arrange(p1, openair_grob, ncol = 1)
