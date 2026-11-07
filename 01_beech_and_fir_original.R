library(readxl)
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggrepel)



cud <- read_excel("data/003a_Cudinka_7ddm.xlsx")
cor <- read_excel("data/004a_CorUvala_11ddm.xlsx")
gul <- read_excel("data/005a_GulVrh_8ddm.xlsx")
uva <- read_excel("data/007a_Uvalica_8ddm.xlsx")
lug <- read_excel("data/009a_Lugarnica_9ddm.xlsx")

meta <- read_excel("dendrometers list.xlsx" )

# change the single digit serial number to double digit
# change the code for C.uval from CU to CO so CU is exluded to Cudinka
meta <- meta %>%
  mutate(
    `ddm_serial_no.` = sprintf("%02d",  as.integer(`ddm_serial_no.`)),
    CODE = if_else(
      Plot=="C.uvala" & str_detect (CODE, "^CU"),
      str_replace(CODE, "^CU", "CO"),
      CODE
    )
  )


head(meta)


# select only beech and fir
target_serials <- meta %>% 
  filter (str_detect(tolower(species), 
                     "fagus_sylvatica|abies_alba")) %>%
  pull ('ddm_serial_no.') %>%
  as.character()

dfs <- list(cud = cud, cor = cor, gul = gul, 
            uva = uva, lug = lug)

select_target <- function (df){
  serials <- str_match(names(df), "^\\d+\\.\\s*(\\d+)")[,2]
  keep <- names(df) == "Time" | serials %in% target_serials
  df [, keep, drop = FALSE]
}

beechfironly <- map(dfs, select_target)


# select the time series from 01/01/2025 to 12/05/2025
start_time <- as.POSIXct(
  "01/01/2025 01:00",
  format = "%m/%d/%Y %H:%M"
)

end_time <- as.POSIXct(
  "12/05/2025 01:00",
  format = "%m/%d/%Y %H:%M"
)


beechfir2025 <- lapply(beechfironly, function(data){
  data$Time <- as.POSIXct(
    data$Time, format = "%m/%d/%Y %H:%M"
  )
  data <- data[data$Time >= start_time & 
                 data$Time <= end_time,]
  return(data)
})


# keep only the time stamps at every half hour
beechfir2025_halfhr <- lapply(beechfir2025, function(data)
{
  data <- data [
    minute(data$Time)%in% c(0,30)&
      second (data$Time) == 0,
  ]
  return(data)
})

# remove N.A.
beechfir2025_halfhr_clean <- lapply(beechfir2025_halfhr, function(data)
{
  data <- na.omit(data) 
  return(data)
})

# define the cleaned data as MYDATA
MYDATA <- beechfir2025_halfhr_clean

# calculate Stem radius variation
SRV <- lapply(names(MYDATA), function(site_name){
  data <- MYDATA[[site_name]]
  
  tree_cols <- colnames(data)[2:ncol(data)]
  SRV <- data.frame(Time = data$Time)
  
  for (col in tree_cols){
    values <- data[[col]]
    fluct <- (values - values[1])/(2*pi)*1000
    SRV[[col]] <- fluct
  }
  SRV$Time <- as.POSIXct(
    SRV$Time, format = "%m/%d/%Y %H:%M"
  )
  
  SRV$Site <-  site_name
  
  return (SRV)
})


# change the df names in SRV
names (SRV) <- c("cud","cor","gul","uva","lug")

# change the column names to each sensor's unique CODE
lookup <- setNames(meta$CODE,meta$ddm_serial_no.)

SRV <- lapply(SRV, function (df){
  old <- names(df)[-1]
  serial <- sub("^\\d+\\.\\s*(\\d+)\\s+Increment.*$", "\\1", old)
  new <- unname(lookup[serial])
  names(df)[-1] <- new
  df
})

# delete the last useless column in each df
SRV <- lapply(SRV, function(d) d[, !is.na(names(d))])

#create df for site elevatoin
elev <- data.frame(
  name = c("rap","med","cud","cor","gul","rje","uva",
           "pan","lug","vre"),
  elevation = c(542,685,1097,912,836,747,858,862,930,921),
  beech_num = c(7,4,2,4,3,6,4,5,4,0),
  fir_num = c(0,0,5,4,3,0,4,0,4,3)
)


# select all the sensors for beech, calculate the mean of beech sensors
# in each site, then create a df for site_mean 

fagus_codes <- meta$CODE [grepl("^Fagus_sylvatica", meta$species, 
                                ignore.case = TRUE)]

SRV_site_mean_beech <- bind_rows(lapply(names(SRV), function (s){
  df <- SRV[[s]]
  cols <- intersect(names(df),fagus_codes)

data.frame(
  Time = as.POSIXct(df$Time),
  site = s,
  mean_incr = rowMeans(df[, cols, drop = FALSE], na.rm = TRUE),
  n_sensors = length(cols)
)
}))


# add elevation to the site means
SRV_site_mean_beech <- SRV_site_mean_beech %>%
  left_join(elev %>% mutate(name = as.character(name)) %>% select(name, elevation),
            by = c("site" = "name")) %>%
  mutate(
    site_label = paste0(site, " (", elevation, " m)"),
    # order the legend from highest to lowest elevation
    site_label = factor(site_label,
                        levels = unique(site_label[order(-elevation)]))
  )

## plot the site means for beeches
ggplot(SRV_site_mean_beech, aes(Time, mean_incr, colour = site_label)) +
  geom_line() +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  labs(
    title = "Stem Radius Variation (SRV) of beeches",
    y = "Site-mean SRV of beeches [mm]", colour = "Site (elevation)") +
  theme_bw()

## plot the site means for beeches
#library(ggrepel) # to add text labels to the lines

site_labels_SRVbeech <- SRV_site_mean_beech %>%
  group_by(site_label) %>%
  filter(Time == max(Time, na.rm = TRUE)) %>%
  ungroup()

ggplot(SRV_site_mean_beech, aes(Time, mean_incr, colour = site_label)) +
  geom_line(linewidth = 0.8) +
  geom_text_repel(
    data = site_labels_SRVbeech,
    aes(label = site_label),
    direction = "y",
    hjust = 0,
    nudge_x = 10000,
    segment.color = NA,
    show.legend = FALSE
  ) +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  labs(
    title = "Stem Radius Variation (SRV) of beeches",
    y = "Site-mean SRV of beeches [mm]",
    x = "Time",
    colour = "Site (elevation)"
  ) +
  theme_bw()



### same thing, for firs

# select all the sensors for firs, calculate the mean of fir sensors
# in each site, then create a df for site_mean 

abies_codes <- meta$CODE [grepl("^Abies_alba", meta$species, 
                                ignore.case = TRUE)]

SRV_site_mean_fir <- bind_rows(lapply(names(SRV), function (s){
  df <- SRV[[s]]
  cols <- intersect(names(df),abies_codes)
  
  data.frame(
    Time = as.POSIXct(df$Time),
    site = s,
    mean_incr = rowMeans(df[, cols, drop = FALSE], na.rm = TRUE),
    n_sensors = length(cols)
  )
}))


# add elevation to the site means
SRV_site_mean_fir <- SRV_site_mean_fir %>%
  left_join(elev %>% mutate(name = as.character(name)) %>% select(name, elevation),
            by = c("site" = "name")) %>%
  mutate(
    site_label = paste0(site, " (", elevation, " m)"),
    # order the legend from highest to lowest elevation
    site_label = factor(site_label,
                        levels = unique(site_label[order(-elevation)]))
  )

## plot the site means for beeches
ggplot(SRV_site_mean_fir, aes(Time, mean_incr, colour = site_label)) +
  geom_line() +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  labs(
    title = "Stem Radius Variation (SRV) of firs",
    y = "Site-mean SRV of firs [mm]", colour = "Site (elevation)") +
  theme_bw()

## plot the site means for beeches
#library(ggrepel) # to add text labels to the lines

site_labels_SRVfir <- SRV_site_mean_fir %>%
  group_by(site_label) %>%
  filter(Time == max(Time, na.rm = TRUE)) %>%
  ungroup()

ggplot(SRV_site_mean_fir, aes(Time, mean_incr, colour = site_label)) +
  geom_line(linewidth = 0.8) +
  geom_text_repel(
    data = site_labels_SRVfir,
    aes(label = site_label),
    direction = "y",
    hjust = 0,
    nudge_x = 10000,
    segment.color = NA,
    show.legend = FALSE
  ) +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  labs(
    title = "Stem Radius Variation (SRV) of firs",
    y = "Site-mean SRV of firs [mm]",
    x = "Time",
    colour = "Site (elevation)"
  ) +
  theme_bw()


## calculate the GRO

GRO <- lapply(SRV, function(df) {
  df[-1] <- lapply(df[-1], cummax)   # all columns except Time
  df
})

GRO_site_mean_beech <- bind_rows(lapply(names(GRO), function (s){
  df <- GRO[[s]]
  cols <- intersect(names(df),fagus_codes)
  
  data.frame(
    Time = as.POSIXct(df$Time),
    site = s,
    mean_incr = rowMeans(df[, cols, drop = FALSE], na.rm = TRUE),
    n_sensors = length(cols)
  )
}))


# add elevation to the site means
GRO_site_mean_beech <- GRO_site_mean_beech %>%
  left_join(elev %>% mutate(name = as.character(name)) %>% select(name, elevation),
            by = c("site" = "name")) %>%
  mutate(
    site_label = paste0(site, " (", elevation, " m)"),
    # order the legend from highest to lowest elevation
    site_label = factor(site_label,
                        levels = unique(site_label[order(-elevation)]))
  )

## plot the site means for beeches
ggplot(GRO_site_mean_beech, aes(Time, mean_incr, colour = site_label)) +
  geom_line() +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  labs(
    title = "Stem Radius Growtn (GRO) of beeches",
    y = "Site-mean GRO of beeches [mm]", colour = "Site (elevation)") +
  theme_bw()

## plot the site means for beeches with added text labels to the lines

site_labels_GRObeech <- GRO_site_mean_beech %>%
  group_by(site_label) %>%
  filter(Time == max(Time, na.rm = TRUE)) %>%
  ungroup()

ggplot(GRO_site_mean_beech, aes(Time, mean_incr, colour = site_label)) +
  geom_line(linewidth = 0.8) +
  geom_text_repel(
    data = site_labels_GRObeech,
    aes(label = site_label),
    direction = "y",
    hjust = 0,
    nudge_x = 10000,
    segment.color = NA,
    show.legend = FALSE
  ) +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  labs(
    title = "Stem Radius Growtn (GRO) of beeches",
    y = "Site-mean GRO of beeches [mm]",
    x = "Time",
    colour = "Site (elevation)"
  ) +
  theme_bw()



### same thing, for firs

GRO_site_mean_fir <- bind_rows(lapply(names(GRO), function (s){
  df <- GRO[[s]]
  cols <- intersect(names(df),abies_codes)
  
  data.frame(
    Time = as.POSIXct(df$Time),
    site = s,
    mean_incr = rowMeans(df[, cols, drop = FALSE], na.rm = TRUE),
    n_sensors = length(cols)
  )
}))


# add elevation to the site means
GRO_site_mean_fir <- GRO_site_mean_fir %>%
  left_join(elev %>% mutate(name = as.character(name)) %>% select(name, elevation),
            by = c("site" = "name")) %>%
  mutate(
    site_label = paste0(site, " (", elevation, " m)"),
    # order the legend from highest to lowest elevation
    site_label = factor(site_label,
                        levels = unique(site_label[order(-elevation)]))
  )

## plot the site means for beeches
ggplot(GRO_site_mean_fir, aes(Time, mean_incr, colour = site_label)) +
  geom_line() +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  labs(
    title = "Stem Radius Growtn (GRO) of firs",
    y = "Site-mean GRO of firs [mm]", colour = "Site (elevation)") +
  theme_bw()

## plot the site means for beeches
#library(ggrepel) # to add text labels to the lines

site_labels_GROfir <- GRO_site_mean_fir %>%
  group_by(site_label) %>%
  filter(Time == max(Time, na.rm = TRUE)) %>%
  ungroup()

ggplot(GRO_site_mean_fir, aes(Time, mean_incr, colour = site_label)) +
  geom_line(linewidth = 0.8) +
  geom_text_repel(
    data = site_labels_GROfir,
    aes(label = site_label),
    direction = "y",
    hjust = 0,
    nudge_x = 10000,
    segment.color = NA,
    show.legend = FALSE
  ) +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  labs(
    title = "Stem Radius Growtn (GRO) of firs",
    y = "Site-mean GRO of firs [mm]",
    x = "Time",
    colour = "Site (elevation)"
  ) +
  theme_bw()

# calculate the TWD tree water deficit based on SRV and GRO

TWD <- Map(function(gro, srv) {
  out <- srv                       # keeps Time and the structure
  out[-1] <- srv[-1] - gro[-1]    # GRO - SRV, all columns except Time
  out
}, GRO, SRV)

TWD_site_mean_beech <- bind_rows(lapply(names(TWD), function (s){
  df <- TWD[[s]]
  cols <- intersect(names(df),fagus_codes)
  
  data.frame(
    Time = as.POSIXct(df$Time),
    site = s,
    mean_incr = rowMeans(df[, cols, drop = FALSE], na.rm = TRUE),
    n_sensors = length(cols)
  )
}))


# add elevation to the site means
TWD_site_mean_beech <- TWD_site_mean_beech %>%
  left_join(elev %>% mutate(name = as.character(name)) %>% select(name, elevation),
            by = c("site" = "name")) %>%
  mutate(
    site_label = paste0(site, " (", elevation, " m)"),
    # order the legend from highest to lowest elevation
    site_label = factor(site_label,
                        levels = unique(site_label[order(-elevation)]))
  )

## plot the site means for beeches
ggplot(TWD_site_mean_beech, aes(Time, mean_incr, colour = site_label)) +
  geom_line() +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  labs(
    title = "Tree Water Deficit (TWD) of beeches",
    y = "Site-mean TWD of beeches [mm]", colour = "Site (elevation)") +
  theme_bw()

### same thing, for firs
TWD_site_mean_fir <- bind_rows(lapply(names(TWD), function (s){
  df <- TWD[[s]]
  cols <- intersect(names(df),abies_codes)
  
  data.frame(
    Time = as.POSIXct(df$Time),
    site = s,
    mean_incr = rowMeans(df[, cols, drop = FALSE], na.rm = TRUE),
    n_sensors = length(cols)
  )
}))


# add elevation to the site means
TWD_site_mean_fir <- TWD_site_mean_fir %>%
  left_join(elev %>% mutate(name = as.character(name)) %>% select(name, elevation),
            by = c("site" = "name")) %>%
  mutate(
    site_label = paste0(site, " (", elevation, " m)"),
    # order the legend from highest to lowest elevation
    site_label = factor(site_label,
                        levels = unique(site_label[order(-elevation)]))
  )

## plot the site means for beeches
ggplot(TWD_site_mean_fir, aes(Time, mean_incr, colour = site_label)) +
  geom_line() +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  labs(
    title = "Tree Water Deficit (TWD) of firs",
    y = "Site-mean TWD of firs [mm]", colour = "Site (elevation)") +
  theme_bw()

## one global max for each species
## standardized by the 95% quantile
## all TWD values are between -1 to 0.
q95beech <-  quantile (abs(TWD_site_mean_beech$mean_incr),0.95, na.rm=TRUE)
TWD_beech_std <- TWD_site_mean_beech
TWD_beech_std$mean_incr_std <- TWD_site_mean_beech$mean_incr / q95beech

q95fir <- quantile (abs(TWD_site_mean_fir$mean_incr), 0.95, na.rm=TRUE)
TWD_fir_std <- TWD_site_mean_fir
TWD_fir_std$mean_incr_std <- TWD_site_mean_fir$mean_incr / q95fir

## complete the two TWD df by adding species column
TWD_beech_std$species <- "beech"
TWD_fir_std$species   <- "fir"
## merge df of two species into one df.
TWD_std <- rbind(TWD_beech_std, TWD_fir_std)


ggplot(TWD_std, aes(x = Time, y = mean_incr_std, color = species)) +
  geom_line(linewidth = 0.4) +
  facet_wrap(~ site_label, ncol = 1) +
  scale_color_manual(values = c(beech = "#fc4e07", fir = "#1177b4")) +
  scale_x_datetime(
    date_breaks = "1 month",      
    date_labels = "%b %d"     # e.g. "Jun 01"; use "%b" for month only
  ) +
  labs(
    title = "Comparing TWD between beech and fir across elevations, Plitvice 2025",
    x = NULL, y = "Standardised TWD (Tree Water Deficit)", color = "Species") +
  theme_bw() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))
