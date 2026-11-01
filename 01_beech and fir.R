library(readxl)
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)


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

# define the cleaned data as md, my data
md <- beechfir2025_halfhr_clean



