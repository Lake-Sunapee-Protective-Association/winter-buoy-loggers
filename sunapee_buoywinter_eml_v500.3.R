#*      Cary Institute of Ecosystem Studies (Millbrook, NY)             *
#*                                                                      *
#* TITLE:   Sunapee_buoywinter_EML_v500.3.R                             *
#* AUTHOR:  Bethel Steele                                               *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 4.1.3, RStudio 2022.2.02      *
#* DATE:    24May2022                                                   *
#* PROJECT: sunapee winter buoy                                         *
#* PURPOSE: create EML for dataset                                      *

library(devtools)
library(EML)
install_github('EDIorg/EMLassemblyline')
library(EMLassemblyline)

library(tidyverse)
library(kableExtra)

#point to directory
dir = 'C:/Users/steeleb/Dropbox/EDI_submissions/sunape_buoy_winter_2014-2021_v24May2022/'

#basic metadata
template_core_metadata(
  path = dir,
  license = "CCBY"
)


#### TABLE ATTRIBUTES ####
template_table_attributes(
  path = dir,
  data.path = file.path(dir, 'data/'),
  data.table = c('2014-2015_wintertempstring_L1_v2022.csv', 
                 '2015-2016_wintertempstring_L1_v2022.csv', 
                 '2016-2017_wintertempstring_L1_v2022.csv', 
                 '2017-2018_wintertempstringdo_L1_v2022.csv', 
                 '2018-2019_wintertempstringdo_L1_v2022.csv', 
                 '2019-2020_wintertempstringdo_L1_v2022.csv', 
                 '2020-2021_wintertempstring_L1_v2022.csv', 
                 '2021-2022_wintertempstring_L1_v2022.csv')
  )

#edit table attributes

## 2014-2015 ##
attr_winter_1415 <- read.table(
  file = paste0(dir, 'attributes_2014-2015_wintertempstring_L1_v2022.txt'),
  header = TRUE,
  sep = '\t',
  quote = "\"",
  as.is = TRUE,
  comment.char = "",
  fill = T,
  colClasses = rep("character", 7)
)
attr_winter_1415

attr_winter_1415 <- attr_winter_1415 %>% 
  mutate(unit = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'celsius',
                          TRUE ~ ''),
         attributeDefinition = case_when(attributeName == 'datetime' ~ 'date and time (GMT -5) of record - no daylight savings observed',
                                         grepl('degC_1m', attributeName, ignore.case = T) ~ 'water temperature at approximately 1 meter depth',
                                         grepl('degC_2m', attributeName, ignore.case = T) ~ 'water temperature at approximately 2 meter depth',
                                         grepl('degC_3m', attributeName, ignore.case = T) ~ 'water temperature at approximately 3 meter depth',
                                         grepl('degC_4m', attributeName, ignore.case = T) ~ 'water temperature at approximately 4 meter depth',
                                         grepl('degC_5m', attributeName, ignore.case = T) ~ 'water temperature at approximately 5 meter depth',
                                         grepl('degC_6m', attributeName, ignore.case = T) ~ 'water temperature at approximately 6 meter depth',
                                         grepl('degC_7m', attributeName, ignore.case = T) ~ 'water temperature at approximately 7 meter depth',
                                         grepl('degC_8m', attributeName, ignore.case = T) ~ 'water temperature at approximately 8 meter depth',
                                         grepl('degC_9m', attributeName, ignore.case = T) ~ 'water temperature at approximately 9 meter depth',
                                         grepl('degC_10m', attributeName, ignore.case = T) ~ 'water temperature at approximately 10 meter depth',
                                         TRUE ~ attributeDefinition),
         class = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'numeric',
                           TRUE ~ class),
         dateTimeFormatString = case_when(attributeName == 'datetime' ~ 'YYYY-MM-DD hh:mm:ss'),
         missingValueCode = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'NA',
                                      TRUE ~ missingValueCode),
         missingValueCodeExplanation = case_when(grepl('NA', missingValueCode, ignore.case = T) ~ 'not available',
                                                 TRUE ~ missingValueCodeExplanation))
attr_winter_1415

write.table(
  x = attr_winter_1415,
  file = paste0(dir, 'attributes_2014-2015_wintertempstring_L1_v2022.txt'),
  sep = '\t',
  row.names = FALSE,
  quote = FALSE
)


## 2015-2016 ##
attr_winter_1516 <- read.table(
  file = paste0(dir, 'attributes_2015-2016_wintertempstring_L1_v2022.txt'),
  header = TRUE,
  sep = '\t',
  quote = "\"",
  as.is = TRUE,
  comment.char = "",
  fill = T,
  colClasses = rep("character", 7)
)
attr_winter_1516

attr_winter_1516 <- attr_winter_1516 %>% 
  mutate(unit = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'celsius',
                          TRUE ~ ''),
         attributeDefinition = case_when(attributeName == 'datetime' ~ 'date and time (GMT -5) of record - no daylight savings observed',
                                         grepl('degC_1m', attributeName, ignore.case = T) ~ 'water temperature at approximately 1 meter depth',
                                         grepl('degC_2m', attributeName, ignore.case = T) ~ 'water temperature at approximately 2 meter depth',
                                         grepl('degC_3m', attributeName, ignore.case = T) ~ 'water temperature at approximately 3 meter depth',
                                         grepl('degC_4m', attributeName, ignore.case = T) ~ 'water temperature at approximately 4 meter depth',
                                         grepl('degC_5m', attributeName, ignore.case = T) ~ 'water temperature at approximately 5 meter depth',
                                         grepl('degC_6m', attributeName, ignore.case = T) ~ 'water temperature at approximately 6 meter depth',
                                         grepl('degC_7m', attributeName, ignore.case = T) ~ 'water temperature at approximately 7 meter depth',
                                         grepl('degC_8m', attributeName, ignore.case = T) ~ 'water temperature at approximately 8 meter depth',
                                         grepl('degC_9m', attributeName, ignore.case = T) ~ 'water temperature at approximately 9 meter depth',
                                         grepl('degC_10m', attributeName, ignore.case = T) ~ 'water temperature at approximately 10 meter depth',
                                         TRUE ~ attributeDefinition),
         class = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'numeric',
                           TRUE ~ class),
         dateTimeFormatString = case_when(attributeName == 'datetime' ~ 'YYYY-MM-DD hh:mm:ss'),
         missingValueCode = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'NA',
                                      TRUE ~ missingValueCode),
         missingValueCodeExplanation = case_when(grepl('NA', missingValueCode, ignore.case = T) ~ 'not available',
                                                 TRUE ~ missingValueCodeExplanation))
attr_winter_1516

write.table(
  x = attr_winter_1516,
  file = paste0(dir, 'attributes_2015-2016_wintertempstring_L1_v2022.txt'),
  sep = '\t',
  row.names = FALSE,
  quote = FALSE
)

## 2016-2017 ##
attr_winter_1617 <- read.table(
  file = paste0(dir, 'attributes_2016-2017_wintertempstring_L1_v2022.txt'),
  header = TRUE,
  sep = '\t',
  quote = "\"",
  as.is = TRUE,
  comment.char = "",
  fill = T,
  colClasses = rep("character", 7)
)
attr_winter_1617

attr_winter_1617 <- attr_winter_1617 %>% 
  mutate(unit = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'celsius',
                          TRUE ~ ''),
         attributeDefinition = case_when(attributeName == 'datetime' ~ 'date and time (GMT -5) of record - no daylight savings observed',
                                         grepl('degC_1m', attributeName, ignore.case = T) ~ 'water temperature at approximately 1 meter depth',
                                         grepl('degC_2m', attributeName, ignore.case = T) ~ 'water temperature at approximately 2 meter depth',
                                         grepl('degC_3m', attributeName, ignore.case = T) ~ 'water temperature at approximately 3 meter depth',
                                         grepl('degC_4m', attributeName, ignore.case = T) ~ 'water temperature at approximately 4 meter depth',
                                         grepl('degC_5m', attributeName, ignore.case = T) ~ 'water temperature at approximately 5 meter depth',
                                         grepl('degC_6m', attributeName, ignore.case = T) ~ 'water temperature at approximately 6 meter depth',
                                         grepl('degC_7m', attributeName, ignore.case = T) ~ 'water temperature at approximately 7 meter depth',
                                         grepl('degC_8m', attributeName, ignore.case = T) ~ 'water temperature at approximately 8 meter depth',
                                         grepl('degC_9m', attributeName, ignore.case = T) ~ 'water temperature at approximately 9 meter depth',
                                         grepl('degC_10m', attributeName, ignore.case = T) ~ 'water temperature at approximately 10 meter depth',
                                         TRUE ~ attributeDefinition),
         class = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'numeric',
                           TRUE ~ class),
         dateTimeFormatString = case_when(attributeName == 'datetime' ~ 'YYYY-MM-DD hh:mm:ss'),
         missingValueCode = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'NA',
                                      TRUE ~ missingValueCode),
         missingValueCodeExplanation = case_when(grepl('NA', missingValueCode, ignore.case = T) ~ 'not available',
                                                 TRUE ~ missingValueCodeExplanation))
attr_winter_1617

write.table(
  x = attr_winter_1617,
  file = paste0(dir, 'attributes_2016-2017_wintertempstring_L1_v2022.txt'),
  sep = '\t',
  row.names = FALSE,
  quote = FALSE
)

## 2017-2018 ##
attr_winter_1718 <- read.table(
  file = paste0(dir, 'attributes_2017-2018_wintertempstringdo_L1_v2022.txt'),
  header = TRUE,
  sep = '\t',
  quote = "\"",
  as.is = TRUE,
  comment.char = "",
  fill = T,
  colClasses = rep("character", 7)
)
attr_winter_1718

attr_winter_1718 <- attr_winter_1718 %>% 
  mutate(unit = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'celsius',
                          grepl('mgl', attributeName, ignore.case = T) ~ 'milligramPerLiter',
                          TRUE ~ ''),
         attributeDefinition = case_when(attributeName == 'datetime' ~ 'date and time (GMT -5) of record - no daylight savings observed',
                                         grepl('degC_1m', attributeName, ignore.case = T) ~ 'water temperature at approximately 1 meter depth from U26 unit',
                                         grepl('degC_2m', attributeName, ignore.case = T) ~ 'water temperature at approximately 2 meter depth',
                                         grepl('degC_3m', attributeName, ignore.case = T) ~ 'water temperature at approximately 3 meter depth',
                                         grepl('degC_4m', attributeName, ignore.case = T) ~ 'water temperature at approximately 4 meter depth',
                                         grepl('degC_5m', attributeName, ignore.case = T) ~ 'water temperature at approximately 5 meter depth',
                                         grepl('degC_6m', attributeName, ignore.case = T) ~ 'water temperature at approximately 6 meter depth',
                                         grepl('degC_7m', attributeName, ignore.case = T) ~ 'water temperature at approximately 7 meter depth',
                                         grepl('degC_8m', attributeName, ignore.case = T) ~ 'water temperature at approximately 8 meter depth',
                                         grepl('degC_9m', attributeName, ignore.case = T) ~ 'water temperature at approximately 9 meter depth',
                                         grepl('degC_10m', attributeName, ignore.case = T) ~ 'water temperature at approximately 10 meter depth',
                                         grepl('mgl', attributeName, ignore.case = T) ~ 'dissolved oxygen of water at approximately 1 meter depth',
                                         TRUE ~ attributeDefinition),
         class = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'numeric',
                           grepl('mgl', attributeName, ignore.case = T) ~ 'numeric',
                           TRUE ~ class),
         dateTimeFormatString = case_when(attributeName == 'datetime' ~ 'YYYY-MM-DD hh:mm:ss'),
         missingValueCode = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'NA',
                                      grepl('mgl', attributeName, ignore.case = T) ~ 'NA',
                                      TRUE ~ missingValueCode),
         missingValueCodeExplanation = case_when(grepl('NA', missingValueCode, ignore.case = T) ~ 'not available',
                                                 TRUE ~ missingValueCodeExplanation))
attr_winter_1718

write.table(
  x = attr_winter_1718,
  file = paste0(dir, 'attributes_2017-2018_wintertempstringdo_L1_v2022.txt'),
  sep = '\t',
  row.names = FALSE,
  quote = FALSE
)


## 2018-2019 ##
attr_winter_1819 <- read.table(
  file = paste0(dir, 'attributes_2018-2019_wintertempstringdo_L1_v2022.txt'),
  header = TRUE,
  sep = '\t',
  quote = "\"",
  as.is = TRUE,
  comment.char = "",
  fill = T,
  colClasses = rep("character", 7)
)
attr_winter_1819

attr_winter_1819 <- attr_winter_1819 %>% 
  mutate(unit = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'celsius',
                          grepl('mgl', attributeName, ignore.case = T) ~ 'milligramPerLiter',
                          TRUE ~ ''),
         attributeDefinition = case_when(attributeName == 'datetime' ~ 'date and time (GMT -5) of record - no daylight savings observed',
                                         grepl('degC_1m', attributeName, ignore.case = T) ~ 'water temperature at approximately 1 meter depth from U26 unit',
                                         grepl('degC_2m', attributeName, ignore.case = T) ~ 'water temperature at approximately 2 meter depth',
                                         grepl('degC_3m', attributeName, ignore.case = T) ~ 'water temperature at approximately 3 meter depth',
                                         grepl('degC_4m', attributeName, ignore.case = T) ~ 'water temperature at approximately 4 meter depth',
                                         grepl('degC_5m', attributeName, ignore.case = T) ~ 'water temperature at approximately 5 meter depth',
                                         grepl('degC_6m', attributeName, ignore.case = T) ~ 'water temperature at approximately 6 meter depth',
                                         grepl('degC_7m', attributeName, ignore.case = T) ~ 'water temperature at approximately 7 meter depth',
                                         grepl('degC_8m', attributeName, ignore.case = T) ~ 'water temperature at approximately 8 meter depth',
                                         grepl('degC_9m', attributeName, ignore.case = T) ~ 'water temperature at approximately 9 meter depth',
                                         grepl('degC_10m', attributeName, ignore.case = T) ~ 'water temperature at approximately 10 meter depth',
                                         grepl('mgl', attributeName, ignore.case = T) ~ 'dissolved oxygen of water at approximately 1 meter depth',
                                         attributeName == 'flag_do' ~ 'flag column for dissolved oxygen value',
                                         TRUE ~ attributeDefinition),
         class = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'numeric',
                           grepl('mgl', attributeName, ignore.case = T) ~ 'numeric',
                           grepl('flag', attributeName, ignore.case = T) ~ 'categorical',
                           TRUE ~ class),
         dateTimeFormatString = case_when(attributeName == 'datetime' ~ 'YYYY-MM-DD hh:mm:ss'),
         missingValueCode = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'NA',
                                      grepl('mgl', attributeName, ignore.case = T) ~ 'NA',
                                      TRUE ~ missingValueCode),
         missingValueCodeExplanation = case_when(grepl('NA', missingValueCode, ignore.case = T) ~ 'not available',
                                                 TRUE ~ missingValueCodeExplanation))
attr_winter_1819

write.table(
  x = attr_winter_1819,
  file = paste0(dir, 'attributes_2018-2019_wintertempstringdo_L1_v2022.txt'),
  sep = '\t',
  row.names = FALSE,
  quote = FALSE
)


## 2019-2020 ##
attr_winter_1920 <- read.table(
  file = paste0(dir, 'attributes_2019-2020_wintertempstringdo_L1_v2022.txt'),
  header = TRUE,
  sep = '\t',
  quote = "\"",
  as.is = TRUE,
  comment.char = "",
  fill = T,
  colClasses = rep("character", 7)
)
attr_winter_1920

attr_winter_1920 <- attr_winter_1920 %>% 
  mutate(unit = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'celsius',
                          grepl('mgl', attributeName, ignore.case = T) ~ 'milligramPerLiter',
                          TRUE ~ ''),
         attributeDefinition = case_when(attributeName == 'datetime' ~ 'date and time (GMT -5) of record - no daylight savings observed',
                                         grepl('degC_1m', attributeName, ignore.case = T) ~ 'water temperature at approximately 1 meter depth from U26 unit',
                                         grepl('degC_2m', attributeName, ignore.case = T) ~ 'water temperature at approximately 2 meter depth',
                                         grepl('degC_3m', attributeName, ignore.case = T) ~ 'water temperature at approximately 3 meter depth',
                                         grepl('degC_4m', attributeName, ignore.case = T) ~ 'water temperature at approximately 4 meter depth',
                                         grepl('degC_5m', attributeName, ignore.case = T) ~ 'water temperature at approximately 5 meter depth',
                                         grepl('degC_6m', attributeName, ignore.case = T) ~ 'water temperature at approximately 6 meter depth',
                                         grepl('degC_7m', attributeName, ignore.case = T) ~ 'water temperature at approximately 7 meter depth',
                                         grepl('degC_8m', attributeName, ignore.case = T) ~ 'water temperature at approximately 8 meter depth',
                                         grepl('degC_9m', attributeName, ignore.case = T) ~ 'water temperature at approximately 9 meter depth',
                                         grepl('degC_10m', attributeName, ignore.case = T) ~ 'water temperature at approximately 10 meter depth',
                                         grepl('mgl', attributeName, ignore.case = T) ~ 'dissolved oxygen of water at approximately 1 meter depth',
                                         TRUE ~ attributeDefinition),
         class = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'numeric',
                           grepl('mgl', attributeName, ignore.case = T) ~ 'numeric',
                           TRUE ~ class),
         dateTimeFormatString = case_when(attributeName == 'datetime' ~ 'YYYY-MM-DD hh:mm:ss'),
         missingValueCode = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'NA',
                                      grepl('mgl', attributeName, ignore.case = T) ~ 'NA',
                                      TRUE ~ missingValueCode),
         missingValueCodeExplanation = case_when(grepl('NA', missingValueCode, ignore.case = T) ~ 'not available',
                                                 TRUE ~ missingValueCodeExplanation))
attr_winter_1920

write.table(
  x = attr_winter_1920,
  file = paste0(dir, 'attributes_2019-2020_wintertempstringdo_L1_v2022.txt'),
  sep = '\t',
  row.names = FALSE,
  quote = FALSE
)

## 2020-2021 ##
attr_winter_2021 <- read.table(
  file = paste0(dir, 'attributes_2020-2021_wintertempstring_L1_v2022.txt'),
  header = TRUE,
  sep = '\t',
  quote = "\"",
  as.is = TRUE,
  comment.char = "",
  fill = T,
  colClasses = rep("character", 7)
)
attr_winter_2021

attr_winter_2021 <- attr_winter_2021 %>% 
  mutate(unit = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'celsius',
                          TRUE ~ ''),
         attributeDefinition = case_when(attributeName == 'datetime' ~ 'date and time (GMT -5) of record - no daylight savings observed',
                                         grepl('degC_1m', attributeName, ignore.case = T) ~ 'water temperature at approximately 1 meter depth',
                                         grepl('degC_2m', attributeName, ignore.case = T) ~ 'water temperature at approximately 2 meter depth',
                                         grepl('degC_3m', attributeName, ignore.case = T) ~ 'water temperature at approximately 3 meter depth',
                                         grepl('degC_4m', attributeName, ignore.case = T) ~ 'water temperature at approximately 4 meter depth',
                                         grepl('degC_5m', attributeName, ignore.case = T) ~ 'water temperature at approximately 5 meter depth',
                                         grepl('degC_6m', attributeName, ignore.case = T) ~ 'water temperature at approximately 6 meter depth',
                                         grepl('degC_7m', attributeName, ignore.case = T) ~ 'water temperature at approximately 7 meter depth',
                                         grepl('degC_8m', attributeName, ignore.case = T) ~ 'water temperature at approximately 8 meter depth',
                                         grepl('degC_9m', attributeName, ignore.case = T) ~ 'water temperature at approximately 9 meter depth',
                                         grepl('degC_10m', attributeName, ignore.case = T) ~ 'water temperature at approximately 10 meter depth',
                                         TRUE ~ attributeDefinition),
         class = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'numeric',
                           TRUE ~ class),
         dateTimeFormatString = case_when(attributeName == 'datetime' ~ 'YYYY-MM-DD hh:mm:ss'),
         missingValueCode = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'NA',
                                      TRUE ~ missingValueCode),
         missingValueCodeExplanation = case_when(grepl('NA', missingValueCode, ignore.case = T) ~ 'not available',
                                                 TRUE ~ missingValueCodeExplanation))
attr_winter_2021

write.table(
  x = attr_winter_2021,
  file = paste0(dir, 'attributes_2020-2021_wintertempstring_L1_v2022.txt'),
  sep = '\t',
  row.names = FALSE,
  quote = FALSE
)

## 2021-2022 ##
attr_winter_2122 <- read.table(
  file = paste0(dir, 'attributes_2021-2022_wintertempstring_L1_v2022.txt'),
  header = TRUE,
  sep = '\t',
  quote = "\"",
  as.is = TRUE,
  comment.char = "",
  fill = T,
  colClasses = rep("character", 7)
)
attr_winter_2122

attr_winter_2122 <- attr_winter_2122 %>% 
  mutate(unit = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'celsius',
                          TRUE ~ ''),
         attributeDefinition = case_when(attributeName == 'datetime' ~ 'date and time (GMT -5) of record - no daylight savings observed',
                                         grepl('degC_1m', attributeName, ignore.case = T) ~ 'water temperature at approximately 1 meter depth',
                                         grepl('degC_2m', attributeName, ignore.case = T) ~ 'water temperature at approximately 2 meter depth',
                                         grepl('degC_3m', attributeName, ignore.case = T) ~ 'water temperature at approximately 3 meter depth',
                                         grepl('degC_4m', attributeName, ignore.case = T) ~ 'water temperature at approximately 4 meter depth',
                                         grepl('degC_5m', attributeName, ignore.case = T) ~ 'water temperature at approximately 5 meter depth',
                                         grepl('degC_6m', attributeName, ignore.case = T) ~ 'water temperature at approximately 6 meter depth',
                                         grepl('degC_7m', attributeName, ignore.case = T) ~ 'water temperature at approximately 7 meter depth',
                                         grepl('degC_8m', attributeName, ignore.case = T) ~ 'water temperature at approximately 8 meter depth',
                                         grepl('degC_9m', attributeName, ignore.case = T) ~ 'water temperature at approximately 9 meter depth',
                                         grepl('degC_10m', attributeName, ignore.case = T) ~ 'water temperature at approximately 10 meter depth',
                                         TRUE ~ attributeDefinition),
         class = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'numeric',
                           TRUE ~ class),
         dateTimeFormatString = case_when(attributeName == 'datetime' ~ 'YYYY-MM-DD hh:mm:ss'),
         missingValueCode = case_when(grepl('degC', attributeName, ignore.case = T) ~ 'NA',
                                      TRUE ~ missingValueCode),
         missingValueCodeExplanation = case_when(grepl('NA', missingValueCode, ignore.case = T) ~ 'not available',
                                                 TRUE ~ missingValueCodeExplanation))
attr_winter_2122

write.table(
  x = attr_winter_2122,
  file = paste0(dir, 'attributes_2021-2022_wintertempstring_L1_v2022.txt'),
  sep = '\t',
  row.names = FALSE,
  quote = FALSE
)

#### create categorical variables metadata ####
template_categorical_variables(
  path = dir,
  data.path = file.path(dir, 'data/'),
)

## buoy location ##
catvars_winter1819 <- read.table(
  file = file.path(dir, 'catvars_2018-2019_wintertempstringdo_L1_v2022.txt'),
  header = TRUE,
  sep = '\t',
  quote = "\"",
  as.is = TRUE,
  comment.char = "",
  fill = T
)
catvars_winter1819

catvars_winter1819 <- catvars_winter1819 %>% 
  mutate(definition = case_when(code == 's' ~ 'dissolved oxygen values are susepect, large changes in value with no correlative change in temperature',
                                TRUE ~ NA_character_))
write.table(
  x = catvars_winter1819,
  file = file.path(dir, 'catvars_2018-2019_wintertempstringdo_L1_v2022.txt'),
  sep = '\t',
  row.names = FALSE,
  quote = FALSE
)


#### OTHER TEXT ENTRIES ####
abstract <- suppressWarnings(
  readLines('abstract.txt')
)

abstract <-  "The Lake Sunapee Protective Association (LSPA) has been monitoring water quality in Lake Sunapee, New Hampshire, USA, since the 1980s. Beginning 
in the winter of 2014-2015, the LSPA deployed a string of HOBO temperature sensors at a location near Loon Island (43.391°N, 72.058°W, where their instrumented 
buoy is located during the summer months) for under-ice water temperature profile monitoring. A HOBO U26 dissolved oxygen sensor was added to this monitoring 
string during the winter of 2017-2018 through the winter of 2019-2020. All sensors record data in 15-minute intervals over the winter and are downloaded after 
ice-off. 
\n
All data have been QAQC\'d to remove obviously errant readings and artifacts of maintenance and flag highly suspicious readings."

knitr::kable(
  row.names = FALSE, 
  col.names = '', 
  abstract, caption = 'abstract.txt') %>%
  kableExtra::kable_styling('bordered', font_size = 12) %>%
  kableExtra::scroll_box(width = '100%', height = '200px')

write_lines(abstract, file.path(dir, 'abstract.txt'))


methods <- suppressWarnings(
  readLines(file.path(dir, 'methods.txt'))
)

methods <-  "The HOBO under-ice temperature line was initially deployed in the winter of 2014-2015 with 9 HOBO temperature/lumen HOBO pendants 
at approximately 1 meter to 9 meters depth at 1 meter intervals. The deployment of the under-ice temperature line for the winter of 2017-2018 until the present
consisted of 9 HOBO U22 temperature sensors at approximate depths of 2 meters to 10 meters below the water\'s surface at 1 meter intervals. The winters of 2017-2018,
2018-2019, and 2019-2020 included a HOBO U26 dissolved oxygen logger at approximately 1 meter depth. The calibration history of this sensor is unknown. All data 
are recorded at 15-minute intervals. The reported depth from surface does not take into account the depth of sensor when ice forms on the lake.  
\n
The loggers are deployed at the summer location of the LSPA buoy near Loon Island (43.391°N, 72.058°W) when the buoy is moved to Sunapee Harbor for winter 
storage. This usually happens after the lake has mixed (Lake Sunapee is a dimictic lake). Loggers are retrieved after ice-off has occurred, when the site is 
accessible, and when the LSPA buoy is moved back to its summer location at Loon Island, providing a nearly continuous year-round data stream for water 
temperature at this location. 
\n
Data from the HOBO units were collated and cleaned using R Studio (v. 2020.2.02), R version 4.1.3 by B. Steele of K.C. Weathers\' Laboratory (a zip folder that 
contains the R code for QAQC at the time of data publishing, which are stored on GitHub, are included in this data package). QAQC consisted of renaming columns
from the raw data to common headings for consistency between data years, as well as recoding obviously errant readings, highly suspicious readings and artifacts 
of buoy maintenance to \'NA\' from the raw data."

knitr::kable(
  row.names = FALSE, 
  col.names = '', 
  methods, caption = 'methods.txt') %>%
  kableExtra::kable_styling('bordered', font_size = 12) %>%
  kableExtra::scroll_box(width = '100%', height = '200px')

write_lines(methods, file.path(dir, 'methods.txt'))


keywords <- read.table(
  file = file.path(dir, 'keywords.txt'),
  header = TRUE,
  sep = '\t',
  quote = "\"",
  as.is = TRUE,
  comment.char = "",
  fill = T,
  colClasses = rep("character", 2)
)

keywords <- keywords %>% 
  add_row(keyword = c('Lake Sunapee Protective Association', 
                      'LSPA',
                      'Global Lake Ecological Observatory Network',
                      'GLEON',
                      'Lake Sunapee',
                      'New Hampshire',
                      'high-frequency data', 
                      'under-ice', 
                      'water temperature',
                      'dissolved oxygen', 
                      'oligotrophic', 
                      'freshwater', 
                      'lakes', 
                      'limnology'),
          keywordThesaurus = c('', 
                               '',
                               '',
                               '',
                               '',
                               '',
                               '',
                               '',
                               'LTER Controlled Vocabulary',
                               'LTER Controlled Vocabulary',
                               'LTER Controlled Vocabulary',
                               'LTER Controlled Vocabulary',
                               'LTER Controlled Vocabulary',
                               'LTER Controlled Vocabulary'
                               )
          )

knitr::kable(
  row.names = FALSE, 
  x = keywords, caption = 'keywords.txt') %>%
  kableExtra::kable_styling('bordered', font_size = 12) %>%
  kableExtra::scroll_box(width = '60%', height = '300px')

write.table(
  x = keywords,
  file = file.path(dir, 'keywords.txt'), 
  sep = '\t',
  row.names = FALSE,
  quote = FALSE
)


personnel <- read.table(
  file = file.path(dir, 'personnel.txt'),
  header = TRUE,
  sep = '\t',
  quote = "\"",
  as.is = TRUE,
  comment.char = "",
  fill = T,
  colClasses = rep("character", 10)
)

personnel <- personnel %>% 
  add_row(givenName = c('',
                        'Bethel',
                        'Kathleen', 
                       'June',
                       'Bethel',
                       'Kathleen', 
                       'Kathleen', 
                       'Geoff',
                       'John'),
         middleInitial = c('',
                           'G',
                           'C',
                           '', 
                           'G',
                           'C',
                           'C',
                           '',
                           ''),
         surName = c('LSPA',
                     'Steele', 
                     'Weathers',
                     'Fichter',
                     'Steele', 
                     'Weathers',
                     'Weathers',
                     'Lizotte',
                     'Merriman'),
         organizationName = c('Lake Sunapee Protective Association',
                              'Cary Institute of Ecosystem Studies',
                              'Cary Institute of Ecosystem Studies',
                              'Lake Sunapee Protective Association',
                              'Cary Institute of Ecosystem Studies',
                              'Cary Institute of Ecosystem Studies',
                              'Cary Institute of Ecosystem Studies',
                              'Lake Sunapee Protective Association',
                              'Lake Sunapee Protective Association'),
         electronicMailAddress = c('lspa@lakesunapee.org',
                                   'steeleb@caryinstitute.org',
                                   'weathersk@caryinstitute.org',
                                   'junef@lakesunapee.org',
                                   'steeleb@caryinstitute.org',
                                   'weathersk@caryinstitute.org',
                                   'weathersk@caryinstitute.org',
                                   '',
                                   ''),
         userId = c('',
                    '0000-0003-4365-4103',
                    '0000-0002-3575-6508',
                    '',
                    '0000-0003-4365-4103',
                    '0000-0002-3575-6508',
                    '0000-0002-3575-6508',
                    '',
                    ''),
         role = c('creator',
                  'creator',
                  'creator',
                  'PI',
                  'contact',
                  'PI',
                  'PI', 
                  'sensor deployment, download and data collection',
                  'sensor technical'),
         projectTitle = c('',
                          '',
                          '',
                          '',
                          '',
                          'Collaborative Research: CI-Team Demonstration: Developing a Model for Engagement of Citizen Scientists: Lake Associations',
                          'Collaborative Research: Building Analytical, Synthesis, and Human Network Skills Needed for Macrosystem Science: a Next Generation Graduate Student Training Model Based on GLEON',
                          '',
                          ''),
         fundingAgency = c('',
                           '',
                           '',
                           'Frey Foundation',
                           '',
                           'National Science Foundation',
                           'National Science Foundation',
                           '',
                           ''),
         fundingNumber = c('',
                           '',
                           '',
                           '',
                           '',
                           '0936174',
                           '1137327',
                           '',
                           '')
  )
   
knitr::kable(
  row.names = FALSE, 
  x = personnel, caption = 'personnel.txt') %>%
  kableExtra::kable_styling('bordered', font_size = 12) %>%
  kableExtra::scroll_box(width = '100%', height = '400px')

write.table(
  x = personnel,
  file = file.path(dir, 'personnel.txt'),
  sep = '\t',
  row.names = FALSE,
  quote = FALSE
)

#### REVIEW OTHER INFORMATION PREVIOSULY FORMATTED ####
intellectual_rights <- suppressWarnings(
  readLines(file.path(dir, 'intellectual_rights.txt'))
)

knitr::kable(
  row.names = FALSE, 
  col.names = '', 
  intellectual_rights, caption = 'intellectual_rights.txt') %>%
  kableExtra::kable_styling('bordered', font_size = 12) %>%
  kableExtra::scroll_box(width = '100%', height = '200px')


additional_info <- suppressWarnings(
  readLines(file.path(dir, 'additional_info.txt'))
)

additional_info <-  'Associated datasets: high-frequency meteorological data from the Lake Sunapee instrumented buoy (edi.234) and underwater temperature and dissolved oxygen data from the Lake Sunapee instrumented buoy (edi.499).'
  
knitr::kable(
  row.names = FALSE, 
  col.names = '', 
  additional_info, caption = 'additional_info.txt') %>%
  kableExtra::kable_styling('bordered', font_size = 12) %>%
  kableExtra::scroll_box(width = '50%', height = '100px')

write_lines(additional_info, file.path(dir, 'additional_info.txt'))


#### CREATE EML ####
make_eml(
  path = dir,
  data.path = file.path(dir, 'data/'),
  eml.path = dir,
  dataset.title = 'High-frequency winter water temperature and dissolved oxygen at Lake Sunapee, New Hampshire, USA, 2014-2022',
  geographic.coordinates = c(43.3913, -72.0576, 43.3913, -72.0576),
  geographic.description = 'Lake Sunapee is located in the Sugar River watershed within Sullivan and Merrimack Counties, NH, USA. It is a drainage lake with 
  predominantly muck substrate. It has a surface area of 1667 hectares, 53 kilometers of developed shoreline and a maximum depth of 33.7 meters. The location 
  of the winter under-ice temperature and do string is 43.3913, -72.0576. Verbal description of winter under-ice location: SSE of Loon Island, water depth 
  10.5-12 meters, water fluctuations seasonally 0.75 to 1 meter.',
  temporal.coverage = c('2014-10-14', '2022-05-02'),
  maintenance.description = 'ongoing',
  data.table = c('2014-2015_wintertempstring_L1_v2022.csv', 
                 '2015-2016_wintertempstring_L1_v2022.csv', 
                 '2016-2017_wintertempstring_L1_v2022.csv', 
                 '2017-2018_wintertempstringdo_L1_v2022.csv', 
                 '2018-2019_wintertempstringdo_L1_v2022.csv', 
                 '2019-2020_wintertempstringdo_L1_v2022.csv', 
                 '2020-2021_wintertempstring_L1_v2022.csv', 
                 '2021-2022_wintertempstring_L1_v2022.csv')
  data.table.name = c('2014-2015 winter tempstring L1 v2022',
                      '2015-2016 winter tempstring L1 v2022',
                      '2016-2017 winter tempstring L1 v2022',
                      '2017-2018 winter tempstring and dissolved oxygen L1 v2022',
                      '2018-2019 winter tempstring and dissolved oxygen L1 v2022',
                      '2019-2020 winter tempstring and dissolved oxygen L1 v2022',
                      '2020-2021 winter tempstring L1 v2022',
                      '2021-2022 winter tempstring L1 v2022'),
  data.table.description = c('2014-2015 winter water temperature profile',
                             '2015-2016 winter water temperature profile',
                             '2016-2017 winter water temperature profile',
                             '2017-2018 winter water temperature profile and dissolved oxygen at 1 m',
                             '2018-2019 winter water temperature profile and dissolved oxygen at 1 m',
                             '2019-2020 winter water temperature profile and dissolved oxygen at 1 m',
                             '2020-2021 winter water temperature profile',
                             '2021-2022 winter water temperature profile'),
  other.entity = c('winter-buoy-loggers-main.zip'),
  other.entity.name = c('winter logger repository archive'),
  other.entity.description = c('zip folder of the GitHub repository at time of publication'),
  user.id = 'steeleb',
  user.domain = 'EDI',
  package.id = 'edi.500.3'
)
