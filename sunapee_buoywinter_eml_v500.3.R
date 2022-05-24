#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_buoywinter_EML_v22April2020.R                *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 3.6.3, RStudio 1.2.5042*
#* DATE:    27April2020                                          *
#* PROJECT: sunapee buoy                                         *
#* PURPOSE: create EML for dataset                               *
#*****************************************************************

library(devtools)
library(EML)
install_github('EDIorg/EMLassemblyline')
library(EMLassemblyline)

library(tidyverse)
library(readxl)
knitr::opts_chunk$set(echo = TRUE)
library(kableExtra)

#import templates
setwd('/Users/steeleb/Dropbox/EDI_submissions/sunape_buoy_winter_2014-2019_v22April2020/')

#basic metadata
template_core_metadata(
  path = '.',
  license = "CCBY"
)


#### TABLE ATTRIBUTES ####
template_table_attributes(
  path = '.',
  data.path = 'data/',
  data.table = c('2014_2019_winterhobodotemp_L1.csv', 'buoylocation_datapub.csv')
)

#edit table attributes

## airtemp ##
attr_winter <- read.table(
  file = 'attributes_2014_2019_winterhobodotemp_L1.txt',
  header = TRUE,
  sep = '\t',
  quote = "\"",
  as.is = TRUE,
  comment.char = "",
  fill = T,
  colClasses = rep("character", 7)
)
attr_winter

attr_winter <- attr_winter %>% 
  mutate(unit = case_when(grepl('TempC', attributeName, ignore.case = T) ~ 'celsius',
                          grepl('ppm', attributeName, ignore.case = T) ~ 'partsPerMillion',
                          TRUE ~ ''),
         class = case_when(attributeName == 'location' ~ 'categorical',
                           TRUE ~ class),
         attributeDefinition = case_when(attributeName == 'datetime' ~ 'date and time (GMT -4) of buoy record - no daylight savings observed',
                                         attributeName == 'do_ppm' ~ 'dissolved oxygen at approximately 1.5 meter depth',
                                         attributeName == 'TempC_1p5m' ~ 'water temperature at approximately 1.5 meter depth',
                                         attributeName == 'TempC_2p5m' ~ 'water temperature at approximately 2.5 meter depth',
                                         attributeName == 'TempC_3p5m' ~ 'water temperature at approximately 3.5 meter depth',
                                         attributeName == 'TempC_4p5m' ~ 'water temperature at approximately 4.5 meter depth',
                                         attributeName == 'TempC_5p5m' ~ 'water temperature at approximately 5.5 meter depth',
                                         attributeName == 'TempC_6p5m' ~ 'water temperature at approximately 6.5 meter depth',
                                         attributeName == 'TempC_7p5m' ~ 'water temperature at approximately 7.5 meter depth',
                                         attributeName == 'TempC_8p5m' ~ 'water temperature at approximately 8.5 meter depth',
                                         attributeName == 'TempC_9p5m' ~ 'water temperature at approximately 9.5 meter depth',
                                         attributeName == 'TempC_10p5m' ~ 'water temperature at approximately 10.5 meter depth',
                                         attributeName == 'location' ~ 'location of under-ice loggers',
                                         TRUE ~ attributeDefinition),
         dateTimeFormatString = case_when(attributeName == 'datetime' ~ 'YYYY-MM-DD hh:mm:ss'),
         missingValueCode = case_when(grepl('TempC', attributeName, ignore.case = T) ~ 'NA',
                                      grepl('ppm', attributeName, ignore.case = T) ~ 'NA',
                                      TRUE ~ missingValueCode),
         missingValueCodeExplanation = case_when(grepl('NA', missingValueCode, ignore.case = T) ~ 'not available',
                                                 TRUE ~ missingValueCodeExplanation))


write.table(
  x = attr_winter,
  file = 'attributes_2014_2019_winterhobodotemp_L1.txt',
  sep = '\t',
  row.names = FALSE,
  quote = FALSE
)


## buoyloc ##
attr_buoyloc <- read.table(
  file = 'attributes_buoylocation_datapub.txt',
  header = TRUE,
  sep = '\t',
  quote = "\"",
  as.is = TRUE,
  comment.char = "",
  fill = T,
  colClasses = rep("character", 7)
)
attr_buoyloc

attr_buoyloc <- attr_buoyloc %>% 
  mutate(unit = case_when(grepl('dd', attributeName, ignore.case = T) ~ 'decimalDegree',
                          TRUE ~ ''),
         class = case_when(attributeName == 'location' ~ 'categorical',
                           TRUE ~ class),
         attributeDefinition = case_when(attributeName == 'location' ~ 'location of buoy',
                                         attributeName == 'lat_dd' ~ 'latitude of buoy location - WGS84',
                                         attributeName == 'long_dd' ~ 'longitude of buoy location - WGS84'))

write.table(
  x = attr_buoyloc,
  file = 'attributes_buoylocation_datapub.txt', 
  sep = '\t',
  row.names = FALSE,
  quote = FALSE
)


#define custom units
custom_units <- read.table(
  file = 'custom_units.txt',
  header = TRUE,
  sep = '\t',
  quote = "\"",
  as.is = TRUE,
  comment.char = "",
  fill = T,
  colClasses = rep("character", 5)
)


#### create categorical variables metadata ####
template_categorical_variables(
  path = '.',
  data.path = './data',
)

## buoy location ##
catvars_buoyloc <- read.table(
  file = 'catvars_buoylocation_datapub.txt',
  header = TRUE,
  sep = '\t',
  quote = "\"",
  as.is = TRUE,
  comment.char = "",
  fill = T
)
catvars_buoyloc

catvars_buoyloc <- catvars_buoyloc %>% 
  mutate(definition = case_when(attributeName == 'location' & code == 'loon' ~ 'buoy is located near loon island',
                                attributeName == 'location' & code == 'harbor' ~ 'buoy is located in harbor',
                                TRUE ~ NA_character_))
write.table(
  x = catvars_buoyloc,
  file = 'catvars_buoylocation_datapub.txt', 
  sep = '\t',
  row.names = FALSE,
  quote = FALSE
)

## buoy location ##
catvars_tempdoloc <- read.table(
  file = 'catvars_2014_2019_winterhobodotemp_L1.txt',
  header = TRUE,
  sep = '\t',
  quote = "\"",
  as.is = TRUE,
  comment.char = "",
  fill = T
)
catvars_tempdoloc

catvars_tempdoloc <- catvars_tempdoloc %>% 
  mutate(definition = case_when(attributeName == 'location' & code == 'loon' ~ 'buoy is located near loon island',
                                TRUE ~ NA_character_))
write.table(
  x = catvars_tempdoloc,
  file = 'catvars_2014_2019_winterhobodotemp_L1.txt', 
  sep = '\t',
  row.names = FALSE,
  quote = FALSE
)


#### GEOGRAPHIC COVERAGE ####
template_geographic_coverage(
  path = '.',
  data.path = './data',
  data.table = 'buoylocation_datapub.csv',
  site.col = 'location',
  lat.col = 'lat_dd',
  lon.col = 'long_dd'
)

#### TAXONOMIC COVERAGE ####
#not applicable


#### OTHER TEXT ENTRIES ####
abstract <- suppressWarnings(
  readLines('abstract.txt')
)

abstract <-  "The Lake Sunapee Protective Association (LSPA) has been monitoring water quality in Lake Sunapee, New Hampshire, USA, since the 1980s. Beginning the winter of 2014-2015, the LSPA deployed a string of HOBO temperature sensors at a location near Loon Island (where their instrumented buoy is located during the summer months) for under-ice water temperature profile monitoring. A HOBO U26 dissolved oxygen sensor was added to this monitoring string during the winter of 2017-2018. All sensors record data in 15-minute intervals over the winter and are downloaded after ice-off.
\n
All data have been QAQC\\'d to remove obviously errant readings, highly suspicious readings, and artifacts of maintenance."

knitr::kable(
  row.names = FALSE, 
  col.names = '', 
  abstract, caption = 'abstract.txt') %>%
  kableExtra::kable_styling('bordered', font_size = 12) %>%
  kableExtra::scroll_box(width = '100%', height = '200px')

write_lines(abstract, 'abstract.txt')


methods <- suppressWarnings(
  readLines('methods.txt')
)

methods <-  "The HOBO under-ice temperature line is currently comprised of 10 HOBO U22 temperature sensors at approximate depths of 1.5 meters to 10.5 meters below the water\\'s surface at 1 meter intervals (winter 2017-2018 through present). This reported depth from surface does not take into account the depth of sensor when ice forms on the lake. In data prior to the 2017-2018 season, the winter temperature line was comprised of 9 HOBO temperature/lumen HOBO pendants at approximately 1.5 meters to 9.5 meters depth at 1 meter intervals. Beginning the winter season of 2017-2018, a HOBO U26 dissolved oxygen logger was deployed at approximately 1.5 meter depth. All data are recorded at 15-minute intervals.
\n
The loggers are deployed at the summer location of the LSPA buoy near Loon Island when the buoy is moved to Sunapee Harbor for winter storage. This usually happens after the lake has mixed (Lake Sunapee is a dimictic lake). Loggers are retrieved after ice-off has occurred, the site is accessible, and when the LSPA buoy is moved back to its summer location at Loon Island, providing a nearly continuous year-round data stream for water temperature at this location. 
\n
Data from the HOBO units were collated and cleaned using R Studio (v. 1.1.383), R version 3.4.3 by B. Steele of K.C. Weathers\\' Laboratory (R code for QAQC included in this data package). QAQC consisted of renaming columns from the raw data to common headings for consistency between data years, as well as recoding obviously errant readings, highly suspicious readings and artifacts of buoy maintenance to \\'NA\\' from the raw data."

knitr::kable(
  row.names = FALSE, 
  col.names = '', 
  methods, caption = 'methods.txt') %>%
  kableExtra::kable_styling('bordered', font_size = 12) %>%
  kableExtra::scroll_box(width = '100%', height = '200px')

write_lines(methods, 'methods.txt')


keywords <- read.table(
  file = 'keywords.txt',
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
  file = 'keywords.txt', 
  sep = '\t',
  row.names = FALSE,
  quote = FALSE
)


personnel <- read.table(
  file = 'personnel.txt',
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
                       'Kathleen', 
                       'Bethel',
                       'June',
                       '',
                       'Kathleen', 
                       'Kathleen', 
                       'Geoff',
                       'John'),
         middleInitial = c('',
                           'C',
                           'G',
                           '', 
                           '',
                           'C',
                           'C',
                           '',
                           ''),
         surName = c('LSPA',
                     'Weathers',
                     'Steele', 
                     'Fichter',
                     'LSPA',
                     'Weathers',
                     'Weathers',
                     'Lizotte',
                     'Merriman'),
         organizationName = c('Lake Sunapee Protective Association',
                              'Cary Institute of Ecosystem Studies',
                              'Cary Institute of Ecosystem Studies',
                              'Lake Sunapee Protective Association',
                              'Lake Sunapee Protective Association',
                              'Cary Institute of Ecosystem Studies',
                              'Cary Institute of Ecosystem Studies',
                              'Lake Sunapee Protective Association',
                              'Lake Sunapee Protective Association'),
         electronicMailAddress = c('lspa@lakesunapee.org',
                                   'weathersk@caryinstitute.org',
                                   'steeleb@caryinstitute.org',
                                   'junef@lakesunapee.org',
                                   'lspa@lakesunapee.org',
                                   'weathersk@caryinstitute.org',
                                   'weathersk@caryinstitute.org',
                                   '',
                                   ''),
         userId = c('',
                    '0000-0002-3575-6508',
                    '0000-0003-4365-4103',
                    '',
                    '',
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
  file = 'personnel.txt', 
  sep = '\t',
  row.names = FALSE,
  quote = FALSE
)

#### REVIEW OTHER INFORMATION PREVIOSULY FORMATTED ####
intellectual_rights <- suppressWarnings(
  readLines('intellectual_rights.txt')
)

knitr::kable(
  row.names = FALSE, 
  col.names = '', 
  intellectual_rights, caption = 'intellectual_rights.txt') %>%
  kableExtra::kable_styling('bordered', font_size = 12) %>%
  kableExtra::scroll_box(width = '100%', height = '200px')


additional_info <- suppressWarnings(
  readLines('additional_info.txt')
)

additional_info <-  'Associated datasets: high-frequency meteorological data from the Lake Sunapee instrumented buoy (edi.234) and underwater temperature and dissolved oxygen data from the Lake Sunapee instrumented buoy (edi.499).'
  
knitr::kable(
  row.names = FALSE, 
  col.names = '', 
  additional_info, caption = 'additional_info.txt') %>%
  kableExtra::kable_styling('bordered', font_size = 12) %>%
  kableExtra::scroll_box(width = '50%', height = '100px')

write_lines(additional_info, 'additional_info.txt')


knitr::kable(
  attr_buoyloc, caption = 'attributes_buoyloc.txt') %>%
  kableExtra::kable_styling('bordered', font_size = 12) %>%
  kableExtra::scroll_box(width = '100%', height = '300px')

knitr::kable(
  attr_winter, caption = 'attributes_winter.txt') %>%
  kableExtra::kable_styling('bordered', font_size = 12) %>%
  kableExtra::scroll_box(width = '100%', height = '300px')

geocoverage <- read.table(
  file = 'geographic_coverage.txt',
  header = TRUE,
  sep = '\t',
  quote = "\"",
  as.is = TRUE,
  comment.char = "",
  fill = T,
  colClasses = rep("character", 5)
)

knitr::kable(
  geocoverage, caption = 'geographic_coverage.txt') %>%
  kableExtra::kable_styling('bordered', font_size = 12) %>%
  kableExtra::scroll_box(width = '100%', height = '400px')


#### CREATE EML ####
make_eml(
  path = '.',
  data.path = './data',
  eml.path = '.',
  dataset.title = 'High-frequency under-ice water temperature and dissolved oxygen at Lake Sunapee, New Hampshire, USA, 2014-2019',
  geographic.coordinates = c(43.4307, -72.0304, 43.3217, -72.0831),
  geographic.description = 'Lake Sunapee is located in the Sugar River watershed within Sullivan and Merrimack Counties, NH, USA. It is a drainage lake with predominantly muck substrate. It has a surface area of 1667 hectares, 53 kilometers of developed shoreline and a maximum depth of 33.7 meters. The location of the winter under-ice temperature and do string is 43.3913, -72.0576. Verbal description of winter under-ice location: SSE of Loon Island, water depth 10.5-12 meters, water fluctuations seasonally 0.75 to 1 meter. The following bounding coordinates are for Lake Sunapee as defined by the polygon contained in the New Hampshire Hydrography Dataset (NHHD) as downloaded from GRANIT (http://www.granit.sr.unh.edu/data/search?dset=nhhd&#).',
  temporal.coverage = c('2014-10-14', '2019-06-06'),
  maintenance.description = 'ongoing',
  data.table = c('2014_2019_winterhobodotemp_L1.csv', 'buoylocation_datapub.csv'),
  data.table.name = c('2014_2019_winterhobodotemp_L1', 'buoylocation_datapub'),
  data.table.description = c('2014-2019 under ice water temperature and dissolved oxygen data', 'buoy location details'),
  other.entity = c('Sunapee_hobo_2014_2015.R',
                   'Sunapee_hobo_2015_2016.R',
                   'Sunapee_hobo_2016_2017.R',
                   'Sunapee_hobo_2017_2018.R',
                   'Sunapee_hobo_2018_2019.R',
                   'winterhobo_tempdo_collation_2014_2019.R',
                   'library_func_lists.R',
                   'sunapee_buoywinter_eml_v22April2020.R'),
  other.entity.name = c('Sunapee_hobo_2014-2015',
                        'Sunapee_hobo_2015-2016',
                        'Sunapee_hobo_2016-2017',
                        'Sunapee_hobo_2017-2018',
                        'Sunapee_hobo_2018-2019',
                        'winterhobo_tempdo_collation_2014-2019',
                        'library_func_lists',
                        'sunapee_buoywinter_eml_v22April2020'),
  other.entity.description = c('code for QAQC of 2014-2015 winter HOBO data',
                               'code for QAQC of 2015-2016 winter HOBO data',
                               'code for QAQC of 2016-2017 winter HOBO data',
                               'code for QAQC of 2017-2018 winter HOBO data',
                               'code for QAQC of 2018-2019 winter HOBO data',
                               'code used to collate historical dataset',
                               'code for libraries, functions and lists used for all QAQC and collation scripts',
                               'code to generate under-ice logger EML'),
  user.id = 'steeleb',
  user.domain = 'EDI',
  package.id = 'edi.500.3'
)
