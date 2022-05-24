#*      Cary Institute of Ecosystem Studies (Millbrook, NY)           *
#*                                                                    *
#* TITLE:   Sunapee_hobo_2020-2021.r                                  *
#* AUTHOR:  Bethel Steele steeleb@caryinstitute.org                   *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 4.1.3, RStudio 2022.02.20   *
#* PURPOSE: Collate and qaqc winter logger data at Sunapee            *


#load libraries
library(tidyverse)
library(lubridate)

# point to directories
data_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/winter hobo data/2020-2021WinterTemp/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/'

# save functions/groms
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom

#bring in winter 17-18 hobo raw data
hobo_w20_2m <- read.csv(file.path(data_dir, 'U22-2m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_2m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))
hobo_w20_3m <- read.csv(file.path(data_dir, 'U22-3m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_3m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))
hobo_w20_4m <- read.csv(file.path(data_dir, 'U22-4m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_4m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))
hobo_w20_5m <- read.csv(file.path(data_dir, 'U22-5m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_5m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))
hobo_w20_6m <- read.csv(file.path(data_dir, 'U22-6m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_6m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))
hobo_w20_7m <- read.csv(file.path(data_dir, 'U22-7m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_7m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))
hobo_w20_8m <- read.csv(file.path(data_dir, 'U22-8m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_8m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))
hobo_w20_9m <- read.csv(file.path(data_dir, 'U22-9m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_9m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))
hobo_w20_10m <- read.csv(file.path(data_dir, 'U22-10m.csv'),
                         col.names = c('obs', 'datetime', 'TempC_10m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                         skip =2) %>% 
  select(-obs, -c(detach:eof))

#no 1m DO deployed this year
hobo_w20 <- full_join(hobo_w20_2m, hobo_w20_3m) %>% 
  full_join(., hobo_w20_4m) %>% 
  full_join(., hobo_w20_5m) %>% 
  full_join(., hobo_w20_6m) %>% 
  full_join(., hobo_w20_7m) %>% 
  full_join(., hobo_w20_8m) %>% 
  full_join(., hobo_w20_9m) %>% 
  full_join(., hobo_w20_10m)

rm(hobo_w20_2m, hobo_w20_3m, hobo_w20_4m, hobo_w20_5m, hobo_w20_6m, hobo_w20_7m, hobo_w20_8m, hobo_w20_9m, hobo_w20_10m)

#format date of hobo sensors
hobo_w20 <- hobo_w20 %>% 
  mutate(datetime = as.POSIXct(datetime, format='%m/%d/%y %I:%M:%S %p', tz='UTC')) %>% #these are all in GMT-4, treating as UTC for simplicity
  arrange(datetime)

#create a L1
hobo_w20_L1 <- hobo_w20

#### hobo line ####
hobo_temp_vert <- hobo_w20 %>%
  gather(variable, value, -datetime)
range(hobo_temp_vert$value, na.rm = T)

hobo_temp_vert_L1 <- hobo_w20_L1 %>%
  gather(variable, value, -datetime)

ggplot(hobo_temp_vert_L1, aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp sensor winter 2020-2021', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_breaks = '1 month') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2020-10-01' & datetime < '2020-11-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Oct 2020', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2020-11-01' & datetime < '2020-12-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Nov 2020', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2020-12-01' & datetime < '2021-01-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Dec 2020', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2021-01-01' & datetime < '2021-02-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Jan 2021', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2021-02-01' & datetime < '2021-03-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Feb 2021', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2021-03-01' & datetime < '2021-04-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Mar 2021', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2021-04-01' & datetime < '2021-05-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Apr 2021', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2021-05-01' & datetime < '2021-06-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp May 2021', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2021-06-01' & datetime < '2021-07-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp June 2021', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#june 3 removal
ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2021-06-03' & datetime < '2021-06-04')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp June 3', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

hobo_w20_L1 <- hobo_w20_L1 %>% 
  mutate_at(vars("TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m"),
            ~(case_when(datetime >= as.POSIXct('2021-06-03 12:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
hobo_temp_vert_L1 <- hobo_w20_L1 %>%
  gather(variable, value, -datetime)

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2021-05-15' & datetime < '2021-06-15')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp May 2021', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(hobo_temp_vert_L1, aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp 2021', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme


#### do unit ####
# no do this year.
  
# data stored in GMT-4, change to GMT-5
hobo_w20_L1 <- hobo_w20_L1 %>% 
  mutate(datetime = datetime - hours(1))
  
#export L1 tempstring file
hobo_w20_L1 %>% 
  select(datetime, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m) %>% 
  rename(waterTemperature_degC_2m = 'TempC_2m',
         waterTemperature_degC_3m = 'TempC_3m',
         waterTemperature_degC_4m = 'TempC_4m',
         waterTemperature_degC_5m = 'TempC_5m',
         waterTemperature_degC_6m = 'TempC_6m',
         waterTemperature_degC_7m = 'TempC_7m',
         waterTemperature_degC_8m = 'TempC_8m',
         waterTemperature_degC_9m = 'TempC_9m',
         waterTemperature_degC_10m = 'TempC_10m') %>%
  mutate(datetime = as.character(datetime)) %>% 
  write_csv(., file.path(dump_dir, paste0('2020-2021_wintertempstring_L1_v', format(Sys.Date(), '%Y'), '.csv')))




