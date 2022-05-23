#*      Cary Institute of Ecosystem Studies (Millbrook, NY)           *
#*                                                                    *
#* TITLE:   Sunapee_hobo_2017-2018.r                                  *
#* AUTHOR:  Bethel Steele steeleb@caryinstitute.org                   *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 4.1.3, RStudio 2022.02.20   *
#* PURPOSE: Collate and qaqc winter logger data at Sunapee            *


#load libraries
library(tidyverse)
library(lubridate)

# point to directories
data_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/winter hobo data/2017-18 Winter Pendant/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/'

# save functions/groms
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom

#bring in winter 17-18 hobo raw data
hobo_w17_2m <- read.csv(file.path(data_dir,'U22-2m.csv'),
                      col.names = c('obs', 'datetime', 'TempC_2m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                      skip =2) %>% 
  select(-obs, -c(detach:eof))

hobo_w17_3m <- read.csv(file.path(data_dir,'U22-3m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_3m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))

hobo_w17_4m <- read.csv(file.path(data_dir,'U22-4m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_4m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))

hobo_w17_5m <- read.csv(file.path(data_dir,'U22-5m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_5m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))

hobo_w17_6m <- read.csv(file.path(data_dir,'U22-6m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_6m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))

hobo_w17_8m <- read.csv(file.path(data_dir,'U22-8m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_8m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))

hobo_w17_9m <- read.csv(file.path(data_dir,'U22-9m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_9m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))

hobo_w17_10m <- read.csv(file.path(data_dir,'U22-10m.csv'),
                         col.names = c('obs', 'datetime', 'TempC_10m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                         skip =2) %>% 
  select(-obs, -c(detach:eof))

hobo_w17_do <- read.csv(file.path(data_dir, '2017-2018_U26DO.csv'),
                        col.names = c('obs', 'datetime', 'do_ppm', 'TempC_1m', 'detach', 'exp_sensor', 'attach', 'connect', 'stop', 'eof'),
                        skip=2) %>% 
  select(-obs, -c(detach:eof)) #exp sensor doesn't have info in it.

hobo_w17 <- full_join(hobo_w17_do, hobo_w17_2m) %>% 
  full_join(., hobo_w17_3m) %>% 
  full_join(., hobo_w17_4m) %>% 
  full_join(., hobo_w17_5m) %>% 
  full_join(., hobo_w17_6m) %>% 
  full_join(., hobo_w17_8m) %>% 
  full_join(., hobo_w17_9m) %>% 
  full_join(., hobo_w17_10m)

rm(hobo_w17_do, hobo_w17_2m, hobo_w17_3m, hobo_w17_4m, hobo_w17_5m, hobo_w17_6m, hobo_w17_8m, hobo_w17_9m, hobo_w17_10m)

#format date of hobo sensors
hobo_w17 <- hobo_w17 %>% 
  mutate(datetime = as.POSIXct(datetime, format='%m/%d/%y %I:%M:%S %p', tz='UTC')) %>%  #no dst observed, using UTC for simplicity; though this is in GMT-4, on DST
  arrange(datetime)

#### hobo line ####
hobo_temp_vert <- hobo_w17 %>%
  select(-do_ppm) %>% 
  gather(variable, value, -datetime)
range(hobo_temp_vert$value, na.rm = T)

hobo_w17_L1 <- hobo_w17 %>% 
  mutate_at(vars("TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_8m", "TempC_9m", "TempC_10m"),
            ~(case_when(. < -5 ~ NA_real_,
                 TRUE ~ .)))
hobo_temp_vert_L1 <- hobo_w17_L1 %>%
  select(-do_ppm) %>% 
  gather(variable, value, -datetime)

ggplot(hobo_temp_vert_L1, aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp sensor winter 2017-2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_breaks = '1 month') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2017-10-01' & datetime < '2017-11-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Oct 2017', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#oct 19 deployment
ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2017-10-19' & datetime < '2017-10-20')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Oct 2017', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

#settles by 16:00
hobo_w17_L1 <- hobo_w17_L1 %>% 
  mutate_at(vars("TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_8m", "TempC_9m", "TempC_10m"),
            ~(case_when(datetime < as.POSIXct('2017-10-19 16:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
hobo_temp_vert_L1 <- hobo_w17_L1 %>%
  select(-do_ppm) %>% 
  gather(variable, value, -datetime)

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2017-10-01' & datetime < '2017-11-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Oct 2017', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme


ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2017-11-01' & datetime < '2017-12-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Nov 2017', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#nov 2 do 1m deployment
ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2017-11-02' & datetime < '2017-11-03')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Nov 2017', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

#full string impacted 14:00-14:30; tempc1 from 13:20-15:00
hobo_w17_L1 <- hobo_w17_L1 %>% 
  mutate_at(vars("TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_8m", "TempC_9m", "TempC_10m"),
            ~(case_when(datetime >= as.POSIXct('2017-11-02 14:00', tz='UTC') &
                             datetime < as.POSIXct('2017-11-02 14:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>%   
  mutate(TempC_1m = case_when(datetime >= as.POSIXct('2017-11-02 13:20', tz='UTC') &
                             datetime < as.POSIXct('2017-11-02 15:00', tz='UTC')~ NA_real_,
                           TRUE ~ TempC_1m))

hobo_temp_vert_L1 <- hobo_w17_L1 %>%
  select(-do_ppm) %>% 
  gather(variable, value, -datetime)

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2017-11-01' & datetime < '2017-12-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Nov 2017', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2017-12-01' & datetime < '2018-01-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Dec 2017', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-01-01' & datetime < '2018-02-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Jan 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-02-01' & datetime < '2018-03-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Feb 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-03-01' & datetime < '2018-04-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Mar 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-04-01' & datetime < '2018-05-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Apr 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-05-01' & datetime < '2018-06-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp May 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#may 19 do removal
ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-05-19' & datetime < '2018-05-20')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp May 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

#may 21 removal
ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-05-21' & datetime < '2018-05-22')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp May 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

#settles by 16:00
hobo_w17_L1 <- hobo_w17_L1 %>% 
  mutate_at(vars("TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_8m", "TempC_9m", "TempC_10m"),
            ~(case_when(datetime >= as.POSIXct('2018-05-21 9:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
hobo_temp_vert_L1 <- hobo_w17_L1 %>%
  select(-do_ppm) %>% 
  gather(variable, value, -datetime)

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-05-01' & datetime < '2018-06-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp May 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(hobo_temp_vert_L1, aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp winter 2017-2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme


#### do unit ####
hobo_do_w17 <- hobo_w17_L1 %>% 
  select(datetime, TempC_1m, do_ppm)

hobo_do_w17_vert <- hobo_do_w17 %>% 
  gather(variable, value, - datetime)

ggplot(hobo_do_w17_vert, aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

hobo_w17_L1 <- hobo_w17_L1 %>% 
  mutate(do_ppm = case_when(do_ppm <0 ~ NA_real_,
                            TRUE ~ do_ppm))
  
hobo_do_w17 <- hobo_w17_L1 %>% 
  select(datetime, TempC_1m, do_ppm)

hobo_do_w17_vert <- hobo_do_w17 %>% 
  gather(variable, value, - datetime)

  
ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2017-11-01', tz='UTC') &
                                            datetime < as.POSIXct('2017-12-01', tz='UTC'))),
              aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2017-11-02', tz='UTC') &
                                            datetime < as.POSIXct('2017-11-03', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

hobo_w17_L1 <- hobo_w17_L1 %>% 
  mutate(do_ppm = case_when(datetime >= as.POSIXct('2017-11-02 13:20', tz='UTC') &
                                datetime < as.POSIXct('2017-11-02 15:00', tz='UTC')~ NA_real_,
                              TRUE ~ do_ppm))

hobo_do_w17 <- hobo_w17_L1 %>% 
  select(datetime, TempC_1m, do_ppm)

hobo_do_w17_vert <- hobo_do_w17 %>% 
  gather(variable, value, - datetime)

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2017-11-01', tz='UTC') &
                                            datetime < as.POSIXct('2017-12-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2017-12-01', tz='UTC') &
                                            datetime < as.POSIXct('2018-01-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-01-01', tz='UTC') &
                                            datetime < as.POSIXct('2018-02-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme


ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-02-01', tz='UTC') &
                                            datetime < as.POSIXct('2018-03-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#flag feb 20-21
ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-02-20', tz='UTC') &
                                            datetime < as.POSIXct('2018-02-21', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-02-21', tz='UTC') &
                                            datetime < as.POSIXct('2018-02-22', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

hobo_w17_L1 <- hobo_w17_L1 %>% 
  mutate(do_flag = case_when(datetime >= as.POSIXct('2018-02-20 11:00', tz='UTC') &
                              datetime < as.POSIXct('2018-02-21 18:00', tz='UTC')~ 'e', #flag for erratic data
                            TRUE ~ NA_character_))

hobo_do_w17 <- hobo_w17_L1 %>% 
  select(datetime, TempC_1m, do_ppm, do_flag)

hobo_do_w17_vert <- hobo_do_w17 %>% 
  gather(variable, value, - datetime, -do_flag)

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-02-01', tz='UTC') &
                                            datetime < as.POSIXct('2018-03-01', tz='UTC'))),
       aes(x=datetime, y=value, color = do_flag)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-03-01', tz='UTC') &
                                            datetime < as.POSIXct('2018-04-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#mar 12
ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-03-12', tz='UTC') &
                                            datetime < as.POSIXct('2018-03-13', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

#mar 30
ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-03-30', tz='UTC') &
                                            datetime < as.POSIXct('2018-03-31', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme


hobo_w17_L1 <- hobo_w17_L1 %>% 
  mutate(do_ppm = case_when(datetime >= as.POSIXct('2018-03-12 7:00', tz='UTC') &
                               datetime < as.POSIXct('2018-03-12 8:20', tz='UTC')~ NA_real_,
                             TRUE ~ do_ppm)) %>% 
  mutate(do_flag = case_when(datetime >= as.POSIXct('2018-03-30 12:15', tz='UTC') &
                               datetime < as.POSIXct('2018-03-30 15:15', tz='UTC')~ 'e',
                             TRUE ~ NA_character_))


hobo_do_w17 <- hobo_w17_L1 %>% 
  select(datetime, TempC_1m, do_ppm, do_flag)

hobo_do_w17_vert <- hobo_do_w17 %>% 
  gather(variable, value, - datetime, -do_flag)

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-03-01', tz='UTC') &
                                            datetime < as.POSIXct('2018-04-01', tz='UTC'))),
       aes(x=datetime, y=value, color = do_flag)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme


ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-04-01', tz='UTC') &
                                            datetime < as.POSIXct('2018-05-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-05-01', tz='UTC') &
                                            datetime < as.POSIXct('2018-06-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w17_vert, subset = (datetime >= as.POSIXct('2018-05-19', tz='UTC') &
                                            datetime < as.POSIXct('2018-05-20', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme


#plot all 
hobo_w17_L1 %>% 
  gather(variable, value, -datetime, -do_flag) %>% 
  ggplot(.,
       aes(x=datetime, y=value, color = do_flag)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2017-2018', x='date') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

#change time from GMT-4 to GMT-5 
hobo_w17_L1 <- hobo_w17_L1 %>% 
  mutate(datetime = datetime - hours(1))
  

  
#export L1 tempstring file
hobo_w17_L1 %>% 
  select(datetime, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_8m, TempC_9m, TempC_10m, do_ppm, do_flag) %>% 
  rename(waterTemperature_DO_degC_1m = 'TempC_1m',
         waterTemperature_degC_2m = 'TempC_2m',
         waterTemperature_degC_3m = 'TempC_3m',
         waterTemperature_degC_4m = 'TempC_4m',
         waterTemperature_degC_5m = 'TempC_5m',
         waterTemperature_degC_6m = 'TempC_6m',
         waterTemperature_degC_8m = 'TempC_8m',
         waterTemperature_degC_9m = 'TempC_9m',
         waterTemperature_degC_10m = 'TempC_10m',
         oxygenDissolved_mgl_1m = do_ppm) %>%
  mutate(datetime = as.character(datetime)) %>% 
  write_csv(., file.path(dump_dir, paste0('2017-2018_hobotempstringdo_L1_v', format(Sys.Date(), '%Y'), '.csv')))

