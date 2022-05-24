#*      Cary Institute of Ecosystem Studies (Millbrook, NY)           *
#*                                                                    *
#* TITLE:   Sunapee_hobo_2018-2019.r                                  *
#* AUTHOR:  Bethel Steele steeleb@caryinstitute.org                   *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 4.1.3, RStudio 2022.02.20   *
#* PURPOSE: Collate and qaqc winter logger data at Sunapee            *

#load libraries
library(tidyverse)
library(lubridate)

# point to directories
data_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/winter hobo data/2018-19 Winter Pendant/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/'

# save functions/groms
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom

#bring in winter 17-18 hobo raw data
hobo_w18_2m <- read.csv(file.path(data_dir, 'U22-2m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_2m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))
hobo_w18_3m <- read.csv(file.path(data_dir, 'U22-3m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_3m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))
hobo_w18_4m <- read.csv(file.path(data_dir, 'U22-4m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_4m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))
hobo_w18_5m <- read.csv(file.path(data_dir, 'U22-5m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_5m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))
hobo_w18_6m <- read.csv(file.path(data_dir, 'U22-6m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_6m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))
hobo_w18_7m <- read.csv(file.path(data_dir, 'U22-7m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_7m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))
hobo_w18_8m <- read.csv(file.path(data_dir, 'U22-8m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_8m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))
hobo_w18_9m <- read.csv(file.path(data_dir, 'U22-9m.csv'),
                        col.names = c('obs', 'datetime', 'TempC_9m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                        skip =2) %>% 
  select(-obs, -c(detach:eof))
hobo_w18_10m <- read.csv(file.path(data_dir, 'U22-10m.csv'),
                         col.names = c('obs', 'datetime', 'TempC_10m', 'detach', 'attach', 'connect', 'stop', 'eof'),
                         skip =2) %>% 
  select(-obs, -c(detach:eof))
hobo_w18_do <- read.csv(file.path(data_dir, '2018-2019_U26DO.csv'),
                        col.names = c('obs', 'datetime', 'do_ppm', 'TempC_1m', 'exp_sensor', 'eof'),
                        skip=2) %>% 
  select(-obs, -c(exp_sensor:eof)) #exp sensor doesn't have info in it.

hobo_w18 <- full_join(hobo_w18_do, hobo_w18_2m) %>% 
  full_join(., hobo_w18_3m) %>% 
  full_join(., hobo_w18_4m) %>% 
  full_join(., hobo_w18_5m) %>% 
  full_join(., hobo_w18_6m) %>% 
  full_join(., hobo_w18_7m) %>% 
  full_join(., hobo_w18_8m) %>% 
  full_join(., hobo_w18_9m) %>% 
  full_join(., hobo_w18_10m)

rm(hobo_w18_do, hobo_w18_2m, hobo_w18_3m, hobo_w18_4m, hobo_w18_5m, hobo_w18_6m, hobo_w18_7m, hobo_w18_8m, hobo_w18_9m, hobo_w18_10m)

#format date of hobo sensors
hobo_w18 <- hobo_w18 %>% 
  mutate(datetime = as.POSIXct(datetime, format='%m/%d/%y %I:%M:%S %p', tz='UTC')) %>% 
  arrange(datetime)

#### hobo line ####
hobo_temp_vert <- hobo_w18 %>%
  select(-do_ppm) %>% 
  gather(variable, value, -datetime)
range(hobo_temp_vert$value, na.rm = T)

hobo_w18_L1 <- hobo_w18 %>% 
  mutate_at(vars("TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m"),
            ~(case_when(. < -5 ~ NA_real_,
                 TRUE ~ .)))
hobo_temp_vert_L1 <- hobo_w18_L1 %>%
  select(-do_ppm) %>% 
  gather(variable, value, -datetime)

ggplot(hobo_temp_vert_L1, aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp sensor winter 2018-2019', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_breaks = '1 month') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-10-01' & datetime < '2018-11-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Oct 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#oct 19 deployment
ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-10-19' & datetime < '2018-10-20')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Oct 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-11-01' & datetime < '2018-12-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Nov 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2018-12-01' & datetime < '2019-01-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Dec 2018', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2019-01-01' & datetime < '2019-02-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Jan 2019', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2019-02-01' & datetime < '2019-03-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Feb 2019', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2019-03-01' & datetime < '2019-04-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Mar 2019', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2019-04-01' & datetime < '2019-05-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Apr 2019', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2019-05-01' & datetime < '2019-06-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp May 2019', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#may 23 removal
ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2019-05-23' & datetime < '2019-05-24')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp May 2019', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

hobo_w18_L1 <- hobo_w18_L1 %>% 
  mutate_at(vars("TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m"),
            ~(case_when(datetime > as.POSIXct('2019-05-23 9:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
hobo_temp_vert_L1 <- hobo_w18_L1 %>%
  select(-do_ppm) %>% 
  gather(variable, value, -datetime)

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2019-05-01' & datetime < '2019-06-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp May 2019', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(hobo_temp_vert_L1, aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp 2019', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme


#### do unit ####
hobo_do_w18 <- hobo_w18_L1 %>% 
  select(datetime, TempC_1m, do_ppm)

hobo_do_w18_vert <- hobo_do_w18 %>% 
  gather(variable, value, - datetime)

ggplot(hobo_do_w18_vert, aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2019', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

hobo_w18_L1 <- hobo_w18_L1 %>% 
  mutate(do_ppm = case_when(do_ppm < -50 ~ NA_real_,
                            is.na(TempC_1m) ~ NA_real_,
                            TRUE ~ do_ppm))
  
hobo_do_w18 <- hobo_w18_L1 %>% 
  select(datetime, TempC_1m, do_ppm)

hobo_do_w18_vert <- hobo_do_w18 %>% 
  gather(variable, value, - datetime)

ggplot(hobo_do_w18_vert, aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2019', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

ggplot(subset(hobo_do_w18_vert, subset = (datetime >= as.POSIXct('2018-11-01', tz='UTC') &
                                            datetime < as.POSIXct('2018-12-01', tz='UTC'))),
              aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2019', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w18_vert, subset = (datetime >= as.POSIXct('2018-12-01', tz='UTC') &
                                            datetime < as.POSIXct('2019-01-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2019', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme


ggplot(subset(hobo_do_w18_vert, subset = (datetime >= as.POSIXct('2019-01-01', tz='UTC') &
                                            datetime < as.POSIXct('2019-02-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2019', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme


ggplot(subset(hobo_do_w18_vert, subset = (datetime >= as.POSIXct('2019-02-01', tz='UTC') &
                                            datetime < as.POSIXct('2019-03-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2019', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w18_vert, subset = (datetime >= as.POSIXct('2019-03-01', tz='UTC') &
                                            datetime < as.POSIXct('2019-04-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2019', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

# erratic temp and do behavior is suspect ~ 16 and 18-20, not associated with temp changes, no associated climactic events
ggplot(subset(hobo_do_w18_vert, subset = (datetime >= as.POSIXct('2019-03-18', tz='UTC') &
                                            datetime < as.POSIXct('2019-03-19', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2019', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

ggplot(subset(hobo_do_w18_vert, subset = (datetime >= as.POSIXct('2019-03-20', tz='UTC') &
                                            datetime < as.POSIXct('2019-03-21', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2019', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

hobo_w18_L1 <- hobo_w18_L1 %>% 
  mutate(flag_do = case_when(datetime > as.POSIXct('2019-03-18 07:00', tz='UTC') &
                               datetime < as.POSIXct('2019-03-20 10:00', tz='UTC')~ 's',
                             TRUE ~ ''))


hobo_do_w18 <- hobo_w18_L1 %>% 
  select(datetime, TempC_1m, do_ppm, flag_do)

hobo_do_w18_vert <- hobo_do_w18 %>% 
  gather(variable, value, - datetime, -flag_do)

ggplot(subset(hobo_do_w18_vert, subset = (datetime >= as.POSIXct('2019-03-01', tz='UTC') &
                                            datetime < as.POSIXct('2019-04-01', tz='UTC'))),
       aes(x=datetime, y=value, color = flag_do)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2019', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w18_vert, subset = (datetime >= as.POSIXct('2019-04-01', tz='UTC') &
                                            datetime < as.POSIXct('2019-05-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2019', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w18_vert, subset = (datetime >= as.POSIXct('2019-05-01', tz='UTC') &
                                            datetime < as.POSIXct('2019-06-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2019', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w18_vert, subset = (datetime >= as.POSIXct('2019-05-19', tz='UTC') &
                                            datetime < as.POSIXct('2019-05-20', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2019', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme


#plot all 
hobo_w18_L1 %>% 
  gather(variable, value, -datetime, -flag_do) %>% 
  ggplot(.,
       aes(x=datetime, y=value, color = flag_do)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2018-2019', x='date') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme
  
#change time from GMT-4 to GMT-5 
hobo_w18_L1 <- hobo_w18_L1 %>% 
  mutate(datetime = datetime - hours(1))


  
#export L1 tempstring file
hobo_w18_L1 %>% 
  select(datetime, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m, do_ppm, flag_do) %>% 
  rename(waterTemperature_DO_degC_1m = 'TempC_1m',
         waterTemperature_degC_2m = 'TempC_2m',
         waterTemperature_degC_3m = 'TempC_3m',
         waterTemperature_degC_4m = 'TempC_4m',
         waterTemperature_degC_5m = 'TempC_5m',
         waterTemperature_degC_6m = 'TempC_6m',
         waterTemperature_degC_7m = 'TempC_7m',
         waterTemperature_degC_8m = 'TempC_8m',
         waterTemperature_degC_9m = 'TempC_9m',
         waterTemperature_degC_10m = 'TempC_10m',
         oxygenDissolved_mgl_1m = do_ppm) %>%
  mutate(datetime = as.character(datetime)) %>% 
  write_csv(., file.path(dump_dir, paste0('2018-2019_wintertempstringdo_L1_v', format(Sys.Date(), '%Y'), '.csv')))




