#*      Cary Institute of Ecosystem Studies (Millbrook, NY)           *
#*                                                                    *
#* TITLE:   Sunapee_hobo_2019-2020.r                                  *
#* AUTHOR:  Bethel Steele steeleb@caryinstitute.org                   *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 4.1.3, RStudio 2022.02.20   *
#* PURPOSE: Collate and qaqc winter logger data at Sunapee            *


#load libraries
library(tidyverse)

# point to directories
data_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/winter hobo data/2019-2020WinterTempDO/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/'

# save functions/groms
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom

#bring in winter 17-18 hobo raw data
hobo_w19_2m <- read.csv(file.path(data_dir, 'u22-2m_20191105-20200503.csv'),
                        col.names = c('obs', 'datetime', 'TempC_2m'),
                        skip =2) %>% 
  select(-obs)
hobo_w19_3m <- read.csv(file.path(data_dir, 'u22-3m_20191105-20200503.csv'),
                        col.names = c('obs', 'datetime', 'TempC_3m'),
                        skip =2) %>% 
  select(-obs)
hobo_w19_4m <- read.csv(file.path(data_dir, 'u22-4m_20191105-20200503.csv'),
                        col.names = c('obs', 'datetime', 'TempC_4m'),
                        skip =2) %>% 
  select(-obs)
hobo_w19_5m <- read.csv(file.path(data_dir, 'u22-5m_20191105-20200503.csv'),
                        col.names = c('obs', 'datetime', 'TempC_5m'),
                        skip =2) %>% 
  select(-obs)
hobo_w19_6m <- read.csv(file.path(data_dir, 'u22-6m_20191105-20200503.csv'),
                        col.names = c('obs', 'datetime', 'TempC_6m'),
                        skip =2) %>% 
  select(-obs)
hobo_w19_7m <- read.csv(file.path(data_dir, 'u22-7m_20191105-20200503.csv'),
                        col.names = c('obs', 'datetime', 'TempC_7m'),
                        skip =2) %>% 
  select(-obs)
hobo_w19_8m <- read.csv(file.path(data_dir, 'u22-8m_20191105-20200503.csv'),
                        col.names = c('obs', 'datetime', 'TempC_8m'),
                        skip =2) %>% 
  select(-obs)
hobo_w19_9m <- read.csv(file.path(data_dir, 'u22-9m_20191105-20200503.csv'),
                        col.names = c('obs', 'datetime', 'TempC_9m'),
                        skip =2) %>% 
  select(-obs)
hobo_w19_10m <- read.csv(file.path(data_dir, 'u22-10m_20191105-20200503.csv'),
                         col.names = c('obs', 'datetime', 'TempC_10m'),
                         skip =2) %>% 
  select(-obs)
hobo_w19_do <- read.csv(file.path(data_dir, 'U26DO_20191105-20200503.csv'),
                        col.names = c('obs', 'datetime', 'do_ppm', 'TempC_1m'),
                        skip=2) %>% 
  select(-obs)

hobo_w19 <- full_join(hobo_w19_do, hobo_w19_2m) %>% 
  full_join(., hobo_w19_3m) %>% 
  full_join(., hobo_w19_4m) %>% 
  full_join(., hobo_w19_5m) %>% 
  full_join(., hobo_w19_6m) %>% 
  full_join(., hobo_w19_7m) %>% 
  full_join(., hobo_w19_8m) %>% 
  full_join(., hobo_w19_9m) %>% 
  full_join(., hobo_w19_10m)

rm(hobo_w19_do, hobo_w19_2m, hobo_w19_3m, hobo_w19_4m, hobo_w19_5m, hobo_w19_6m, hobo_w19_7m, hobo_w19_8m, hobo_w19_9m, hobo_w19_10m)

#format date of hobo sensors
hobo_w19 <- hobo_w19 %>% 
  mutate(datetime = as.POSIXct(datetime, format='%m/%d/%y %I:%M:%S %p', tz='UTC')) %>% #these are all in GMT-5, treating as UTC for simplicity
  arrange(datetime)

#create a L1
hobo_w19_L1 <- hobo_w19

#### hobo line ####
hobo_temp_vert <- hobo_w19 %>%
  select(-do_ppm) %>% 
  gather(variable, value, -datetime)
range(hobo_temp_vert$value, na.rm = T)

hobo_temp_vert_L1 <- hobo_w19_L1 %>%
  select(-do_ppm) %>% 
  gather(variable, value, -datetime)

ggplot(hobo_temp_vert_L1, aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp sensor winter 2019-2020', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_breaks = '1 month') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2019-11-01' & datetime < '2019-12-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Nov 2019', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2019-12-01' & datetime < '2020-01-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Dec 2019', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2020-01-01' & datetime < '2020-02-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Jan 2020', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2020-02-01' & datetime < '2020-03-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Feb 2020', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2020-03-01' & datetime < '2020-04-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Mar 2020', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2020-04-01' & datetime < '2020-05-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Apr 2020', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2020-05-01' & datetime < '2020-06-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp May 2020', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#may 3 removal
ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2020-05-03' & datetime < '2020-05-04')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp May 2020', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

hobo_w19_L1 <- hobo_w19_L1 %>% 
  mutate_at(vars("TempC_1m", "TempC_2m", "TempC_3m", "TempC_4m", "TempC_5m", "TempC_6m", "TempC_7m", "TempC_8m", "TempC_9m", "TempC_10m"),
            ~(case_when(datetime >= as.POSIXct('2020-05-03 9:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
hobo_temp_vert_L1 <- hobo_w19_L1 %>%
  select(-do_ppm) %>% 
  gather(variable, value, -datetime)

ggplot(subset(hobo_temp_vert_L1, subset=(datetime>='2020-05-01' & datetime < '2020-06-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp May 2020', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(hobo_temp_vert_L1, aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp 2020', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme


#### do unit ####
hobo_do_w19 <- hobo_w19_L1 %>% 
  select(datetime, TempC_1m, do_ppm)

hobo_do_w19_vert <- hobo_do_w19 %>% 
  gather(variable, value, - datetime)

ggplot(hobo_do_w19_vert, aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2019-2020', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

hobo_w19_L1 <- hobo_w19_L1 %>% 
  mutate(do_ppm = case_when(is.na(TempC_1m) ~ NA_real_,
                            TRUE ~ do_ppm))
  
hobo_do_w19 <- hobo_w19_L1 %>% 
  select(datetime, TempC_1m, do_ppm)

hobo_do_w19_vert <- hobo_do_w19 %>% 
  gather(variable, value, - datetime)

ggplot(hobo_do_w19_vert, aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2019', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

  
ggplot(subset(hobo_do_w19_vert, subset = (datetime >= as.POSIXct('2019-11-01', tz='UTC') &
                                            datetime < as.POSIXct('2019-12-01', tz='UTC'))),
              aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp nov 2019', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w19_vert, subset = (datetime >= as.POSIXct('2019-12-01', tz='UTC') &
                                            datetime < as.POSIXct('2020-01-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp dec 2019', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w19_vert, subset = (datetime >= as.POSIXct('2020-01-01', tz='UTC') &
                                            datetime < as.POSIXct('2020-02-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp jan 2020', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w19_vert, subset = (datetime >= as.POSIXct('2020-02-01', tz='UTC') &
                                            datetime < as.POSIXct('2020-03-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp feb 2020', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w19_vert, subset = (datetime >= as.POSIXct('2020-03-01', tz='UTC') &
                                            datetime < as.POSIXct('2020-04-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp mar 2020', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w19_vert, subset = (datetime >= as.POSIXct('2020-04-01', tz='UTC') &
                                            datetime < as.POSIXct('2020-05-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp apr 2020', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_do_w19_vert, subset = (datetime >= as.POSIXct('2020-05-01', tz='UTC') &
                                            datetime < as.POSIXct('2020-06-01', tz='UTC'))),
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp may 2020', x='date', y='temp (deg C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#plot all 
hobo_w19_L1 %>% 
  gather(variable, value, -datetime) %>% 
  ggplot(.,
       aes(x=datetime, y=value)) +
  facet_grid(variable ~. , scales = 'free_y') +
  geom_point() +
  labs(title='hobo do/temp 2019-2020', x='date') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme
  

  
#export L1 tempstring file
hobo_w19_L1 %>% 
  select(datetime, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m, TempC_10m, do_ppm) %>% 
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
  write_csv(., file.path(dump_dir, paste0('2019-2020_hobotempstringdo_L1_v', format(Sys.Date(), '%Y'), '.csv')))




