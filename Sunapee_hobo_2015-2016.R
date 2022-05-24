#*      Cary Institute of Ecosystem Studies (Millbrook, NY)           *
#*                                                                    *
#* TITLE:   Sunapee_hobo_2015-2016.r                                  *
#* AUTHOR:  Bethel Steele steeleb@caryinstitute.org                   *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 4.1.3, RStudio 2022.02.20   *
#* PURPOSE: Collate and qaqc winter logger data at Sunapee            *


#load libraries
library(tidyverse)
library(lubridate)

# point to directories
data_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/winter hobo data/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/'

# save functions/groms
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom

#bring in summer 2015 hobo raw data
hobo_w15 <- read.csv(file.path(data_dir, '2015-16 Winter Pendant.csv'))

#format date of hobo sensors
hobo_w15 <- hobo_w15 %>% 
  mutate(datetime = as.POSIXct(Date.Time, format='%m/%d/%Y %H:%M', tz='UTC')) %>% #no dst observed, using UTC for simplicity
  select(-Date.Time)

#### hobo line ####
hobo_vert <- hobo_w15 %>%
  select(datetime, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m) %>%
  gather(variable, value, -datetime)

ggplot(hobo_vert, aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp sensor winter 2015-2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_breaks = '1 month') +
  final_theme

ggplot(subset(hobo_vert, subset=(datetime>='2015-10-01' & datetime < '2015-11-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Oct 2015', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_vert, subset=(datetime>='2015-11-01' & datetime < '2015-12-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Nov 2015', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_vert, subset=(datetime>='2015-12-01' & datetime < '2016-01-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Dec 2015', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_vert, subset=(datetime>='2016-01-01' & datetime < '2016-02-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Jan 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_vert, subset=(datetime>='2016-02-01' & datetime < '2016-03-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Feb 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_vert, subset=(datetime>='2016-03-01' & datetime < '2016-04-01')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Mar 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_vert, subset=(datetime>='2016-04-01' & datetime < '2016-05-04')), aes(x=datetime, y=value, col=variable)) +
  geom_point() +
  labs(title='hobo temp Apr 2016', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
   final_theme


#since these were deployed pre-DST change, we assume they are stored in GMT-4, here we change to GMT-5
hobo_w15 <- hobo_w15 %>% 
  mutate(datetime = datetime-hours(1))

#export L1 tempstring file
hobo_w15 %>%
  select(datetime, TempC_1m, TempC_2m, TempC_3m, TempC_4m, TempC_5m, TempC_6m, TempC_7m, TempC_8m, TempC_9m) %>%
  rename(waterTemperature_degC_1m = 'TempC_1m',
         waterTemperature_degC_2m = 'TempC_2m',
         waterTemperature_degC_3m = 'TempC_3m',
         waterTemperature_degC_4m = 'TempC_4m',
         waterTemperature_degC_5m = 'TempC_5m',
         waterTemperature_degC_6m = 'TempC_6m',
         waterTemperature_degC_7m = 'TempC_7m',
         waterTemperature_degC_8m = 'TempC_8m',
         waterTemperature_degC_9m = 'TempC_9m') %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(dump_dir, paste0('2015-2016_wintertempstring_L1_v', format(Sys.Date(), '%Y'), '.csv')))





