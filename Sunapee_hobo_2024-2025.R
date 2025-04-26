#*      Cary Institute of Ecosystem Studies (Millbrook, NY)           *
#*                                                                    *
#* TITLE:   Sunapee_hobo_2024-2025.r                                  *
#* AUTHOR:  Bethel Steele steeleb@caryinstitute.org                   *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 4.4.0, RStudio 2024.12      *
#* PURPOSE: Collate and qaqc winter logger data at Sunapee            *


#load libraries
library(tidyverse)

# point to directories
data_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L0/winter hobo data/2024-2025WinterTemp/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/'

# save functions/groms
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom

files <- list.files(data_dir, pattern = ".csv", full.names = T)

hobo_w24 <- map(.x = files, 
                .f = \(x) read_csv(x, 
                                   col_names = c('obs', 'datetime', 'TempC', 'detach', 'attach', 'connect', 'stop', 'eof'),
                                   skip = 2) %>% 
                  select(datetime, TempC) %>% 
                  mutate(source = basename(x))) %>% 
  bind_rows() %>% 
  mutate(datetime = as.POSIXct(datetime, format='%m/%d/%y %I:%M:%S %p', tz='UTC')) %>% #these are all in GMT-4, treating as UTC for simplicity
  mutate(depth = str_extract(source, "(?<=-)\\d+(?=m)"),
         .by = source) %>% 
  mutate(depth_label = paste0("TempC_", depth, "m")) %>% 
  arrange(datetime)

#create a L1
hobo_w24_L1 <- hobo_w24

#### hobo line ####
range(hobo_w24_L1$TempC, na.rm = T)
range(hobo_w24_L1$datetime)

ggplot(hobo_w24_L1, aes(x=datetime, y=TempC, col=depth)) +
  geom_point() +
  labs(title='hobo temp sensor winter 2024-2025', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_breaks = '1 month') +
  final_theme


ggplot(subset(hobo_w24_L1, subset=(datetime>='2024-10-01' & datetime < '2024-11-01')), aes(x=datetime, y=TempC, col=depth)) +
  geom_point() +
  labs(title='hobo temp Oct 2024', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme
# deployment looks good! earlier in the season than usual, so more separation in temp at start of record.

ggplot(subset(hobo_w24_L1, subset=(datetime>='2024-11-01' & datetime < '2024-12-01')), aes(x=datetime, y=TempC, col=depth)) +
  geom_point() +
  labs(title='hobo temp Nov 2024', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_w24_L1, subset=(datetime>='2024-12-01' & datetime < '2025-01-01')), aes(x=datetime, y=TempC, col=depth)) +
  geom_point() +
  labs(title='hobo temp Dec 2024', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_w24_L1, subset=(datetime>='2025-01-01' & datetime < '2025-02-01')), aes(x=datetime, y=TempC, col=depth)) +
  geom_point() +
  labs(title='hobo temp Jan 2025', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_w24_L1, subset=(datetime>='2025-02-01' & datetime < '2025-03-01')), aes(x=datetime, y=TempC, col=depth)) +
  geom_point() +
  labs(title='hobo temp Feb 2025', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_w24_L1, subset=(datetime>='2025-03-01' & datetime < '2025-04-01')), aes(x=datetime, y=TempC, col=depth)) +
  geom_point() +
  labs(title='hobo temp Mar 2025', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(subset(hobo_w24_L1, subset=(datetime>='2025-04-01' & datetime < '2025-05-01')), aes(x=datetime, y=TempC, col=depth)) +
  geom_point() +
  labs(title='hobo temp Apr 2025', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

# loggers removed April 25
ggplot(subset(hobo_w24_L1, subset=(datetime>=ymd('2025-04-25') & datetime < ymd('2025-04-26'))), aes(x=datetime, y=TempC, col=depth)) +
  geom_point() +
  labs(title='hobo temp Removal April 25 2025', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme


hobo_w24_L1 <- hobo_w24_L1 %>% 
  mutate(TempC = if_else(datetime >= as.POSIXct('2025-04-25 09:10', tz='UTC'), NA_real_, TempC)) %>% 
  filter(datetime < ymd('2025-05-01'))

ggplot(subset(hobo_w24_L1, subset=(datetime>='2025-04-01' & datetime < '2025-05-01')), aes(x=datetime, y=TempC, col=depth)) +
  geom_point() +
  labs(title='hobo temp April 2025', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

ggplot(hobo_w24_L1, aes(x=datetime, y=TempC, col=depth)) +
  geom_point() +
  labs(title='hobo temp 2025', x='date', y='temp (deg C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3", "#004d13",
                              "#00e639", "#d4c711", "#0081cc", "#66c7ff")) +
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme

  
# logger deployed before DST, so already in GMT-5
# checked Nov 3, no artifacts of DST

#export L1 tempstring file
hobo_w24_L1 %>% 
  select(-c(source, depth)) %>% 
  pivot_wider(names_from = depth_label, values_from = TempC) %>% 
  rename(waterTemperature_degC_1m = 'TempC_1m',
         waterTemperature_degC_2m = 'TempC_2m',
         waterTemperature_degC_3m = 'TempC_3m',
         waterTemperature_degC_4m = 'TempC_4m',
         waterTemperature_degC_5m = 'TempC_5m',
         waterTemperature_degC_6m = 'TempC_6m',
         waterTemperature_degC_7m = 'TempC_7m',
         waterTemperature_degC_8m = 'TempC_8m',
         waterTemperature_degC_9m = 'TempC_9m',
         waterTemperature_degC_10m = 'TempC_10m') %>%
  mutate(datetime = as.character(datetime)) %>% 
  write_csv(., file.path(dump_dir, paste0('2024-2025_wintertempstring_L1_v', format(Sys.Date(), '%Y'), '.csv')))
