# winter-buoy-loggers

This repo of code is used to collate and QAQC the Lake Sunapee winter temperature and dissolved oxygen HOBO dataloggers.

All data are processed/cleaned approximately 1x per year.

All data created with these scritps are archived at EDI (https://portal.edirepository.org/nis/mapbrowse?packageid=edi.500.3). Please review the metadata at EDI for data caveats, notes, and code definitions. 

All questions about these scripts and data should be directed to B Steele (steeleb@caryinstitute.org) and Kathie Weathers (weathersk@caryinstitute.org).

# File Descriptions

The order of scripts, as presented here, is the order in which they are run. Note, all raw data are stored locally (B. Steele, Cary Institute, steeleb@caryinstitute.org) and the following programs reference locally-sourced data.

## processing and collation

 * Sunapee_hobo_2014-2015.R - processing of the 2014-2015 HOBO light/temperature pendants
 * Sunapee_hobo_2015-2016.R - processing of the 2015-2016 HOBO light/temperature pendants
 * Sunapee_hobo_2016-2017.R - processing of the 2016-2017 HOBO light/temperature pendants
 -- change in equipment --
 * Sunapee_hobo_2017-2018.R - processing of the 2017-2018 HOBO U22 temperature sensors and U26 dissolved oxygen and temperature sensor
 * Sunapee_hobo_2018-2019.R - processing of the 2018-2019 HOBO U22 temperature sensors and U26 dissolved oxygen and temperature sensor
 * Sunapee_hobo_2019-2020.R - processing of the 2019-2020 HOBO U22 temperature sensors and U26 dissolved oxygen and temperature sensor
 -- change in equipment --
 * Sunapee_hobo_2020-2021.R - processing of the 2020-2021 HOBO U22 tempearture sensors
 * Sunapee_hobo_2021-2022.R - processing of the 2021-2022 HOBO U22 tempearture sensors

## prep for push to EDI

 * sunapee_buoywinter_eml_v500.3.R - this script creates the EML metadata for upload to EDI