##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

##############################################
################### SETUP ####################
##############################################

#Load packages

library(tidyverse)
library(stringr)
library(tidyr)
library(data.table)
library(readxl)
library(writexl)
library(janitor)
library(aws.s3)

#Clean up the global environment
rm(list = ls())

#Directories in S3

IHT_bucket <- "s3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp"
PHIN_subfolder <- "/Private healthcare"
R_workbench <- path.expand("~")
localgit <- dirname(rstudioapi::getSourceEditorContext()$path)

##############################################
################### By site ##################
##############################################

vol_nation_proc_2122 <-  s3read_using(read_excel,
                                 object = paste0(PHIN_subfolder,"/Volume and length of stay/Hospital/Volume and Length of Stay - Site and National (1 Apr 21 - 31 Mar 22).xlsx"), # File to open
                                 sheet="Vol by Nation by Proc",
                                 skip=9,
                                 bucket = IHT_bucket) %>%
  janitor::clean_names()

vol_nation_proc_2021 <-  s3read_using(read_excel,
                                      object = paste0(PHIN_subfolder,"/Volume and length of stay/Hospital/Volume and Length of Stay - Site and National (1 April 2020 - 31 March 2021).xlsx"), # File to open
                                      sheet="Vol by Nation by Proc",
                                      skip=9,
                                      bucket = IHT_bucket) %>%
  janitor::clean_names()

####################################################
################### By consultant ##################
####################################################

#Consultant reference data
consultants <-  s3read_using(read_excel,
                                          object = paste0(PHIN_subfolder,"/Consultant reference data/Consultant Reference Data (8 Feb 22).xlsx"), # File to open
                                          sheet="Consultant Reference Data",
                                          skip=8,
                                          bucket = IHT_bucket) %>%
  janitor::clean_names()

#Vol by Nation by Proc
#From first sheet, looks like only England has NHS

vol_consultant_proc_2122 <-  s3read_using(read_excel,
                                          object = paste0(PHIN_subfolder,"/Volume and length of stay/Consultant/Volume and Length of Stay - Consultant (1 Apr 21 - 31 Mar 22).xlsx"), # File to open
                                          sheet="Vol by Consultant",
                                          skip=11,
                                          bucket = IHT_bucket) %>%
  janitor::clean_names()

vol_consultant_and_procedure_2122 <-  s3read_using(read_excel,
                                          object = paste0(PHIN_subfolder,"/Volume and length of stay/Consultant/Volume and Length of Stay - Consultant (1 Apr 21 - 31 Mar 22).xlsx"), # File to open
                                          sheet="Vol by Consultant and Proc",
                                          skip=11,
                                          bucket = IHT_bucket) %>%
  janitor::clean_names()

vol_consultant_proc_2021 <-  s3read_using(read_excel,
                                 object = paste0(PHIN_subfolder,"/Volume and length of stay/Consultant/Volume and Length of Stay - Consultant (1 April 2020 - 31 March 2021).xlsx"), # File to open
                                 sheet="Vol by Consultant",
                                 skip=11,
                                 bucket = IHT_bucket) %>%
  janitor::clean_names()

vol_consultant_and_procedure_2021 <-  s3read_using(read_excel,
                                          object = paste0(PHIN_subfolder,"/Volume and length of stay/Consultant/Volume and Length of Stay - Consultant (1 April 2020 - 31 March 2021).xlsx"), # File to open
                                          sheet="Vol by Consultant and Proc",
                                          skip=11,
                                          bucket = IHT_bucket) %>%
  janitor::clean_names()

#################################################
################### Complexity ##################
#################################################

los_nation_proc_2122 <-  s3read_using(read_excel,
                                      object = paste0(PHIN_subfolder,"/Volume and length of stay/Hospital/Volume and Length of Stay - Site and National (1 Apr 21 - 31 Mar 22).xlsx"), # File to open
                                      sheet="LoS by Nation by Proc - 1",
                                      skip=9,
                                      bucket = IHT_bucket) %>%
  janitor::clean_names()

los_nation_proc_2021 <-  s3read_using(read_excel,
                                      object = paste0(PHIN_subfolder,"/Volume and length of stay/Hospital/Volume and Length of Stay - Site and National (1 April 2020 - 31 March 2021).xlsx"), # File to open
                                      sheet="LoS by Nation by Proc - 1",
                                      skip=9,
                                      bucket = IHT_bucket) %>%
  janitor::clean_names()

#########################################
################### SU ##################
#########################################

su_consultants <-  s3read_using(fread,
                                      object = paste0(PHIN_subfolder,"/consultantNhsEpisodesSpells_SUS_221209.csv"), # File to open
                                      header=TRUE,
                                      bucket = IHT_bucket) %>%
  janitor::clean_names()

su_consultants_procedure <-  s3read_using(fread,
                                object = paste0(PHIN_subfolder,"/consultantNhsEpisodesSpells_selectedProcedures_SUS_221209.csv"), # File to open
                                header=TRUE,
                                bucket = IHT_bucket) %>%
  janitor::clean_names()
