##############################################
################### SETUP ####################
##############################################

#Load packages

library(tidyverse)
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

#Import data

source('0. Import data.R')

#Theme

custom_theme <- theme(panel.border = element_blank(),
                      strip.text = element_text(size=10),
                      text = element_text(size = 10),
                      legend.title=element_text(size=10),
                      legend.text=element_text(size=10),
                      axis.text = element_text(size = 10),
                      axis.text.y = element_text(size = 10),
                      axis.text.x = element_text(angle = 45, hjust = 1,size = 10),
                      axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 10),
                      axis.title.y = element_text(size = 10))

####################################################
################### By consultant ##################
####################################################

compclean <- function(x){
  out <- x %>% as.numeric() %>% ifelse(is.na(.),0,.)
  return(out)
}

phin_vols <- vol_consultant_proc_2122 %>%
  select(consultant_gmc,nhs_episodes,nhs_spells) %>%
  rename(nhs_episodes_phin=nhs_episodes,
         nhs_spells_phin=nhs_spells)

su_vols <- su_consultants %>%
  filter(der_financial_year=="2021/22") %>%
  select(-"der_financial_year")

combined <- phin_vols %>%
  left_join(.,su_vols,by=c("consultant_gmc"="consultant_code")) %>%
  mutate_at(c("nhs_episodes_phin","nhs_spells_phin","nhs_episodes","nhs_spells"), compclean) %>% 
  mutate(episodes_diff=abs(nhs_episodes_phin-nhs_episodes),
         spells_diff=abs(nhs_spells_phin-nhs_spells)) %>%
  arrange(desc(episodes_diff))

hist(combined$episodes_diff)

##################################################################
################### By consultant and procedure ##################
##################################################################

#Cataracts

phin_cataracts <- vol_consultant_and_procedure_2122 %>%
  filter(str_detect(tolower(procedure_group),"cataract"))

su_cataracts <- su_consultants_procedure %>%
  filter(phin_proc_grp=="cataract_surgery"&der_financial_year=="2021/22") %>%
  select(consultant_code,nhs_episodes,nhs_spells) %>%
  rename(hes_nhs_episodes=nhs_episodes,
         hes_nhs_spells=nhs_spells)

combined_cataracts <- left_join(phin_cataracts,su_cataracts,by=c("consultant_gmc"="consultant_code")) %>%
  select(consultant_gmc,starts_with("name"),procedure_group,nhs_procedures,starts_with("hes")) %>%
  mutate(nhs_procedures_clean=ifelse(is.na(nhs_procedures),0,nhs_procedures) %>% as.numeric(),
         hes_nhs_spells_clean=ifelse(is.na(hes_nhs_spells),0,hes_nhs_spells) %>% as.numeric()) %>% 
  mutate(abs_diff=ifelse(is.na(nhs_procedures)&is.na(hes_nhs_spells),
                         NA,
                         abs(nhs_procedures_clean-hes_nhs_spells_clean))) %>%
    select(-contains("clean")) %>%
  arrange(desc(abs_diff))

boxplot(combined_cataracts$abs_diff)
quantile(combined_cataracts$abs_diff, probs = seq(.1, .9, by = .1), na.rm=TRUE)

