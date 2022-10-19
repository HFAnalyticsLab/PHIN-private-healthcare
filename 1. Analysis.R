##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

#Cost?
#Ratio of private to NHS?

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
library(ggbeeswarm)
library(ggrepel)
library(plotly)
library(ggbreak)

#Clean up the global environment
rm(list = ls())

#Directories in S3

IHT_bucket <- "s3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp"
PHIN_subfolder <- "/Private healthcare"
R_workbench <- path.expand("~")
localgit <- dirname(rstudioapi::getSourceEditorContext()$path)

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

custom_theme_horiz <- theme(legend.position = "none",
                      panel.border = element_blank(),
                      strip.text = element_text(size=10),
                      text = element_text(size = 10),
                      legend.title=element_text(size=10),
                      legend.text=element_text(size=10),
                      axis.text = element_text(size = 10),
                      axis.text.y = element_text(size = 10),
                      axis.text.x = element_text(angle = 0, hjust = 1,size = 10),
                      axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 10),
                      axis.title.y = element_text(size = 10))

#Import data

source('0. Import data.R')

##############################################
################### By site ##################
##############################################

vol_nation_proc <- plyr::rbind.fill(vol_nation_proc_2122,vol_nation_proc_2021) %>%
  mutate(year=case_when(period_from=="2021-04-01"&period_to=="2022-03-31" ~ "20212022",
                        period_from=="2020-04-01"&period_to=="2021-03-31" ~ "20202021",
                        TRUE ~ "NA"))

vol_nation_proc_clean <- vol_nation_proc %>%
  filter(.,nation=="England") %>% 
  select(.,year,nation,procedure_group,procedure_group_id,private_procedures,nhs_procedures,all_activity_procedures,period_from,period_to) %>%
  mutate_at(c("private_procedures","nhs_procedures"), as.numeric) %>%
  mutate(new_total = rowSums(across(c(private_procedures,nhs_procedures)), na.rm = TRUE)) %>%
  mutate(pct_NHS=nhs_procedures/new_total*100) %>% 
  arrange(desc(new_total))

#Chart 0
#What are private hospitals (incl private wings in NHS) doing? How much of that is paid by NHS?

top10_private_2122 <- vol_nation_proc_clean %>%  filter(year=="20212022") %>% slice_max(private_procedures,n=10) %>% pull(procedure_group)
top10_nhs_2122 <- vol_nation_proc_clean %>%  filter(year=="20212022") %>% slice_max(nhs_procedures,n=10) %>% pull(procedure_group)

cross_bubble_data <- vol_nation_proc_clean %>%
  filter(year=="20212022"&(procedure_group %in% c(top10_nhs_2122,top10_private_2122))) %>%
  select(procedure_group,private_procedures,nhs_procedures) %>%
  pivot_longer(!procedure_group, names_to = "type", values_to = "treatments") %>%
  mutate(list=case_when((procedure_group %in% top10_nhs_2122)&(procedure_group %in% top10_private_2122) ~ "both",
                        (procedure_group %in% top10_nhs_2122)&!(procedure_group %in% top10_private_2122) ~ "nhs",
                        !(procedure_group %in% top10_nhs_2122)&(procedure_group %in% top10_private_2122) ~ "private",
                        TRUE ~ "NA"), 
         type=str_replace_all(type,"_procedures",""),
         nhs=ifelse(type=="nhs",treatments,NA),
         private=ifelse(type=="private",treatments,NA)) %>%
  group_by(procedure_group) %>%
  mutate(total=sum(treatments,na.rm=TRUE),
         nhs=max(nhs,na.rm=TRUE),
         private=max(private,na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(pct=treatments/total*100) %>% 
  arrange(desc(nhs))

bubble_table <- vol_nation_proc_clean %>%
  filter(year=="20212022"&(procedure_group %in% c(top10_nhs_2122,top10_private_2122))) %>%
  select(procedure_group,private_procedures,nhs_procedures)

cross_bubble_nhs <-  filter(cross_bubble_data,list=="nhs"|list=="both") %>%
  ggplot(., aes(x = type,y = reorder(procedure_group,nhs),
                colour = type, size = treatments)) +
  geom_point() +
  geom_text(aes(label = paste0(round(pct,0),"%")), 
            colour = "black", 
            size = 3) +
  scale_x_discrete(position = "top") +
  scale_size_continuous(range = c(5, 25)) + # Adjust as required.
  scale_color_brewer(palette = "Set1") +
  labs(title='Top 10 NHS procedures by volume', subtitle='2021/22') +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  custom_theme_horiz
cross_bubble_nhs

cross_bubble_private <-  filter(cross_bubble_data,list=="private"|list=="both") %>%
  ggplot(., aes(x = type,y = reorder(procedure_group,private),
                colour = type, size = treatments)) +
  geom_point() +
  geom_text(aes(label = paste0(round(pct,0),"%")), 
            colour = "black", 
            size = 3) +
  scale_x_discrete(position = "top") +
  scale_size_continuous(range = c(5, 25)) + # Adjust as required.
  scale_color_brewer(palette = "Set1") +
  labs(title='Top 10 private procedures by volume', subtitle='2021/22') +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  custom_theme_horiz
cross_bubble_private

# chart0a <-  vol_nation_proc_clean %>%
#   select(.,year,nation,procedure_group,procedure_group,procedure_group_id,
#          private_procedures,nhs_procedures,new_total) %>%
#   pivot_longer(!c(year,nation,procedure_group,procedure_group_id), names_to = "type", values_to = "procedures") %>%
#   filter(.,type!="new_total"&year=="20212022") %>%
#   group_by(year,nation,procedure_group,procedure_group_id) %>%
#   mutate(total_procedures=sum(procedures,na.rm=TRUE)) %>% 
#   ungroup() %>%
#   mutate(.,pct_NHS=ifelse(type=="nhs_procedures",procedures/total_procedures,NA)) %>% 
#   slice_max(.,total_procedures,n=10*2) %>% 
#   ggplot(.) +
#   geom_bar(aes(fill=type, y=procedures, x=reorder(procedure_group,total_procedures)),
#            position="stack", stat="identity") +
#   geom_line(aes(x = reorder(procedure_group,total_procedures), y = 1000000*pct_NHS,group = type),
#             size = 1.5, color="black") +
#   scale_y_continuous(labels = scales::comma, name="Procedures",
#                      sec.axis = sec_axis(~./1000000, name = "Share of NHS funded",labels = scales::percent)) +
#   xlab("Procedure group") +
#   ggtitle("FY 2021/22, top 10 highest volume procedures") +
#   labs(fill = "Funding") +
#   theme_bw() +
#   scale_fill_brewer(palette="Set1",direction=-1) +
#   custom_theme
# chart0

#Chart 1: where has the private sector added most treatments in past year? Did it disturb the NHS/private mix?

delta_rankings <- vol_nation_proc_clean %>%
  select(.,year,procedure_group,
         private_procedures,nhs_procedures,new_total) %>%
  mutate(pct_nhs=nhs_procedures/new_total*100) %>%
  pivot_wider(
    names_from = year,
    names_sep = "_",
    values_from = c(new_total,private_procedures,nhs_procedures,pct_nhs)
  ) %>%
  mutate(delta_pct_NHS=pct_nhs_20212022-pct_nhs_20202021,
         delta_all=new_total_20212022-new_total_20202021,
         delta_NHS=nhs_procedures_20212022-nhs_procedures_20202021,
         delta_private=private_procedures_20212022-private_procedures_20202021) %>%
  select(procedure_group,delta_all,delta_NHS,delta_private) %>%
  pivot_longer(!procedure_group, names_to = "type", values_to = "delta") %>%
  filter(type!="delta_all"&!is.na(delta)) %>%
  mutate(abs_delta=abs(delta)) %>% 
  arrange(desc(abs_delta))

table_delta_private <- delta_rankings %>%
  filter(type=="delta_private") %>% 
  slice_max(abs_delta,n=5) %>%
  select(-"abs_delta")

top_private_changes <- delta_rankings %>%
  filter(type=="delta_private") %>% 
  slice_max(abs_delta,n=5) %>%
  pull(procedure_group)

chart1 <- vol_nation_proc_clean %>%
  select(.,year,procedure_group,private_procedures,nhs_procedures) %>%
  filter(procedure_group %in% top_private_changes) %>%
  arrange(procedure_group,year) %>%
  mutate(year=ifelse(year=="20202021","2020/21","2021/22")) %>%
  pivot_longer(!c(year,procedure_group), names_to = "type", values_to = "count") %>%
  group_by(year,procedure_group) %>%
  mutate(new_total=sum(count)) %>% 
  ungroup() %>% 
  mutate(count_pct=ifelse(type=="private_procedures",count/new_total,NA)) %>%
  ggplot(.) +
  facet_wrap(~procedure_group, ncol=3) +
  geom_bar(aes(fill=type, y=count, x=year), position="stack", stat="identity") +
  geom_line(aes(x = year, y = count_pct*1000000, group=type), stat="identity",
            size = 1, col="black") +
  scale_y_continuous(labels = scales::comma, name="Treatments",
                     sec.axis = sec_axis(~./1000000, name = "Share of private procedures",labels = scales::percent)) +
  scale_fill_brewer(palette="Set1",direction=-1) +
  theme_bw() +
  custom_theme
chart1

#Chart 2
#What else are they doing? (with low NHS funding)
# 
# chart2 <-  vol_nation_proc_clean %>%
#   select(.,year,nation,procedure_group,procedure_group,procedure_group_id,
#          private_procedures,nhs_procedures,new_total) %>%
#   pivot_longer(!c(`nation`,`procedure_group`,`procedure_group_id`), names_to = "type", values_to = "procedures") %>%
#   filter(.,type!="new_total"&year=="20212022") %>%
#   group_by(Nation,`procedure_group`,`procedure_group_id`) %>%
#   mutate(total_procedures=sum(procedures,na.rm=TRUE)) %>% 
#   ungroup() %>%
#   mutate(.,pct_NHS=ifelse(type=="nhs_procedures",procedures/total_procedures,NA)) %>%
#   group_by(Nation,procedure_group,procedure_group_id) %>%
#   mutate(pct_NHS=max(pct_NHS,na.rm=TRUE)) %>% 
#   ungroup() %>%
#   filter(pct_NHS<0.5) %>% 
#   slice_max(.,total_procedures,n=10*2) %>% 
#   ggplot(.) +
#   geom_bar(aes(fill=type, y=procedures, x=reorder(`Procedure Group`,total_procedures)),
#            position="stack", stat="identity") +
#   geom_line(aes(x = reorder(`Procedure Group`,total_procedures), y = 50000*pct_NHS,group = type),
#             size = 1.5, color="black") +
#   scale_y_continuous(labels = scales::comma, name="Treatments",
#                      sec.axis = sec_axis(~./50000, name = "Share of NHS funded",labels = scales::percent)) +
#   xlab("Procedure group") +
#   ggtitle("FY 2021/22, top 10 treatments in private settings (incl. private NHS wards)\n in England *with <50% NHS funding*") +
#   labs(fill = "Funding") +
#   theme_bw() +
#   scale_fill_brewer(palette="Set1",direction=-1) +
#   custom_theme
# 
# chart2

####################################################
################### By consultant ##################
####################################################

#Consultant reference data
#unique(consultants$gmc_specialty_1) %>%  sort()

consultants <- consultants %>%
  select(consultant_gmc,starts_with("gmc_specialty")) %>%
  splitstackshape::merged.stack(., var.stubs ="gmc_specialty", sep = "_") %>%
  select(-c(".time_1")) %>%
  dplyr::rename(order='.time_2') %>%
  filter(!is.na(gmc_specialty)) %>%
  mutate(ophthalmology=ifelse(gmc_specialty %in% c("Ophthalmology","Ophthalmic Surgery"),1,0),
         gastroenterology=ifelse(gmc_specialty %in% c("Gastroenterology","Gastro-enterology"),1,0),
         orthopaedics=ifelse(gmc_specialty %in% c("Orthopaedics",
                                                  "Orthopaedic surgery",
                                                  "Trauma and Orthopaedics",
                                                  "Trauma and orthopaedic surgery"),1,0),
         plastics=ifelse(gmc_specialty %in% c("Plastic surgery"),1,0),
         cardiology=ifelse(gmc_specialty %in% c("Cardiology","Cardio-thoracic surgery","Cardiothoracic surgery"),1,0)) %>% 
  group_by(consultant_gmc) %>%
  summarise(number_specialty=max(order),
            first_specialty=first(gmc_specialty),
            ophthalmology=max(ophthalmology),
            gastroenterology=max(gastroenterology),
            orthopaedics=max(orthopaedics),
            plastics=max(plastics),
            cardiology=max(cardiology)) %>% 
  ungroup() %>%
  mutate(specialty=ifelse(number_specialty>1,"Multiple",first_specialty)) %>%
  select(consultant_gmc,specialty,ophthalmology,gastroenterology,orthopaedics,plastics,cardiology)

#Vol by Nation by Proc
#Cleaning

vol_consultant_proc <- plyr::rbind.fill(vol_consultant_proc_2021,vol_consultant_proc_2122) %>%
  left_join(.,consultants,by="consultant_gmc") %>%
  mutate(specialty=ifelse(is.na(specialty),"Unknown",specialty)) %>%
  mutate(.,private_episodes_num=ifelse(private_episodes=="*","5",private_episodes) %>% as.numeric(),
         nhs_episodes_num=ifelse(nhs_episodes=="*","5",nhs_episodes) %>% as.numeric(),
         year=case_when(period_from=="2021-04-01"&period_to=="2022-03-31" ~ "20212022",
                        period_from=="2020-04-01"&period_to=="2021-03-31" ~ "20202021",
                        TRUE ~ "NA")) %>%
  mutate(.,total_episodes_num=rowSums(across(ends_with("_num"))),
         nhs_to_private_episodes=nhs_episodes_num/private_episodes_num) %>%
  mutate(
    pct_private=private_episodes_num/total_episodes_num*100,
    more_ratio=ifelse(nhs_to_private_episodes<1,"more private","more NHS"))

# vol_consultant_proc %>%
#   filter(nhs_episodes_num==0|private_episodes_num==0)

vol_consultant_proc_wide <- vol_consultant_proc %>%
  select(.,consultant_gmc,consultant_forenames,consultant_surname,specialty,
         ophthalmology,gastroenterology,orthopaedics,plastics,cardiology,
         pct_private,nhs_to_private_episodes,nhs_episodes_num,private_episodes_num,
         total_episodes_num,year) %>%
  filter(!is.na(nhs_to_private_episodes)) %>%
  pivot_wider(names_from = year,
              names_sep = "_",
              values_from = c(pct_private,nhs_to_private_episodes, private_episodes_num, nhs_episodes_num,total_episodes_num)) %>%
  mutate(pct_private_diff=pct_private_20212022-pct_private_20202021,
         ratio_diff=nhs_to_private_episodes_20212022-nhs_to_private_episodes_20202021,
         ratio_direction=ifelse(nhs_to_private_episodes_20212022>nhs_to_private_episodes_20202021,
                                "more NHS","more private")) %>%
  mutate(specsum=rowSums(across(c("ophthalmology","gastroenterology","orthopaedics","plastics","cardiology")))) %>%
  mutate(.,spec_catgory_bis=case_when(specsum>1 ~ "multiple main specialties",
                                      ophthalmology==1 ~ "ophthalmology",
                                      gastroenterology==1 ~ "gastroenterology",
                                      orthopaedics==1 ~ "orthopaedics",
                                      plastics==1 ~ "plastics",
                                      cardiology==1 ~ "cardiology",
                                      TRUE ~ "other"))

# fabricatr::split_quantile(x = vol_consultant_proc_wide$nhs_to_private_episodes_2122, type = 5)
# gtools::quantcut(vol_consultant_proc_wide$nhs_to_private_episodes_2122, seq(0, 1, by = 0.2))
# cut(vol_consultant_proc_wide$nhs_to_private_episodes_2122, breaks = c(seq(0,10,by=1),100))
#nhs_to_private_episodes_bin=cut(nhs_to_private_episodes,breaks = c(seq(0,10,by=1),15,20,25,100))

#Chart 1: Overall ratio
 
# vol_consultant_proc %>%
#   group_by(specialty,year) %>%
#   summarise(nhs_episodes_num=sum(nhs_episodes_num,na.rm=TRUE),
#             private_episodes_num=sum(private_episodes_num,na.rm=TRUE),
#             total_episodes_num=sum(total_episodes_num,na.rm=TRUE)) %>% 
#   ungroup() %>%
#   mutate(pct_private=private_episodes_num/total_episodes_num*100,
#          nhs_to_private_episodes=nhs_episodes_num/private_episodes_num)

#Chart 3: Ratio by year

chart3_data <- vol_consultant_proc %>%
  mutate(year=ifelse(year=="20202021","2020/21","2021/22")) %>%
  group_by(year) %>%
  summarise(pct_private=mean(pct_private,na.rm=TRUE)) %>% 
  ungroup()

chart3 <- vol_consultant_proc %>%
  mutate(year=ifelse(year=="20202021","2020/21","2021/22")) %>% 
  ggplot(., aes(x=pct_private)) +
  facet_wrap(~year, scales = "free") +
  geom_histogram(fill="aquamarine", position="dodge",col="black")+
  geom_vline(xintercept =29,col="red",lwd=1.5) +
  xlab("Percent of treatments delivered privately") +
  ylab("Number of consultants") +
  theme_bw() +
  custom_theme
chart3

# chart4 <- vol_consultant_proc %>%
#   group_by(year,more_ratio) %>% 
#   summarise(n=n()) %>%
#   ungroup() %>%
#   filter(!is.na(more_ratio)) %>%
#   group_by(year) %>% 
#   mutate(n_year=sum(n)) %>%
#   ungroup() %>%
#     mutate(pct=n/n_year) %>% 
#   ggplot(., aes(fill=more_ratio, y=pct, x=year)) +
#   ggtitle("Consultants") +
#   geom_bar(position="fill", stat="identity")  +
#   geom_text(aes(label = paste0(round(pct*100,1),"%")), size = 3, hjust = 0.5, vjust = 3, position =     "stack") +
#   scale_y_continuous(labels = scales::percent) +
#   scale_fill_brewer(palette="Set1", direction=-1) +
#   theme_bw() +
#   custom_theme
# chart4

chart5 <- vol_consultant_proc_wide %>% 
  ggplot(., aes(x=pct_private_diff)) +
  geom_histogram(fill="tomato", position="dodge",col="black")+
  geom_vline(xintercept =0,col="red",lwd=1.5) +
  xlab("Change in percentage private (points)") +
  ylab("Number of consultants") +
  annotate("text", x = 40, y = 200, size = 3, label = "Did more private →") +
  annotate("text", x = -40, y = 200, size = 3, label = "← Did more NHS") +
  theme_bw() +
  custom_theme
chart5

# chart6 <- vol_consultant_proc_wide %>%
#   ggplot(., aes(x=pct_private_20202021, y=pct_private_20212022,col=ratio_direction)) +
#   geom_point(size=2, shape=23, alpha=0.5) +
#   xlim(0,100) + ylim(0,100) +
#   geom_abline(intercept = 0, slope = 1,col="black",lty="dotted") +
#   scale_color_brewer(palette="Set1", direction=-1) +
#   theme_bw() +
#   custom_theme
# chart6

# mean_pct_diff_by_main_specialty <- vol_consultant_proc_wide %>%
#   group_by(specialty) %>%
#   summarise(pct_private_diff=mean(pct_private_diff,na.rm=TRUE),
#             consultants=n(),
#             total_episodes_num_20212022=sum(total_episodes_num_20212022,na.rm=TRUE)) %>% 
#   ungroup() %>%
#   arrange(desc(pct_private_diff))

mean_pct_diff_by_main_specialty_bis <- vol_consultant_proc_wide %>%
  group_by(spec_catgory_bis) %>%
  summarise(pct_private_diff=mean(pct_private_diff,na.rm=TRUE),
            consultants=n(),
            total_episodes_num_20212022=sum(total_episodes_num_20212022,na.rm=TRUE)) %>% 
  ungroup() %>%
  arrange(desc(pct_private_diff))

beeswarm <- ggplot(vol_consultant_proc_wide, aes(x = spec_catgory_bis, y = pct_private_diff, fill = spec_catgory_bis)) +
  geom_violin(alpha = 0.5) +
  xlab("Main specialty") +
  ylab("Change in percentage private (points)") +
  annotate("text", x = 1, y = 45, size = 3, label = "Did more private ↑") +
  annotate("text", x = 1, y = -40, size = 3, label = "Did more NHS ↓") +
  theme_bw() +
  custom_theme +
  theme(legend.position = "none")
beeswarm

#Add specialty info at the consultant level and simplify

# chart7 <- vol_consultant_proc_wide %>%
#   filter(ophthalmology==1) %>%
#   filter(nhs_to_private_episodes_20202021<200&nhs_to_private_episodes_20212022<200) %>% 
#   ggplot(., aes(x=nhs_to_private_episodes_20202021, y=nhs_to_private_episodes_20212022,col=ratio_direction)) +
#   geom_point(size=2, shape=23, alpha=0.5) +
#   xlim(0,25) + ylim(0,25) +
#   geom_abline(intercept = 0, slope = 1,col="red",lty="dotted") +
#   scale_color_brewer(palette="Set1", direction=-1) +
#   theme_bw() +
#   custom_theme
# chart7

#################################################
################### Complexity ##################
#################################################

los_nation_proc <- plyr::rbind.fill(los_nation_proc_2122,los_nation_proc_2021) %>%
  mutate(year=case_when(period_from=="2021-04-01"&period_to=="2022-03-31" ~ "20212022",
                        period_from=="2020-04-01"&period_to=="2021-03-31" ~ "20202021",
                        TRUE ~ "NA"))

los_nation_proc <- los_nation_proc %>%
  filter(.,nation=="England") %>% 
  select(.,year,nation,procedure_group,procedure_group_id,starts_with("private"),starts_with("nhs"),starts_with("all_activity"),period_from,period_to) %>%
  mutate_at(c("private_inpatient_median_los","nhs_inpatient_median_los",
              "private_daycase_rate_percent","nhs_daycase_rate_percent",
              "private_inpatient_spells","nhs_inpatient_spells","all_activity_inpatient_spells"), as.numeric) %>%
  mutate(pct_private=private_inpatient_spells/all_activity_inpatient_spells*100)

los_nation_proc_wide <- los_nation_proc %>%
  mutate(spread_dayrate=private_daycase_rate_percent-nhs_daycase_rate_percent,
         spread_median=private_inpatient_median_los-nhs_inpatient_median_los) %>%
  mutate(spread_direction=ifelse(spread_dayrate>0,"more day-cases in private","more day-cases in NHS")) %>% 
  select(year,procedure_group,private_inpatient_spells,nhs_inpatient_spells,all_activity_inpatient_spells,
         private_daycase_rate_percent,nhs_daycase_rate_percent,spread_dayrate,spread_median,
         pct_private,spread_direction) %>%
  pivot_wider(names_from = year,
              names_sep = "_",
              values_from = c(private_inpatient_spells,nhs_inpatient_spells,all_activity_inpatient_spells,
                              private_daycase_rate_percent,nhs_daycase_rate_percent,
                              spread_dayrate,spread_median,pct_private,spread_direction)) %>%
  mutate(spread_dayrate_delta=spread_dayrate_20212022-spread_dayrate_20202021,
         spread_median_delta=spread_median_20212022-spread_median_20202021)

static_spread_ranking <- los_nation_proc_wide %>%
  filter(!is.na(spread_dayrate_20212022)) %>%
  select(procedure_group,spread_dayrate_20212022,private_daycase_rate_percent_20212022,
         nhs_daycase_rate_percent_20212022,private_inpatient_spells_20212022,
         nhs_inpatient_spells_20212022,all_activity_inpatient_spells_20212022,
         pct_private_20212022,spread_direction_20212022) %>% 
  mutate(abs_spread_dayrate_20212022=abs(spread_dayrate_20212022)) %>% 
  arrange(desc(abs_spread_dayrate_20212022))

#Chart 0: how many procedures are faster in private vs NHS

barchart_comparison <- static_spread_ranking %>%
  tabyl(spread_direction_20212022) %>% 
  ggplot(data=., aes(x=spread_direction_20212022, y=n, fill=spread_direction_20212022)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
  theme_bw() +
  labs(title='Day-cases in NHS vs private', subtitle='2021/22, by procedures') +
  xlab(" ") +
  ylab("Procedures") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette="Set1",direction=-1) +
  theme_bw() +
  theme(legend.position = "none")
barchart_comparison

#Chart 1: dumbell plot

#Bottom
barbell_data_bottom <- static_spread_ranking %>%
  slice_min(spread_dayrate_20212022,n=10) %>%
  arrange(desc(spread_dayrate_20212022)) %>% 
  select(procedure_group,spread_dayrate_20212022,nhs_daycase_rate_percent_20212022,private_daycase_rate_percent_20212022) %>%
  pivot_longer(!c(procedure_group,spread_dayrate_20212022), names_to = "sector", values_to = "pct_day_case") %>%
  mutate(sector_name=case_when(sector=="nhs_daycase_rate_percent_20212022" ~ "NHS",
                               sector=="private_daycase_rate_percent_20212022" ~ "Private",
                               TRUE ~ "NA")) %>%
  select(-c("sector"))
barbell_data_bottom_nhs <- barbell_data_bottom %>%  filter(sector_name=="NHS")
barbell_data_bottom_private <- barbell_data_bottom %>%  filter(sector_name=="Private")

barbell_bottom <-  ggplot(barbell_data_bottom) +
  geom_segment(data=barbell_data_bottom_nhs,
               aes(x = pct_day_case, y = reorder(procedure_group,-pct_day_case),
                   yend = barbell_data_bottom_private$procedure_group, xend = barbell_data_bottom_private$pct_day_case), #use the $ operator to fetch data from our "Females" tibble
               color = "#aeb6bf",
               size = 4.5, #Note that I sized the segment to fit the points
               alpha = .5) +
  geom_point(aes(x = pct_day_case, y = reorder(procedure_group,-pct_day_case), color = sector_name), size = 4, show.legend = TRUE)+
  theme_bw() +
  labs(title='Procedures the NHS does quicker', subtitle='2021/22, top 10 largest gap') +
  xlab("Percentage of day-cases") +
  ylab("Procedure") +
  scale_color_brewer(name="Sector",palette="Set1",direction=-1)
barbell_bottom

#Top
barbell_data_top <- static_spread_ranking %>%
  slice_max(spread_dayrate_20212022,n=10) %>%
  arrange(desc(spread_dayrate_20212022)) %>% 
  select(procedure_group,spread_dayrate_20212022,nhs_daycase_rate_percent_20212022,private_daycase_rate_percent_20212022) %>%
  pivot_longer(!c(procedure_group,spread_dayrate_20212022), names_to = "sector", values_to = "pct_day_case") %>%
  mutate(sector_name=case_when(sector=="nhs_daycase_rate_percent_20212022" ~ "NHS",
                               sector=="private_daycase_rate_percent_20212022" ~ "Private",
                               TRUE ~ "NA")) %>%
  select(-c("sector"))
barbell_data_top_nhs <- barbell_data_top %>%  filter(sector_name=="NHS")
barbell_data_top_private <- barbell_data_top %>%  filter(sector_name=="Private")

barbell_top <-  ggplot(barbell_data_top) +
  geom_segment(data=barbell_data_top_nhs,
               aes(x = pct_day_case, y = reorder(procedure_group,-pct_day_case),
                   yend = barbell_data_top_private$procedure_group, xend = barbell_data_top_private$pct_day_case), #use the $ operator to fetch data from our "Females" tibble
               color = "#aeb6bf",
               size = 4.5, #Note that I sized the segment to fit the points
               alpha = .5) +
  geom_point(aes(x = pct_day_case, y = reorder(procedure_group,-pct_day_case), color = sector_name), size = 4, show.legend = TRUE)+
  theme_bw() +
  labs(title='Procedures the private sector does quicker', subtitle='2021/22, top 10 largest gap') +
  xlab("Percentage of day-cases") +
  ylab("Procedure") +
  scale_color_brewer(name="Sector",palette="Set1",direction=-1)
barbell_top

#Selection of longitudinal
barbell_data_selection <- los_nation_proc %>%
  mutate(spread_dayrate=private_daycase_rate_percent-nhs_daycase_rate_percent,
         year=case_when(year=="20212022" ~ "2021/22",
                        year=="20202021" ~ "2020/21",
                        TRUE ~ "NA")) %>%
  mutate(procedure_year=paste(procedure_group,year,sep=" ")) %>%
  filter(procedure_group %in% c("Cataract surgery",
                                "Hip replacement (primary)",
                                "Knee replacement (primary)",
                                "Diagnostic upper GI endoscopy")) %>% 
  select(procedure_year,spread_dayrate,private_daycase_rate_percent,nhs_daycase_rate_percent) %>%
  pivot_longer(!c(procedure_year,spread_dayrate), names_to = "sector", values_to = "pct_day_case") %>%
  mutate(sector_name=case_when(sector=="nhs_daycase_rate_percent" ~ "NHS",
                               sector=="private_daycase_rate_percent" ~ "Private",
                               TRUE ~ "NA")) %>%
  select(-c("sector"))
barbell_data_selection_nhs <- barbell_data_selection %>%  filter(sector_name=="NHS")
barbell_data_selection_private <- barbell_data_selection %>%  filter(sector_name=="Private")
# diff_selection <- barbell_data_selection %>% 
#   filter(sector_name == "NHS") %>%
#   mutate(x_pos = as.numeric(pct_day_case)+as.numeric(spread_dayrate)/2)

barbell_selection <-  ggplot(barbell_data_selection) +
  geom_segment(data=barbell_data_selection_nhs,
               aes(x = pct_day_case, y = reorder(procedure_year,desc(procedure_year)),
                   yend = barbell_data_selection_private$procedure_year, xend = barbell_data_selection_private$pct_day_case), #use the $ operator to fetch data from our "Females" tibble
               color = "#aeb6bf",
               size = 4.5, #Note that I sized the segment to fit the points
               alpha = .5) +
  geom_point(aes(x = pct_day_case, y = reorder(procedure_year,desc(procedure_year)), color = sector_name), size = 4, show.legend = TRUE)+
  theme_bw() + scale_x_break(c(5, 95)) +
  labs(title='Change in day-rate', subtitle='2020/21 and 2021/22, selected procedures') +
  xlab("Percentage of day-cases") +
  ylab("Procedure/year") +
  scale_color_brewer(name="Sector",palette="Set1",direction=-1)
barbell_selection

#Chart 2: bubbles

static_spread_ranking_highlight <- static_spread_ranking %>%
  filter(procedure_group %in% c("Cataract surgery",
                                "Hip replacement (primary)",
                                "Knee replacement (primary)",
                                "Diagnostic upper GI endoscopy"))

# static_spread_ranking_highlight_bis <- static_spread_ranking %>%
#   filter(all_activity_inpatient_spells_20212022>8000)
# 
# bubble_chart <- static_spread_ranking %>%
#   ggplot(., aes(x=spread_dayrate_20212022, y=pct_private_20212022, size = all_activity_inpatient_spells_20212022)) +
#   geom_point(alpha=0.5, shape=21, color="black") +
#   geom_point(data=static_spread_ranking_highlight,
#              aes(x=spread_dayrate_20212022, y=pct_private_20212022,fill=procedure_group),alpha=0.8, shape=21) +
#   geom_label_repel(aes(label = procedure_group),
#                    nudge_x = 2,
#                    nudge_y = 1,
#                    na.rm = TRUE,size=3,alpha=0.7) +
#   scale_size(range = c(.1, 12), name="Total spells\n(NHS and private)") +
#   scale_fill_brewer(name="Procedures",palette="Dark2") +
#   xlim(c(-85,85)) +
#   theme_bw() +
#   labs(title='Complexity of NHS vs. private procedures', subtitle='2021/22') +
#   xlab("Spread in day-case rate\n(% day-cases private - % day-cases NHS)") +
#   ylab("Percent private") +
#   annotate("text", x = 45, y = 15, size = 3, label = "More day-cases in private →") +
#   annotate("text", x = -45, y = 15, size = 3, label = "← More day-cases in NHS")
# bubble_chart

bubble_chart_mini <- static_spread_ranking %>%
  filter(pct_private_20212022<50) %>% 
  ggplot(., aes(x=spread_dayrate_20212022, y=pct_private_20212022, size = all_activity_inpatient_spells_20212022)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  geom_point(data=static_spread_ranking_highlight,
             aes(x=spread_dayrate_20212022, y=pct_private_20212022,fill=procedure_group),alpha=0.8, shape=21) +
  geom_label_repel(aes(label = procedure_group),
                   nudge_x = 2,
                   nudge_y = 1,
                   na.rm = TRUE,size=3,alpha=0.7) +
  scale_size(range = c(.1, 12), name="Total spells\n(NHS and private)") +
  scale_fill_brewer(name="Procedures",palette="Dark2") +
  xlim(c(-85,85)) +
  theme_bw() +
  labs(title='Complexity of NHS vs. private procedures', subtitle='2021/22') +
  xlab("Spread in day-case rate\n(% day-cases private - % day-cases NHS)") +
  ylab("Percent private") +
  annotate("text", x = 45, y = 15, size = 3, label = "More day-cases in private →") +
  annotate("text", x = -45, y = 15, size = 3, label = "← More day-cases in NHS")
bubble_chart_mini