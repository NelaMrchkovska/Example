## Final Data Wrangling ##

gc()
rm(list=ls())
cat('\014')

require(data.table)
library(readxl)
library(vroom)
library(tidyverse)
library(ltm)
library(dplyr)
library(psych)
library(gt)        # for summary stat table
library(gtsummary) # for summary stat table
library(sandwich)
library(lmtest)
library(ggplot2)


# loading datasets to create one final dataset 

getwd()
df_control <- read.table('Final Sample Data/Final_Control.tsv', header=FALSE, skip=3, sep="\t", fileEncoding="UTF-16", quote = "")

q_names <- colnames(df_control) # first with header=TRE
q_names <- as.data.frame(q_names)
q_names1 <- colnames(df_control) # then with header=FALSE 
q_names1 <- as.data.frame(q_names1)
q_names <- cbind(q_names, q_names1) # save for reference


# subseting only complete surveys 

df_control <- df_control[df_control$V7=="True",]
# incorrect attention check questions 
head(df_control)
head(df_control$V18)
df_control <- df_control %>%
  mutate(quick_click=ifelse(V18>=1 & V18<=8 & V19>=1 & V19<=8,1,0))
df_control <- df_control %>%
  mutate(quick_click1=ifelse(V18>=1 & V18<=4,1,0))
table(df_control$quick_click1)
table(df_control$quick_click)


table(df_control$quick_click) #52 that went through the priming too impatiently  
df_check1 <- df_control[df_control$quick_click==1,] 
df_check1$reason1 <- "inattentiveness when reading the priming"
df_check1 <- df_check1 %>%
  select(V90, "reason1")

df_control <- df_control %>%
  mutate(a1_wrong=ifelse(V22=="Войната между Русия и Украйна е институционална война" |
                           V22=="Войната между Русия и Украйна е културно-религиозна война" |
                           V22=="Войната между Русия и Украйна е икономическа война", 1,0))


table(df_control$a1_wrong) #126 got a1 wrong 
df_check2 <- df_control[df_control$a1_wrong==1,] 
df_check2$reason2 <- "attention question 1 wrong"
df_check2 <- df_check2 %>%
  select(V90, "reason2")

df_control <- df_control %>%
  mutate(a2_wrong=ifelse(V23=="Не", 1,0))
table(df_control$a2_wrong) #80 got a2 wrong 
df_check3 <- df_control[df_control$a2_wrong==1,] 
df_check3$reason3 <- "attention question 2 wrong"
df_check3 <- df_check3 %>%
  select(V90, "reason3")

df_control <- df_control %>%
  mutate(quick_click_dv=ifelse(V25>=1 & V25<=3 & V26>=1 & V26<=3,1,0))
table(df_control$quick_click_dv) # 8 people clicked too fast on DV 
df_check4 <- df_control[df_control$quick_click_dv==1,] 
df_check4$reason4 <- "inattentiveness when answering the DV"
df_check4 <- df_check4 %>%
  select(V90, "reason4")

# wrong year 
df_check5 <- df_control[nchar(df_control$V79) < 4, ]
df_check5$reason5 <- "incorrect written value for year/age"
df_check5 <- df_check5 %>%
  select(V90, "reason5")

merged_df <- full_join(df_check1, df_check2, by = "V90") %>%
  full_join(df_check3, by = "V90") %>%
  full_join(df_check4, by = "V90") %>%
  full_join(df_check5, by = "V90")

subset_merged_df_control <- merged_df[rowSums(!is.na(merged_df[, c("reason1", "reason2", "reason3", "reason4", "reason5")])) >= 2, ]
subset_merged_df_control <- rename(subset_merged_df_control, "CintID"="V90") #402 quality responses 
list <- subset_merged_df_control$CintID
df_control_quality <- df_control[!df_control$V90 %in% list,]
df_control$group <- "control" #whole sample 
df_control_quality$group <- "control" #quality sample 


####******T1*****####

df_t1 <- read.table('Final Sample Data/Final_T1.tsv', header=FALSE, skip=3, sep="\t", fileEncoding="UTF-16", quote = "")
# to check which observations are missing due to incorrect read of lines 
df_t1 <- df_t1[df_t1$V7=="True",]


# incorrect attention check questions 
head(df_t1)
head(df_t1$V18)
df_t1 <- df_t1 %>%
  mutate(quick_click=ifelse(V18>=1 & V18<=8 & V19>=1 & V19<=8,1,0))
df_t1 <- df_t1 %>%
  mutate(quick_click1=ifelse(V18>=1 & V18<=5 & V19>=1 & V19<=5,1,0))

table(df_t1$quick_click) #38 that went through the priming too impatiently  
df_check1 <- df_t1[df_t1$quick_click==1,] 
df_check1$reason1 <- "inattentiveness when reading the priming"
df_check1 <- df_check1 %>%
  select(V90, "reason1")

df_t1 <- df_t1 %>%
  mutate(a1_wrong=ifelse(V22=="Войната между Русия и Украйна е институционална война" |
                           V22=="Войната между Русия и Украйна е война с исторически корени" |
                           V22=="Войната между Русия и Украйна е икономическа война", 1,0))


table(df_t1$a1_wrong) #106 got a1 wrong 
df_check2 <- df_t1[df_t1$a1_wrong==1,] 
df_check2$reason2 <- "attention question 1 wrong"
df_check2 <- df_check2 %>%
  select(V90, "reason2")

df_t1 <- df_t1 %>%
  mutate(a2_wrong=ifelse(V23=="Не", 1,0))
table(df_t1$a2_wrong) #43 got a2 wrong 
df_check3 <- df_t1[df_t1$a2_wrong==1,] 
df_check3$reason3 <- "attention question 2 wrong"
df_check3 <- df_check3 %>%
  select(V90, "reason3")

df_t1 <- df_t1 %>%
  mutate(quick_click_dv=ifelse(V25>=1 & V25<=3 & V26>=1 & V26<=3,1,0))
table(df_t1$quick_click_dv) # 5 people clicked too fast on DV 
df_check4 <- df_t1[df_t1$quick_click_dv==1,] 
df_check4$reason4 <- "inattentiveness when answering the DV"
df_check4 <- df_check4 %>%
  select(V90, "reason4")

# wrong year 
df_check5 <- df_t1[nchar(df_t1$V79) < 4, ]
df_check5$reason5 <- "incorrect written value for year/age"
df_check5 <- df_check5 %>%
  select(V90, "reason5")

merged_df <- full_join(df_check1, df_check2, by = "V90") %>%
  full_join(df_check3, by = "V90") %>%
  full_join(df_check4, by = "V90") %>%
  full_join(df_check5, by = "V90")

subset_merged_df_t1 <- merged_df[rowSums(!is.na(merged_df[, c("reason1", "reason2", "reason3", "reason4", "reason5")])) >= 2, ]
subset_merged_df_t1 <- rename(subset_merged_df_t1, "CintID"="V90")
list <- subset_merged_df_t1$CintID
df_t1_quality <- df_t1[!df_t1$V90 %in% list,] #403
df_t1_quality$group <- "t1"
df_t1$group <- "t1"

####******T2*****####

getwd()
df_t2 <- read.table('Final Sample Data/Final_T2.tsv', header=FALSE, skip=3, sep="\t", fileEncoding="UTF-16")
# to check which observations are missing due to incorrect read of lines 
#CintID_test <- read_excel("CintID_test.xlsx")
#list <- CintID_test$CintID
#list <- as.data.frame(list)
#values_not_in_list <- setdiff(list, df_t2$V90) # missing the first two rows of downloaded dataset  

df_t2 <- df_t2[df_t2$V7=="True",]

# incorrect attention check questions 
head(df_t2)
head(df_t2$V18)
df_t2 <- df_t2 %>%
  mutate(quick_click=ifelse(V18>=1 & V18<=8 & V19>=1 & V19<=8,1,0))
df_t2 <- df_t2 %>%
  mutate(quick_click1=ifelse(V18>=1 & V18<=5 & V19>=1 & V19<=5,1,0))

table(df_t2$quick_click) #39 that went through the priming too impatiently  
df_check1 <- df_t2[df_t2$quick_click==1,] 
df_check1$reason1 <- "inattentiveness when reading the priming"
df_check1 <- df_check1 %>%
  select(V90, "reason1")

df_t2 <- df_t2 %>%
  mutate(a1_wrong=ifelse(V22=="Войната между Русия и Украйна е институционална война" |
                           V22=="Войната между Русия и Украйна е война с исторически корени" |
                           V22=="Войната между Русия и Украйна е културно-религиозна война", 1,0))


table(df_t2$a1_wrong) #62 got a1 wrong 
df_check2 <- df_t2[df_t2$a1_wrong==1,] 
df_check2$reason2 <- "attention question 1 wrong"
df_check2 <- df_check2 %>%
  select(V90, "reason2")

df_t2 <- df_t2 %>%
  mutate(a2_wrong=ifelse(V23=="Не", 1,0))
table(df_t2$a2_wrong) #28 got a2 wrong 
df_check3 <- df_t2[df_t2$a2_wrong==1,] 
df_check3$reason3 <- "attention question 2 wrong"
df_check3 <- df_check3 %>%
  select(V90, "reason3")

df_t2 <- df_t2 %>%
  mutate(quick_click_dv=ifelse(V25>=1 & V25<=3 & V26>=1 & V26<=3,1,0))
table(df_t2$quick_click_dv) # 7 people clicked too fast on DV 
df_check4 <- df_t2[df_t2$quick_click_dv==1,] 
df_check4$reason4 <- "inattentiveness when answering the DV"
df_check4 <- df_check4 %>%
  select(V90, "reason4")

# wrong year 
df_check5 <- df_t2[nchar(df_t2$V79) < 4, ]
df_check5$reason5 <- "incorrect written value for year/age"
df_check5 <- df_check5 %>%
  select(V90, "reason5")

merged_df <- full_join(df_check1, df_check2, by = "V90") %>%
  full_join(df_check3, by = "V90") %>%
  full_join(df_check4, by = "V90") %>%
  full_join(df_check5, by = "V90")

subset_merged_df_t2 <- merged_df[rowSums(!is.na(merged_df[, c("reason1", "reason2", "reason3", "reason4", "reason5")])) >= 2, ]
subset_merged_df_t2 <- rename(subset_merged_df_t2, "CintID"="V90")
list <- subset_merged_df_t2$CintID
df_t2_quality <- df_t2[!df_t2$V90 %in% list,] #408
df_t2$group <- "t2"
df_t2_quality$group <- "t2"

####******T3****####

getwd()
df_t3 <- read.table('Final Sample Data/Final_T3.tsv', header=FALSE, skip=3, sep="\t", fileEncoding="UTF-16", quote = "")
df_t3 <- df_t3[df_t3$V7=="True",]


# incorrect attention check questions 
head(df_t3)
head(df_t3$V18)
df_t3 <- df_t3 %>%
  mutate(quick_click=ifelse(V18>=1 & V18<=8 & V19>=1 & V19<=8,1,0))
df_t3 <- df_t3 %>%
  mutate(quick_click1=ifelse(V18>=1 & V18<=5 & V19>=1 & V19<=5,1,0))

table(df_t3$quick_click) #47 that went through the priming too impatiently  
df_check1 <- df_t3[df_t3$quick_click==1,] 
df_check1$reason1 <- "inattentiveness when reading the priming"
df_check1 <- df_check1 %>%
  select(V90, "reason1")

df_t3 <- df_t3 %>%
  mutate(a1_wrong=ifelse(V22=="Войната между Русия и Украйна е война с исторически корени" |
                           V22=="Войната между Русия и Украйна е културно-религиозна война" |
                           V22=="Войната между Русия и Украйна е икономическа война", 1,0))


table(df_t3$a1_wrong) #136 got a1 wrong 
df_check2 <- df_t3[df_t3$a1_wrong==1,] 
df_check2$reason2 <- "attention question 1 wrong"
df_check2 <- df_check2 %>%
  select(V90, "reason2")

df_t3 <- df_t3 %>%
  mutate(a2_wrong=ifelse(V23=="Не", 1,0))
table(df_t3$a2_wrong) #48 got a2 wrong 
df_check3 <- df_t3[df_t3$a2_wrong==1,] 
df_check3$reason3 <- "attention question 2 wrong"
df_check3 <- df_check3 %>%
  select(V90, "reason3")

df_t3 <- df_t3 %>%
  mutate(quick_click_dv=ifelse(V25>=1 & V25<=3 & V26>=1 & V26<=3,1,0))
table(df_t3$quick_click_dv) # 9 people clicked too fast on DV 
df_check4 <- df_t3[df_t3$quick_click_dv==1,] 
df_check4$reason4 <- "inattentiveness when answering the DV"
df_check4 <- df_check4 %>%
  select(V90, "reason4")

# wrong year 
df_check5 <- df_t3[nchar(df_t3$V79) < 4, ]
df_check5$reason5 <- "incorrect written value for year/age"
df_check5 <- df_check5 %>%
  select(V90, "reason5")

merged_df <- full_join(df_check1, df_check2, by = "V90") %>%
  full_join(df_check3, by = "V90") %>%
  full_join(df_check4, by = "V90") %>%
  full_join(df_check5, by = "V90")

subset_merged_df_t3 <- merged_df[rowSums(!is.na(merged_df[, c("reason1", "reason2", "reason3", "reason4", "reason5")])) >= 2, ]
subset_merged_df_t3 <- rename(subset_merged_df_t3, "CintID"="V90") 
list <- subset_merged_df_t3$CintID
df_t3_quality <- df_t3[!df_t3$V90 %in% list,] #403
df_t3$group <- "t3"
df_t3_quality$group <- "t3"

#bind the whole sample together 

df_quality <- rbind(df_control_quality, df_t1_quality, df_t2_quality, df_t3_quality)
table(df_quality$group)
write.csv(df_quality, "Final Sample Data/df_quality.csv") #saved the final dataset (quality only) # but also, just load the whole sample and subset below 
df_all <- rbind(df_control, df_t1, df_t2, df_t3)
table(df_all$group)
write.csv(df_all, "Final Sample Data/df_all.csv") # saved the final dataset (quality and not)

#df_quality <- read_csv("Final Sample Data/df_quality.csv")
#df_all <- read_csv("Final Sample Data/df_all.csv")

## let's wrangle the data for summary stats 

# remove variables that I do not need and label variables 

q_names_labeled <- q_names[-c(1:8),]
q_names_labeled <- q_names_labeled[-c(2:13),]

q_names_labeled <- q_names_labeled %>%
  mutate(labels=case_when(q_names=="ResponseId" ~ "ID",
                          q_names=="Q4" ~ "att1",
                          q_names=="Q5" ~ "att2",
                          q_names=="Q6_1" ~ "dv",
                          q_names=="Q7_1" ~ "mech_west_blame",
                          q_names=="Q7_2" ~ "mech_anti_gment",
                          q_names=="Q7_3" ~ "mech_sover_ter",
                          q_names=="Q7_4" ~ "mech_opp_cost",
                          q_names=="Q7_5" ~ "mech_hum",
                          q_names=="Q8_1" ~ "stat1_nato_fear",
                          q_names=="Q8_2" ~ "stat2_EU_integ",
                          q_names=="Q8_3" ~ "stat3_EU_anti",
                          q_names=="Q8_4" ~ "stat4_elite_anti",
                          q_names=="Q8_5" ~ "stat5_popul",
                          q_names=="Q8_6" ~ "state6_anti_mig",
                          q_names=="Q8_7" ~ "state7_anti_terr",
                          q_names=="Q8_8" ~ "state8_anti_mor",
                          q_names=="Q9_1" ~ "trust_bul_gment",
                          q_names=="Q9_2" ~ "trust_EU",
                          q_names=="Q9_3" ~ "trust_NATO",
                          q_names=="Q9_4" ~ "trust_civil",
                          q_names=="Q9_5" ~ "trust_rel",
                          q_names=="Q9_6" ~ "trust_army",
                          q_names=="Q9_1" ~ "trust_bul_gment",
                          q_names=="Q28_1" ~ "identity_bul",
                          q_names=="Q28_2" ~ "identity_slavic",
                          q_names=="Q28_3" ~ "identity_euro",
                          q_names=="Q28_4" ~ "identity_world",
                          q_names=="Q28_6" ~ "identity_orthodox",
                          q_names=="Q28_5" ~ "identity_other_nat",
                          q_names=="Q28_5_TEXT" ~ "identity_other_nat_text",
                          q_names=="Q28_7" ~ "identity_other_orthodox",
                          q_names=="Q28_7_TEXT" ~ "identity_other_orthodox_text",
                          q_names=="Q10" ~ "news_hours",
                          q_names=="Q11" ~ "socialmedia_hours",
                          q_names=="Q12" ~ "war_hours",
                          q_names=="Q15" ~ "channel_choices",
                          q_names=="Q15_9_TEXT" ~ "channel_choices_other_text",
                          q_names=="Q13_1" ~ "putin_rating",
                          q_names=="Q14" ~ "russia_imp",
                          q_names=="Q16" ~ "russia_born",
                          q_names=="Q17" ~ "russia_speak",
                          q_names=="Q18" ~ "russia_educ",
                          q_names=="Q19" ~ "russia_live",
                          q_names=="Q20" ~ "gender",
                          q_names=="Q21" ~ "location",
                          q_names=="Q22" ~ "ideology",
                          q_names=="Q23" ~ "year_born",
                          q_names=="Q24" ~ "educ",
                          q_names=="Q25" ~ "income",
                          q_names=="Q26" ~ "income_worsen",
                          q_names=="Q27" ~ "emp",
                          q_names=="Q29" ~ "religious",
                          q_names=="Q30" ~ "religion_aff",
                          q_names=="Q30_9_TEXT" ~ "religion_aff_other_text",
                          q_names=="Q31" ~ "religion_imp",
                          q_names=="Q32" ~ "prayer_freq",
                          q_names=="Q33" ~ "attend_freq",
                          q_names=="CintID" ~ "CintID"))

q_names_labeled <- q_names_labeled[!is.na(q_names_labeled$labels),]
write.csv(q_names_labeled, "survey_labels.csv")

vars <- q_names_labeled$q_names1 #58 variables 
vars <- c(vars, "quick_click", "a1_wrong", "a2_wrong","quick_click_dv","group")
names <- q_names_labeled$labels  
names <- c(names, "quick_click", "a1_wrong", "a2_wrong","quick_click_dv","group")

df_quality_clean <- df_quality[, colnames(df_quality) %in% vars]
colnames(df_quality_clean) <- names # prepared quality dataset 

df_all_clean <- df_all[, colnames(df_all) %in% vars]
colnames(df_all_clean) <- names # prepared all dataset 

write.csv(df_quality_clean, "Final Sample Data/df_quality_clean.csv") #saved the final clean dataset (quality only)
write.csv(df_all_clean, "Final Sample Data/df_all_clean.csv") #saved the final clean dataset (quality only)

###############---------START HERE --------###########  load dataset above 

df_all_clean <- read_csv("Final Sample Data/df_all_clean.csv")
df_all_clean <- df_all_clean[,-1]
df <- df_all_clean #1836 all of them 

q_names_labeled <- read_csv("survey_labels.csv")
q_names_labeled <- q_names_labeled[,-1]

df <- df %>%
  mutate(control=ifelse(group=="control",1,0),
         rel_frame=ifelse(group=="t1",1,0),
         inst_frame=ifelse(group=="t3",1,0),
         econ_frame=ifelse(group=="t2",1,0))

#mutating variables  
str(df)

df <- df %>%
  mutate(mech_west_blame_num=case_when(mech_west_blame=="Изобщо неважни" ~ 0,
                                       mech_west_blame=="Малко важни" ~ 1,
                                       mech_west_blame=="Важни" ~ 2,
                                       mech_west_blame=="Много важни" ~ 3,
                                       mech_west_blame=="Изключително важни" ~ 4))

df <- df %>%
  mutate(mech_anti_gment_num=case_when(mech_anti_gment=="Изобщо неважни" ~ 0,
                                       mech_anti_gment=="Малко важни" ~ 1,
                                       mech_anti_gment=="Важни" ~ 2,
                                       mech_anti_gment=="Много важни" ~ 3,
                                       mech_anti_gment=="Изключително важни" ~ 4))


df <- df %>%
  mutate(mech_sover_ter_num=case_when(mech_sover_ter=="Изобщо неважни" ~ 0,
                                      mech_sover_ter=="Малко важни" ~ 1,
                                      mech_sover_ter=="Важни" ~ 2,
                                      mech_sover_ter=="Много важни" ~ 3,
                                      mech_sover_ter=="Изключително важни" ~ 4))


df <- df %>%
  mutate(mech_opp_cost_num=case_when(mech_opp_cost=="Изобщо неважни" ~ 0,
                                     mech_opp_cost=="Малко важни" ~ 1,
                                     mech_opp_cost=="Важни" ~ 2,
                                     mech_opp_cost=="Много важни" ~ 3,
                                     mech_opp_cost=="Изключително важни" ~ 4))
table(df$mech_opp_cost_num)

df <- df %>%
  mutate(mech_hum_num=case_when(mech_hum=="Изобщо неважни" ~ 0,
                                mech_hum=="Малко важни" ~ 1,
                                mech_hum=="Важни" ~ 2,
                                mech_hum=="Много важни" ~ 3,
                                mech_hum=="Изключително важни" ~ 4))



df <- df %>%
  mutate(stat1_nato_fear_num=case_when(stat1_nato_fear=="Категорично несъгласен/а" ~ 0,
                                       stat1_nato_fear=="Донякъде несъгласен/а" ~ 1,
                                       stat1_nato_fear=="Важни" ~ 2,
                                       stat1_nato_fear=="Неутрален– нито съгласен, нито несъгласен" ~ 3,
                                       stat1_nato_fear=="Донякъде съгласен/a" ~ 4,
                                       stat1_nato_fear=="Напълно съгласен/a" ~ 5,
                                       stat1_nato_fear=="Не знам/не мога да кажа" ~ NA))

df <- df %>%
  mutate(stat2_EU_integ_num=case_when(stat2_EU_integ=="Категорично несъгласен/а" ~ 0,
                                      stat2_EU_integ=="Донякъде несъгласен/а" ~ 1,
                                      stat2_EU_integ=="Важни" ~ 2,
                                      stat2_EU_integ=="Неутрален– нито съгласен, нито несъгласен" ~ 3,
                                      stat2_EU_integ=="Донякъде съгласен/a" ~ 4,
                                      stat2_EU_integ=="Напълно съгласен/a" ~ 5,
                                      stat2_EU_integ=="Не знам/не мога да кажа" ~ NA))

df <- df %>%
  mutate(stat3_EU_anti_num=case_when(stat3_EU_anti=="Категорично несъгласен/а" ~ 0,
                                     stat3_EU_anti=="Донякъде несъгласен/а" ~ 1,
                                     stat3_EU_anti=="Важни" ~ 2,
                                     stat3_EU_anti=="Неутрален– нито съгласен, нито несъгласен" ~ 3,
                                     stat3_EU_anti=="Донякъде съгласен/a" ~ 4,
                                     stat3_EU_anti=="Напълно съгласен/a" ~ 5,
                                     stat3_EU_anti=="Не знам/не мога да кажа" ~ NA))

df <- df %>%
  mutate(stat4_elite_anti_num=case_when(stat4_elite_anti=="Категорично несъгласен/а" ~ 0,
                                        stat4_elite_anti=="Донякъде несъгласен/а" ~ 1,
                                        stat4_elite_anti=="Важни" ~ 2,
                                        stat4_elite_anti=="Неутрален– нито съгласен, нито несъгласен" ~ 3,
                                        stat4_elite_anti=="Донякъде съгласен/a" ~ 4,
                                        stat4_elite_anti=="Напълно съгласен/a" ~ 5,
                                        stat4_elite_anti=="Не знам/не мога да кажа" ~ NA))

df <- df %>%
  mutate(stat5_popul_num=case_when(stat5_popul=="Категорично несъгласен/а" ~ 0,
                                   stat5_popul=="Донякъде несъгласен/а" ~ 1,
                                   stat5_popul=="Важни" ~ 2,
                                   stat5_popul=="Неутрален– нито съгласен, нито несъгласен" ~ 3,
                                   stat5_popul=="Донякъде съгласен/a" ~ 4,
                                   stat5_popul=="Напълно съгласен/a" ~ 5,
                                   stat5_popul=="Не знам/не мога да кажа" ~ NA))

df <- df %>%
  mutate(state6_anti_mig_num=case_when(state6_anti_mig=="Категорично несъгласен/а" ~ 0,
                                       state6_anti_mig=="Донякъде несъгласен/а" ~ 1,
                                       state6_anti_mig=="Важни" ~ 2,
                                       state6_anti_mig=="Неутрален– нито съгласен, нито несъгласен" ~ 3,
                                       state6_anti_mig=="Донякъде съгласен/a" ~ 4,
                                       state6_anti_mig=="Напълно съгласен/a" ~ 5,
                                       state6_anti_mig=="Не знам/не мога да кажа" ~ NA))

df <- df %>%
  mutate(state7_anti_terr_num=case_when(state7_anti_terr=="Категорично несъгласен/а" ~ 0,
                                        state7_anti_terr=="Донякъде несъгласен/а" ~ 1,
                                        state7_anti_terr=="Важни" ~ 2,
                                        state7_anti_terr=="Неутрален– нито съгласен, нито несъгласен" ~ 3,
                                        state7_anti_terr=="Донякъде съгласен/a" ~ 4,
                                        state7_anti_terr=="Напълно съгласен/a" ~ 5,
                                        state7_anti_terr=="Не знам/не мога да кажа" ~ NA))

df <- df %>%
  mutate(state8_anti_mor_num=case_when(state8_anti_mor=="Категорично несъгласен/а" ~ 0,
                                       state8_anti_mor=="Донякъде несъгласен/а" ~ 1,
                                       state8_anti_mor=="Важни" ~ 2,
                                       state8_anti_mor=="Неутрален– нито съгласен, нито несъгласен" ~ 3,
                                       state8_anti_mor=="Донякъде съгласен/a" ~ 4,
                                       state8_anti_mor=="Напълно съгласен/a" ~ 5,
                                       state8_anti_mor=="Не знам/не мога да кажа" ~ NA))


# wrangling a write-in variable where individuals could identify with other identity/religion not listed

unique(df$identity_other_nat_text)
df <- df %>%
  mutate(other_identity=case_when(identity_other_nat_text=="Гръцка" | identity_other_nat=="\"Грък \"" | identity_other_nat=="Грък" ~ "Greek",
                                  identity_other_nat_text=="Анериканец" | identity_other_nat_text=="Канадска" ~ "American",
                                  identity_other_nat_text=="Франция" | identity_other_nat_text=="англичанин" | identity_other_nat_text=="италианска" |
                                    identity_other_nat_text=="Германка" |  identity_other_nat_text=="Германец" | identity_other_nat_text=="Италянска" 
                                  | identity_other_nat_text=="ХОЛАНДСКА" | identity_other_nat_text=="\"Италия \"" | identity_other_nat_text=="Ангичанин"
                                  | identity_other_nat_text=="исландска" | identity_other_nat_text=="Италианска" | identity_other_nat_text=="Италианка" |
                                    identity_other_nat_text=="Швед" |
                                    identity_other_nat_text=="немска" | identity_other_nat_text=="Нидерландец" | identity_other_nat_text=="Австриец "
                                  | identity_other_nat_text=="Италиянец" | identity_other_nat_text=="Германец " | identity_other_nat_text=="Испанец" |
                                    identity_other_nat_text=="австриец" | identity_other_nat_text=="Ирландец" | identity_other_nat_text=="Немец"
                                  | identity_other_nat_text=="germany" | identity_other_nat_text=="Финландец" | identity_other_nat_text=="Германия"
                                  | identity_other_nat_text=="Чех" | identity_other_nat_text=="Норвежец" | identity_other_nat_text=="\"англичанин \""~ "Other European",
                                  identity_other_nat_text=="Китаец" | identity_other_nat_text=="азиатки" | identity_other_nat_text=="Кореец" ~ "Asian",
                                  identity_other_nat_text=="арабин" ~ "Arab",
                                  identity_other_nat_text=="\"American \"" | identity_other_nat_text=="сащ" | identity_other_nat_text=="\"бразилец \""
                                  | identity_other_nat_text=="Американец" | identity_other_nat_text=="Канадец" | identity_other_nat_text=="Америка"
                                  | identity_other_nat_text=="\"Американец \"" | identity_other_nat_text=="американец"~ "American",
                                  identity_other_nat_text=="Турска" | identity_other_nat_text=="туркиня" | 
                                  identity_other_nat_text=="турчин, ром" | identity_other_nat_text=="Турци" | identity_other_nat_text=="македонец" |
                                    identity_other_nat_text=="Турчин" | identity_other_nat_text=="турчин" | identity_other_nat_text=="Turchin" 
                                  | identity_other_nat_text=="turska" | identity_other_nat_text=="turk"~ "Turkish", 
                                  identity_other_nat_text=="Ромска" |identity_other_nat_text=="циганин"  ~ "Roma",
                                  identity_other_nat_text=="балканец" | identity_other_nat_text=="Сърбин"| identity_other_nat_text=="Сърбин" | 
                                  identity_other_nat_text=="Македонец" | identity_other_nat_text=="balkan" ~ "Balkan",
                                  identity_other_nat_text=="Руснак" | identity_other_nat_text=="Руска" | identity_other_nat_text=="rusnak" 
                                  | identity_other_nat_text=="Русия" | identity_other_nat_text=="русский" ~ "Russian",
                                  identity_other_nat_text=="\"Afghanistan \"" | identity_other_nat_text=="\" арменец\"" ~ "Middle Eastern",
                                  identity_other_nat_text=="Украинец " | identity_other_nat_text=="украинец"  ~ "Ukranian"
                                  ))

table(df$other_identity) #11 other nationalities 

unique(df$identity_other_orthodox_text)
df <- df %>%
  mutate(other_identity_red=case_when(identity_other_orthodox_text=="Мюсюлманка" | identity_other_orthodox_text=="мюсюлманин" |
                                        identity_other_orthodox_text=="Ислям" | identity_other_orthodox_text=="\"Мюсюлманите \"" |
                                        identity_other_orthodox_text=="\"Ислям \"" | identity_other_orthodox_text=="Мюсюлманска" |
                                        identity_other_orthodox_text=="Мюсюлманин" | identity_other_orthodox_text=="мюсюлманка" |
                                        identity_other_orthodox_text=="\"Мюсюлманството \"" | identity_other_orthodox_text=="\"Мюсюлмани \"" |
                                        identity_other_orthodox_text=="Muslimanin" | identity_other_orthodox_text=="\"Мусульмане \"" |
                                        identity_other_orthodox_text=="Islam" | identity_other_orthodox_text=="Мюсулманин" | 
                                        identity_other_orthodox_text=="Ислям " | identity_other_orthodox_text=="мюсулманин"|
                                        identity_other_orthodox_text=="Ислямист" | identity_other_orthodox_text=="ислам"|
                                        identity_other_orthodox_text=="\"Мюсилман \"" | identity_other_orthodox_text=="мюсюлмани"|
                                        identity_other_orthodox_text=="Мюсюлманство" | identity_other_orthodox_text=="\"Мюсюлманка \""|
                                        identity_other_orthodox_text=="musilmani" ~ "Muslim"))

table(df$other_identity_red)


nat <- df %>%
  pivot_wider(id_cols=ID, names_from="other_identity", values_from="identity_other_nat", names_prefix = "identity_")

rel <- df %>%
  pivot_wider(id_cols=ID, names_from="other_identity_red", values_from="identity_other_orthodox", names_prefix = "rel_identity_")

natrel <- full_join(nat, rel, by="ID") 
natrel <- natrel %>%
  select(-c("identity_NA", "rel_identity_NA"))

df <- full_join(df, natrel, by="ID")

# weekly news consumption 

table(df$news_hours) 
df <- df %>%
  mutate(news_hours_num=case_when(news_hours=="0-1 часа" ~ 0,
                                  news_hours=="2-4 часа" ~ 1,
                                  news_hours=="4-6 часа" ~ 2,
                                  news_hours=="6-8 часа" ~ 3,
                                  news_hours=="Повече от  8 часа" ~ 4))

table(df$socialmedia_hours) 
df <- df %>%
  mutate(socialmedia_hours_cat=case_when(socialmedia_hours=="Никаква" |  socialmedia_hours=="По-малко от половината" ~ "Low Social Media Consump.",
                                         socialmedia_hours=="Около половината" |  socialmedia_hours=="Повече от половината" ~ "High Social Media Consump."))


table(df$war_hours) 
df <- df %>%
  mutate(war_hours_cat=case_when(war_hours=="Никаква" |  war_hours=="По-малко от половината" ~ "Low War Info Consump.",
                                 war_hours=="Около половината" |  war_hours=="Повече от половината" ~ "High War Info Consump."))

table(df$war_hours_cat) 


# importance of russia 
df <- df %>%
  mutate(russia_imp_num=case_when(russia_imp=="Категорично несъгласен/a" ~ 0,
                                  russia_imp=="Донякъде несъгласен/a" ~ 1,
                                  russia_imp=="Неутрален– нито съгласен/a, нито несъгласен/a" ~ 2,
                                  russia_imp=="Донякъде съгласен/a" ~ 3,
                                  russia_imp=="Напълно съгласен/a" ~ 4,
                                  russia_imp=="Не знам/не мога да кажа" ~ NA))

table(df$russia_imp_num)

table(df$russia_born)
df <- df %>%
  mutate(russia_born_cat=case_when(russia_born=="Да" ~ "1",
                                   russia_born=="Не" ~ "0",
                                   russia_born=="Не съм сигурен/a" ~ NA))

table(df$russia_speak)
df <- df %>%
  mutate(russia_speak_cat=case_when(russia_speak=="Да" ~ "1",
                                    russia_speak=="Не" ~ "0",
                                    russia_speak=="Аз не, не съм сигурен/a за близките ми" ~ NA))


table(df$russia_educ)
df <- df %>%
  mutate(russia_educ_cat=case_when(russia_educ=="Да" ~ "1",
                                   russia_educ=="Не" ~ "0",
                                   russia_educ=="Не съм сигурен/a" ~ NA))

table(df$russia_live)
df$russia_live[df$russia_live=="Членове на семейството,Не" | df$russia_live=="Приятели,Не" |
                 df$russia_live=="Близки роднини,Не" ] <- NA
df <- df %>%
  mutate(russia_live_cat=ifelse(russia_live=="Не", "0", "1"))

table(df$russia_live_cat)

table(df$gender)
df <- df %>%
  mutate(gender_cat=case_when(gender=="Женски" ~ "1",
                              gender=="Мъжки" ~ "0",
                              gender=="Нито едното/нито другото (самоопределям се по друг начин)" ~ NA))

table(df$location)
table(df$ideology)

df <- df %>%
  mutate(ideology_num=case_when(ideology=="Много консервативен" ~ 1,
                                ideology=="Консервативен" ~ 2,
                                ideology=="Донякъде консервативен" ~ 3,
                                ideology=="По средата - умерено либерален, умерено консервативен" ~ 4,
                                ideology=="Донякъде либерален" ~ 5,
                                ideology=="Либерален" ~ 6,
                                ideology=="Много либерален" ~ 7,
                                ideology=="Не съм мислил за това" ~ 0)) # should consider multiple imputation because this is kinda wrong  

summary(df$year_born)
df$year_born <- as.numeric(df$year_born)
for (i in 1:nrow(df)) {
  year_born <- df$year_born[i]
  
  # Check condition and assign NA to age
  if (is.na(year_born) || nchar(as.character(year_born)) < 4 ||nchar(as.character(year_born)) > 4 ) {
    df$year_born[i] <- NA
  }
}


for (i in 1:nrow(df)) {
  birth_year <- df$year_born[i]
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  age <- current_year - birth_year
  df$age[i] <- age
}

table(df$age)

df$age_1 <- NA  # Create an empty column for age_1

for (i in 1:nrow(df)) {
  if (!is.na(df$age[i]) & df$age[i] >= 18 & df$age[i] <= 65) {
    df$age_1[i] <- df$age[i]
  } else {
    df$age_1[i] <- NA
  }
}

table(df$age) # for regressions 
table(df$age_1) # for stats 
summary(df$age_1) 
summary(df$age)


table(df$educ)

df <- df %>%
  mutate(educ_cat=ifelse(educ=="Основно" | educ=="Средно", "0","1"))

table(df$educ_cat)

table(df$income)
df <- df %>%
  mutate(income_cat=case_when(income=="до 1000 лв"  | income=="между 1000 и 2000 лв" | income=="между 2000 и 3000 лв" ~ "below county's average",
                              income=="между 3000 и 4000 лв"  | income=="между 4000 и 5000 лв" | 
                                income=="между 5000 и 6000 лв"  | income=="повече от 6000 лв" ~ "above county's average"))
table(df$income_cat)

table(df$income_worsen)
df <- df %>%
  mutate(income_worsen_cat=case_when(income_worsen=="Стана много по-зле"  | income_worsen=="Малко се влоши" ~ "income worsened",
                                     income_worsen=="Малко по-добро"  | income_worsen=="Много по-добро"  ~ "income improved",
                                     income_worsen=="Почти без промяна" ~ "stayed the same"))


df <- df %>%
  mutate(income_worsen_num=case_when(income_worsen=="Стана много по-зле" ~ -2, 
                                     income_worsen=="Малко се влоши" ~ -1 ,
                                     income_worsen=="Малко по-добро"  ~ 1,
                                     income_worsen=="Много по-добро" ~ 2,
                                     income_worsen=="Почти без промяна" ~ 0))

summary(df$income_worsen_num)

table(df$emp)

table(df$religious)
table(df$religion_aff)

# only 22 other religions;not considering sample 
unique(df$religion_aff_other_text)

table(df$religion_imp)
df <- df %>%
  mutate(religion_imp_num=case_when(religion_imp=="Изобщо не е важна" ~ 0, 
                                    religion_imp=="Донякъде важна" ~ 1 ,
                                    religion_imp=="Малко важна"  ~ 2,
                                    religion_imp=="Изключително важна" ~ 3))


table(df$prayer_freq)
df <- df %>%
  mutate(prayer_freq_num=case_when(prayer_freq=="Никога" ~ 0, 
                                   prayer_freq=="Не много често" ~ 1 ,
                                   prayer_freq=="Само когато посещавам църква/ джамия / храм"  ~ 2,
                                   prayer_freq=="Веднъж дневно (всяка сутрин или вечер)" ~ 3,
                                   prayer_freq=="Няколко пъти на ден" ~ 4))

table(df$attend_freq)
df <- df %>%
  mutate(attend_freq_num=case_when(attend_freq=="Никога" ~ 0, 
                                   attend_freq=="По-малко от веднъж годишно" ~ 1 ,
                                   attend_freq=="Само за важни религиозни празници в годината"  ~ 2,
                                   attend_freq=="Няколко пъти в годината" ~ 3,
                                   attend_freq=="Няколко пъти в месеца" ~ 4,
                                   attend_freq=="Веднъж седмично" ~ 5,
                                   attend_freq=="Повече от веднъж седмично" ~ 6))
table(df$attend_freq_num)
str(df)

df <- df %>%
  mutate(inattentive = ifelse(rowSums(select(., quick_click, a1_wrong, a2_wrong, quick_click_dv)) >= 2, 1, 0))

table(df$inattentive) #1629 attentive ones 

data <- df # switcing to data but everything is the same 

# let's create indices 

### economic vulnerability index ###

table(data$income_cat) #higher values indicate higher income 


table(data$income_worsen_num) #higher values indicate more secure income 
data$income_worsen_num <- as.numeric(data$income_worsen_num)

table(data$emp) #higher values indicate more secure employment 
data <- data %>%
  mutate(emp_cat=ifelse(emp=="Безработен" | emp=="Търся работа" | emp=="Пенсионер" | emp=="Служител на непълен работен ден" | emp=="Друго", 0, 1))
data$emp_cat <- as.numeric(data$emp_cat) #0-1

table(data$educ_cat) #0-1 #higher values mean higher educational attainment 
data$educ_cat <- as.numeric(data$educ_cat)

table(data$mech_opp_cost) # higher values indicate more fiscal conservative, so I need to reverse  
table(data$mech_opp_cost_num) 

### religiosity index ###

names(data)
# items: prayer_freq_num, religion_imp_num, attend_freq_num
table(data$prayer_freq)
table(data$prayer_freq_num) #higher values, higher religiosity 
data$scaled_prayer <- scale(data$prayer_freq_num)

table(data$religion_imp)
table(data$religion_imp_num) #higher values, higher religiosity 
data$scaled_imp <- scale(data$religion_imp_num)

table(data$attend_freq)
table(data$attend_freq_num) #higher values, higher religiosity 
data$scaled_attend <- scale(data$attend_freq_num)

data_rel <- data[data$religious=="Да" | data$religious=="Не, но вярвам в Бог", ] #among religious, cronbach is about the same 
cor_items <- data %>%
  dplyr::select(scaled_attend,scaled_imp, scaled_prayer) %>% 
  na.omit()
cronbach.alpha(cor_items) #0.72 which is medium good

# so, let's construct religious index 
data <- data %>%
  mutate(rel_index = rowSums(dplyr::select(., scaled_attend,scaled_imp, scaled_prayer), na.rm = TRUE))
summary(data$rel_index)
head(data$rel_index)

na_count <- sum(is.na(data$rel_index))

# factor analysis for religious index
data_factor <- data %>%
  dplyr::select(c(scaled_attend,scaled_imp, scaled_prayer)) %>%
  na.omit()
KMO(data_factor)
cortest.bartlett(data_factor)
ev <- eigen(cor(data_factor)) # get eigenvalues
ev$values
scree(data_factor, pc=FALSE)
Nfacs <- 1  
fit <- factanal(data_factor, Nfacs, rotation="promax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)
alpha(data_factor) #0.76


## populist index 
attach(data)
table(mech_anti_gment)
table(mech_anti_gment_num) #higher values indicate higher anti-gment sentiment
data$mech_anti_gment_scaled <- scale(data$mech_anti_gment_num)
table(stat3_EU_anti)
table(stat3_EU_anti_num) #higher values indicate higher anti-EU sentiment 
data$stat3_EU_anti_scaled <- scale(data$stat3_EU_anti_num)
table(stat4_elite_anti)
table(stat4_elite_anti_num) #higher values indicate higher anti-elite sentiment
data$stat4_elite_anti_scaled <- scale(data$stat4_elite_anti_num)
table(stat5_popul) 
table(stat5_popul_num) #higher values indicate higher populist sentiment
data$stat5_popul_scaled <- scale(data$stat5_popul_num)
table(state6_anti_mig)
table(state6_anti_mig_num) #higher values indicate higher concern with migration
data$state6_anti_mig_scaled <- scale(data$state6_anti_mig_num)
table(state7_anti_terr)
table(state7_anti_terr_num) #higher values indicate higher concern with terrorism
data$state7_anti_terr_scaled <- scale(data$state7_anti_terr_num)
table(state8_anti_mor)
table(state8_anti_mor_num) #higher values indicate higher concern with moral decline
data$state8_anti_mor_scaled <- scale(data$state8_anti_mor_num)
table(trust_bul_gment) #higher values means higher concern so I need to reverse
data$trust_bul_gment <- as.numeric(data$trust_bul_gment)
data$trust_bul_gment_rev <- 8 - data$trust_bul_gment
table(data$trust_bul_gment_rev) # use this one for the index 
data$trust_bul_gment_rev_scaled <- scale(data$trust_bul_gment_rev)

#items: trust_bul_gment_rev_scaled, state8_anti_mor_scaled, state7_anti_terr_scaled, state6_anti_mig_scaled, stat5_popul_scaled, 
## stat4_elite_anti_scaled, stat3_EU_anti_scaled, mech_anti_gment_scaled


cor_items <- data %>%
  dplyr::select(trust_bul_gment_rev_scaled, state8_anti_mor_scaled, state7_anti_terr_scaled, state6_anti_mig_scaled, stat5_popul_scaled,
                mech_anti_gment_scaled) %>% 
  na.omit()
cronbach.alpha(cor_items) #0.76

# also factor analysis 

data_factor <- data %>%
  dplyr::select(c(trust_bul_gment_rev_scaled, state8_anti_mor_scaled, state7_anti_terr_scaled, state6_anti_mig_scaled, stat5_popul_scaled,
                  mech_anti_gment_scaled)) %>%
  na.omit()
KMO(data_factor)
cortest.bartlett(data_factor)
ev <- eigen(cor(data_factor)) # get eigenvalues
ev$values
scree(data_factor, pc=FALSE)
Nfacs <- 1  
fit <- factanal(data_factor, Nfacs, rotation="promax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)
fa.parallel(data_factor, fa="fa") #2 factor 
alpha(data_factor) #0.76

loads <- fit$loadings
fa.diagram(loads)

data <- data %>%
  mutate(populist_index = rowSums(dplyr::select(., trust_bul_gment_rev_scaled, state8_anti_mor_scaled, state7_anti_terr_scaled, state6_anti_mig_scaled, stat5_popul_scaled,
                                                mech_anti_gment_scaled), na.rm = TRUE))
summary(data$populist_index)
head(data$populist_index)

## let's check Russophilia index 
table(putin_rating) 
data$putin_rating <- as.numeric(data$putin_rating) #higher values indicate higher rating of Putin 
data$putin_rating_scaled <- scale(data$putin_rating)
table(russia_imp_num)
table(russia_imp)
data$russia_imp_num <- as.numeric(data$russia_imp_num) #higher values indicate higher cultural importance of Russia 
data$russia_imp_scaled <- scale(data$russia_imp_num)
summary(data$russia_imp_scaled)
table(russia_born_cat)
data$russia_born_cat <- as.numeric(as.character(data$russia_born_cat))
table(russia_educ_cat)
data$russia_educ_cat <- as.numeric(as.character(data$russia_educ_cat))
table(russia_speak_cat)
data$russia_speak_cat <- as.numeric(as.character(data$russia_speak_cat))
table(russia_live_cat)
data$russia_live_cat <- as.numeric(as.character(data$russia_live_cat))

# items: putin_rating_scaled, russia_imp_scaled, russia_born_cat, russia_educ_cat, russia_speak_cat, russia_live_cat 

data_factor <- data %>%
  dplyr::select(c(putin_rating_scaled, russia_imp_scaled, russia_born_cat, russia_educ_cat, russia_live_cat)) %>%
  na.omit()
KMO(data_factor) #mediocre 
cortest.bartlett(data_factor) #significant 
ev <- eigen(cor(data_factor)) # get eigenvalues
ev$values # 2 factors 
scree(data_factor, pc=FALSE) #1 factor 
fa.parallel(data_factor, fa="fa") #2 factor 
Nfacs <- 2  
fit <- factanal(data_factor, Nfacs, rotation="varimax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)
alpha(data_factor) #0.51

loads <- fit$loadings
fa.diagram(loads)

cor_items <- data %>%
  dplyr::select(putin_rating_scaled, russia_imp_scaled) %>% 
  na.omit()
cronbach.alpha(cor_items) #0.703 #good enough 

cor_items <- data %>%
  dplyr::select(russia_born_cat, russia_educ_cat, russia_live_cat) %>% 
  na.omit()
cronbach.alpha(cor_items) #0.702 # good enough 


# based on this, creating two indices 

#Russophilia

data <- data %>%
  mutate(russophilia_index = rowSums(dplyr::select(., putin_rating_scaled, russia_imp_scaled), na.rm = TRUE))
summary(data$russophilia_index)
head(data$russophilia_index)

# Russo-connections 

data <- data %>%
  mutate(russoconnect_index = rowSums(dplyr::select(., russia_born_cat, russia_educ_cat, russia_live_cat), na.rm = TRUE))
summary(data$russoconnect_index)
data$russoconnect_index_scaled <- scale(data$russoconnect_index)
head(data$russoconnect_index)


## let's check EU-skepticism index 


# items: mech_west_blame, stat2_EU_integ, stat3_EU_anti, stat4_elite_anti, trust_EU, trust_NATO 
table(data$mech_west_blame_num) #higher values mean more salience in their decision, so more blame to West 
data$mech_west_blame_num <- as.numeric(data$mech_west_blame_num)
summary(data$mech_west_blame_num)
data$mech_west_blame_scaled <- scale(data$mech_west_blame_num) 
table(stat2_EU_integ_num) #higher values mean more dissatisfation with EU intergation, so more EU blame 
data$stat2_EU_integ_num <- as.numeric(data$stat2_EU_integ_num)
summary(data$stat2_EU_integ_num)
data$stat2_EU_integ_scaled <- scale(data$stat2_EU_integ_num) 
table(stat3_EU_anti_num) #higher values mean more dissatisfation with EU leaders, so more EU blame 
data$stat3_EU_anti_num <- as.numeric(data$stat3_EU_anti_num)
summary(data$stat3_EU_anti_num)
data$stat3_EU_anti_scaled <- scale(data$stat3_EU_anti_num) 
table(stat4_elite_anti_num) #higher values mean more belief in  EU elitism, so more EU blame 
data$stat4_elite_anti_num <- as.numeric(data$stat4_elite_anti_num)
summary(data$stat4_elite_anti_num)
data$stat4_elite_anti_scaled <- scale(data$stat4_elite_anti_num) 
table(trust_EU) #higher values means higher trust so I need to reverse
data$trust_EU <- as.numeric(data$trust_EU)
data$trust_EU_rev <- 8 - data$trust_EU
table(data$trust_EU_rev) #higher values means lower trust now
data$trust_EU_rev_scaled <- scale(data$trust_EU_rev)
table(trust_NATO) #higher values means higher trust so I need to reverse
data$trust_NATO <- as.numeric(data$trust_NATO)
data$trust_NATO_rev <- 8 - data$trust_NATO
table(data$trust_NATO_rev) #higher values means lower trust now
data$trust_NATO_rev_scaled <- scale(data$trust_NATO_rev)

# items: mech_west_blame_scaled, stat2_EU_integ_scaled, stat3_EU_anti_scaled, stat4_elite_anti_scaled, trust_EU_rev_scaled, trust_NATO_rev_scaled

data_factor <- data %>%
  dplyr::select(c(mech_west_blame_scaled, stat2_EU_integ_scaled, stat3_EU_anti_scaled, stat4_elite_anti_scaled, trust_EU_rev_scaled, trust_NATO_rev_scaled)) %>%
  na.omit()
KMO(data_factor) #meritorious
cortest.bartlett(data_factor) #significant 
ev <- eigen(cor(data_factor)) # get eigenvalues
ev$values # 1 factor 
scree(data_factor, pc=FALSE) #1 factor 
fa.parallel(data_factor, fa="fa") #1 factor 
Nfacs <- 1  
fit <- factanal(data_factor, Nfacs, rotation="varimax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)
alpha(data_factor) #0.88

loads <- fit$loadings
fa.diagram(loads)

data <- data %>%
  mutate(euskeptic_index = rowSums(dplyr::select(., mech_west_blame_scaled, stat2_EU_integ_scaled, stat3_EU_anti_scaled, stat4_elite_anti_scaled, trust_EU_rev_scaled, trust_NATO_rev_scaled), na.rm = TRUE))
summary(data$euskeptic_index)
head(data$euskeptic_index)



# now we want stat summary for variables of interest; 

#cat items: gender_cat, location,educ_cat, income_cat, emp_cat, religious, religion_aff, war_hours_cat, socialmedia_hours_cat
#numeric items: ideology_num, populist_index, rel_index, age, news_hours_num 


# quick change to income: 
table(data$income)
data <- data %>%
  mutate(income_num=case_when(income=="до 1000 лв" ~ 0,
                              income=="между 1000 и 2000 лв" ~ 1,
                              income=="между 2000 и 3000 лв" ~ 2,
                              income=="между 3000 и 4000 лв" ~ 3,
                              income=="между 4000 и 5000 лв" ~ 4,
                              income=="между 5000 и 6000 лв" ~ 5,
                              income=="повече от 6000 лв" ~ 6))

table(data$income_num)

table(data$ideology)
data <- data %>%
  mutate(ideology_cat=case_when(ideology=="Много консервативен" | ideology=="Консервативен" | ideology=="Донякъде консервативен" ~ "conservative",
                                ideology=="Много либерален" | ideology=="Либерален" | ideology=="Донякъде либерален" ~ "liberal",
                                ideology=="По средата - умерено либерален, умерено консервативен" ~ "center",
                                ideology=="Не съм мислил за това" ~ "not political"))

#data$ideology_scaled <- scale(data$ideology_num) #standardizing numerical variables 
data$news_hours_scaled <- scale(data$news_hours_num)
data$income_scaled <- scale(data$income_num)


data <- data %>%
  mutate(group_num=case_when(group=="control" ~ 0,
                             group=="t1" ~ 1,
                             group=="t2" ~ 2,
                             group=="t3" ~ 3))


# separate data into non-quality and quality 

str(data)

data_qual <- data[!data$inattentive==1,] #1629 
table(data_qual$group) # data is named the quality set, df is the whole sample 

write.csv(data_qual, "Final Sample Data/df_quality_final.csv")
write.csv(data, "Final Sample Data/df_all_final.csv")















