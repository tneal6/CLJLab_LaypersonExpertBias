library(readxl)
library(tidyverse)
library(dplyr)
library(data.table)
library(readr)
library(lmerTest)
library(sjPlot)
library(MuMIn)
library(corrplot)
library(brms)
require(rstan)
library(sjlabelled)
library(sjmisc)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
## First, import the data (Percepts of Expert Bias Datset_Functional Dataset - CSV)
## Then, name the datafile df
df <- Percepts_of_Expert_Bias_Dataset_Functional_Dataset

#Added a subject number column
df <- tibble::rowid_to_column(df, "Subject")

#Created a list of all of the column names so we could see the original problematic names in order to change them
varnames <- colnames(df)

#Uses the data.table package function setNames to change all of the original problematic column names into 
#better, more useable variable names
df1 <- setNames(df, c("Subject", "Progress", "Duration", "Pilot_Stability", "Pilot_Clarity", "Pilot_Accuracy", "Pilot_Disagree", "Pilot_Like", 
                      "Pilot_Training", "Pilot_Contact", "Pilot_Dogmatic", "Pilot_Discretion", 
                      "Pilot_CognitiveBias", "Pilot_MotivatedBias", "Pilot_BiasMitigating", "Judge_Stability", 
                      "Judge_Clarity", "Judge_Accuracy","Judge_Disagree","Judge_Like", "Judge_Training",
                      "Judge_Contact", "Judge_Dogmatic","Judge_Discretion", "Judge_CognitiveBias",
                      "Judge_MotivatedBias", "Judge_BiasMitigating", "Broker_Stability", "Broker_Clarity",
                      "Broker_Accuracy","Broker_Disagree","Broker_Like","Broker_Training", "Broker_Contact",
                      "Broker_Dogmatic", "Broker_Discretion", "Broker_CognitiveBias", 
                      "Broker_MotivatedBias", "Broker_BiasMitigating", "Chess_Stability", "Chess_Clarity",
                      "Chess_Accuracy","Chess_Disagree","Chess_Like", "Chess_Training",
                      "Chess_Contact", "Chess_Dogmatic", "Chess_Discretion", "Chess_CognitiveBias", 
                      "Chess_MotivatedBias", "Chess_BiasMitigating", "Firefighter_Stability", "Firefighter_Clarity",
                      "Firefighter_Accuracy","Firefighter_Disagree","Firefighter_Like", "Firefighter_Training", 
                      "Firefighter_Contact", "Firefighter_Dogmatic", "Firefighter_Discretion", "Firefighter_CognitiveBias", 
                      "Firefighter_MotivatedBias", "Firefighter_BiasMitigating", "Physicist_Stability", "Physicist_Clarity",
                      "Physicist_Accuracy","Physicist_Disagree","Physicist_Like","Physicist_Training", 
                      "Physicist_Contact", "Physicist_Dogmatic", "Physicist_Discretion", "Physicist_CognitiveBias", 
                      "Physicist_MotivatedBias", "Physicist_BiasMitigating", "Tax_Assessor_Stability", "Tax_Assessor_Clarity",
                      "Tax_Assessor_Accuracy", "Tax_Assessor_Disagree","Tax_Assessor_Like", "Tax_Assessor_Training", 
                      "Tax_Assessor_Contact", "Tax_Assessor_Dogmatic",
                      "Tax_Assessor_Discretion", "Tax_Assessor_CognitiveBias", 
                      "Tax_Assessor_MotivatedBias", "Tax_Assessor_BiasMitigating", 
                      "Nurse_Stability", "Nurse_Clarity", 
                      "Nurse_Accuracy","Nurse_Disagree", 
                      "Nurse_Like", 
                      "Nurse_Training", 
                      "Nurse_Contact", 
                      "Nurse_Dogmatic", 
                      "Nurse_Discretion", 
                      "Nurse_CognitiveBias", 
                      "Nurse_MotivatedBias", 
                      "Nurse_BiasMitigating", 
                      "CIA_Stability", 
                      "CIA_Clarity",
                      "CIA_Accuracy", 
                      "CIA_Disagree",
                      "CIA_Like", 
                      "CIA_Training",
                      "CIA_Contact", 
                      "CIA_Dogmatic", 
                      "CIA_Discretion", 
                      "CIA_CognitiveBias", 
                      "CIA_MotivatedBias", 
                      "CIA_BiasMitigating", 
                      "Surgeon_Stability", 
                      "Surgeon_Clarity", 
                      "Surgeon_Accuracy", 
                      "Surgeon_Disagree", 
                      "Surgeon_Like", 
                      "Surgeon_Training", 
                      "Surgeon_Contact",
                      "Surgeon_Dogmatic",   
                      "Surgeon_Discretion", 
                      "Surgeon_CognitiveBias",
                      "Surgeon_MotivatedBias", 
                      "Surgeon_BiasMitigating", 
                      "Doctor_Stability",    
                      "Doctor_Clarity", 
                      "Doctor_Accuracy", 
                      "Doctor_Disagree", 
                      "Doctor_Like",  
                      "Doctor_Training",
                      "Doctor_Contact", 
                      "Doctor_Dogmatic", 
                      "Doctor_Discretion", 
                      "Doctor_CognitiveBias", 
                      "Doctor_MotivatedBias", 
                      "Doctor_BiasMitigating", 
                      "Psychologist_Stability", 
                      "Psychologist_Clarity", 
                      "Psychologist_Accuracy", 
                      "Psychologist_Disagree", 
                      "Psychologist_Like",
                      "Psychologist_Training", 
                      "Psychologist_Contact",
                      "Psychologist_Dogmatic",  
                      "Psychologist_Discretion",   
                      "Psychologist_CognitiveBias",
                      "Psychologist_MotivatedBias", 
                      "Psychologist_BiasMitigating",
                      "DNA_Stability",   
                      "DNA_Clarity", 
                      "DNA_Accuracy", 
                      "DNA_Disagree", 
                      "DNA_Like", 
                      "DNA_Training", 
                      "DNA_Contact",
                      "DNA_Dogmatic",
                      "DNA_Discretion", 
                      "DNA_CognitiveBias", 
                      "DNA_MotivatedBias", 
                      "DNA_BiasMitigating", 
                      "Bloodstain_Stability", 
                      "Bloodstain_Clarity", 
                      "Bloodstain_Accuracy", 
                      "Bloodstain_Disagree", "Bloodstain_Like",
                      "Bloodstain_Training",    
                      "Bloodstain_Contact",
                      "Bloodstain_Dogmatic", 
                      "Bloodstain_Discretion", 
                      "Bloodstain_CognitiveBias", 
                      "Bloodstain_MotivatedBias", 
                      "Bloodstain_BiasMitigating", 
                      "NFL_Stability",
                      "NFL_Clarity",
                      "NFL_Accuracy", 
                      "NFL_Disagree", 
                      "NFL_Like", 
                      "NFL_Training", 
                      "NFL_Contact",
                      "NFL_Dogmatic", 
                      "NFL_Discretion",
                      "NFL_CognitiveBias", 
                      "NFL_MotivatedBias", 
                      "NFL_BiasMitigating", 
                      "Restaurant_Stability",
                      "Restaurant_Clarity", 
                      "Restaurant_Accuracy", 
                      "Restaurant_Disagree", 
                      "Restaurant_Like",
                      "Restaurant_Training", 
                      "Restaurant_Contact",
                      "Restaurant_Dogmatic", 
                      "Restaurant_Discretion",
                      "Restaurant_CognitiveBias", 
                      "Restaurant_MotivatedBias", 
                      "Restaurant_BiasMitigating", 
                      "Human_Resource_Stability",    
                      "Human_Resource_Clarity", 
                      "Human_Resource_Accuracy", 
                      "Human_Resource_Disagree", 
                      "Human_Resource_Like",  
                      "Human_Resource_Training", 
                      "Human_Resource_Contact",
                      "Human_Resource_Dogmatic", 
                      "Human_Resource_Discretion", 
                      "Human_Resource_CognitiveBias",
                      "Human_Resource_MotivatedBias", 
                      "Human_Resource_BiasMitigating",
                      "Election_Stability",   
                      "Election_Clarity", 
                      "Election_Accuracy", 
                      "Election_Disagree", 
                      "Election_Like",
                      "Election_Training", 
                      "Election_Contact",
                      "Election_Dogmatic", 
                      "Election_Discretion",
                      "Election_CognitiveBias", 
                      "Election_MotivatedBias", 
                      "Election_BiasMitigating", 
                      "AttnCheck1",
                      "AttnCheck2", 
                      "NRS1", 
                      "NRS2", 
                      "NRS3",
                      "NRS4",
                      "NRS5",
                      "NRS6",
                      "NRS7",
                      "NRS8",
                      "NRS9",
                      "NRS10",
                      "NRS11",
                      "Gender", 
                      "Age",
                      "Degree",
                      "Degree_Text_1", 
                      "Degree_Text_2", 
                      "Degree_Text_3", 
                      "Degree_Text_4",
                      "Degree_Text_5", 
                      "Job_1",
                      "Job_2",
                      "Ethnicity_1",
                      "Ethnicity_2", 
                      "Ethnicity_3",
                      "Ethnicity_4",
                      "Ethnicity_5",
                      "Ethnicity_6",
                      "Ethnicity_Other", 
                      "Political"))

df1 <- df1%>%
  mutate(AttnCheck2 = replace(AttnCheck2,AttnCheck2 == "in", 1))%>%
  mutate(AttnCheck2=replace(AttnCheck2,AttnCheck2=="In", 1))%>%
  mutate(AttnCheck2=replace(AttnCheck2,AttnCheck2=="IN", 1))%>%
  mutate(AttnCheck2 = replace(AttnCheck2, AttnCheck2 != 1, 0))%>%
  mutate(AttnCheck2 = as.numeric(AttnCheck2))%>%
  mutate(AttnCheck1 = as.numeric(AttnCheck1))%>%
  mutate(failed_AttnCheck = AttnCheck2 + AttnCheck1)%>%
  filter(failed_AttnCheck != 0)

# Longformatting expert type (18 domains) and question (12 Qs, like stability, clarity, etc.)

# We will keep all variables in the first gather but keep only the two new columns 
# for the rest

# Stability
data_full <- df1 %>%
  gather(Expert_Type, Stability, c(Pilot_Stability, Judge_Stability, Broker_Stability,
                                   Chess_Stability, Firefighter_Stability, Physicist_Stability, 
                                   Tax_Assessor_Stability, Nurse_Stability, CIA_Stability,
                                   Surgeon_Stability, Doctor_Stability, Psychologist_Stability, 
                                   DNA_Stability, Bloodstain_Stability, NFL_Stability, 
                                   Restaurant_Stability, Human_Resource_Stability, Election_Stability))

# Clarity
data_clarity <- df1 %>%
  gather(Expert_Type1, Clarity, c(Pilot_Clarity, Judge_Clarity, Broker_Clarity,
                                  Chess_Clarity, Firefighter_Clarity, Physicist_Clarity, 
                                  Tax_Assessor_Clarity, Nurse_Clarity, CIA_Clarity,
                                  Surgeon_Clarity, Doctor_Clarity, Psychologist_Clarity, 
                                  DNA_Clarity, Bloodstain_Clarity, NFL_Clarity, 
                                  Restaurant_Clarity, Human_Resource_Clarity, Election_Clarity))

data_clarity <- data_clarity %>%
  select(Expert_Type1, Clarity)

# Accuracy
data_accuracy<- df1 %>%
  gather(Expert_Type2, Accuracy, c(Pilot_Accuracy, Judge_Accuracy, Broker_Accuracy,
                                   Chess_Accuracy, Firefighter_Accuracy, Physicist_Accuracy, 
                                   Tax_Assessor_Accuracy, Nurse_Accuracy, CIA_Accuracy,
                                   Surgeon_Accuracy, Doctor_Accuracy, Psychologist_Accuracy, 
                                   DNA_Accuracy, Bloodstain_Accuracy, NFL_Accuracy, 
                                   Restaurant_Accuracy, Human_Resource_Accuracy, Election_Accuracy))

data_accuracy <- data_accuracy %>%
  select(Expert_Type2, Accuracy)

# Disagree
data_disagree <- df1 %>%
  gather(Expert_Type3, Disagree, c(Pilot_Disagree, Judge_Disagree, Broker_Disagree,
                                   Chess_Disagree, Firefighter_Disagree, Physicist_Disagree, 
                                   Tax_Assessor_Disagree, Nurse_Disagree, CIA_Disagree,
                                   Surgeon_Disagree, Doctor_Disagree, Psychologist_Disagree, 
                                   DNA_Disagree, Bloodstain_Disagree, NFL_Disagree, 
                                   Restaurant_Disagree, Human_Resource_Disagree, Election_Disagree))

data_disagree <- data_disagree %>%
  select(Expert_Type3, Disagree)


# Like
data_like <- df1 %>%
  gather(Expert_Type4, Like, c(Pilot_Like, Judge_Like, Broker_Like,
                               Chess_Like, Firefighter_Like, Physicist_Like, 
                               Tax_Assessor_Like, Nurse_Like, CIA_Like,
                               Surgeon_Like, Doctor_Like, Psychologist_Like, 
                               DNA_Like, Bloodstain_Like, NFL_Like, 
                               Restaurant_Like, Human_Resource_Like, Election_Like))

data_like <- data_like %>%
  select(Expert_Type4, Like)

# Training
data_training <- df1 %>%
  gather(Expert_Type5, Training, c(Pilot_Training, Judge_Training, Broker_Training,
                                   Chess_Training, Firefighter_Training, Physicist_Training, 
                                   Tax_Assessor_Training, Nurse_Training, CIA_Training,
                                   Surgeon_Training, Doctor_Training, Psychologist_Training, 
                                   DNA_Training, Bloodstain_Training, NFL_Training, 
                                   Restaurant_Training, Human_Resource_Training, Election_Training))

data_training <- data_training %>%
  select(Expert_Type5, Training)


# Contact
data_contact <- df1 %>%
  gather(Expert_Type6, Contact, c(Pilot_Contact, Judge_Contact, Broker_Contact,
                                  Chess_Contact, Firefighter_Contact, Physicist_Contact, 
                                  Tax_Assessor_Contact, Nurse_Contact, CIA_Contact,
                                  Surgeon_Contact, Doctor_Contact, Psychologist_Contact, 
                                  DNA_Contact, Bloodstain_Contact, NFL_Contact, 
                                  Restaurant_Contact, Human_Resource_Contact, Election_Contact))

data_contact <- data_contact %>%
  select(Expert_Type6, Contact)

# Dogmatic
data_dogmatic <- df1 %>%
  gather(Expert_Type7, Dogmatic, c(Pilot_Dogmatic, Judge_Dogmatic, Broker_Dogmatic,
                                   Chess_Dogmatic, Firefighter_Dogmatic, Physicist_Dogmatic, 
                                   Tax_Assessor_Dogmatic, Nurse_Dogmatic, CIA_Dogmatic,
                                   Surgeon_Dogmatic, Doctor_Dogmatic, Psychologist_Dogmatic, 
                                   DNA_Dogmatic, Bloodstain_Dogmatic, NFL_Dogmatic, 
                                   Restaurant_Dogmatic, Human_Resource_Dogmatic, Election_Dogmatic))

data_dogmatic <- data_dogmatic %>%
  select(Expert_Type7, Dogmatic)

# Discretion
data_discretion <- df1 %>%
  gather(Expert_Type8, Discretion, c(Pilot_Discretion, Judge_Discretion, Broker_Discretion,
                                     Chess_Discretion, Firefighter_Discretion, Physicist_Discretion, 
                                     Tax_Assessor_Discretion, Nurse_Discretion, CIA_Discretion,
                                     Surgeon_Discretion, Doctor_Discretion, Psychologist_Discretion, 
                                     DNA_Discretion, Bloodstain_Discretion, NFL_Discretion, 
                                     Restaurant_Discretion, Human_Resource_Discretion, Election_Discretion))

data_discretion <- data_discretion %>%
  select(Expert_Type8, Discretion)

# Cognitive Bias
data_cognitiveBias <- df1 %>%
  gather(Expert_Type9, Cognitive_Bias, c(Pilot_CognitiveBias, Judge_CognitiveBias, Broker_CognitiveBias,
                                         Chess_CognitiveBias, Firefighter_CognitiveBias, Physicist_CognitiveBias, 
                                         Tax_Assessor_CognitiveBias, Nurse_CognitiveBias, CIA_CognitiveBias,
                                         Surgeon_CognitiveBias, Doctor_CognitiveBias, Psychologist_CognitiveBias, 
                                         DNA_CognitiveBias, Bloodstain_CognitiveBias, NFL_CognitiveBias, 
                                         Restaurant_CognitiveBias, Human_Resource_CognitiveBias, Election_CognitiveBias))

data_cognitiveBias <- data_cognitiveBias %>%
  select(Expert_Type9, Cognitive_Bias)

# Motivated Bias
data_motivated <- df1 %>%
  gather(Expert_Type10, Motivated_Bias, c(Pilot_MotivatedBias, Judge_MotivatedBias, Broker_MotivatedBias,
                                          Chess_MotivatedBias, Firefighter_MotivatedBias, Physicist_MotivatedBias, 
                                          Tax_Assessor_MotivatedBias, Nurse_MotivatedBias, CIA_MotivatedBias,
                                          Surgeon_MotivatedBias, Doctor_MotivatedBias, Psychologist_MotivatedBias, 
                                          DNA_MotivatedBias, Bloodstain_MotivatedBias, NFL_MotivatedBias, 
                                          Restaurant_MotivatedBias, Human_Resource_MotivatedBias, Election_MotivatedBias))

data_motivated <- data_motivated %>%
  select(Expert_Type10, Motivated_Bias)

# Bias Mitigating
data_biasMit <- df1 %>%
  gather(Expert_Type11, Bias_Mitigating, c(Pilot_BiasMitigating, Judge_BiasMitigating, Broker_BiasMitigating,
                                           Chess_BiasMitigating, Firefighter_BiasMitigating, Physicist_BiasMitigating, 
                                           Tax_Assessor_BiasMitigating, Nurse_BiasMitigating, CIA_BiasMitigating,
                                           Surgeon_BiasMitigating, Doctor_BiasMitigating, Psychologist_BiasMitigating, 
                                           DNA_BiasMitigating, Bloodstain_BiasMitigating, NFL_BiasMitigating, 
                                           Restaurant_BiasMitigating, Human_Resource_BiasMitigating, Election_BiasMitigating))

data_biasMit <- data_biasMit %>%
  select(Expert_Type11, Bias_Mitigating)



# Binding all new dataframes together and deleting superflous variables

df_long <- cbind(data_full, data_clarity, data_accuracy, data_disagree, data_like, 
                 data_training, data_contact, data_dogmatic, data_discretion,
                 data_cognitiveBias, data_motivated, data_biasMit)

df_long1 <- df_long %>%
  select(-c(Expert_Type1, Expert_Type2, Expert_Type3, Expert_Type4, Expert_Type5,
            Expert_Type6, Expert_Type7, Expert_Type8, Expert_Type9, Expert_Type10,
            Expert_Type11)) %>%
  select(-c(Pilot_Clarity:Election_BiasMitigating))

# This renames the values in our remaining "Expert Type" column so that they are just 
# the names of the expert types
df_long2 <- df_long1 %>%
  mutate(Expert_Type = recode(Expert_Type, Pilot_Stability = "Pilot", Judge_Stability = "Judge",
                              Broker_Stability = "Broker", Chess_Stability = "Chess",
                              Firefighter_Stability = "Firefighter", Physicist_Stability = "Physics", 
                              Tax_Assessor_Stability = "Tax_Assessor", Nurse_Stability = "Nurse", 
                              CIA_Stability = "CIA",
                              Surgeon_Stability = "Surgeon", Doctor_Stability = "Doctor", 
                              Psychologist_Stability = "Psychologist", 
                              DNA_Stability = "DNA_Analyst", Bloodstain_Stability = "Bloodstain_Analyst", 
                              NFL_Stability = "NFL", 
                              Restaurant_Stability = "Restaurant_Critic", 
                              Human_Resource_Stability = "HR_Agent", Election_Stability = "Election"))


# Recodes Cognitive Bias and Motivated Bias to be reverse coded - creates new variables for this. 

df_long3 <- df_long2 %>%
  mutate(Cognitive_Bias_Rv = recode(Cognitive_Bias, '1'=7, '2'=6, '3'=5, '4'=4, '5'=3, '6'=2, '7'=1)) %>%
  mutate(Motivated_Bias_Rv = recode(Motivated_Bias, '1'=7, '2'=6, '3'=5, '4'=4, '5'=3, '6'=2, '7'=1))

#creates objectivity score
#this is a combination of two items. In a previous 
#dataset (R. Velez thesis), these two items were highly correlated (r=.794). As
#a two-item scale they had a cronbach's alpha of .882. They are "Cognitive_Bias" 
#= "When [experts] make judgments as part of their work, to what extent are 
#those judgments influenced by their expectations,preconceptions, and intutions 
#(rather than reflecting a rational analysis of the facts? and "Motivated_Bias" 
#= "When [experts] make judgments as part of their work, how much are those 
#judments motivated by a desire to protect (or boost) their reputation, ego,
#or self-interest?"
df_long3.1 <- df_long3%>%
  mutate(Objectivity = (Cognitive_Bias_Rv + Motivated_Bias_Rv) / 2)

#creating objectivity NRS and Disagreement NRS
#score the Naive Realism Scale
#name the total scaled variable for the Objectivity subscale "NRS_Objectivity"
#name the total scaled variable for the Disagreement subscale "NRS_Disagreement"

df_long3.2 <- df_long3.1 %>%
  mutate(NRS_Objectivity = (NRS4 + NRS5 + NRS7 + NRS8 + NRS10) / 5)

df_long3.3 <- df_long3.2%>%
  mutate(NRS_Disagreement = (NRS1 + NRS2 + NRS3 + NRS6 + NRS8 + NRS11) / 6)

df_long3.4 <- df_long3.3%>%
  mutate(NRS_Total = (NRS1 + NRS2 + NRS3 + NRS4 + NRS5 +NRS6 + NRS7+ NRS8 + NRS9+ NRS10 + NRS11) /11)


# Creates new variable for contact/familiarity variable where no contact(1)=0 and any contact (2-7) = 1. 
# Creates a new variable for disagreement where no disagreement(1) = 0 and any disagreement (2-7) = 1.
df_long4 <- df_long3.4 %>%
  mutate(Contact_ReScored = recode(Contact, '1'=0, '2'=1, '3'=1, '4'=1, '5'=1, '6'=1, '7'=1)) %>%
  mutate(Disagree_ReScored= recode(Disagree, '1'=0, '2'=1, '3'=1, '4'=1, '5'=1, '6'=1, '7'=1))

set.seed(121)

# ------------------------------------------------------------------------------
# TESTING THE HYPOTHESES
# ------------------------------------------------------------------------------

# NOTES:
# 
# We use the function r.squaredGLMM() throughout to calculate the shared variance
# accounted for by each model. There are two values outputted by this function:
# 
# Marginal R_GLMM² represents the variance explained by the fixed effect
# 
# Conditional R_GLMM² is interpreted as a variance explained by the entire model, 
# including both fixed and random effects

# ---------------------------------------------------------------------------------
#Hypothesis 1: We expect that people will believe experts are largely protected against bias 
# (i.e., an illusion of objectivity in experts)
H1_table <- df_long4 %>%
  summarise(mean_obj = mean(Objectivity, na.rm = TRUE),
            sd_obj = sd(Objectivity, na.rm = TRUE))

H1_experts_table <- df_long4 %>%
  group_by(Expert_Type) %>%
  summarise(mean_obj = mean(Objectivity, na.rm = TRUE),
            sd_obj = sd(Objectivity, na.rm = TRUE))

##Histogram of objectivity (averaged across everything)
df_long4 %>%
  ggplot() +
  geom_histogram(aes(Objectivity)) +
  theme_classic()

# this creates a bar plot with standard error bars of the average 
# objectivity score per expert type (you'll need to press zoom to see 
# the full graph)

df_long4 %>%
  group_by(Expert_Type) %>%
  summarise(mean_obj = mean(Objectivity, na.rm = TRUE),
            sd_obj = sd(Objectivity, na.rm = TRUE)) %>%
  ggplot() + 
  coord_cartesian (ylim = c(1, 7), xlim = c(1, 7)) +
  ylab(label = "Objectivity Score") + 
  xlab(label = "Expert Type") + 
  geom_bar(aes(x = reorder(Expert_Type, -mean_obj), mean_obj), stat = "identity", 
           width = .5) +
  geom_errorbar(aes(x = Expert_Type, ymin=mean_obj - (sd_obj/sqrt(nrow(H1_experts_table))), 
                    ymax = mean_obj + (sd_obj/sqrt(nrow(H1_experts_table)))), width = .5) +
  coord_flip() +
  theme_classic()


#Hypothesis 2: We expect people will perceive experts as more objective when they 
#perceive expert domains as yielding more accurate judgments.

# using lmer, we can incorporate random intercepts (1|Subject) 
# and random slopes (1 + Accuracy|Subject) into our model
hyp2.lmr <- lmer(Objectivity ~ Accuracy + (1 + Accuracy|Subject), data = df_long4)
summary(hyp2.lmr)

#gives us the model Rsquare value
r.squaredGLMM(hyp2.lmr)

# Descriptives for accuracy
H2_table <- df_long4 %>%
  summarise(mean_acc = mean(Accuracy, na.rm = TRUE),
            sd_acc = sd(Accuracy, na.rm = TRUE))

H2_experts_table <- df_long4 %>%
  group_by(Expert_Type) %>%
  summarise(mean_acc = mean(Accuracy, na.rm = TRUE),
            sd_acc = sd(Accuracy, na.rm = TRUE))

##Histogram of accuracy (averaged across everything)
df_long4 %>%
  ggplot() +
  geom_histogram(aes(Accuracy)) +
  theme_classic()


# this makes a geom_smooth plot - basically a regression line with shaded error
# (you'll see for this one our error is really small!)
df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Accuracy)) %>%
  ggplot() +
  geom_smooth(aes(Accuracy, Objectivity), method = "lm") +
  scale_y_continuous(breaks = c(1:7), limits = c(1, 7)) +
  scale_x_continuous(breaks = c(1:7)) +
  theme_classic(20)


df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Accuracy)) %>%
  filter(!is.na(Expert_Type)) %>%
  filter(Expert_Type == "DNA_Analyst" || Expert_Type == "Firefighter" 
         || Expert_Type == "Judge" || Expert_Type == "Restaurant_Critic") %>%
  ggplot() +
  geom_smooth(aes(Accuracy, Objectivity, fill = Expert_Type), method = "lm") +
  scale_y_continuous(breaks = c(1:7), limits = c(1, 7)) +
  scale_x_continuous(breaks = c(1:7)) +
  theme_classic(20)

#regression line with the labels 
m1 <- df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Accuracy)) %>%
  group_by(Expert_Type) %>%
  summarise(mean_obj = mean(Objectivity), 
            mean_acc = mean(Accuracy)) %>%
  mutate(Expert_Type = recode(Expert_Type, DNA_Analyst = "DNA",
                              Restaurant_Critic = "Restaurant",
                              HR_Agent = "HR",
                              Bloodstain_Analyst = "Bloodstain",
                              Tax_Assessor = "Tax"))

model <- ggplot(m1)+
  geom_point(aes(x = mean_obj, y = mean_acc)) + 
  geom_text(mapping = aes(x = mean_obj, y = mean_acc, label = Expert_Type))+
  geom_smooth(data = df_long4, aes(x = Objectivity, y = Accuracy), method = "lm")+
  scale_y_continuous(breaks = c(1:7), limits = c(1, 7)) +
  scale_x_continuous(breaks = c(1:7), limit = c(1, 7))+
  labs(x = "Accuracy", y = "Objectivity")+
  theme_classic()

show(model)

# plotting the lmer model

p <- sjPlot::plot_model(hyp2.lmr, type = "pred") 

plot <- p[[1]] +
  geom_smooth(color = "blue") +
  scale_y_continuous(limits = c(1, 7), breaks = c(1:7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1:7)) +
  ggtitle("Objectivity Predicted on Accuracy") +
  theme_grey(20) 

plot + 
  geom_point(data = m1, aes(x = mean_obj, y = mean_acc), size = .1, 
             position = position_jitter(width = .4, seed = 32)) + 
  geom_text(data = m1, mapping = aes(x = mean_obj, y = mean_acc, label = Expert_Type),
            position = position_jitter(width = .4, height = .3, seed = 32), label.size = .25)


#Hypothesis 3: 
#As people perceive expertise to increase, we expect perceptions of 
#expert objectivity to also increase (i.e., we expect people will conflate expertise with objectivity). 

# using lmer, we can incorporate random intercepts (1|Subject) 
# and random slopes (1 + Training|Subject) into our model
hyp3.lmr <- lmer(Objectivity ~ Training + (1 + Training|Subject), data = df_long4)
summary(hyp3.lmr)

hyp3.bm <- brm(Objectivity ~ Training + (1 + Training|Subject), data = df_long4, family = "gaussian")
summary(hyp3.bm)

#gives us the model Rsquare value
r.squaredGLMM(hyp3.lmr)


# Descriptives for training
H3_table <- df_long4 %>%
  summarise(mean_trn = mean(Training, na.rm = TRUE),
            sd_trn = sd(Training, na.rm = TRUE))

H3_experts_table <- df_long4 %>%
  group_by(Expert_Type) %>%
  summarise(mean_trn = mean(Training, na.rm = TRUE),
            sd_trn = sd(Training, na.rm = TRUE))

##Histogram of training (averaged across everything)
df_long4 %>%
  ggplot() +
  geom_histogram(aes(Training))
theme_classic()

#regression line with shaded error
df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Training)) %>%
  ggplot() +
  geom_smooth(aes(Training, Objectivity), method = "lm") +
  scale_y_continuous(breaks = c(1:7), limits = c(1, 7)) +
  scale_x_continuous(breaks = c(1:7)) +
  theme_classic(20)


# this makes a table with averages for these variables across the experts
t3 <- df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Training)) %>%
  group_by(Expert_Type) %>%
  summarise(mean_obj = mean(Objectivity), 
            mean_acc = mean(Training)) %>%
  mutate(Expert_Type = recode(Expert_Type, DNA_Analyst = "DNA",
                              Restaurant_Critic = "Restaurant",
                              HR_Agent = "HR",
                              Bloodstain_Analyst = "Bloodstain",
                              Tax_Assessor = "Tax"))


p3 <- sjPlot::plot_model(hyp3.bm, type = "pred") 

plot3 <- p3[[1]] +
  geom_smooth(color = "blue") +
  scale_y_continuous(limits = c(1, 7), breaks = c(1:7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1:7)) +
  ggtitle("Objectivity Predicted on Training") +
  theme_grey(20) 

plot3 + 
  geom_point(data = t3, aes(x = mean_obj, y = mean_acc), size = .1) + 
  geom_text(data = t3, mapping = aes(x = mean_obj, y = mean_acc, label = Expert_Type))

#Hypothesis 4: We expect to observe consequences of the illusion of objectivity 
#and conflation of expertise with objectivity. Specifically, we expect that as 
#people perceive experts as having more expertise, they will (a) rate as lower 
#the usefulness of bias mitigating procedures, and (b) consistent with the Earned 
#Dogmatism Hypothesis, endorse closemindedness and dogmatism as more appropriate for experts.  
#We think these patterns in a and b will replicate as people perceive experts to be more objective. 

# using lmer, we can incorporate random intercepts (1|Subject) 
# and random slopes (1 + X|Subject) into our model

hyp4a.lmr <- lmer(Bias_Mitigating ~ Training + (1 + Training|Subject), data = df_long4)
summary(hyp4a.lmr)
r.squaredGLMM(hyp4a.lmr)

# bayesian model version (because the other is not converging) of 4a
hyp4a.bm <- brm(Bias_Mitigating ~ Training + (1 + Training|Subject), data = df_long4)
summary(hyp4a.bm)

# lmer version - currently does not converge
hyp4b.lmr <- lmer(Dogmatic ~ Training + (1 + Training|Subject), data = df_long4)
summary(hyp4b.lmr)
r.squaredGLMM(hyp4b.lmr)

# bayesian model version 4b
hyp4b.bm <- brm(Dogmatic ~ Training + (1 + Training|Subject), data = df_long4)
summary(hyp4b.bm)

# lmer version - currently does not converge
hyp4a2.lmr <- lmer(Bias_Mitigating ~ Objectivity + (1 + Objectivity|Subject), data = df_long4)
summary(hyp4a2.lmr)
r.squaredGLMM(hyp4a2.lmr)

# bayesian model version 4a.2
hyp4a2.bm <- brm(Bias_Mitigating ~ Objectivity + (1 + Objectivity|Subject), data = df_long4)
summary(hyp4a2.bm)

# lmer version - currently does not converge
hyp4b2.lmr <- lmer(Dogmatic ~ Objectivity + (1 + Objectivity|Subject), data = df_long4)
summary(hyp4b2.lmr)
r.squaredGLMM(hyp4b2.lmr)

# bayesian model version 4b.2
hyp4b2.bm <- brm(Dogmatic ~ Objectivity + (1 + Objectivity|Subject), data = df_long4)
summary(hyp4b2.bm)

# Descriptives for Bias Mitigating
H4a_table <- df_long4 %>%
  summarise(mean_bm = mean(Bias_Mitigating, na.rm = TRUE),
            sd_bm = sd(Bias_Mitigating, na.rm = TRUE))

H4a_experts_table <- df_long4 %>%
  group_by(Expert_Type) %>%
  summarise(mean_bm = mean(Bias_Mitigating, na.rm = TRUE),
            sd_bm = sd(Bias_Mitigating, na.rm = TRUE))

##Histogram of Bias_Mitigating (averaged across everything)
df_long4 %>%
  ggplot() +
  geom_histogram(aes(Bias_Mitigating))
theme_classic()

# this makes a geom_smooth plot - basically a regression line with shaded error
#for hypothesis 4a2 - (no figure for 4a b/c no relationship)
df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Bias_Mitigating)) %>%
  ggplot() +
  geom_smooth(aes(Bias_Mitigating, Objectivity), method = "lm") +
  scale_y_continuous(breaks = c(1:7), limits = c(1, 7)) +
  scale_x_continuous(breaks = c(1:7)) +
  theme_classic(20)

# Descriptives for Dogmatic
H4b_table <- df_long4 %>%
  summarise(mean_dgm = mean(Dogmatic, na.rm = TRUE),
            sd_dgm = sd(Dogmatic, na.rm = TRUE))

H4b_experts_table <- df_long4 %>%
  group_by(Expert_Type) %>%
  summarise(mean_dgm = mean(Dogmatic, na.rm = TRUE),
            sd_dgm = sd(Dogmatic, na.rm = TRUE))

##Histogram of Dogmatic (averaged across everything)
df_long4 %>%
  ggplot() +
  geom_histogram(aes(Dogmatic))
theme_classic()

# this makes a geom_smooth plot - basically a regression line with shaded error
#hypothesis 4b
df_long4%>%
  filter(!is.na(Training)) %>%
  filter(!is.na(Dogmatic)) %>%
  ggplot() +
  geom_smooth(aes(Dogmatic, Training), method = "lm") +
  scale_y_continuous(breaks = c(1:7), limits = c(1, 7)) +
  scale_x_continuous(breaks = c(1:7)) +
  theme_classic(20)

#hypothesis 4b2
df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Dogmatic)) %>%
  ggplot() +
  geom_smooth(aes(Dogmatic, Objectivity), method = "lm") +
  scale_y_continuous(breaks = c(1:7), limits = c(1, 7)) +
  scale_x_continuous(breaks = c(1:7)) +
  theme_classic(20)

# this makes a table with averages for these variables across the experts
# 4b - Dogmatic ~ Training
t4.b <- df_long4%>%
  filter(!is.na(Dogmatic)) %>%
  filter(!is.na(Training)) %>%
  group_by(Expert_Type) %>%
  summarise(mean_dog = mean(Dogmatic), 
            mean_train = mean(Training)) %>%
  mutate(Expert_Type = recode(Expert_Type, DNA_Analyst = "DNA",
                              Restaurant_Critic = "Restaurant",
                              HR_Agent = "HR",
                              Bloodstain_Analyst = "Bloodstain",
                              Tax_Assessor = "Tax"))

t4.b2 <- df_long4%>%
  filter(!is.na(Dogmatic)) %>%
  filter(!is.na(Objectivity)) %>%
  group_by(Expert_Type) %>%
  summarise(mean_dog = mean(Dogmatic), 
            mean_train = mean(Objectivity)) %>%
  mutate(Expert_Type = recode(Expert_Type, DNA_Analyst = "DNA",
                              Restaurant_Critic = "Restaurant",
                              HR_Agent = "HR",
                              Bloodstain_Analyst = "Bloodstain",
                              Tax_Assessor = "Tax"))


p4.b <- sjPlot::plot_model(hyp4b.bm, type = "pred") 

p4.b2 <- sjPlot::plot_model(hyp4b2.bm, type = "pred") 

plot4.b <- p4.b[[1]] +
  geom_smooth(color = "blue") +
  scale_y_continuous(limits = c(1, 7), breaks = c(1:7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1:7)) +
  ggtitle("Dogmatic Predicted on Training") +
  theme_grey(20) 

plot4.b + 
  geom_point(data = t4.b, aes(x = mean_dog, y = mean_train), size = .1) + 
  geom_text(data = t4.b, mapping = aes(x = mean_dog, y = mean_train, label = Expert_Type))

plot4.b2 <- p4.b2[[1]] +
  geom_smooth(color = "blue") +
  scale_y_continuous(limits = c(1, 7), breaks = c(1:7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1:7)) +
  ggtitle("Dogmatic Predicted on Objectivity") +
  theme_grey(20) 

plot4.b2 + 
  geom_point(data = t4.b2, aes(x = mean_dog, y = mean_obj), size = .1) + 
  geom_text(data = t4.b2, mapping = aes(x = mean_dog, y = mean_obj, label = Expert_Type))

#Hypothesis 5: We expect people will perceive experts as more objective when 
#they perceive domains as having more stable environmental cues, and when they 
#perceive domains as providing clearer feedback.

# using lmer, we can incorporate random intercepts (1|Subject) 
# and random slopes (1 + X|Subject) into our model
hyp5.1.lmr <- lmer(Objectivity ~ Stability + (1 + Stability|Subject), data = df_long4)
summary(hyp5.1.lmr)
r.squaredGLMM(hyp5.1.lmr)

hyp5.2.lmr <- lmer(Objectivity ~ Clarity + (1 + Clarity|Subject), data = df_long4)
summary(hyp5.2.lmr)
r.squaredGLMM(hyp5.2.lmr)

hyp5.2.bm <- brm(Objectivity ~ Clarity + (1 + Clarity|Subject), data = df_long4)
summary(hyp5.2.bm)

# Descriptives for stability
H5.1_table <- df_long4 %>%
  summarise(mean_stb = mean(Stability, na.rm = TRUE),
            sd_stb = sd(Stability, na.rm = TRUE))

H5.1_experts_table <- df_long4 %>%
  group_by(Expert_Type) %>%
  summarise(mean_stb = mean(Stability, na.rm = TRUE),
            sd_stb = sd(Stability, na.rm = TRUE))

##Histogram of Stability (averaged across everything)
df_long4 %>%
  ggplot() +
  geom_histogram(aes(Stability))
theme_classic()

# Descriptives for Clarity
H5.2_table <- df_long4 %>%
  summarise(mean_cla = mean(Clarity, na.rm = TRUE),
            sd_cla = sd(Clarity, na.rm = TRUE))

H5.2_experts_table <- df_long4 %>%
  group_by(Expert_Type) %>%
  summarise(mean_cla = mean(Clarity, na.rm = TRUE),
            sd_cla = sd(Clarity, na.rm = TRUE))

##Histogram of Clarity (averaged across everything)
df_long4 %>%
  ggplot() +
  geom_histogram(aes(Clarity))
theme_classic()

# this makes a geom_smooth plot - basically a regression line with shaded error
#Stability plot-Hyp 5.1
df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Stability)) %>%
  ggplot() +
  geom_smooth(aes(Stability, Objectivity), method = "lm") +
  scale_y_continuous(breaks = c(1:7), limits = c(1, 7)) +
  scale_x_continuous(breaks = c(1:7)) +
  theme_classic(20)

#Clarity plot - Hyp 5.2
df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Clarity)) %>%
  ggplot() +
  geom_smooth(aes(Clarity, Objectivity), method = "lm") +
  scale_y_continuous(breaks = c(1:7), limits = c(1, 7)) +
  scale_x_continuous(breaks = c(1:7)) +
  theme_classic(20)


# this makes a table with averages for these variables across the experts
t5.1 <- df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Stability)) %>%
  group_by(Expert_Type) %>%
  summarise(mean_obj = mean(Objectivity), 
            mean_acc = mean(Stability)) %>%
  mutate(Expert_Type = recode(Expert_Type, DNA_Analyst = "DNA",
                              Restaurant_Critic = "Restaurant",
                              HR_Agent = "HR",
                              Bloodstain_Analyst = "Bloodstain",
                              Tax_Assessor = "Tax"))

t5.2 <- df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Clarity)) %>%
  group_by(Expert_Type) %>%
  summarise(mean_obj = mean(Objectivity), 
            mean_acc = mean(Clarity)) %>%
  mutate(Expert_Type = recode(Expert_Type, DNA_Analyst = "DNA",
                              Restaurant_Critic = "Restaurant",
                              HR_Agent = "HR",
                              Bloodstain_Analyst = "Bloodstain",
                              Tax_Assessor = "Tax"))


p5.1 <- sjPlot::plot_model(hyp5.1.lmr, type = "pred") 
p5.2 <- sjPlot::plot_model(hyp5.2.lmr, type = "pred") 

plot5.1 <- p5.1[[1]] +
  geom_smooth(color = "blue", fill = "blue") +
  scale_y_continuous(limits = c(1, 7), breaks = c(1:7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1:7)) +
  ggtitle("Objectivity Predicted on Stability") +
  theme_grey(20) 

plot5.1 + 
  geom_point(data = t5.1, aes(x = mean_obj, y = mean_acc), size = .1, 
             position = position_jitter(width = .3, seed = 32)) + 
  geom_text(data = t5.1, mapping = aes(x = mean_obj, y = mean_acc, label = Expert_Type),
            position = position_jitter(width = .3, height = .4, seed = 32), label.size = .2)

plot5.2 <- p5.2[[1]] +
  geom_smooth(color = "blue", fill = "blue") +
  scale_y_continuous(limits = c(1, 7), breaks = c(1:7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1:7)) +
  ggtitle("Objectivity Predicted on Clarity") +
  theme_grey(20) 

plot5.2 + 
  geom_point(data = t5.1, aes(x = mean_obj, y = mean_acc), size = .1,
             position = position_jitter(width = .2, seed = 32)) + 
  geom_text(data = t5.1, mapping = aes(x = mean_obj, y = mean_acc, label = Expert_Type),
            position = position_jitter(width = .2, height = .3, seed = 32), label.size = .2)

#Hypothesis 6: We expect people will perceive experts as more objective when 
#they perceive domains as allowing for less discretion. 

# using lmer, we can incorporate random intercepts (1|Subject) 
# and random slopes (1 + X|Subject) into our model
hyp6.lmr <- lmer(Objectivity ~ Discretion + (1 + Discretion|Subject), data = df_long4)
summary(hyp6.lmr)
r.squaredGLMM(hyp6.lmr)

# Descriptives for Discretion
H6_table <- df_long4 %>%
  summarise(mean_dsc = mean(Discretion, na.rm = TRUE),
            sd_dsc = sd(Discretion, na.rm = TRUE))

H6_experts_table <- df_long4 %>%
  group_by(Expert_Type) %>%
  summarise(mean_dsc = mean(Discretion, na.rm = TRUE),
            sd_dsc = sd(Discretion, na.rm = TRUE))

##Histogram of Discretion (averaged across everything)
df_long4 %>%
  ggplot() +
  geom_histogram(aes(Discretion))
theme_classic()

# this makes a geom_smooth plot - basically a regression line with shaded error
df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Discretion)) %>%
  ggplot() +
  geom_smooth(aes(Discretion, Objectivity), method = "lm") +
  scale_y_continuous(breaks = c(1:7), limits = c(1, 7)) +
  scale_x_continuous(breaks = c(1:7)) +
  theme_classic(20)

# makes a table of the averages across experts
t6 <- df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Discretion)) %>%
  group_by(Expert_Type) %>%
  summarise(mean_obj = mean(Objectivity), 
            mean_acc = mean(Discretion)) %>%
  mutate(Expert_Type = recode(Expert_Type, DNA_Analyst = "DNA",
                              Restaurant_Critic = "Restaurant",
                              HR_Agent = "HR",
                              Bloodstain_Analyst = "Bloodstain",
                              Tax_Assessor = "Tax"))


p6 <- sjPlot::plot_model(hyp6.lmr, type = "pred") 

plot6 <- p6[[1]] +
  geom_smooth(color = "blue") +
  scale_y_continuous(limits = c(1, 7), breaks = c(1:7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1:7)) +
  ggtitle("Objectivity Predicted on Discretion") +
  theme_grey(20) 

plot6 + 
  geom_point(data = t6, aes(x = mean_obj, y = mean_acc), size = .1) + 
  geom_text(data = t6, mapping = aes(x = mean_obj, y = mean_acc, label = Expert_Type))

#Hypothesis 7: We expect people will perceive experts as more objective when 
#they are less familiar with  the expert domain.

# using lmer, we can incorporate random intercepts (1|Subject) 
# and random slopes (1 + X|Subject) into our model
hyp7.lmr <- lmer(Objectivity ~ Contact + (1 + Contact|Subject), data = df_long4)
summary(hyp7.lmr)
r.squaredGLMM(hyp7.lmr)


# Same model but using the dichotomous version of Contact 
hyp7.2lmr <- lmer(Objectivity ~ Contact_ReScored + (1 + Contact_ReScored|Subject), data = df_long4)
summary(hyp7.2lmr)
r.squaredGLMM(hyp7.2lmr)


# brms model because the one above didn't converge
hyp7.bm <- brm(Objectivity ~ Contact + (1 + Contact|Subject), data = df_long4)
summary(hyp7.bm)

# Descriptives for Contact
H7_table <- df_long4 %>%
  summarise(mean_con = mean(Contact, na.rm = TRUE),
            sd_con = sd(Contact, na.rm = TRUE))

H7_experts_table <- df_long4 %>%
  group_by(Expert_Type) %>%
  summarise(mean_con = mean(Contact, na.rm = TRUE),
            sd_con = sd(Contact, na.rm = TRUE))

##Contact, we want histogram of contact likert responses (low contact overall)
df_long4 %>%
  ggplot() +
  geom_histogram(aes(Contact))
theme_classic()

# this makes a geom_smooth plot - basically a regression line with shaded error
df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Contact)) %>%
  ggplot() +
  geom_smooth(aes(Contact, Objectivity), method = "lm") +
  scale_y_continuous(breaks = c(1:7), limits = c(1, 7)) +
  scale_x_continuous(breaks = c(1:7)) +
  theme_classic(20)

df_long4 %>%
  ggplot() +
  geom_histogram(aes(Contact))
theme_classic()

# makes a table of the averages across experts
t7 <- df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Contact)) %>%
  group_by(Expert_Type) %>%
  summarise(mean_obj = mean(Objectivity), 
            mean_acc = mean(Contact)) %>%
  mutate(Expert_Type = recode(Expert_Type, DNA_Analyst = "DNA",
                              Restaurant_Critic = "Restaurant",
                              HR_Agent = "HR",
                              Bloodstain_Analyst = "Bloodstain",
                              Tax_Assessor = "Tax"))


p7 <- sjPlot::plot_model(hyp7.bm, type = "pred") 

plot7 <- p7[[1]] +
  geom_smooth(color = "blue") +
  scale_y_continuous(limits = c(1, 7), breaks = c(1:7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1:7)) +
  ggtitle("Objectivity Predicted on Discretion") +
  theme_grey(20) 

plot7 + 
  geom_point(data = t7, aes(x = mean_obj, y = mean_acc), size = .1) + 
  geom_text(data = t7, mapping = aes(x = mean_obj, y = mean_acc, label = Expert_Type))

#Hypothesis 8: We expect people will perceive experts as more objective when 
#they like experts more

# using lmer, we can incorporate random intercepts (1|Subject) 
# and random slopes (1 + X|Subject) into our model
hyp8.lmr <- lmer(Objectivity ~ Like + (1 + Like|Subject), data = df_long4)
summary(hyp8.lmr)
r.squaredGLMM(hyp8.lmr)

# Descriptives for Like
H8_table <- df_long4 %>%
  summarise(mean_lik = mean(Like, na.rm = TRUE),
            sd_lik = sd(Like, na.rm = TRUE))

H8_experts_table <- df_long4 %>%
  group_by(Expert_Type) %>%
  summarise(mean_lik = mean(Like, na.rm = TRUE),
            sd_lik = sd(Like, na.rm = TRUE))

##Histogram of like overall (avg across everything)
df_long4 %>%
  ggplot() +
  geom_histogram(aes(Like)) +
  theme_classic()

# this makes a geom_smooth plot - basically a regression line with shaded error
df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Like)) %>%
  ggplot() +
  geom_smooth(aes(Like, Objectivity), method = "lm") +
  scale_y_continuous(breaks = c(1:7), limits = c(1, 7)) +
  scale_x_continuous(breaks = c(1:7)) +
  theme_classic(20)

# makes a table of the averages across experts
t8 <- df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Like)) %>%
  group_by(Expert_Type) %>%
  summarise(mean_obj = mean(Objectivity), 
            mean_acc = mean(Like)) %>%
  mutate(Expert_Type = recode(Expert_Type, DNA_Analyst = "DNA",
                              Restaurant_Critic = "Restaurant",
                              HR_Agent = "HR",
                              Bloodstain_Analyst = "Bloodstain",
                              Tax_Assessor = "Tax"))

p8 <- sjPlot::plot_model(hyp8.lmr, type = "pred") 

plot8 <- p8[[1]] +
  geom_smooth(color = "blue") +
  scale_y_continuous(limits = c(1, 7), breaks = c(1:7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1:7)) +
  ggtitle("Objectivity Predicted on Like") +
  theme_grey(20) 

plot8 + 
  geom_point(data = t8, aes(x = mean_obj, y = mean_acc), size = .1) + 
  geom_text(data = t8, mapping = aes(x = mean_obj, y = mean_acc, label = Expert_Type))

#Hypothesis 9: We expect people will perceive experts as more objective when 
# they have lower experiences of disagreement  with experts in a field.

# using lmer, we can incorporate random intercepts (1|Subject) 
# and random slopes (1 + X|Subject) into our model
hyp9.lmr <- lmer(Objectivity ~ Disagree + (1 + Disagree|Subject), data = df_long4)
summary(hyp9.lmr)
r.squaredGLMM(hyp9.lmr)


# Same model but using dichotomous version of Disagree
hyp9.2lmr <- lmer(Objectivity ~ Disagree_ReScored + (1 + Disagree_ReScored|Subject), data = df_long4)
summary(hyp9.2lmr)
r.squaredGLMM(hyp9.2lmr)

# Descriptives for Disagree
H9_table <- df_long4 %>%
  summarise(mean_dis = mean(Disagree, na.rm = TRUE),
            sd_dis = sd(Disagree, na.rm = TRUE))

H9_experts_table <- df_long4 %>%
  group_by(Expert_Type) %>%
  summarise(mean_dis = mean(Disagree, na.rm = TRUE),
            sd_dis = sd(Disagree, na.rm = TRUE))

##Histogram of Disagree overall (avg across everything)
df_long4 %>%
  ggplot() +
  geom_histogram(aes(Disagree))
theme_classic()

# this makes a geom_smooth plot - basically a regression line with shaded error
df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Disagree)) %>%
  ggplot() +
  geom_smooth(aes(Disagree, Objectivity), method = "lm") +
  scale_y_continuous(breaks = c(1:7), limits = c(1, 7)) +
  scale_x_continuous(breaks = c(1:7)) +
  theme_classic(20)

# makes a table of the averages across experts
t9 <- df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(Disagree)) %>%
  group_by(Expert_Type) %>%
  summarise(mean_obj = mean(Objectivity), 
            mean_acc = mean(Disagree)) %>%
  mutate(Expert_Type = recode(Expert_Type, DNA_Analyst = "DNA",
                              Restaurant_Critic = "Restaurant",
                              HR_Agent = "HR",
                              Bloodstain_Analyst = "Bloodstain",
                              Tax_Assessor = "Tax"))

p9 <- sjPlot::plot_model(hyp9.lmr, type = "pred") 

plot9 <- p9[[1]] +
  geom_smooth(color = "blue") +
  scale_y_continuous(limits = c(1, 7), breaks = c(1:7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1:7)) +
  ggtitle("Objectivity Predicted on Disagree") +
  theme_grey(20) 

plot9 + 
  geom_point(data = t9, aes(x = mean_obj, y = mean_acc), size = .1) + 
  geom_text(data = t9, mapping = aes(x = mean_obj, y = mean_acc, label = Expert_Type))

#Hypothesis 10: We expect that people who score higher on the Naïve Realism 
#Scale will impute more bias in experts than those who score lower on the scale
#Two NRS subscales: "NRS_Objectivity" and "NRS_Disagreement"

# using lmer, we can incorporate random intercepts (1|Subject) 
# and random slopes (1 + X|Subject) into our model
hyp10.lmr <- lmer(Objectivity ~ NRS_Objectivity + NRS_Disagreement + (1 + NRS_Objectivity + NRS_Disagreement|Subject), data = df_long4)
summary(hyp10.lmr)
r.squaredGLMM(hyp10.lmr)

# Descriptives for NRS Subscales - Objectivity subscale first
H10a_table <- df_long4 %>%
  summarise(mean_NRS_0 = mean(Objectivity, na.rm = TRUE),
            sd_NRS_0 = sd(Objectivity, na.rm = TRUE))
H10a_experts_table <- df_long4 %>%
  group_by(Expert_Type) %>%
  summarise(mean_NRS_0 = mean(Objectivity, na.rm = TRUE),
            sd_NRS_0 = sd(Objectivity, na.rm = TRUE))

# Descriptives for NRS Subscales - Disagreement subscale second
H10b_table <- df_long4 %>%
  summarise(mean_NRS_D = mean(Objectivity, na.rm = TRUE),
            sd_NRS_D = sd(Objectivity, na.rm = TRUE))
H10b_experts_table <- df_long4 %>%
  group_by(Expert_Type) %>%
  summarise(mean_NRS_D = mean(Objectivity, na.rm = TRUE),
            sd_NRS_D = sd(Objectivity, na.rm = TRUE))

##Histogram of NRS_Objectivity overall (avg across everything)
df_long4 %>%
  ggplot() +
  geom_histogram(aes(NRS_Objectivity))
theme_classic()

##Histogram of NRS_Disagreement overall (avg across everything)
df_long4 %>%
  ggplot() +
  geom_histogram(aes(NRS_Disagreement))
theme_classic()

# this makes a geom_smooth plot - basically a regression line with shaded error
# I put both predictors on this graph, specified by color. May not be the 
# best way to do it but it works for now! -TN copied EL code for Hyp 5 in earlier draft of script:)
df_long4%>%
  filter(!is.na(Objectivity)) %>%
  filter(!is.na(NRS_Objectivity)) %>%
  ggplot() +
  geom_smooth(aes(NRS_Objectivity, Objectivity), method = "lm", fill = "blue") +
  geom_smooth(aes(NRS_Disagreement, Objectivity), method = "lm", color = "dark green", fill = "dark green") +
  xlab(label = "NRS_Objectivity (blue) and NRS_Disagreement (green)") + 
  scale_y_continuous(breaks = c(1:7), limits = c(1, 7)) +
  scale_x_continuous(breaks = c(1:7)) +
  theme_classic(20)


# -----------------------------------------------------------------------------
# EXPLORATORY PLOTS AND MODELS
# -----------------------------------------------------------------------------


#Correlation matrix and graph of all key variables
data.cor = cor(select(df_long4,Stability:Discretion, Bias_Mitigating:Motivated_Bias_Rv))

data.cor

corrplot(data.cor, tl.col = "black")
#red is negative corr, blue is possitive corr, dot size is size of the relationship


# -------------------------------
# Some model building with multiple predictors
# -------------------------------


# ---------------------------------------------
# More info on beta plot graphs can be found here: 
# https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html
# ---------------------------------------------


#Exploratory Model-Building 1.  Using lmer with all of the predictors entered in together (main effects) + random intercepts

BigModel1.lmr <- lmer(Objectivity ~ Accuracy + Training + Stability + Clarity + Discretion + Contact +
                        Like + Disagree + (1|Subject), data = df_long4)
summary(BigModel1.lmr)
r.squaredGLMM(BigModel1.lmr)

plot_model(BigModel1.lmr)

#Exploratory Model-Building 1.2.  Using lmer with all of the predictors entered in together (main effects) + random intercepts
# Using the dichotomous versions of Contact and Disagree

BigModel1.2lmr <- lmer(Objectivity ~ Accuracy + Training + Stability + Clarity + Discretion + Contact_ReScored +
                        Like + Disagree_ReScored + (1|Subject), data = df_long4)
summary(BigModel1.2lmr)
r.squaredGLMM(BigModel1.2lmr)

plot_model(BigModel1.2lmr)

#Exploratory Model-Building 2. using lmer with all predictors + the NRS subscales entered together (as main effects) + random intercepts

BigModel2.lmr <- lmer(Objectivity ~ Accuracy + Training + Stability + Clarity + Discretion + Contact +
                        Like + Disagree + NRS_Objectivity + NRS_Disagreement + (1|Subject), data = df_long4)
summary(BigModel2.lmr)
r.squaredGLMM(BigModel2.lmr)

plot_model(BigModel2.lmr)

#Exploratory Model-Building 2.2 using lmer with all predictors + the NRS subscales entered together (as main effects) + random intercepts
# Using the dichotomous versions of Contact and Disagree

BigModel2.2lmr <- lmer(Objectivity ~ Accuracy + Training + Stability + Clarity + Discretion + Contact_ReScored +
                        Like + Disagree_ReScored + NRS_Objectivity + NRS_Disagreement + (1|Subject), data = df_long4)
summary(BigModel2.2lmr)
r.squaredGLMM(BigModel2.2lmr)

plot_model(BigModel2.2lmr)

#Exploratory Model-Building 3. using lmer with all predictors (as main effects) + all 2-way interactions + random intercepts (no NRS)

BigModel3.lmr <- lmer(Objectivity ~ Accuracy + Training + Stability + Clarity + Discretion + Contact +
                        Like + Disagree + Accuracy*Training + Accuracy*Stability + Accuracy*Clarity + 
                        Accuracy*Discretion + Accuracy*Contact + Accuracy*Like + Accuracy*Disagree +
                        Training*Stability  + Training*Clarity + Training*Discretion + Training*Contact 
                      + Training*Like + Training*Disagree + Stability*Clarity + Stability*Discretion 
                      + Stability*Contact + Stability*Like + Stability*Disagree + Clarity*Discretion 
                      + Clarity*Contact + Clarity*Like + Clarity*Disagree + Discretion*Contact + 
                        Discretion*Like + Discretion*Disagree + Like*Disagree + (1|Subject), data = df_long4)
summary(BigModel3.lmr)
r.squaredGLMM(BigModel3.lmr)

plot_model(BigModel3.lmr)

#Exploratory Model-Building 3.2 using lmer with all predictors (as main effects) + all 2-way interactions + random intercepts (no NRS)
# Using the dichotomous versions of Contact and Disagree

BigModel3.2lmr <- lmer(Objectivity ~ Accuracy + Training + Stability + Clarity + Discretion + Contact_ReScored +
                         Like + Disagree_ReScored + Accuracy*Training + Accuracy*Stability + Accuracy*Clarity + 
                         Accuracy*Discretion + Accuracy*Contact_ReScored + Accuracy*Like + Accuracy*Disagree_ReScored +
                         Training*Stability  + Training*Clarity + Training*Discretion + Training*Contact_ReScored 
                       + Training*Like + Training*Disagree_ReScored + Stability*Clarity + Stability*Discretion 
                       + Stability*Contact_ReScored + Stability*Like + Stability*Disagree_ReScored + Clarity*Discretion 
                       + Clarity*Contact_ReScored + Clarity*Like + Clarity*Disagree_ReScored + Discretion*Contact_ReScored + 
                         Discretion*Like + Discretion*Disagree_ReScored + Like*Disagree_ReScored + (1|Subject), data = df_long4)
summary(BigModel3.2lmr)
r.squaredGLMM(BigModel3.2lmr)

plot_model(BigModel3.2lmr)


##TODO
##1) Some of the lmer models look like they're failing to converge...what do we do with this?
##2) Draft the code to make the plots directly from the lmer regression models - a better representation of the error - Emily L. wrote code for hypothesis 2, we should be able to make the rest based on that one
##3) try to create figures for each analysis that overlays the scatterplot for each expert domain (with labels) onto the regression line (over the lmer plots)
##4) I think the code for hypothesis 10 isn't right - the random slope part with two IVs doesn't seem right...(1+ NRS_Objectivity + NRS_Disagreement|Subject)?
##5) do we need to do any ordinal regressions??  instead of linear??

####TODO COMMENTS FROM EMILY P by email in response to these results that Tess sent by word doc on 11/23.
####   "So cool to see these preliminary analyses! Looks like we designed a good study 🙂  
####   I think it will be interesting to put the predictors of bias perception into a single model 
####   in order to compare the strength of the different predictors (since, right now it looks like 
####   a lot of different things correlate with seeing experts as biased). 
####  
####  Also, I guess the current analyses are comparing between subjects, collapsed across expert domains, 
####  if i'm understanding correctly. So it will be interesting to cut the data a slightly different way -- 
####  where we essentially code the different expert domains in terms of how much bias is, on average, 
####  imputed to them (so restaurant critics, who are seen as least objective, would be a 2.84, and DNA analyst, 
####  who are seen as most objective, a 5.17), and see which predictors explain why some experts are seen as 
####  more biased than others. For example, I noticed that disagreement with rest critics is 3.53 whereas 
####  disagree with DNA is 1.55, suggesting that the reason why some experts are seen as more biased than 
####  other experts is because people have more experience disagreeing with those experts. I'm not exactly
####   sure what is the most "correct" way to do these analyses, but the basic idea is to see why experts in 
####   some domains are seen as more objective than experts in other domains, and the type of correlation analysis 
####   I am describing would accomplish that, albeit in a very unsophisticated way. The current analyses lump
####    experts together as a group, but of course Tess and I are curious why different experts are seen as
####     differing in their objectivity (especially interesting is how the forensics folks are seen as 
####     objective -- probably because people don't understand what they do and have no experience disagreeing 
####     with them...)
####   
####   old, incorrect models 
####   #this one is lm only - between subjects not modeling any within ss variance  
# hypothesis_2 <- lm(Objectivity ~ Accuracy, data = df_long4)
# this gives us the regression output that we usually see in SPSS
#summary(hypothesis_2)
##this one was a little better, adding random intercept...but we also wanted random slopes too, which is the final models above
#hyp2.lm <- lmer(Objectivity ~ Accuracy + (1|Subject), data = df_long4)
#summary(hyp2.lm)

#hypothesis_3 <- lm(Objectivity ~ Training, data = df_long4)
# this gives us the regression output that we usually see in SPSS
#summary(hypothesis_3)
# the random intercept version
#hyp3.lm <- lmer(Objectivity ~ Training + (1|Subject), data = df_long4)
#summary(hyp3.lm)
#Hypothesis 4 old models
#hypothesis_4.a <- lm(Bias_Mitigating ~ Training, data = df_long4)
#hypothesis_4.b <- lm(Dogmatic ~ Training, data = df_long4)
#hypothesis_4.a2 <- lm(Bias_Mitigating ~ Objectivity, data = df_long4)
#hypothesis_4.b2 <- lm(Dogmatic ~ Objectivity, data = df_long4)
# this gives us the regression output that we usually see in SPSS
#summary(hypothesis_4.a)
#summary(hypothesis_4.b)
#summary(hypothesis_4.a2)
#summary(hypothesis_4.b2)
# random intercept versions: 
#hyp4a.lm <- lmer(Bias_Mitigating ~ Training + (1|Subject), data = df_long4)
#summary(hyp4a.lm)
#hyp4b.lm <- lmer(Dogmatic ~ Training + (1|Subject), data = df_long4)
#summary(hyp4b.lm)
#hyp4a2.lm <- lmer(Bias_Mitigating ~ Objectivity + (1|Subject), data = df_long4)
#summary(hyp4a2.lm)
#hyp4b2.lm <- lmer(Dogmatic ~ Objectivity + (1|Subject), data = df_long4)
#summary(hyp4b2.lm)
######Hypothesis 5 old models
#hypothesis_5.1 <- lm(Objectivity ~ Stability, data = df_long4)
#hypothesis_5.2 <- lm(Objectivity ~ Clarity, data = df_long4)
# this gives us the regression output that we usually see in SPSS
#summary(hypothesis_5.1)
#summary(hypothesis_5.2)
# random intercepts version
#hyp5.1.lm <- lmer(Objectivity ~ Stability + (1|Subject), data = df_long4)
#summary(hyp5.1.lm)
#hyp5.2.lm <- lmer(Objectivity ~ Clarity + (1|Subject), data = df_long4)
#summary(hyp5.2.lm)
#Hypothesis 6 old models
#hypothesis_6 <- lm(Objectivity ~ Discretion, data = df_long4)
# this gives us the regression output that we usually see in SPSS
#summary(hypothesis_6)
# random intercepts version
#hyp6.lm <- lmer(Objectivity ~ Discretion + (1|Subject), data = df_long4)
#summary(hyp6.lm)
##Old Hypothesis 7 models
#hypothesis_7 <- lm(Objectivity ~ Contact, data = df_long4)
# this gives us the regression output that we usually see in SPSS
#summary(hypothesis_7)
# random intercepts version
#hyp7.lm <- lmer(Objectivity ~ Contact + (1|Subject), data = df_long4)
#summary(hyp7.lm)
##Old Hypothesis 8 models  
#hypothesis_8 <- lm(Objectivity ~ Like, data = df_long4)
# this gives us the regression output that we usually see in SPSS
#summary(hypothesis_8)
# random intercepts version
#hyp8.lm <- lmer(Objectivity ~ Like + (1|Subject), data = df_long4)
#summary(hyp8.lm)
#Old hypothesis 9 models
#hypothesis_9 <- lm(Objectivity ~ Disagree, data = df_long4)
# this gives us the regression output that we usually see in SPSS
#summary(hypothesis_9)
# random intercepts version
#hyp9.lm <- lmer(Objectivity ~ Disagree + (1|Subject), data = df_long4)
#summary(hyp9.lm)
#Old Hypothesis 10 model
#hypothesis_10 <- lm(Objectivity ~ NRS_Objectivity + NRS_Disagreement, data = df_long4)
# this gives us the regression output that we usually see in SPSS
#summary(hypothesis_10)
# random intercepts version
#hyp10.lmr <- lmer(Objectivity ~ NRS_Objectivity + NRS_Disagreement + (1|Subject), data = df_long4)
#summary(hyp10.lmr)