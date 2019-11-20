library(readxl)
library(tidyverse)
library(dplyr)
library(data.table)
library(readr)

## Import the data (Percepts of Expert Bias Datset_Functional Dataset - CSV)
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

#creates objectivity score
df_long3 <- df_long2%>%
  mutate(Objectivity = (Cognitive_Bias + Motivated_Bias) / 2)


#Hypothesis 1: We expect that people will believe experts are largely protected against bias (i.e., an illusion of objectivity in experts)
H1_table <- df_long3 %>%
  summarise(mean_obj = mean(Objectivity, na.rm = TRUE),
            sd_obj = sd(Objectivity, na.rm = TRUE))

H1_experts_table <- df_long3 %>%
  group_by(Expert_Type) %>%
  summarise(mean_obj = mean(Objectivity, na.rm = TRUE),
            sd_obj = sd(Objectivity, na.rm = TRUE))

#Hypothesis 2: We expect people will perceive experts as more objective when they 
#perceive expert domains as yielding more accurate judgments.
hypothesis_2 <- lm(Objectivity ~ Accuracy, data = df_long3)

#Hypothesis 3: As people perceive expertise to increase, we expect perceptions of 
#expert objectivity to also increase (i.e., we expect people will conflate expertise with objectivity). 
hypothesis_3 <- lm(Objectivity ~ Training, data = df_long3)

#Hypothesis 4: We expect to observe consequences of the illusion of objectivity 
#and conflation of expertise with objectivity. Specifically, we expect that as 
#people perceive experts as having more expertise, they will (a) rate as lower 
#the usefulness of bias mitigating procedures, and (b) consistent with the Earned 
#Dogmatism Hypothesis, endorse closemindedness and dogmatism as more appropriate for experts.  
#We think these patterns in a and b will replicate as people perceive experts to be more objective. 
#CHECK WHICH VARIABLE IS CLOSE MINDED
hypothesis_4.1 <- lm(Bias_Mitigating ~ Training, data = df_long3)
hypothesis_4.2 <- lm(Accuracy ~ Training, data = df_long3)
hypothesis_4.3 <- lm(Dogmatic ~ Training, data = df_long3)
hypothesis_4.4 <- lm(Accuracy ~ Objectivity, data = df_long3)
hypothesis_4.5 <- lm(Dogmatic ~ Objectivity, data = df_long3)

#Hypothesis 5: We expect people will perceive experts as more objective when 
#they perceive domains as having more stable environmental cues, and when they 
#perceive domains as providing clearer feedback. 
hypothesis_5.1 <- lm(Objectivity ~ Stability, data = df_long3)
hypothesis_5.2 <- lm(Objectivity ~ Clarity, data = df_long3)

#Hypothesis 6: We expect people will perceive experts as more objective when 
#they perceive domains as allowing for less discretion. 
hypothesis_6 <- lm(Objectivity ~ Discretion, data = df_long3)

#Hypothesis 7: We expect people will perceive experts as more objective when 
#they are less familiar with  the expert domain.
hypothesis_7 <- lm(Objectivity ~ Contact, data = df_long3)

#Hypothesis 8: We expect people will perceive experts as more objective when 
#they like experts more
hypothesis_8 <- lm(Objectivity ~ Like, data = df_long3)

#Hypothesis 9: We expect people will perceive experts as more objective when 
# they have lower experiences of disagreement  with experts in a field.
hypothesis_9 <- lm(Objectivity ~ Disagree, data = df_long3)

#Hypothesis 10: We expect that people who score higher on the Naïve Realism 
#Scale will impute more bias in experts than those who score lower on the scale

