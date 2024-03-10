#SARA GARZA GONZALEZ
library(readr)
library(dplyr)
library(ggformula)
library(lm.beta)
library(supernova)
library(pwr)
library(interactions)
library(lsr)
library(effsize)
library(car)

Study2_data <- read_csv("Downloads/Study2_data.csv")
View(Study2_data)

#DISTRIBUTION of variables
gf_histogram(P_bi_comb ~ ANY_ABUSE_CHILD_DV, data = Study2_data)
gf_histogram(P_bi_comb ~ INC_NEEDS, data = Study2_data)

#Checking power
pwr.r.test(n = 262, r = NULL, sig.level = 0.05, power = 0.8)

#REPLICATING model and correlation between age and the p-factor at baseline 
firstregression <- lm(P_bi_comb ~ S1AGE, data = Study2_data)
summary(firstregression)
cor(Study2_data$P_bi_comb, Study2_data$S1AGE, use="pairwise.complete.obs")

#REPLICATING model and correlation between sex and the p-factor at baseline 
secondregression <- lm(P_bi_comb ~ SEX, data = Study2_data)
summary(secondregression)
cor(Study2_data$P_bi_comb, Study2_data$SEX, use="pairwise.complete.obs")

#FILTERING to create a new data set for conducting t-test and obtaining Cohen's d
male <- filter(Study2_data, SEX == 0)
female <- filter(Study2_data, SEX == 1)

t.test(male$P_bi_comb, female$P_bi_comb)

cohen.d(male$P_bi_comb,female$P_bi_comb)

#FITTING Interaction model between age and sex in relation to psychopathology with Boolean sex variable TO CHECK FOR STATISTICAL ESTIMATES OF INTEREST
interaction_model1 <- lm(P_bi_comb ~ S1AGE + SEX + S1AGE*SEX, data = Study2_data)
interaction_model1
summary(interaction_model1)
supernova(interaction_model1)

#CHECK ASSUMPTIONS

#LINEARITY
Study2_data$predict <- predict(interaction_model1, Study2_data)
Study2_data$resid <- Study2_data$P_bi_comb - predict(interaction_model1, Study2_data)
gf_point(resid ~ predict, data = Study2_data)%>% gf_smooth() #passed, a little wobbly but mostly straight

#VIF multicollinearity 
vif(interaction_model1) #did not pass

#normality of residuals
gf_histogram(~resid, data = Study2_data) #passed!

#Homoscedasticity
gf_point(resid ~ S1AGE, data = Study2_data) #passed!
#DID NOT CHECK SEX BECAUSE IT IS EITHER FEMALE (1) OR MALE (0)

#Independence of Residuals: EACH subject responded once so passed!

#FITTING Interaction model between age and sex in relation to psychopathology with categorical sex variable FOR PLOT
interaction_model <- lm(P_bi_comb ~ S1AGE + Sex + S1AGE*Sex, data = Study2_data)
interaction_model

#BUILDING interaction plots to depict the relationships
interact_plot(interaction_model, pred = S1AGE, modx = Sex, plot.points = TRUE,  
              jitter = 0.3, size = 0.3, alpha = 0.5, point.shape = TRUE, interval = TRUE,
              int.width = 0.95, x.label = "Participant Age", 
              y.label = "Psychopathy P factor (baseline)", 
              main.title = "Effects of the Interaction between Sex and Age on Psychopathology")

#SEPARATED interaction plots
interact_plot(interaction_model, pred = S1AGE, modx = Sex, plot.points = TRUE,  
              jitter = 0.3, size = 0.3, alpha = 0.5, point.shape = TRUE, interval = TRUE,
              int.width = 0.95, linearity.check = TRUE, x.label = "Participant Age", 
              y.label = "Psychopathy P factor (baseline)", 
              main.title = "Effects of the Interaction between Sex and Age on Psychopathology")

#ADDITION TO STUDY...FITTING NEW INTERACTION MODEL WITH BOOLEAN VIOLENCE VARIABLE TO CHECK FOR STATISTICAL ESTIMATES OF INTEREST
new_interaction_model2 <- lm(P_bi_comb ~ ANY_ABUSE_CHILD_DV + INC_NEEDS + ANY_ABUSE_CHILD_DV*INC_NEEDS, data = Study2_data)
new_interaction_model2
summary(new_interaction_model2)
supernova(new_interaction_model2)

#CHECK ASSUMPTIONS

#LINEARITY
Study2_data$predict2 <- predict(new_interaction_model2, Study2_data)
Study2_data$resid2 <- Study2_data$P_bi_comb - predict(new_interaction_model2, Study2_data)
gf_point(resid2 ~ predict2, data = Study2_data)%>% gf_smooth() #did not pass

#VIF multicollinearity 
vif(new_interaction_model2) #did not pass

#normality of residuals
gf_histogram(~resid2, data = Study2_data) #passed!

#Homoscedasticity
gf_point(resid2 ~ S1AGE, data = Study2_data) #passed!
#DID NOT CHECK SEX BECAUSE IT IS EITHER FEMALE (1) OR MALE (0)

#Independence of Residuals: EACH subject responded once so passed!

#CHANGING BOOLEAN TO CATEGORICAL FOR PLOT
str(Study2_data$ANY_ABUSE_CHILD_DV)

Study2_data2 <- data.frame(Study2_data)
Study2_data2
Study2_data2$ANY_ABUSE_CHILD_DV <- factor(Study2_data$ANY_ABUSE_CHILD_DV, levels= c(0,1),
                                          labels= c("No experience of domestic violence", "Experienced domestic violence"))
str(Study2_data2$ANY_ABUSE_CHILD_DV)

#BUILDING interaction plots to depict the relationships
new_interaction_model3 <- lm(P_bi_comb ~ ANY_ABUSE_CHILD_DV + INC_NEEDS + ANY_ABUSE_CHILD_DV*INC_NEEDS, data = Study2_data2)
new_interaction_model3
summary(new_interaction_model3)

interact_plot(new_interaction_model3, pred = INC_NEEDS, modx = ANY_ABUSE_CHILD_DV, plot.points = TRUE,  
              jitter = 0.5, size = 0.6, alpha = 0.5, point.shape = TRUE, interval = TRUE,
              int.width = 0.95, x.label = "Income to Needs Ratio", 
              y.label = "Psychopathy P factor (baseline)", 
              main.title = "Effects of the Interaction between SES and Violence Exposure on Psychopathology",
              legend.main = "Domestic Violence")

#SEPARATED interaction plots              
interact_plot(new_interaction_model3, pred = INC_NEEDS, modx = ANY_ABUSE_CHILD_DV, plot.points = TRUE,  
              jitter = 0.5, size = 0.6, alpha = 0.5, point.shape = TRUE, interval = TRUE,
              int.width = 0.95, linearity.check = TRUE, x.label = "Income to Needs Ratio", 
              y.label = "Psychopathy P factor (baseline)",
              main.title = "Effects of the Interaction between SES and Violence Exposure on Psychopathology",
              legend.main = "Domestic Violence")