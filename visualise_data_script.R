library(tidyverse)
library(ggthemes)
library(readxl)
library(DescTools)
library(multcomp)
library(ggplot2)
library(superb)
library(dplyr)
library(doBy)
library(paletteer)
library(emmeans)
library(forcats)


#######FOR COMPARING OVERALL DISTRIBUTION OF PTAU MARKERS IN DIFFERENT TYPES OF PLAQUE CLASSES########

###STEP 1: Assigned data to dataframe, load and view excel sheet using read_excel()
summary_pTau <- read_excel("E:/03_data_analysis/03_summary_data/20260918_PCF_plaque_analysis.xlsx", sheet = "perPlaque_pTauALL")
View(summary_pTau)
print(summary_pTau)

###STEP 2: Generate boxplot to view data distribution - percentage of pTau area vs pTau marker grouped by plaque class
#Classification category should be sorted using the order of "Diffuse", "Fibrillar", and "Cored" 
classification_levels <- c("Diffuse", "Fibrillar", "Cored")
#pTau markers category sorted according to type of pTau from smallest number to highest
pTau_levels <- c("pTau181", "pTau205", "pTau231", "pTau396", "Total pTau")
boxplot_pTau_result <- ggplot(summary_pTau, aes(x=`pTau Marker`, y=`Area %`)) + 
  geom_point() + geom_boxplot(aes(x = factor(`pTau Marker`, levels = pTau_levels), fill = factor(Classification, levels = classification_levels))) + 
  labs(y="Mean area of pTau detected within a plaque (%)", fill = "Classification") + 
  theme_classic() + scale_colour_paletteer_d("ggthemes::Classic_10_Light") + scale_fill_paletteer_d("ggthemes::Classic_10_Light")
print(boxplot_pTau_result)

#####################################################################################################################################
###STEP 3: Descriptive statistics for dataframe - percentage of pTau area vs pTau marker grouped by plaque class
group_mean_pTau <- summary_pTau |> group_by(`pTau Marker`, Classification) |> summarise("N"=n(), "Mean"=mean(`Area %`), "SD"=sd(`Area %`), "SEM"=sd(`Area %`/sqrt(n())), "ymin"=mean(`Area %`)-sd(`Area %`)/sqrt(n()), "ymax"=mean(`Area %`)+sd(`Area %`)/sqrt(n()))
View(group_mean_pTau)
print(group_mean_pTau)

###STEP 4: Visualise data - percentage of pTau area vs pTau marker grouped by plaque class (with raw data as geom_point)
ggplot_pTau_result <- ggplot(summary_pTau, aes(x=factor(`pTau Marker`, levels = pTau_levels), y=`Percentage Area %`, fill=factor(Classification, levels = classification_levels))) + 
  stat_summary(aes(y=`Percentage Area %`, group=factor(Classification, levels = classification_levels), colour=factor(Classification, levels = classification_levels)), fun.data=mean_se, geom = "bar", width = 0.8, position=position_dodge(0.81)) + 
  stat_summary(aes(y=`Percentage Area %`, group=factor(Classification, levels = classification_levels)), fun.data=mean_se, geom = "errorbar", width = 0.2, position=position_dodge(0.81)) + 
  labs(x="Type of pTau Markers", y="Percentage of pTau area within the amyloid-beta plaques (%)", fill="Classification", shape="Classification", colour="Classification") + 
  geom_point(summary_pTau, mapping=aes(x=factor(`pTau Marker`, levels = pTau_levels), y=`Percentage Area %`, shape=factor(Classification, levels = classification_levels)), position=position_jitterdodge(0.1), alpha = 0.1) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 70), breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) + 
  theme_classic() + scale_colour_paletteer_d("ggthemes::Classic_10_Light") + scale_fill_paletteer_d("ggthemes::Classic_10_Light")
print(ggplot_pTau_result)


#####################################################################################################################################
###STEP 5: Visualise data - percentage of pTau area vs pTau marker for diffuse plaque
diffuse_data <- filter(summary_pTau, Classification == "Diffuse")
View(diffuse_data)
print(diffuse_data)

group_diffuse_data <- diffuse_data |> group_by(`pTau Marker`) |> summarise("N"=n(), "Mean"=mean(`Area %`), "SD"=sd(`Area %`), "SEM"=sd(`Area %`/sqrt(n())), "ymin"=mean(`Area %`)-sd(`Area %`)/sqrt(n()), "ymax"=mean(`Area %`)+sd(`Area %`)/sqrt(n()))
View(group_diffuse_data)
print(group_diffuse_data)

ggplot_diffuse_plot <- ggplot(diffuse_data, aes(x=factor(`pTau Marker`, levels = pTau_levels), y=`Area %`)) + 
  stat_summary(aes(y=`Area %`, fill=`pTau Marker`), fun.y=mean, geom = "bar", width = 0.5, position=position_dodge(0.1)) +
  stat_summary(aes(y=`Area %`, fill=`pTau Marker`), fun.data=mean_se, geom = "errorbar", width = 0.15, position=position_dodge(0.1)) +
  labs(x="", y="Mean area of pTau \ndetected within a plaque %") +
  geom_point(diffuse_data, mapping=aes(x=factor(`pTau Marker`, levels = pTau_levels), y=`Area %`, colour = `pTau Marker`), position=position_jitterdodge(0.1), alpha = 0.1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks = c(0, 20, 40, 60)) + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme_classic() + scale_colour_paletteer_d("ggthemes::Classic_10_Light") + scale_fill_paletteer_d("ggthemes::Classic_10_Light")
print(ggplot_diffuse_plot)

#####################################################################################################################################
###STEP 5: Descriptive statistics for dataframe - mean of percentage of pTau area within each plaque class for each pTau marker by ID 
df_mean <- read_excel("Summary_PCFanalysis_ethel.xlsx", sheet = "IDandplaque_summary")
View(df_mean)
print(df_mean)
ID_mean_pTau <- df_mean |> group_by(`pTau Marker`, Classification) |> summarise("N"=n(), "Mean"=mean(`Mean_Percentage Area %`), "SD"=sd(`Mean_Percentage Area %`), "SEM"=sd(`Mean_Percentage Area %`/sqrt(n())), "ymin"=mean(`Mean_Percentage Area %`)-sd(`Mean_Percentage Area %`)/sqrt(n()), "ymax"=mean(`Mean_Percentage Area %`)+sd(`Mean_Percentage Area %`)/sqrt(n()))
print(ID_mean_pTau)
View(ID_mean_pTau)

###visualise data - mean of percentage of pTau area within each plaque class for each pTau marker by ID
ggplot_pTau_result <- ggplot(summary_pTau, aes(x=factor(`pTau Marker`, levels = pTau_levels), y=`Percentage Area %`, fill=factor(Classification, levels = classification_levels))) + 
  stat_summary(df_mean, mapping=aes(x=`pTau Marker`, y=`Mean_Percentage Area %`, group=factor(Classification, levels = classification_levels), colour=factor(Classification, levels = classification_levels)), fun.data=mean_se, geom = "bar", width = 0.8, position=position_dodge(0.83)) + 
  stat_summary(df_mean, mapping=aes(x=`pTau Marker`, y=`Mean_Percentage Area %`, group=factor(Classification, levels = classification_levels)), fun.data=mean_se, geom = "errorbar", width = 0.2, position=position_dodge(0.83)) + 
  labs(x="pTau Markers detected", y="Area pTau marker co-localized with amyloid-beta within the plaque (%)", fill="Classification", shape="Classification", colour="Classification") + 
  geom_point(df_mean, mapping=aes(y=as.numeric(`Mean_Percentage Area %`), shape=factor(Classification, levels = classification_levels)), position=position_jitterdodge(0.0005), alpha = 0.5) +
  #geom_point(summary_pTau, mapping=aes(x=factor(`pTau Marker`, levels = pTau_levels), y=`Percentage Area %`, shape=factor(Classification, levels = classification_levels)), position=position_jitterdodge(0.1), alpha = 0.1) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20), breaks = c(0, 10, 20)) + 
  theme_classic() + scale_colour_paletteer_d("ggthemes::Classic_10_Light") + scale_fill_paletteer_d("ggthemes::Classic_10_Light")
print(ggplot_pTau_result)

####################################################################################################################################

#######FOR COMPARING DISTRIBUTION OF TOTAL PTAU IN DIFFERENT TYPES OF PLAQUE CLASSES IN GG/AA########
##assigned data to dataframe, load and view excel sheet using read_excel()
pTau_sST2 <- read_excel("Summary_pTau.xlsx", sheet = "pTau_sST2")
View(pTau_sST2)
print(pTau_sST2)

##Generate boxplot to view data distribution - percentage of pTau area vs sST2 group by plaque class
pTau_sST2$sST2 <- as.character(pTau_sST2$sST2)
boxplot_pTau_result <- ggplot(pTau_sST2, aes(x=Classification, y=`Percentage Area %`)) + geom_point() + geom_boxplot(aes(fill = sST2)) + labs(x="Plaque Class", y="Percentage of pTau Area in Amyloid Plaques (%)") + theme_classic() + scale_colour_paletteer_d("ggthemes::Classic_10_Light") + scale_fill_paletteer_d("ggthemes::Classic_10_Light")
print(boxplot_pTau_result)

##Descriptive statistics for dataframe - percentage of pTau area vs sST2 group by plaque class
group_mean_pTau <- pTau_sST2 |> group_by(Classification, sST2) |> summarise("N"=n(), "Mean"=mean(`Percentage Area %`), "SD"=sd(`Percentage Area %`), "SEM"=sd(`Percentage Area %`/sqrt(n())), "ymin"=mean(`Percentage Area %`)-sd(`Percentage Area %`)/sqrt(n()), "ymax"=mean(`Percentage Area %`)+sd(`Percentage Area %`)/sqrt(n()))
print(group_mean_pTau)

##Visualise data - percentage of pTau area vs sST2 group by plaque class
ggplot_pTau_result <- ggplot(pTau_sST2, aes(x=Classification, y=`Percentage Area %`, fill=sST2)) + stat_summary(aes(y=`Percentage Area %`, group=sST2, colour=sST2), fun.data=mean_se, geom = "bar", width = 0.8, position=position_dodge(0.81)) + stat_summary(aes(y=`Percentage Area %`, group=sST2), fun.data=mean_se, geom = "errorbar", width = 0.2, position=position_dodge(0.81)) + labs(x="Plaque Type", y="Percentage of pTau Area/Amyloid Plaques (%)", fill="sST2", shape="sST2", colour="sST2") + geom_point(pTau_sST2, mapping=aes(x=Classification, y=`Percentage Area %`, shape=sST2), position=position_jitterdodge(0.1), alpha = 0.3) + scale_y_continuous(expand = c(0, 0), limits = c(0, 80), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) + theme_classic() + scale_colour_paletteer_d("ggthemes::Classic_10_Light") + scale_fill_paletteer_d("ggthemes::Classic_10_Light")
print(ggplot_pTau_result)


##****FOR COMPARING DISTRIBUTION OF NEURITIC PLAQUES IN DIFFERENT TYPES OF PLAQUE CLASSES IN GG/AA
##assigned data to dataframe, load and view excel sheet using read_excel()
NP_sST2 <- read_excel("Summary_pTau.xlsx", sheet = "NP_sST2_all")
View(NP_sST2)
print(NP_sST2)

##Generate boxplot to view data distribution - percentage of NP vs type of plaques by sST2
boxplot_pTau_result <- ggplot(NP_sST2, aes(x=`Type of plaques`, y=plaque_percentage)) + geom_point() + geom_boxplot(aes(fill = sST2)) + labs(x= "Type of Plaques",y="Percentage of Plaques (%)") + theme_classic() + scale_colour_paletteer_d("ggthemes::Classic_10_Light") + scale_fill_paletteer_d("ggthemes::Classic_10_Light")
print(boxplot_pTau_result)

##Descriptive statistics for dataframe - percentage of pTau area vs sST2 group by plaque class
group_mean_pTau <- NP_sST2 |> group_by(`Type of plaques`, sST2) |> summarise("N"=n(), "Mean"=mean(plaque_percentage), "SD"=sd(plaque_percentage), "SEM"=sd(plaque_percentage/sqrt(n())), "ymin"=mean(plaque_percentage)-sd(plaque_percentage)/sqrt(n()), "ymax"=mean(plaque_percentage)+sd(plaque_percentage)/sqrt(n()))
print(group_mean_pTau)

##Visualise data - percentage of pTau area vs pTau marker grouped by plaque class
ggplot_pTau_result <- ggplot(NP_sST2, aes(x=`Type of plaques`, y=plaque_percentage, fill=sST2)) + stat_summary(aes(y=plaque_percentage, group=sST2, colour=sST2), fun.data=mean_se, geom = "bar", width = 0.8, position=position_dodge(0.81)) + stat_summary(aes(y=plaque_percentage, group=sST2), fun.data=mean_se, geom = "errorbar", width = 0.2, position=position_dodge(0.81)) + labs(x="Type of Plaques", y="Percentage of plaques (%)", fill="sST2", shape="sST2", colour="sST2") + geom_point(NP_sST2, mapping=aes(x=`Type of plaques`, y=plaque_percentage, shape=sST2), position=position_jitterdodge(0.1), alpha = 0.25) + scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + theme_classic() + scale_colour_paletteer_d("ggthemes::Classic_10_Light") + scale_fill_paletteer_d("ggthemes::Classic_10_Light")
print(ggplot_pTau_result)
