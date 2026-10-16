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
library(ggprism)
library(scales)

#######FOR COMPARING OVERALL DISTRIBUTION OF PTAU MARKERS IN DIFFERENT TYPES OF PLAQUE CLASSES########

#####STEP 1: Assigned data to dataframe, load and view excel sheet using read_excel()
summary_pTau <- read_excel("E:/03_data_analysis/03_summary_data/20260918_PCF_plaque_analysis.xlsx", sheet = "perPlaque_pTauALL")
View(summary_pTau)
print(summary_pTau)


##############################################################################################################################################################


#####STEP 2: Generate plots to view data distribution - percentage of pTau area vs pTau marker grouped by plaque class

###Before generating the boxplot, sort the category for "Classification" and "pTau markers":
###Classification category should be sorted using the order of "Diffuse", "Fibrillar", and "Cored" 
classification_levels <- c("Diffuse", "Fibrillar", "Cored") 
###pTau markers category sorted according to type of pTau from smallest number to highest
pTau_levels <- c("pTau181", "pTau205", "pTau231", "pTau396", "Total pTau")

###For box plot:
boxplot_pTau_result <- ggplot(summary_pTau, aes(x=`pTau Marker`, y=`Area %`)) + 
  geom_point() + geom_boxplot(aes(x = factor(`pTau Marker`, levels = pTau_levels), fill = factor(Classification, levels = classification_levels))) + 
  labs(y="Mean area of pTau detected within a plaque (%)", fill = "Classification") + 
  theme_classic() + scale_colour_paletteer_d("ggthemes::Classic_10_Light") + scale_fill_paletteer_d("ggthemes::Classic_10_Light")
print(boxplot_pTau_result)

###Descriptive statistics for data frame - Area % vs pTau markers grouped by plaque classes
group_mean_pTau <- summary_pTau |> group_by(`pTau Marker`, Classification) |> 
  summarise(N=n(), Mean=mean(`Area %`), SD=sd(`Area %`), SEM=sd(`Area %`)/sqrt(n()), 
            ylower=mean(`Area %`)-sd(`Area %`)/sqrt(n()), yupper=mean(`Area %`)+sd(`Area %`)/sqrt(n()))
View(group_mean_pTau)
print(group_mean_pTau)

###For bar plot:

##Prism palette as theme
#names(ggprism_data$themes)
#preview_theme("spring")
#show_palette <- function(palette) {
#  scales::show_col(
#    prism_colour_pal(palette = palette)(
#      attr(prism_color_pal(palette = palette), "max_n")
#    )
#  )
#}
#show_palette("colors") 
##or, use this website as reference for colour codes in R: https://r-charts.com/colors/
#colours use: (orange=#FFD39A; blue=#9FD5FF; pink=#FFA7F3; violetred=#8B2252)

ggplot_pTau <- ggplot(summary_pTau, aes(x=factor(`pTau Marker`, levels = pTau_levels), y=`Area %`, fill=factor(Classification, levels = classification_levels))) +
  labs(x="", y="Mean area of pTau \ndetected within a plaque %", fill="Classification", shape="Classification") +
  geom_point(summary_pTau, mapping=aes(x=factor(`pTau Marker`, levels = pTau_levels), y=`Area %`, shape=factor(Classification, levels = classification_levels)), position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), alpha = 0.1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  geom_col(group_mean_pTau, mapping=aes(x=factor(`pTau Marker`, levels = pTau_levels), y=Mean, fill=factor(Classification, levels = classification_levels)), position=position_dodge(0.8), width = 0.8) + 
  geom_errorbar(group_mean_pTau, mapping=aes(x=factor(`pTau Marker`, levels = pTau_levels), y=Mean, ymin = ylower, ymax = yupper), position=position_dodge(0.8), width = 0.2) +
  theme_prism(axis_text_angle = 45, base_size = 12, base_family = "Arial") + scale_fill_manual(values = c("#FFD39A", "#9FD5FF", "#FFA7F3")) +
  theme(legend.title = element_text(size=12, family = "Arial"), legend.text = element_text(size=12, family = "Arial", face = "bold"))
print(ggplot_pTau)


#####################################################################################################################################


#####STEP 3: Visualise data - percentage of pTau area vs pTau marker for diffuse plaque ONLY

###Filter group data frame to only include diffuse plaque
diffuse_pTau <- filter(summary_pTau, Classification == "Diffuse")
View(diffuse_pTau)
print(diffuse_pTau)

###Descriptive statistics for data frame - Area % vs pTau markers for diffuse plaque ONLY
group_diffuse_pTau <- diffuse_pTau |> group_by(`pTau Marker`) |>
  summarise(N=n(), Mean=mean(`Area %`), SD=sd(`Area %`), SEM=sd(`Area %`/sqrt(n())), 
                      ymin=mean(`Area %`)-sd(`Area %`)/sqrt(n()), ymax=mean(`Area %`)+sd(`Area %`)/sqrt(n()))
View(group_diffuse_pTau)
print(group_diffuse_pTau)


###For bar plot:
ggplot_diffuse_pTau <- ggplot(diffuse_pTau, aes(x=factor(`pTau Marker`, levels = pTau_levels), y=`Area %`)) +
  labs(title="Diffuse plaques \n", x="", y="Mean area of pTau \ndetected within a plaque %") +
  geom_point(diffuse_pTau, mapping=aes(x=factor(`pTau Marker`, levels = pTau_levels), y=`Area %`, shape="point"), position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), alpha = 0.2) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  geom_col(group_diffuse_pTau, mapping=aes(x=factor(`pTau Marker`, levels = pTau_levels), y=Mean, fill=c("#81B3EB", "#FFCDB0", "#FFEDFF","#FFFFFF", "#35FFB8")), colour="black", position=position_dodge(0.8), width = 0.8) + 
  geom_errorbar(group_diffuse_pTau, mapping=aes(x=factor(`pTau Marker`, levels = pTau_levels), y=Mean, ymin = ymin, ymax = ymax), position=position_dodge(0.8), width = 0.2) +
  theme_prism(axis_text_angle = 45, base_size = 12, base_family = "Arial") + scale_fill_manual(values = c("#35FFB8", "#FFFFFF", "#81B3EB","#FFCDB0", "#FFEDFF")) +
  theme(legend.position = "none")
print(ggplot_diffuse_pTau)


#######################################################################################################################################


#####STEP 4: Visualise data - percentage of pTau area vs pTau marker for fibrillar plaque ONLY

###Filter group data frame to only include fibrillar plaque
fibrillar_pTau <- filter(summary_pTau, Classification == "Fibrillar")
View(fibrillar_pTau)
print(fibrillar_pTau)

###Descriptive statistics for data frame - Area % vs pTau markers for fibrillar plaque ONLY
group_fibrillar_pTau <- fibrillar_pTau |> group_by(`pTau Marker`) |>
  summarise(N=n(), Mean=mean(`Area %`), SD=sd(`Area %`), SEM=sd(`Area %`/sqrt(n())), 
            ymin=mean(`Area %`)-sd(`Area %`)/sqrt(n()), ymax=mean(`Area %`)+sd(`Area %`)/sqrt(n()))
View(group_fibrillar_pTau)
print(group_fibrillar_pTau)


###For bar plot:
ggplot_fibrillar_pTau <- ggplot(fibrillar_pTau, aes(x=factor(`pTau Marker`, levels = pTau_levels), y=`Area %`)) +
  labs(title="Fibrillar plaques \n", x="", y="Mean area of pTau \ndetected within a plaque %") +
  geom_point(fibrillar_pTau, mapping=aes(x=factor(`pTau Marker`, levels = pTau_levels), y=`Area %`, shape="point"), position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), alpha = 0.2) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  geom_col(group_fibrillar_pTau, mapping=aes(x=factor(`pTau Marker`, levels = pTau_levels), y=Mean, fill=c("#81B3EB", "#FFCDB0", "#FFEDFF","#FFFFFF", "#35FFB8")), colour="black", position=position_dodge(0.8), width = 0.8) + 
  geom_errorbar(group_fibrillar_pTau, mapping=aes(x=factor(`pTau Marker`, levels = pTau_levels), y=Mean, ymin = ymin, ymax = ymax), position=position_dodge(0.8), width = 0.2) +
  theme_prism(axis_text_angle = 45, base_size = 12, base_family = "Arial") + scale_fill_manual(values = c("#35FFB8", "#FFFFFF", "#81B3EB","#FFCDB0", "#FFEDFF")) +
  theme(legend.position = "none")
print(ggplot_fibrillar_pTau)


#####################################################################################################################################


#####STEP 5: Visualise data - percentage of pTau area vs pTau marker for cored plaque ONLY

###Filter group data frame to only include cored plaque
cored_pTau <- filter(summary_pTau, Classification == "Cored")
View(cored_pTau)
print(cored_pTau)

###Descriptive statistics for data frame - Area % vs pTau markers for cored plaque ONLY
group_cored_pTau <- cored_pTau |> group_by(`pTau Marker`) |>
  summarise(N=n(), Mean=mean(`Area %`), SD=sd(`Area %`), SEM=sd(`Area %`/sqrt(n())), 
            ymin=mean(`Area %`)-sd(`Area %`)/sqrt(n()), ymax=mean(`Area %`)+sd(`Area %`)/sqrt(n()))
View(group_cored_pTau)
print(group_cored_pTau)


###For bar plot:
ggplot_cored_pTau <- ggplot(cored_pTau, aes(x=factor(`pTau Marker`, levels = pTau_levels), y=`Area %`)) +
  labs(title="Cored plaques \n", x="", y="Mean area of pTau \ndetected within a plaque %") +
  geom_point(cored_pTau, mapping=aes(x=factor(`pTau Marker`, levels = pTau_levels), y=`Area %`, shape="point"), position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), alpha = 0.2) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  geom_col(group_cored_pTau, mapping=aes(x=factor(`pTau Marker`, levels = pTau_levels), y=Mean, fill=c("#81B3EB", "#FFCDB0", "#FFEDFF","#FFFFFF", "#35FFB8")), colour="black", position=position_dodge(0.8), width = 0.8) + 
  geom_errorbar(group_cored_pTau, mapping=aes(x=factor(`pTau Marker`, levels = pTau_levels), y=Mean, ymin = ymin, ymax = ymax), position=position_dodge(0.8), width = 0.2) +
  theme_prism(axis_text_angle = 45, base_size = 12, base_family = "Arial") + scale_fill_manual(values = c("#35FFB8", "#FFFFFF", "#81B3EB","#FFCDB0", "#FFEDFF")) +
  theme(legend.position = "none")
print(ggplot_cored_pTau)

