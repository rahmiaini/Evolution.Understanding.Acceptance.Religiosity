################################################################################
#                                                                              #
#                    ABT MANUSCRIPT R CODE FOR ANALYSES                        #
#                                                                              #
################################################################################

# Needed packages for analyses:
###############################
library(tidyverse)
library(tidyr)
library(dplyr)
library(lme4)
library(nlme)
library(dplyr)
library(MASS)
library(gtsummary)
library(magrittr)
library(jtools)
library(huxtable)
library(broom.mixed)
library(readr)
library(WebPower) 
library(interactions)
library(ltm)
library(sjPlot)
library(TAM)
library(sjmisc)
library(emmeans)
library(data.table)
library(Hmisc)
library(ggpubr)
library(haven)

################################################################################
# 1)                                                                           #
# The survey was administrated to 11,995 undergraduate biology students.       #
#                                                                              #
# Students with missing data in the necessary variables for the analyses       #
# were removed.                                                                #
#                                                                              #
# Students are nested in courses (random effects). As recommended by           #
# Simmons et al (2011), participants in courses with less than 20 students     #
# were removed from the data.                                                  #
#                                                                              #
# Provided data shows these reductions and has an n = 11,786                   #
################################################################################

# Load up the data to be used in the analyses using the file titled "24_03_08_supplemental_data.csv"
# Check "yes" for headings and rename the file "data".
################################################################################

################################################################################
# 2)                                                                           #
# Below tests for the best fitting mixed effects models by comparing AIC       #
# values as recommended by Theobald (2018).                                    #
#                                                                              #
# Models where the difference in AIC values (ΔAIC) are =< 2 are considered to  #
# have equivalent fit.                                                         #
#                                                                              #
# If ΔAIC values in two or more models is =< 2, the model with the fewest      #
# parameters is selected as the best fitting model.                            #
################################################################################

# 2.1) TEST ONE:
# OUTCOME VARIABLE: Microevolution acceptance
# PREDICTOR VARIABLES: Interaction between evolution understanding (evound) and 
#                      raw religiosity score (rel)
################################################################################

# No nested / mixed effects variables
mod1 <- lm(micro ~ evound * rel, data = data)
# Nested by course, instructor, and institution
mod2 <- lmer(micro ~ evound * rel + (1|course) + (1|instructor) + (1|institution), data = data)
# Nested by course and instructor
mod3 <- lmer(micro ~ evound * rel + (1|course) + (1|instructor), data = data)
# Nested by course and institution
mod4 <- lmer(micro ~ evound * rel + (1|course) + (1|institution), data = data)
# Nested by instructor and institution
mod5 <- lmer(micro ~ evound * rel + (1|instructor) + (1|institution), data = data)
# Nested by course only
mod6 <- lmer(micro ~ evound * rel + (1|course), data = data)
# Nested by instructor only
mod7 <- lmer(micro ~ evound * rel + (1|instructor), data = data)
# Nested by institution only
mod8 <- lmer(micro ~ evound * rel + (1|institution), data = data)

# COMPARE ALL MODELS
####################
AIC(mod1)
AIC(mod2)
AIC(mod3)
AIC(mod4)
AIC(mod5)
AIC(mod6)
AIC(mod7)
AIC(mod8)

# Lowest AIC with fewest parameters = mod6

# 2.2) TEST TWO:
# OUTCOME VARIABLE: Macroevolution acceptance
# PREDICTOR VARIABLES: Interaction between evolution understanding (evound) and 
#                      raw religiosity score (rel)
################################################################################

# No nested / mixed effects variables
mod9 <- lm(macro ~ evound * rel, data = data)
# Nested by course, instructor, and institution
mod10 <- lmer(macro ~ evound * rel + (1|course) + (1|instructor) + (1|institution), data = data)
# Nested by course and instructor
mod11 <- lmer(macro ~ evound * rel + (1|course) + (1|instructor), data = data)
# Nested by course and institution
mod12 <- lmer(macro ~ evound * rel + (1|course) + (1|institution), data = data)
# Nested by instructor and institution
mod13 <- lmer(macro ~ evound * rel + (1|instructor) + (1|institution), data = data)
# Nested by course only
mod14 <- lmer(macro ~ evound * rel + (1|course), data = data)
# Nested by instructor only
mod15 <- lmer(macro ~ evound * rel + (1|instructor), data = data)
# Nested by institution only
mod16 <- lmer(macro ~ evound * rel + (1|institution), data = data)

# COMPARE ALL MODELS
####################
AIC(mod9)
AIC(mod10)
AIC(mod11)
AIC(mod12)
AIC(mod13)
AIC(mod14)
AIC(mod15)
AIC(mod16)

# Lowest AIC with fewest parameters = mod12


# 2.3) TEST THREE:
# OUTCOME VARIABLE: Human evolution acceptance
# PREDICTOR VARIABLES: Interaction between evolution understanding (evound) and 
#                      raw religiosity score (rel)
################################################################################

# No nested / mixed effects variables
mod17 <- lm(human ~ evound * rel, data = data)
# Nested by course, instructor, and institution
mod18 <- lmer(human ~ evound * rel + (1|course) + (1|instructor) + (1|institution), data = data)
# Nested by course and instructor
mod19 <- lmer(human ~ evound * rel + (1|course) + (1|instructor), data = data)
# Nested by course and institution
mod20 <- lmer(human ~ evound * rel + (1|course) + (1|institution), data = data)
# Nested by instructor and institution
mod21 <- lmer(human ~ evound * rel + (1|instructor) + (1|institution), data = data)
# Nested by course only
mod22 <- lmer(human ~ evound * rel + (1|course), data = data)
# Nested by instructor only
mod23 <- lmer(human ~ evound * rel + (1|instructor), data = data)
# Nested by institution only
mod24 <- lmer(human ~ evound * rel + (1|institution), data = data)

# COMPARE ALL MODELS
####################
AIC(mod17)
AIC(mod18)
AIC(mod19)
AIC(mod20)
AIC(mod21)
AIC(mod22)
AIC(mod23)
AIC(mod24)

# Lowest AIC with fewest parameters = mod20


################################################################################
# 3)                                                                           #
# Below regressions and plots test to what extent the predictor variables:     #
# evolution understanding (evound) and religiosity (rel) have on the outcome   #
# variables: microevolution acceptance (micro), macroevolution acceptance      #
# (macro) and human evolution acceptance (human).                              #
#                                                                              #
# Some regressions in this section also test for an interaction effect between #
# religiosity and evolution understanding.                                     #
################################################################################

# 3.1)
# What is the interaction between evolution understanding and raw religiosity
# scores as predictors of the three contexts of evolution?
################################################################################

mic.mod <- lmer(micro ~ evound * rel + (1|course) + (1|institution), data = data)
mac.mod <- lmer(macro ~ evound * rel + (1|course) + (1|institution), data = data)
hum.mod <- lmer(human ~ evound * rel + (1|course) + (1|institution), data = data)

tab_model(mic.mod, mac.mod, hum.mod)

# 3.2)
# Simple Slope Analysis Regressions to explore the relationships between 
# evolution understanding and acceptance of three contexts of evolution in all
# religiosity groups.
################################################################################

#Create data frame for only low, moderate, and high religiosity groups
lowrel.data <- dplyr::filter(data, religiosity == "low")
modrel.data <- dplyr::filter(data, religiosity == "moderate")
highrel.data <- dplyr::filter(data, religiosity == "high")

#low religiosity students only
lowrelSS.mic <- lmer(micro ~ evound + (1|course) + (1|institution), data = lowrel.data)
lowrelSS.mac <- lmer(macro ~ evound + (1|course) + (1|institution), data = lowrel.data)
lowrelSS.hum <- lmer(human ~ evound + (1|course) + (1|institution), data = lowrel.data)

#moderate religiosity students only
modrelSS.mic <- lmer(micro ~ evound + (1|course) + (1|institution), data = modrel.data)
modrelSS.mac <- lmer(macro ~ evound + (1|course) + (1|institution), data = modrel.data)
modrelSS.hum <- lmer(human ~ evound + (1|course) + (1|institution), data = modrel.data)

#high religiosity students only
highrelSS.mic <- lmer(micro ~ evound + (1|course) + (1|institution), data = highrel.data)
highrelSS.mac <- lmer(macro ~ evound + (1|course) + (1|institution), data = highrel.data)
highrelSS.hum <- lmer(human ~ evound + (1|course) + (1|institution), data = highrel.data)

#View analysis results in tables
tab_model(lowrelSS.mic, lowrelSS.mac, lowrelSS.hum)
tab_model(modrelSS.mic, modrelSS.mac, modrelSS.hum)
tab_model(highrelSS.mic, highrelSS.mac, highrelSS.hum)

# 3.3)
# Create plots to visualize the simple slope analyses above. (fig 2 in article)
################################################################################

# Create regression models using the category version of the religiosity
# variable, not the raw religiosity scores
mic.plot.mod <- lmer(micro ~ evound * religiosity + (1|course) + (1|institution), data = data)
mac.plot.mod <- lmer(macro ~ evound * religiosity + (1|course) + (1|institution), data = data)
hum.plot.mod <- lmer(human ~ evound * religiosity + (1|course) + (1|institution), data = data)

# Create microevolution acceptance plot (fig 2a)
mic.plot<-interact_plot(mic.plot.mod,
                        pred = evound,
                        modx = religiosity,               
                        interval = TRUE)+ggplot2::theme(legend.position = c(.95, .05),
                                                        legend.justification = c("right", "bottom"),
                                                        legend.box.just = "right",
                                                        legend.margin = margin(6, 6, 6, 6))+ scale_y_continuous(limits = c(1, 5), breaks = c(1,2,3,4,5))+
  labs(
    title = "A",
    x = "Evolution Understanding",
    y = "Microevolution Acceptance")

# Create macroevolution acceptance plot (fig 2b)
mac.plot <- interact_plot(mac.plot.mod,
                          pred = evound,
                          modx = religiosity, 
                          interval = TRUE)+ggplot2::theme(legend.position = "none")+ scale_y_continuous(limits = c(1, 5), breaks = c(1,2,3,4,5))+
  labs(
    title = "B",
    x = "Evolution Understanding",
    y = "Macroevolution Acceptance")

# Create human evolution acceptance plot (fig 2c)
hum.plot<-interact_plot(hum.plot.mod,
                        pred = evound,
                        modx = religiosity,
                        interval = TRUE)+ggplot2::theme(legend.position = "none")+ scale_y_continuous(limits = c(1, 5), breaks = c(1,2,3,4,5))+
  labs(
    title = "C",
    x = "Evolution Understanding",
    y = "Human Evolution Acceptance")

# combine above plots into a three-paneled figure
ggarrange(mic.plot, mac.plot, hum.plot,
          ncol = 3, nrow = 1 )


################################################################################
# 4)                                                                           #
# Below regressions and plots test to what extent the predictor variables:     #
# evolution understanding (evound) and religiosity (rel) have on the outcome   #
# variables: average acceptance of common ancestry related macroevolution      #
# items (ca) and average acceptance of non-common ancestry related             #
# macroevolution items (non.ca)                                                #
################################################################################

# 4.1)
# What is the interaction between evolution understanding and raw religiosity
# scores as predictors of acceptance of the common ancestry macroevolution items
# and acceptance of the non-common ancestry macroevolution items?
################################################################################

# Create a new data frame with common ancestry macroevolution items listed 
# together
ca.data <- data[,c(1:39,41:43,40,44:68)]
# create new variables: average acceptance of common ancestry related
# macroevolution items and non common ancestry 
ca.data$ca <- rowMeans(ca.data[43:44])
ca.data$non.ca <- rowMeans(ca.data[37:42])

# regressions: test for interaction effects
ca.mod <- lmer(ca ~ evound * rel + (1|course) + (1|institution), data = ca.data)
non.ca.mod <-lmer(non.ca ~ evound * rel + (1|course) + (1|institution), data = ca.data)

#view regression tables
tab_model(ca.mod, non.ca.mod)

# 4.2)
# Simple Slope Analysis Regressions to explore the relationships between 
# evolution understanding and acceptance of common ancestry items and non 
# common ancestry items.
################################################################################

# Simple slope analyses for common ancestry and non common ancestry
# Create data frames with only low, moderate, and high religiosity groups
ca.data.low <- dplyr::filter(ca.data, religiosity == "low")
ca.data.mod <- dplyr::filter(ca.data, religiosity == "moderate")
ca.data.high <- dplyr::filter(ca.data, religiosity == "high")

#low religiosity students only
lowrelSS.ca <- lmer(ca ~ evound + (1|course) + (1|institution), data = ca.data.low)
lowrelSS.non.ca <- lmer(non.ca ~ evound + (1|course) + (1|institution), data = ca.data.low)

#moderate religiosity students only
modrelSS.ca <- lmer(ca ~ evound + (1|course) + (1|institution), data = ca.data.mod)
modrelSS.non.ca <- lmer(non.ca ~ evound + (1|course) + (1|institution), data = ca.data.mod)

#high religiosity students only
highrelSS.ca <- lmer(ca ~ evound + (1|course) + (1|institution), data = ca.data.high)
highrelSS.non.ca <- lmer(non.ca ~ evound + (1|course) + (1|institution), data = ca.data.high)

#View analysis results in tables
tab_model(lowrelSS.ca, lowrelSS.non.ca)
tab_model(modrelSS.ca, modrelSS.non.ca)
tab_model(highrelSS.ca, highrelSS.non.ca)

# 4.3)
# Create plots to visualize the simple slope analyses above.
############################################################

# Create regression models using the category version of the religiosity
# variable, not the raw religiosity scores
ca.plot.mod <- lmer(ca ~ evound * religiosity + (1|course) + (1|institution), data = ca.data)
non.ca.plot.mod <- lmer(non.ca ~ evound * religiosity + (1|course) + (1|institution), data = ca.data)

# Create microevolution acceptance plot (fig 4b)
ca.plot<-interact_plot(ca.plot.mod,
                        pred = evound,
                        modx = religiosity,               
                        interval = TRUE)+ggplot2::theme(legend.position = "none") + scale_y_continuous(limits = c(1, 5), breaks = c(1,2,3,4,5))+
  labs(
    title = "B",
    x = "Evolution Understanding",
    y = "Acceptance of Common Ancestry Items")

# Create macroevolution acceptance plot (fig 4a)
non.ca.plot <- interact_plot(non.ca.plot.mod,
                          pred = evound,
                          modx = religiosity, 
                          interval = TRUE)+ggplot2::theme(legend.position = c(.95, .05),
                                                          legend.justification = c("right", "bottom"),
                                                          legend.box.just = "right",
                                                          legend.margin = margin(6, 6, 6, 6))+ scale_y_continuous(limits = c(1, 5), breaks = c(1,2,3,4,5))+
  labs(
    title = "A",
    x = "Evolution Understanding",
    y = "Acceptance of Non-Common Ancestry Macro Item")


# combine above plots into a three-paneled figure
ggarrange(non.ca.plot, ca.plot,
          ncol = 2, nrow = 1 )




################################################################################
# 5)                                                                           #
# Descriptive statistics                                                       #
################################################################################

# Mean acceptance of entire sample
mean.micro <- mean(data$micro)
mean.macro <- mean(data$macro)
mean.human <- mean(data$human)
# Standard deviation of religiosity in entire sample
sd.micro <- sd(data$micro)
sd.macro <- sd(data$macro)
sd.human <- sd(data$human)

# 5.1)
# Create box plots that shows mean acceptance of evolution. Also disagregated 
# by religiosity group.
########################################################################

##### Create box plot for all data set #####

# Convert data to long format
data_long <- tidyr::gather(data[,12:14], key = "Question", value = "Rating")

# Reorder the levels of the Question variable
data_long$Question <- factor(data_long$Question, levels = c("micro", "macro", "human"))

# Create boxplot figure
ggplot(data_long, aes(x = Question, y = Rating, fill = Question)) +
  geom_boxplot() +
  labs(y = "Median Evolution Acceptance") +
  scale_fill_grey(start = 0.9, end = 0.9) +
  stat_summary(fun = "mean", geom = "point", shape = 20, size = 4, color = "black") +
  theme_minimal()

##### Create box plot by religiosity #####

# Reorder the data
data$religiosity <- factor(data$religiosity, levels = c("low", "moderate", "high"))

# Create the microevolution plot
mic.box <- 
  ggplot(data, aes(x = religiosity, y = micro, fill = religiosity)) +
  geom_boxplot() +
  labs(title = "A",
       x = "Religiosity", 
       y = "Microevolution Acceptance",) +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 14),
    axis.title.y = element_text(vjust = 2.5),
    axis.title.x = element_text(vjust = -.6),
    axis.text.x = element_text(size = 12),
    legend.position = "none") +
  scale_fill_grey(start = 0.4, end = 0.9)

# Create the macroevolution plot
mac.box <-
  ggplot(data, aes(x = religiosity, y = macro, fill = religiosity)) +
  geom_boxplot() +
  labs(title = "B",
       x = "Religiosity", 
       y = "Macroevolution Acceptance") +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 14),
    axis.title.y = element_text(vjust = 2.5),
    axis.title.x = element_text(vjust = -.6),
    axis.text.x = element_text(size = 12),
    legend.position = "none") +
  scale_fill_grey(start = 0.4, end = 0.9)

# Create the human evolution plot
hum.box <-
  ggplot(data, aes(x = religiosity, y = human, fill = religiosity)) +
  geom_boxplot() +
  labs(title = "C",
    x = "Religiosity", 
    y = "Human Evolution Acceptance",
    fill = "reli") +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 14),
    axis.title.y = element_text(vjust = 2.5),
    axis.title.x = element_text(vjust = -.6),
    axis.text.x = element_text(size = 12),
    legend.position = "none") +
  scale_fill_grey(start = 0.4, end = 0.9)

# Put all three plots in one figure
ggarrange(mic.box, mac.box, hum.box,
          ncol = 3, nrow = 1 )

# 5.2)
# Religiosity
############################################

# Mean religiosity of entire sample
mean(data$rel)
# Standard deviation of religiosity in entire sample
sd(data$rel)

# Mean religiosity of each religiosity group
mean(lowrel.data$rel)
mean(modrel.data$rel)
mean(highrel.data$rel)

#Standard deviation of religiosity in each religiosity group
sd(lowrel.data$rel)
sd(modrel.data$rel)
sd(highrel.data$rel)

# 5.3)
# Evolution Acceptance in the whole sample
############################################

mean(data$micro)
mean(data$macro)
mean(data$human)

sd(data$micro)
sd(data$macro)
sd(data$human)

# 5.4)
# Evolution Acceptance in each religiosity group
################################################

# Mean microevolution acceptance in each religiosity group
mean(lowrel.data$micro)
mean(modrel.data$micro)
mean(highrel.data$micro)

# Standard deviation of microevolution acceptance in each religiosity group
sd(lowrel.data$micro)
sd(modrel.data$micro)
sd(highrel.data$micro)

# Mean macroevolution acceptance in each religiosity group
mean(lowrel.data$macro)
mean(modrel.data$macro)
mean(highrel.data$macro)

# Standard deviation of macroevolution acceptance in each religiosity group
sd(lowrel.data$macro)
sd(modrel.data$macro)
sd(highrel.data$macro)

# Human evolution acceptance in each religiosity group
mean(lowrel.data$human)
mean(modrel.data$human)
mean(highrel.data$human)

# Standard deviation of human evolution acceptance in each religiosity group
sd(lowrel.data$human)
sd(modrel.data$human)
sd(highrel.data$human)

# 5.5
# Create a bar plot that shows acceptance of each macroevolution item
#####################################################################

macro.data <- data[,35:42]
data.long <- gather(macro.data)

ggplot(data.long, aes(x = key, fill = factor(value))) +
  geom_bar(position = "fill") +
  labs(
    x = "Macroevolution Acceptance Items",
    y = "Proportion",
    fill = "Item Responses") +
  scale_fill_grey(start = 0, end = 0.9) +
  theme_minimal()












