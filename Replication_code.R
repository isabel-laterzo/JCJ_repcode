########################################################
##### JPV Survey Regressions: Experimental Analysis ####
########################################################

##################################################################################
#################################### SET UP ######################################
##################################################################################
rm(list = ls())
setwd("/Users/il6279/Dropbox/Research Projects/GarciaPonce_Laterzo_Experiment/Models/Main Analysis")
pacman::p_load(stargazer, tidyverse, ggplot2)

data <- readRDS("/Users/il6279/Dropbox/Research Projects/GarciaPonce_Laterzo_Experiment/Models/data_1_18_22.rds")

#libraries
library(stargazer)
library(tidyverse)


################################################################################
##################### Modify Data and Divide into Subgroups ####################
################################################################################

#indicator for average of blame for gov & society
data$blame_ext <- (data$blame_gov + data$blame_soc)/2

#divide by homicide median
high_hom <- data %>% filter(hom_100k > summary(data$hom_100k)[3])
low_hom <- data %>% filter(hom_100k <= summary(data$hom_100k)[3])


################################################################################
########################## EXPERIMENTAL MODELS #################################
################################################################################

#### 
#### Part 1: Blame for Perpetrator/internal blame
#### 

# 1) blame for Rodrigo with continuous crime severity
mod_rod <- lm(blame_rod ~ x1 + x2_cont + x3 + x4, data = data)

# num. of sds from mean for low --> mid class
mod_rod$coefficients[2]/sd(data$blame_rod, na.rm = T)

# num. of sds from mean for follower --> leader
mod_rod$coefficients[6]/sd(data$blame_rod, na.rm = T)

# 2) blame for Rodrigo with continuous crime severity PLUS marginalization
mod_rod_marg <- lm(blame_rod ~ x1 + x2_cont + x3 + x4 + indice_marginacion, data = data)


# 3) blame for Rodrigo with continuous crime severity PLUS SES status
mod_rod_ses <- lm(blame_rod ~ x1 + x2_cont + x3 + x4 + ses, data = data)



#not sure if these are

#4) blame for rod in high hom areas
mod_rod_highhom <- lm(blame_rod ~ x1 + x2_cont + x3 + x4, data = high_hom)

# num. of sds from mean for low --> mid class
mod_rod_highhom$coefficients[2]/sd(data$blame_rod, na.rm = T)

#5) blame for rod in low hom areas
mod_rod_lowhom <- lm(blame_rod ~ x1 + x2_cont + x3 + x4, data = low_hom)

# num. of sds from mean for low --> mid class
mod_rod_lowhom$coefficients[2]/sd(data$blame_rod, na.rm = T)

#6) blame for rod in high hom areas with marginalization
mod_rod_highhom_marg <- lm(blame_rod ~ x1 + x2_cont + x3 + x4 + indice_marginacion, data = high_hom)

#7) blame for rod in low hom areas with marginalization
mod_rod_lowhom_marg <- lm(blame_rod ~ x1 + x2_cont + x3 + x4 + indice_marginacion, data = low_hom)

#8) blame for rod in high hom areas with ses
mod_rod_highhom_ses <- lm(blame_rod ~ x1 + x2_cont + x3 + x4 + ses, data = high_hom)

#9) blame for rod in low hom areas with ses
mod_rod_lowhom_ses <- lm(blame_rod ~ x1 + x2_cont + x3 + x4 + ses, data = low_hom)

#Table 2 - Blame for Perpetrator: Pooled Results
stargazer(mod_rod, mod_rod_marg, mod_rod_ses,
          covariate.labels = c("Class: Middle",
                               "Crime Severity",
                               "Victim: Worker",
                               "\\hspace{1cm}Local Politician",
                               "Perpetrator: Gang Leader",
                               "Marginalization",
                               "SES",
                               "Constant"),
          no.space = TRUE,
          model.numbers = FALSE,
          dep.var.labels.include = FALSE,
          font.size = "tiny",
          title = "Internal Blame (Pooled Results)")



#shorten names for stargazer
mod_r1 <- mod_rod_highhom
mod_r2 <- mod_rod_lowhom
mod_r3 <- mod_rod_highhom_marg
mod_r4 <-  mod_rod_lowhom_marg
mod_r5 <- mod_rod_highhom_ses
mod_r6 <-  mod_rod_lowhom_ses

#Table 3 - Blame for Perpetrator: Divided by Homicide Level
stargazer(mod_r1, mod_r2, mod_r3, 
          mod_r4, mod_r5, mod_r6,
          covariate.labels = c("Class: Middle",
                               "Crime Severity",
                               "Victim: Worker",
                               "\\hspace{1cm}Local Politician",
                               "Perpetrator: Gang Leader",
                               "Marginalization Index",
                               "SES",
                               "Constant"),
          no.space = TRUE,
          dep.var.labels.include = FALSE,
          font.size = "tiny",
          title = "Internal Blame: Divided by Community Homicide Level",
          column.labels = c("High Homicide", "Low Homicide",
                            "High Homicide", "Low Homicide",
                            "High Homicide", "Low Homicide"))




##### Predicted probabilities for marginalization and SES

### Marginalization vs. Blame
new_data1 <- with(data, data.frame(x1 = "Lower Class",
                                   x2_cont = median(data$x2_cont),
                                   x3 = "Worker",
                                   x4 = "Follower",
                                   indice_marginacion = seq(min(data$indice_marginacion),
                                                            max(data$indice_marginacion),
                                                            length = 100)))

preds <- predict(mod_rod_marg, newdata = new_data1, type = "response",
                 se.fit = TRUE)  
new_data1$predf <- preds$fit
new_data1$lower <- preds$fit - (1.645*preds$se.fit) #90% CI
new_data1$upper <- preds$fit + (1.645*preds$se.fit) #90% CI


#Fig 1 - left panel
ggplot(new_data1, aes(x = indice_marginacion, y = predf)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  theme_bw() +
  xlab("Marginalization Index") +
  ylab("Blame Predicted Values") + 
  theme(text = element_text(size = 18))


### SES vs. Blame
new_data2 <- with(data, data.frame(x1 = "Lower Class",
                                   x2_cont = median(data$x2_cont),
                                   x3 = "Worker",
                                   x4 = "Follower",
                                   ses = seq(min(data$ses), max(data$ses),
                                             length = 100)))

preds2 <- predict(mod_rod_ses, newdata = new_data2, type = "response",
                  se.fit = TRUE)  
new_data2$predf <- preds2$fit
new_data2$lower <- preds2$fit - (1.645*preds2$se.fit) #90% CI
new_data2$upper <- preds2$fit + (1.645*preds2$se.fit) #90% CI


# Fig 1 - right panel
ggplot(new_data2, aes(x = ses, y = predf)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  theme_bw() +
  xlab("SES Index") +
  ylab("Blame Predicted Values") + 
  theme(text = element_text(size = 18))




#Predicted probabilities

##### 1) Class of Perp

#vary class: high homicide
newdata_high1 <- with(high_hom, data.frame(x1 = c("Lower Class", "Middle Class"),
                                           x2_cont = median(high_hom$x2_cont),
                                           x3 = "Businessman",
                                           x4 = "Leader"))
pred_high1 <- as.data.frame(predict(mod_rod_highhom, newdata_high1,
                                    type = "response",
                                    interval = "confidence",
                                    level = 0.9))

pred_high1$class <- c("Lower", "Upper")
pred_high1$hom <- "High"

#vary class: low homicide
newdata_low1 <- with(low_hom, data.frame(x1 = c("Lower Class", "Middle Class"),
                                         x2_cont = median(low_hom$x2_cont),
                                         x3 = "Businessman",
                                         x4 = "Leader"))
pred_low1 <- as.data.frame(predict(mod_rod_lowhom, newdata_low1,
                                   type = "response",
                                   interval = "confidence",
                                   level = 0.9))

pred_low1$class <- c("Lower", "Upper")
pred_low1$hom <- "Low"


## bind hom preds together
hom_preds <- rbind(pred_high1, pred_low1)

# Figure 2 - Right Panel
ggplot(hom_preds, aes(x = hom, y = fit, color = class)) +
  geom_point() +
  geom_errorbar(aes(y = fit, 
                    ymin = lwr,
                    ymax = upr,),
                alpha = 0.7,
                lwd = 1,
                width = 0.15) +
  labs(x = "Homicide Level",
       y = "Predicted Blame Value",
       color = "Perp. Class") +
  theme_bw() +
  theme(text = element_text(size = 18),
        legend.position = "bottom") +
  ylim(4.5, 6)

##### 2) Perpetrator Position
#vary position: high homicide
newdata_high3 <- with(high_hom, data.frame(x1 = c("Lower Class"),
                                           x2_cont = median(x2_cont),
                                           x3 = "Businessman",
                                           x4 = c("Leader", "Follower")))

pred_high3 <- as.data.frame(predict(mod_rod_highhom, newdata_high3,
                                    type = "response",
                                    interval = "confidence",
                                    level = 0.9))

pred_high3$position <- c("Leader", "Follower")
pred_high3$hom <- "High"

#vary position: low homicide
newdata_low3 <- with(low_hom, data.frame(x1 = c("Lower Class"),
                                         x2_cont = median(x2_cont),
                                         x3 = "Businessman",
                                         x4 = c("Leader", "Follower")))

pred_low3 <- as.data.frame(predict(mod_rod_lowhom, newdata_low3,
                                   type = "response",
                                   interval = "confidence",
                                   level = 0.9))

pred_low3$position <- c("Leader", "Follower")
pred_low3$hom <- "Low"

## bind hom preds together
hom_preds3 <- rbind(pred_high3, pred_low3)

# Figure 2 - Left Panel
ggplot(hom_preds3, aes(x = hom, y = fit, color = position)) +
  geom_point() +
  geom_errorbar(aes(y = fit, 
                    ymin = lwr,
                    ymax = upr),
                alpha = 0.7,
                lwd = 1,
                width = 0.15) + 
  labs(x = "Homicide Level",
       y = "Predicted Blame Value",
       color = "Perp. Position") +
  theme_bw() +
  theme(text = element_text(size = 18),
        legend.position = "bottom")+
  ylim(4.5, 6)




##### 3) Crime Severity
#vary severity: high homicide
newdata_high2 <- with(high_hom, data.frame(x1 = c("Lower Class"),
                                           x2_cont = seq(min(x2_cont),
                                                         max(x2_cont),
                                                         length = 100),
                                           x3 = "Businessman",
                                           x4 = "Leader"))
pred_high2 <- as.data.frame(predict(mod_rod_highhom, newdata_high2,
                                    type = "response",
                                    interval = "confidence",
                                    level = 0.9))

pred_high2$severity <- seq(min(high_hom$x2_cont),
                           max(high_hom$x2_cont),
                           length = 100)
pred_high2$hom <- "High"


#vary class: low homicide
newdata_low2 <- with(low_hom, data.frame(x1 = c("Lower Class"),
                                         x2_cont = seq(min(x2_cont),
                                                       max(x2_cont),
                                                       length = 100),
                                         x3 = "Businessman",
                                         x4 = "Leader"))
pred_low2 <- as.data.frame(predict(mod_rod_lowhom, newdata_low2,
                                   type = "response",
                                   interval = "confidence",
                                   level = 0.9))
pred_low2$severity <- seq(min(low_hom$x2_cont),
                          max(low_hom$x2_cont),
                          length = 100)
pred_low2$hom <- "Low"


## bind hom preds together
hom_preds2 <- rbind(pred_high2, pred_low2)

# Figure 3
ggplot(hom_preds2, aes(x = severity, y = fit)) +
  geom_line() +
  facet_wrap(~hom) +
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr,),
              alpha = 0.5) + 
  labs(x = "Crime Severity",
       y = "Predicted Blame Value",
       fill = "Homicide Level") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  ylim(4.75, 6)


#### 
#### Part 2: Blame for Government & Society
#### 


# 1) blame  with continuous crime severity
mod_ext <- lm(blame_ext ~ x1 + x2_cont + x3 + x4, data = data)

# num. of sds from mean for follower --> leader
mod_ext$coefficients[6]/sd(data$blame_ext, na.rm = T)

# 2) blame with continuous crime severity PLUS marginalization
mod_ext_marg <- lm(blame_ext ~ x1 + x2_cont + x3 + x4 + indice_marginacion, data = data)

# 3) blame  with continuous crime severity PLUS SES status
mod_ext_ses <- lm(blame_ext ~ x1 + x2_cont + x3 + x4 + ses, data = data)

#4) blame  in high hom areas
mod_ext_highhom <- lm(blame_ext ~ x1 + x2_cont + x3 + x4, data = high_hom)

#5) blame  in low hom areas
mod_ext_lowhom <- lm(blame_ext ~ x1 + x2_cont + x3 + x4, data = low_hom)

#6) blame in high hom areas with marginalization
mod_ext_highhom_marg <- lm(blame_ext ~ x1 + x2_cont + x3 + x4 + indice_marginacion, data = high_hom)

#7) blame  in low hom areas with marginalization
mod_ext_lowhom_marg <- lm(blame_ext ~ x1 + x2_cont + x3 + x4 + indice_marginacion, data = low_hom)

#8) blame in high hom areas with ses
mod_ext_highhom_ses <- lm(blame_ext ~ x1 + x2_cont + x3 + x4 + ses, data = high_hom)

#9) blame in low hom areas with ses
mod_ext_lowhom_ses <- lm(blame_ext ~ x1 + x2_cont + x3 + x4 + ses, data = low_hom)


#Table 4 - Blame for External Actors: Pooled Results
stargazer(mod_ext, mod_ext_marg, mod_ext_ses,
          covariate.labels = c("Class: Middle",
                               "Crime Severity",
                               "Victim: Worker",
                               "\\hspace{1cm}Local Politician",
                               "Perpetrator: Gang Leader",
                               "Marginalization",
                               "SES",
                               "Constant"),
          no.space = TRUE,
          model.numbers = FALSE,
          dep.var.labels.include = FALSE,
          font.size = "tiny",
          title = "External Blame (Pooled Results)")


#shorten names for stargazer
mod_e1 <- mod_ext_highhom
mod_e2 <- mod_ext_lowhom
mod_e3 <- mod_ext_highhom_marg
mod_e4 <- mod_ext_lowhom_marg
mod_e5 <- mod_ext_highhom_ses
mod_e6 <- mod_ext_lowhom_ses

#Table 5 - Blame for External Actors: Divided by Homicide Level
stargazer(mod_e1, mod_e2, mod_e3, mod_e4, mod_e5, mod_e6,
          covariate.labels = c("Class: Middle",
                               "Crime Severity",
                               "Victim: Worker",
                               "\\hspace{1cm}Local Politician",
                               "Perpetrator: Gang Leader",
                               "Marginalization Index",
                               "SES",
                               "Constant"),
          no.space = TRUE,
          dep.var.labels.include = FALSE,
          font.size = "tiny",
          title = "Blame for External Actors (Divided by Community Homicide Levels)",
          column.labels = c("High Homicide", "Low Homicide",
                            "High Homicide", "Low Homicide",
                            "High Homicide", "Low Homicide"))



##### 1) Class of Perp

#vary class: high homicide
pred_ext_high1 <- as.data.frame(predict(mod_ext_highhom, newdata_high1,
                                        type = "response",
                                        interval = "confidence",
                                        level = 0.9))

pred_ext_high1$class <- c("Lower", "Upper")
pred_ext_high1$hom <- "High"

#vary class: low homicide
pred_ext_low1 <- as.data.frame(predict(mod_ext_lowhom, newdata_low1,
                                       type = "response",
                                       interval = "confidence",
                                       level = 0.9))

pred_ext_low1$class <- c("Lower", "Upper")
pred_ext_low1$hom <- "Low"


## bind hom preds together
hom_ext_preds <- rbind(pred_ext_high1, pred_ext_low1)

# FIgure 4 - right panel
ggplot(hom_ext_preds, aes(x = hom, y = fit, color = class)) +
  geom_point() +
  geom_errorbar(aes(y = fit, 
                    ymin = lwr,
                    ymax = upr,),
                alpha = 0.7,
                lwd = 1,
                width = 0.15) +
  labs(x = "Homicide Level",
       y = "Predicted External Blame Value",
       color = "Perp. Class") +
  theme_bw() +
  theme(text = element_text(size = 18),
        legend.position = "bottom") +
  ylim(4, 5.5)


##### 2) Perpetrator Position
#vary position: high homicide
pred_ext_high3 <- as.data.frame(predict(mod_ext_highhom, newdata_high3,
                                        type = "response",
                                        interval = "confidence",
                                        level = 0.9))

pred_ext_high3$position <- c("Leader", "Follower")
pred_ext_high3$hom <- "High"

#vary position: low homicide
pred_ext_low3 <- as.data.frame(predict(mod_ext_lowhom, newdata_low3,
                                       type = "response",
                                       interval = "confidence",
                                       level = 0.9))

pred_ext_low3$position <- c("Leader", "Follower")
pred_ext_low3$hom <- "Low"

## bind hom preds together
hom_ext_preds3 <- rbind(pred_ext_high3, pred_ext_low3)

# Figure 4 - left panel
ggplot(hom_ext_preds3, aes(x = hom, y = fit, color = position)) +
  geom_point() +
  geom_errorbar(aes(y = fit, 
                    ymin = lwr,
                    ymax = upr),
                alpha = 0.7,
                lwd = 1,
                width = 0.15) + 
  labs(x = "Homicide Level",
       y = "Predicted Blame Value",
       color = "Perp. Position") +
  theme_bw() +
  theme(text = element_text(size = 18),
        legend.position = "bottom") +
  ylim(4, 5.5)



##### 3) Crime Severity
#vary severity: high homicide
pred_ext_high2 <- as.data.frame(predict(mod_ext_highhom, newdata_high2,
                                        type = "response",
                                        interval = "confidence",
                                        level = 0.9))

pred_ext_high2$severity <- seq(min(high_hom$x2_cont),
                               max(high_hom$x2_cont),
                               length = 100)
pred_ext_high2$hom <- "High"


#vary class: low homicide
pred_ext_low2 <- as.data.frame(predict(mod_ext_lowhom, newdata_low2,
                                       type = "response",
                                       interval = "confidence",
                                       level = 0.9))
pred_ext_low2$severity <- seq(min(low_hom$x2_cont),
                              max(low_hom$x2_cont),
                              length = 100)
pred_ext_low2$hom <- "Low"


## bind hom preds together
hom_ext_preds2 <- rbind(pred_ext_high2, pred_ext_low2)

# Figure 5
ggplot(hom_ext_preds2, aes(x = severity, y = fit)) +
  geom_line() +
  facet_wrap(~hom) +
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr,),
              alpha = 0.5) + 
  labs(x = "Crime Severity",
       y = "Predicted Blame Value",
       fill = "Homicide Level") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  ylim(4, 5)



#### 
#### Part 3: Relative Blame
#### 

# 1) blame with continuous crime severity
mod_relat <- lm(blame3_norm ~ x1 + x2_cont + x3 + x4, data = data)

# 2) blame with continuous crime severity PLUS marginalization
mod_relat_marg <- lm(blame3_norm ~ x1 + x2_cont + x3 + x4 + indice_marginacion, data = data)

# 3) blame  with continuous crime severity PLUS SES status
mod_relat_ses <- lm(blame3_norm ~ x1 + x2_cont + x3 + x4 + ses, data = data)

#4) blame  in high hom areas
mod_relat_highhom <- lm(blame3_norm ~ x1 + x2_cont + x3 + x4, data = high_hom)

#5) blame in low hom areas
mod_relat_lowhom <- lm(blame3_norm ~ x1 + x2_cont + x3 + x4, data = low_hom)

#6) blame in high hom areas with marginalization
mod_relat_highhom_marg <- lm(blame3_norm ~ x1 + x2_cont + x3 + x4 + indice_marginacion, data = high_hom)

#7) blame  in low hom areas with marginalization
mod_relat_lowhom_marg <- lm(blame3_norm ~ x1 + x2_cont + x3 + x4 + indice_marginacion, data = low_hom)

#8) blame in high hom areas with ses
mod_relat_highhom_ses <- lm(blame3_norm ~ x1 + x2_cont + x3 + x4 + ses, data = high_hom)

#9) blame in low hom areas with ses
mod_relat_lowhom_ses <- lm(blame3_norm ~ x1 + x2_cont + x3 + x4 + ses, data = low_hom)


#Table 6 - Relative Blame: Pooled Results
stargazer(mod_relat, mod_relat_marg, mod_relat_ses,
          covariate.labels = c("Class: Middle",
                               "Crime Severity",
                               "Victim: Worker",
                               "\\hspace{1cm}Local Politician",
                               "Perpetrator: Gang Leader",
                               "Marginalization",
                               "SES",
                               "Constant"),
          no.space = TRUE,
          model.numbers = FALSE,
          dep.var.labels.include = FALSE,
          font.size = "tiny",
          title = "Relative Blame - Internal vs. External Blame (Pooled)")



#shorten names for stargazer
mod_re1 <- mod_relat_highhom
mod_re2 <- mod_relat_lowhom
mod_re3 <- mod_relat_highhom_marg
mod_re4 <-  mod_relat_lowhom_marg
mod_re5 <- mod_relat_highhom_ses
mod_re6 <-  mod_relat_lowhom_ses


#Table 7 - Relative Blame: Divided by Homicide Level
stargazer(mod_re1, mod_re2, mod_re3, mod_re4, mod_re5, mod_re6,
          covariate.labels = c("Class: Middle",
                               "Crime Severity",
                               "Victim: Worker",
                               "\\hspace{1cm}Local Politician",
                               "Perpetrator: Gang Leader",
                               "Marginalization Index",
                               "SES",
                               "Constant"),
          no.space = TRUE,
          dep.var.labels.include = FALSE,
          font.size = "tiny",
          title = "Relative Blame - Internal vs. External Blame (Divided by Homicide Levels)",
          column.labels = c("High Homicide", "Low Homicide",
                            "High Homicide", "Low Homicide",
                            "High Homicide", "Low Homicide"))







