library(dplyr)
library(vegan)
library(car)
#-- packages for SEM --
library(piecewiseSEM)
library(nlme)
library(lavaan)
library(lme4)

#---- Load data ----
data = read.csv('./Empirical_moth_data.csv',header = TRUE)
data$Elev_up_std <- NA
data$Elev_low_std <- NA
data$Elev_mid_std <- NA
data$RS_std <- NA
for (loc in c('Malaysia', 'Taiwan', 'China')){
  data_std = data[(data$Location == loc),]
  if(loc == 'Malaysia'){
    vec_up_relative = data_std$Elev_up/1959
    vec_low_relative = data_std$Elev_low/1959
    vec_rs_relative = data_std$RS/1959
    vec_mid_relative = data_std$Elev_mid/1959
  }else if(loc == 'Taiwan'){
    vec_up_relative = data_std$Elev_up/3140
    vec_low_relative = data_std$Elev_low/3140
    vec_rs_relative = data_std$RS/3140
    vec_mid_relative = data_std$Elev_mid/3140
  }else if(loc == 'China'){
    vec_up_relative = data_std$Elev_up/4152
    vec_low_relative = data_std$Elev_low/4152
    vec_rs_relative = data_std$RS/4152
    vec_mid_relative = data_std$Elev_mid/4152
  }
  
  data[(data$Location == loc),]$Elev_up_std = vec_up_relative
  data[(data$Location == loc),]$Elev_low_std = vec_low_relative
  data[(data$Location == loc),]$RS_std = vec_rs_relative
  data[(data$Location == loc),]$Elev_mid_std = vec_mid_relative
}

#---- Fig. 5c: Lower limits SEM
# model: CTmax~ STmax+ STmin
m_CTmax = lmer(CTmax ~ STmax+ STmin+ (1|Location)+ (1|Family), data= data)
# model: CTmin~ STmax+ STmin+ Body length
m_CTmin = lmer(CTmin ~ STmax+ STmin+ B_length+ (1|Location)+ (1|Family), data= data)
# model: Lower distribution limits~ CTmax+ CTmin+ Body length
m_low = lmer(Elev_low_std ~ CTmax+ CTmin+ B_length+ (1|Location)+ (1|Family), data= data)
# Build SEM, including correlated errors
sem_low <- psem(m_CTmax, m_CTmin, m_low,
                CTmax %~~% CTmin,
                Elev_low_std %~~% STmax,
                Elev_low_std %~~% STmin,
                data = data)
summary(sem_low)
plot(sem_low)

#---- Fig. 5d: Upper limits SEM
# model: CTmax~ STmax+ STmin
m_CTmax = lmer(CTmax ~ STmax+ STmin+ (1|Location)+ (1|Family), data= data)
# model: CTmin~ STmax+ STmin+ Body length
m_CTmin = lmer(CTmin ~ STmax+ STmin+ B_length+ (1|Location)+ (1|Family), data= data)
# model: Upper distribution limits~ CTmax+ CTmin+ Body length
m_up = lmer(Elev_up_std ~ CTmax+ CTmin+ B_length+ (1|Location)+ (1|Family), data= data)
# Build SEM, including correlated errors
sem_up <- psem(m_CTmax, m_CTmin, m_up,
               CTmax %~~% CTmin,
               Elev_up_std %~~% STmax,
               Elev_up_std %~~% STmin,
               data = data)
summary(sem_up)
plot(sem_up)