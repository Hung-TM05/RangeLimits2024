library(dplyr)
library(vegan)
library(car)

all_data = read.csv('Empirical_moth_data.csv',header = TRUE)

all_data$Elev_up_std <- NA
all_data$Elev_low_std <- NA
all_data$Elev_mid_std <- NA
all_data$RS_std <- NA
for (loc in c('Malaysia', 'Taiwan', 'China')){
  df_std = all_data[(all_data$Location == loc),]
  if(loc == 'Malaysia'){
    vec_up_relative = df_std$Elev_up/1959
    vec_low_relative = df_std$Elev_low/1959
    vec_rs_relative = df_std$RS/1959
    vec_mid_relative = df_std$Elev_mid/1959
  }else if(loc == 'Taiwan'){
    vec_up_relative = df_std$Elev_up/3140
    vec_low_relative = df_std$Elev_low/3140
    vec_rs_relative = df_std$RS/3140
    vec_mid_relative = df_std$Elev_mid/3140
  }else if(loc == 'China'){
    vec_up_relative = df_std$Elev_up/4152
    vec_low_relative = df_std$Elev_low/4152
    vec_rs_relative = df_std$RS/4152
    vec_mid_relative = df_std$Elev_mid/4152
  }
  
  all_data[(all_data$Location == loc),]$Elev_up_std = vec_up_relative
  all_data[(all_data$Location == loc),]$Elev_low_std = vec_low_relative
  all_data[(all_data$Location == loc),]$RS_std = vec_rs_relative
  all_data[(all_data$Location == loc),]$Elev_mid_std = vec_mid_relative
}

#---- Fig.3c: Range Size SEM ----
library(piecewiseSEM)
library(nlme)
library(lavaan)
library(lme4)

data = all_data
# model: CTmax~ STmax+ STmin
m_CTmax = lmer(CTmax ~ STmax+ STmin+ (1|Location)+ (1|Family), data= data)
# model: CTmin~ STmax+ STmin+ Body length
m_CTmin = lmer(CTmin ~ STmax+ STmin+ B_length+ (1|Location)+ (1|Family), data= data)
# model: RS~ CTmax+ CTmin+ Body length
m_rs = lmer(RS_std ~ CTmax+ CTmin+ B_length+ (1|Location)+ (1|Family), data= data)
# Build SEM, including correlated errors
RS_all <- psem(m_rs, m_CTmax, m_CTmin,
               CTmax %~~% CTmin,
               RS_std %~~% STmax,
               RS_std %~~% STmin,
               data = data)
summary(RS_all)
# plot(RS_all)