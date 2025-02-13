library(dplyr)
library(vegan)
library(car)
library(lme4)
library(MuMIn)
library(nlme)
library(scales)
library(AICcmodavg)

df = read.csv('Empirical_moth_data.csv',header = TRUE)

df$Elev_up_std <- NA
df$Elev_low_std <- NA
df$Elev_mid_std <- NA
df$RS_std <- NA
for (loc in c('Malaysia', 'Taiwan', 'China')){
  df_std = df[(df$Location == loc),]
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
  
  df[(df$Location == loc),]$Elev_up_std = vec_up_relative
  df[(df$Location == loc),]$Elev_low_std = vec_low_relative
  df[(df$Location == loc),]$RS_std = vec_rs_relative
  df[(df$Location == loc),]$Elev_mid_std = vec_mid_relative
}

pdf(file = './Fig3a-b.pdf', width = 8, height = 4, onefile = TRUE)
par(mfrow=c(1,2))

#---- Fig.3a: STR - TTrange ----
# Create an empty plot with customized axes and limits
plot(x=df$STR, y=df$TTrange, las=1, cex= 1.5, 
     xlab = '', ylab = '',
     xlim = c(5,35), ylim = c(22,50), type="n", axes = F)
box() # Add a box around the plot
# Add x-axis with labeled tick marks
axis(side=1, at=seq(5,35,5),cex.axis=1)
mtext(side=1, line=3, "Seasonal temperature range (°C)", col="black", font=1,cex=1)
# Add y-axis with labeled tick marks
axis(side=2, at=seq(22,50,4),cex.axis=1, las=1)
mtext(side=2, line=3, "Thermal tolerance range (°C)", col="black", font=1,cex=1)

# Extract x and y values from the data-frame
df_x = df$STR
df_y = df$TTrange
# Fit a linear mixed-effects model with random effects for Location and Family
md_x <- lmer(TTrange~ STR+ (1|Location)+ (1|Family), data = df)
# Generate a sequence of x values for prediction
mydata.x <- data.frame(expand.grid(STR = seq(min(df_x),max(df_x),0.1)))
# Predict fitted values and standard errors
pred.x <- predictSE(md_x,mydata.x,se.fit=TRUE)
upper.x <- pred.x$fit+1.96*pred.x$se.fit
lower.x <- pred.x$fit-1.96*pred.x$se.fit
# Extract model summary statistics
sum=summary(md_x)
slope = round(sum$coefficients[2,1], 4)
r = round(r.squaredGLMM(md_x)[1], 4)
p_ = round(car::Anova(md_x)[['Pr(>Chisq)']], 4)

# Define text position for displaying statistical results
text_pos_x = sum(c(5,35))/2
ylim = max(df$TTrange)
# Display statistical results with appropriate significance symbols
if(p_ < 0.05 && p_ >= 0.01){
  text(text_pos_x, ylim, paste('p=',p_,'*, slope=',slope,', r2=',r,sep=''), cex=0.7)
}else if(p_ < 0.01 && p_ >= 0.001){
  text(text_pos_x, ylim, paste('p=',p_,'**, slope=',slope,', r2=',r,sep=''), cex=0.7)
}else if(p_ < 0.001){
  text(text_pos_x, ylim, paste('p < 0.001***, slope=',slope,', r2=',r,sep=''), cex=0.7)
}else{
  text(text_pos_x, ylim, paste('p=',p_,', slope=',slope,', r2=',r,sep=''), cex=0.7)
}
# Define line type: dashed if p-value > 0.05, solid otherwise
if(p_ > 0.05) lty = 2 else{
  lty = 1
  # Draw confidence interval
  polygon(x=c(mydata.x$STR,rev(mydata.x$STR)),y=c(lower.x,rev(upper.x)),
          col=scales::alpha('grey80', 0.5),border=NA)
}
# Add regression line
lines(x=mydata.x$STR,y=pred.x$fit,col='grey20',lwd=1.5,lty=lty)
# Plot data points
points(x=df$STR,y=df$TTrange,col=scales::alpha('grey30', 0.5),pch=16,cex=0.7)

#---- Fig.3b: TTrange - RS ----
# Create an empty plot with customized axes and limits
plot(x=df$TTrange, y=df$RS_std, las=1, cex= 1.5, 
     xlab = '', ylab = '',
     xlim = c(22,50), ylim = c(0,1), type="n", axes = F)
box()# Add a box around the plot
# Add x-axis with labeled tick marks
axis(side=1, at=seq(22,50,4),cex.axis=1)
mtext(side=1, line=3, "Thermal tolerance range (°C)", col="black", font=1,cex=1)
# Add y-axis with labeled tick marks
axis(side=2, at=seq(0,1,0.2),cex.axis=1, las=1)
mtext(side=2, line=3, "Distributional range size (relative)", col="black", font=1,cex=1)

# Extract x and y values from the data-frame
df_x = df$TTrange
df_y = df$RS_std
# Fit a linear mixed-effects model with random effects for Location and Family
md_x <- lmer(RS_std~ TTrange+ (1|Location)+ (1|Family), data = df)
# Generate a sequence of x values for prediction
mydata.x <- data.frame(expand.grid(TTrange = seq(min(df_x),max(df_x),0.1)))
# Predict fitted values and standard errors
pred.x <- predictSE(md_x,mydata.x,se.fit=TRUE)
upper.x <- pred.x$fit+1.96*pred.x$se.fit
lower.x <- pred.x$fit-1.96*pred.x$se.fit
# Extract model summary statistics
sum=summary(md_x)
slope = round(sum$coefficients[2,1], 4)
r = round(r.squaredGLMM(md_x)[1], 4)
p_ = round(car::Anova(md_x)[['Pr(>Chisq)']], 4)

# Define text position for displaying statistical results
text_pos_x = sum(c(22,50))/2
ylim = max(df$RS_std)
# Display statistical results with appropriate significance symbols
if(p_ < 0.05 && p_ >= 0.01){
  text(text_pos_x, ylim, paste('p=',p_,'*, slope=',slope,', r2=',r,sep=''), cex=0.7)
}else if(p_ < 0.01 && p_ >= 0.001){
  text(text_pos_x, ylim, paste('p=',p_,'**, slope=',slope,', r2=',r,sep=''), cex=0.7)
}else if(p_ < 0.001){
  text(text_pos_x, ylim, paste('p < 0.001***, slope=',slope,', r2=',r,sep=''), cex=0.7)
}else{
  text(text_pos_x, ylim, paste('p=',p_,', slope=',slope,', r2=',r,sep=''), cex=0.7)
}
# Define line type: dashed if p-value > 0.05, solid otherwise
if(p_ > 0.05) lty = 2 else{
  lty = 1
  # Draw confidence interval
  polygon(x=c(mydata.x$TTrange,rev(mydata.x$TTrange)),y=c(lower.x,rev(upper.x)),
          col=scales::alpha('grey80', 0.5),border=NA)
}
# Add regression line
lines(x=mydata.x$TTrange,y=pred.x$fit,col='grey20',lwd=1.5,lty=lty)
# Plot data points
points(x=df$TTrange,y=df$RS_std,col=scales::alpha('grey30', 0.5),pch=16,cex=0.7)

dev.off()

#---- Fig.3c: Range Size SEM ----
library(piecewiseSEM)
# library(nlme)
# library(lavaan)
# library(lme4)

data = df
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