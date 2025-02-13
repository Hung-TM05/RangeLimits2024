library(dplyr)
library(vegan)
library(lme4)
library(MuMIn)
library(nlme)
library(scales)
library(AICcmodavg)

#---- Global option ----
axis_text = 1.2
label_text = 1
col.list = c('#ED784A', '#ED784A', '#005CAF', '#005CAF')

#---- Load data ----
df = read.csv('./Empirical_moth_data.csv',header = TRUE)
df$Elev_up_std <- NA
df$Elev_low_std <- NA
df$RS_std <- NA
df$Elev_mid_std <- NA
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

#---- Fig. 5a-b: CTlimits vs RSlimits (relative) ----
pdf(file = './Fig5.pdf', width = 8, height = 8, onefile = TRUE)
par(mfrow=c(2,2))

#-- CTmin vs RSlimits --
#### Lower limit - CTmin (Fig.5a- Left panel) ####
# Create an empty plot with customized axes and limits
plot(x=df$CTmin, y=df$Elev_low_std, las=1, cex= 1.5, 
     xlab = '', ylab = '',
     xlim = c(-6,18), ylim = c(0,1), type="n", axes = F)
box()# Add a box around the plot
# Add x-axis with labeled tick marks
axis(side=1, at=seq(-6,18,4),cex.axis=axis_text)
mtext(side=1, line=3, "Critical thermal minimum (°C)", col="black", font=1,cex=label_text)
# Add y-axis with labeled tick marks
axis(side=2, at=seq(0,1,0.2),cex.axis=axis_text, las=1)
mtext(side=2, line=3, "Lower distribution limits (relative)", col="black", font=1,cex=label_text)

# Extract x and y values from the data-frame
df_x = df$CTmin
df_y = df$Elev_low_std
# Fit a linear mixed-effects model with random effects for Location and Family
md_x <- lmer(Elev_low_std~ CTmin+ (1|Location)+ (1|Family), data = df)
# Generate a sequence of x values for prediction
mydata.x <- data.frame(expand.grid(CTmin = seq(min(df_x),max(df_x),0.1)))
# Predict fitted values and standard errors
pred.x <- predictSE(md_x,mydata.x,se.fit=TRUE)
upper.x <- pred.x$fit+1.96*pred.x$se.fit
lower.x <- pred.x$fit-1.96*pred.x$se.fit
# Extract model summary statistics
sum=summary(md_x)
slope = round(sum$coefficients[2,1], 4)
r = round(r.squaredGLMM(md_x)[1], 4)
p_ = car::Anova(md_x)[['Pr(>Chisq)']]

# Define text position for displaying statistical results
text_pos_x = sum(c(-6,18))/2
ylim = max(df$Elev_low_std)
# Display statistical results with appropriate significance symbols
if(p_ < 0.05 && p_ >= 0.01){
  text(text_pos_x, ylim, paste('p=',p_,'*, slope=',slope,', r2=',r,sep=''), cex=1)
}else if(p_ < 0.01 && p_ >= 0.001){
  text(text_pos_x, ylim, paste('p=',p_,'**, slope=',slope,', r2=',r,sep=''), cex=1)
}else if(p_ < 0.001){
  text(text_pos_x, ylim, paste('p < 0.001***, slope=',slope,', r2=',r,sep=''), cex=1)
}else{
  text(text_pos_x, ylim, paste('p=',p_,', slope=',slope,', r2=',r,sep=''), cex=1)
}
# Define line type: dashed if p-value > 0.05, solid otherwise
if(p_ > 0.05) lty = 2 else{
  lty = 1
  # Draw confidence interval
  polygon(x=c(mydata.x$CTmin,rev(mydata.x$CTmin)),y=c(lower.x,rev(upper.x)),
          col=scales::alpha(col.list[4], 0.1),border=NA)
} 
# Add regression line
lines(x=mydata.x$CTmin,y=pred.x$fit,col=col.list[3],lwd=1.2,lty=lty)
# Plot data points
points(x=df$CTmin,y=df$Elev_low_std,col=scales::alpha(col.list[3], 0.5),pch=16,cex=0.7)

#### Upper limit - CTmin (Fig.5b- Left panel) ####
# Create an empty plot with customized axes and limits
plot(x=df$CTmin, y=df$Elev_up_std, las=1, cex= 1.5, 
     xlab = '', ylab = '',
     xlim = c(-6,18), ylim = c(0,1), type="n", axes = F)
box()# Add a box around the plot
# Add x-axis with labeled tick marks
axis(side=1, at=seq(-6,18,4),cex.axis=axis_text)
mtext(side=1, line=3, "Critical thermal minimum (°C)", col="black", font=1,cex=label_text)
# Add y-axis with labeled tick marks
axis(side=2, at=seq(0,1,0.2),cex.axis=axis_text, las=1)
mtext(side=2, line=3, "Upper distribution limits (relative)", col="black", font=1,cex=label_text)

# Extract x and y values from the data-frame
df_x = df$CTmin
df_y = df$Elev_up_std
# Fit a linear mixed-effects model with random effects for Location and Family
md_x <- lmer(Elev_up_std~ CTmin+ (1|Location)+ (1|Family), data = df)
# Generate a sequence of x values for prediction
mydata.x <- data.frame(expand.grid(CTmin = seq(min(df_x),max(df_x),0.1)))
# Predict fitted values and standard errors
pred.x <- predictSE(md_x,mydata.x,se.fit=TRUE)
upper.x <- pred.x$fit+1.96*pred.x$se.fit
lower.x <- pred.x$fit-1.96*pred.x$se.fit
# Extract model summary statistics
sum=summary(md_x)
slope = round(sum$coefficients[2,1], 4)
r = round(r.squaredGLMM(md_x)[1], 4)
p_ = car::Anova(md_x)[['Pr(>Chisq)']]

# Define text position for displaying statistical results
text_pos_x = sum(c(-6,18))/2
ylim = max(df$Elev_up_std)
# Display statistical results with appropriate significance symbols
if(p_ < 0.05 && p_ >= 0.01){
  text(text_pos_x, ylim, paste('p=',p_,'*, slope=',slope,', r2=',r,sep=''), cex=1)
}else if(p_ < 0.01 && p_ >= 0.001){
  text(text_pos_x, ylim, paste('p=',p_,'**, slope=',slope,', r2=',r,sep=''), cex=1)
}else if(p_ < 0.001){
  text(text_pos_x, ylim, paste('p < 0.001***, slope=',slope,', r2=',r,sep=''), cex=1)
}else{
  text(text_pos_x, ylim, paste('p=',p_,', slope=',slope,', r2=',r,sep=''), cex=1)
}
# Define line type: dashed if p-value > 0.05, solid otherwise
if(p_ > 0.05) lty = 2 else{
  lty = 1
  # Draw confidence interval
  polygon(x=c(mydata.x$CTmin,rev(mydata.x$CTmin)),y=c(lower.x,rev(upper.x)),
          col=scales::alpha(col.list[4], 0.1),border=NA)
}
# Add regression line
lines(x=mydata.x$CTmin,y=pred.x$fit,col=col.list[3],lwd=1.2,lty=lty)
# Plot data points
points(x=df$CTmin,y=df$Elev_up_std,col=scales::alpha(col.list[3], 0.5),pch=16,cex=0.7)

#-- CTmax vs RSlimits
#### Lower limit - CTmax (Fig.5a- Right panel) ####
plot(x=df$CTmax, y=df$Elev_low_std, las=1, cex= 1.5, 
     xlab = '', ylab = '',
     xlim = c(25,50), ylim = c(0,1), type="n", axes = F)
box()
# x-axis
axis(side=1, at=seq(25,50,5),cex.axis=axis_text)
mtext(side=1, line=3, "Critical thermal maximum (°C)", col="black", font=1,cex=label_text)
# y-axis
axis(side=2, at=seq(0,1,0.2),cex.axis=axis_text, las=1)
mtext(side=2, line=3, "Lower distribution limits (relative)", col="black", font=1,cex=label_text)

df_x = df$CTmax
df_y = df$Elev_low_std
md_x <- lmer(Elev_low_std~ CTmax+ (1|Location)+ (1|Family), data = df)
mydata.x <- data.frame(expand.grid(CTmax = seq(min(df_x),max(df_x),0.1)))
pred.x <- predictSE(md_x,mydata.x,se.fit=TRUE)
upper.x <- pred.x$fit+1.96*pred.x$se.fit
lower.x <- pred.x$fit-1.96*pred.x$se.fit
sum=summary(md_x)
slope = round(sum$coefficients[2,1], 4)
r = round(r.squaredGLMM(md_x)[1], 4)
p_ = car::Anova(md_x)[['Pr(>Chisq)']]

text_pos_x = sum(c(25,50))/2
ylim = max(df$Elev_low_std)
if(p_ < 0.05 && p_ >= 0.01){
  text(text_pos_x, ylim, paste('p=',p_,'*, slope=',slope,', r2=',r,sep=''), cex=1)
}else if(p_ < 0.01 && p_ >= 0.001){
  text(text_pos_x, ylim, paste('p=',p_,'**, slope=',slope,', r2=',r,sep=''), cex=1)
}else if(p_ < 0.001){
  text(text_pos_x, ylim, paste('p < 0.001***, slope=',slope,', r2=',r,sep=''), cex=1)
}else{
  text(text_pos_x, ylim, paste('p=',p_,', slope=',slope,', r2=',r,sep=''), cex=1)
}
if(p_ > 0.05) lty = 2 else{
  lty = 1
  polygon(x=c(mydata.x$CTmax,rev(mydata.x$CTmax)),y=c(lower.x,rev(upper.x)),
          col=scales::alpha(col.list[2], 0.1),border=NA)
} 
lines(x=mydata.x$CTmax,y=pred.x$fit,col=col.list[1],lwd=1.2,lty=lty)
points(x=df$CTmax,y=df$Elev_low_std,col=scales::alpha(col.list[1], 0.5),pch=16,cex=0.7)

#### Upper limit - CTmax (Fig.5b- Right panel) ####
plot(x=df$CTmax, y=df$Elev_up_std, las=1, cex= 1.5, 
     xlab = '', ylab = '', #main = loc,
     xlim = c(25,50), ylim = c(0,1), type="n", axes = F)
box()
# x-axis
axis(side=1, at=seq(25,50,5),cex.axis=axis_text)
mtext(side=1, line=3, "Critical thermal maximum (°C)", col="black", font=1,cex=label_text)
# y-axis
axis(side=2, at=seq(0,1,0.2),cex.axis=axis_text, las=1)
mtext(side=2, line=3, "Upper distribution limits (relative)", col="black", font=1,cex=label_text)

df_x = df$CTmax
df_y = df$Elev_up_std
md_x <- lmer(Elev_up_std~ CTmax+ (1|Location)+ (1|Family), data = df)
mydata.x <- data.frame(expand.grid(CTmax = seq(min(df_x),max(df_x),0.1)))
pred.x <- predictSE(md_x,mydata.x,se.fit=TRUE)
upper.x <- pred.x$fit+1.96*pred.x$se.fit
lower.x <- pred.x$fit-1.96*pred.x$se.fit
sum=summary(md_x)
slope = round(sum$coefficients[2,1], 4)
r = round(r.squaredGLMM(md_x)[1], 4)
p_ = car::Anova(md_x)[['Pr(>Chisq)']]

text_pos_x = sum(c(25,50))/2
ylim = max(df$Elev_up_std)
if(p_ < 0.05 && p_ >= 0.01){
  text(text_pos_x, ylim, paste('p=',p_,'*, slope=',slope,', r2=',r,sep=''), cex=1)
}else if(p_ < 0.01 && p_ >= 0.001){
  text(text_pos_x, ylim, paste('p=',p_,'**, slope=',slope,', r2=',r,sep=''), cex=1)
}else if(p_ < 0.001){
  text(text_pos_x, ylim, paste('p < 0.001***, slope=',slope,', r2=',r,sep=''), cex=1)
}else{
  text(text_pos_x, ylim, paste('p=',p_,', slope=',slope,', r2=',r,sep=''), cex=1)
}
if(p_ > 0.05) lty = 2 else{
  lty = 1
  polygon(x=c(mydata.x$CTmax,rev(mydata.x$CTmax)),y=c(lower.x,rev(upper.x)),
          col=scales::alpha(col.list[2], 0.1),border=NA)
} 
lines(x=mydata.x$CTmax,y=pred.x$fit,col=col.list[1],lwd=1.2,lty=lty)
points(x=df$CTmax,y=df$Elev_up_std,col=scales::alpha(col.list[1], 0.5),pch=16,cex=0.7)

dev.off()
