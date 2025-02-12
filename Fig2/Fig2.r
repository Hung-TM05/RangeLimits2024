## Author: Tzu-Man, Hung. (https://github.com/Hung-TM05/RangeLimits2024)
## Last update: 2025-02-10
## abbreviation: CA= Malaysia data; MC= Taiwan data; SJ= China data

library(phytools)
library(tidyverse)
library(ggtree)
library(viridisLite)

## Color palatte
rbPal <- colorRampPalette(c('#113285','#005CAF','grey85','#ED784A','#D0104C'))

################# MAIN #################
pdf("./Fig2.pdf", width=15, height=12, onefile = TRUE)
par(mfrow=c(3,3))

##---- Fig.2a-c: Malaysia ----
## Load tree data
ca_tree <- read.nexus("./Data/ca_20230611_dna.tree")
## Load trait data
ca_data <- read.csv("./Data/ca_subf_trait20230609_envi.csv", header = T, row.names = 1)

## Fig.2a: CTmax
# Extract trait values and assign names
cactr <- setNames(ca_data$CTmax,rownames(ca_data))
# Count the number of distinct colors to be inferred
n_cols1 <- n_distinct(cactr)
# Create continuous trait mapping for the tree
cactr_plot <- contMap(ca_tree,cactr,plot=FALSE)
# Set the color mapping for visualization
cactr_plot <- setMap(cactr_plot,rbPal(n_cols1))
# Plot the tree with trait mapping
plot(cactr_plot,legend=0.7*max(nodeHeights(ca_tree)),
     fsize=c(0.7,0.9),leg.txt="Critical thermal maximum (°C)")

## Fig.2b: CTmin
cactr <- setNames(ca_data$CTmin,rownames(ca_data))
n_cols1 <- n_distinct(cactr)
cactr_plot <- contMap(ca_tree,cactr,plot=FALSE)
cactr_plot <- setMap(cactr_plot,rbPal(n_cols1))
plot(cactr_plot,legend=0.7*max(nodeHeights(ca_tree)),
     fsize=c(0.7,0.9),leg.txt="Critical thermal minimum (°C)")

## Fig.2c: TTrange
cactr <- setNames(ca_data$TTrange,rownames(ca_data))
n_cols1 <- n_distinct(cactr)
cactr_plot <- contMap(ca_tree,cactr,plot=FALSE)
cactr_plot <- setMap(cactr_plot,rbPal(n_cols1))
plot(cactr_plot,legend=0.7*max(nodeHeights(ca_tree)),
     fsize=c(0.7,0.9),leg.txt="Thermal tolerence range (°C)")

##---- Fig.2d-f: Taiwan ----
mc_tree <- read.nexus("./Data/mc_subf_20230113_final2.tree")
mc_data <- read.csv("./Data/mc_subf_trait20230609_envi.csv", header = T, row.names = 1)

## Fig.2d: CTmax
mcctr <- setNames(mc_data$CTmax,rownames(mc_data))
n_cols <- n_distinct(mcctr)
mcctr_plot <- contMap(mc_tree,mcctr,plot=FALSE)
mcctr_plot <- setMap(mcctr_plot,rbPal(n_cols))
plot(mcctr_plot,legend=0.7*max(nodeHeights(mc_tree)),
     fsize=c(0.7,0.9),leg.txt="Critical thermal maximum (°C)")

## Fig.2e: CTmin
mcctr <- setNames(mc_data$CTmin,rownames(mc_data))
n_cols <- n_distinct(mcctr)
mcctr_plot <- contMap(mc_tree,mcctr,plot=FALSE)
mcctr_plot <- setMap(mcctr_plot,rbPal(n_cols))
plot(mcctr_plot,legend=0.7*max(nodeHeights(mc_tree)),
     fsize=c(0.7,0.9),leg.txt="Critical thermal minimum (°C)")

## Fig.2f: TTrange
mcctr <- setNames(mc_data$TTrange,rownames(mc_data))
n_cols <- n_distinct(mcctr)
mcctr_plot <- contMap(mc_tree,mcctr,plot=FALSE)
mcctr_plot <- setMap(mcctr_plot,rbPal(n_cols))
plot(mcctr_plot,legend=0.7*max(nodeHeights(mc_tree)),
     fsize=c(0.7,0.9),leg.txt="Thermal tolerence range (°C)")

##---- Fig.2g-i: China ----
sj_tree <- read.nexus("./Data/sj_20230611dna.tree")
sj_data <- read.csv("./Data/sj_subf_trait20230609_envi.csv", header = T, row.names = 1)

# Fig.2g: CTmax
sjctr <- setNames(sj_data$CTmax,rownames(sj_data))
n_cols <- n_distinct(sjctr)
sjctr_plot <- contMap(sj_tree,sjctr,plot=FALSE)
sjctr_plot <- setMap(sjctr_plot,rbPal(n_cols))
plot(sjctr_plot,legend=0.7*max(nodeHeights(sj_tree)),
     fsize=c(0.7,0.9),leg.txt="Critical thermal maximum (°C)")

# Fig.2h: CTmin
sjctr <- setNames(sj_data$CTmin,rownames(sj_data))
n_cols <- n_distinct(sjctr)
sjctr_plot <- contMap(sj_tree,sjctr,plot=FALSE)
sjctr_plot <- setMap(sjctr_plot,rbPal(n_cols))
plot(sjctr_plot,legend=0.7*max(nodeHeights(sj_tree)),
     fsize=c(0.7,0.9),leg.txt="Critical thermal minimum (°C)")

## Fig.2i: TTrange
sjctr <- setNames(sj_data$TTrange,rownames(sj_data))
n_cols <- n_distinct(sjctr)
sjctr_plot <- contMap(sj_tree,sjctr,plot=FALSE)
sjctr_plot <- setMap(sjctr_plot,rbPal(n_cols))
plot(sjctr_plot,legend=0.7*max(nodeHeights(sj_tree)),
     fsize=c(0.7,0.9),leg.txt="Thermal tolerence range (°C)")

dev.off()