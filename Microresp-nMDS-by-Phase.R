setwd("C:/Users/Camilla/Dropbox/Data & analysis/WP3 Slurry disturbance/Plotting Data/PhenotypicData-R")

library(vegan)
library(MASS)
library(ggplot2)

# read in untransformed data
pod <- read.table("MResp-BioReps_UnTransformed.txt", sep = "\t", header=T, row.names=1)

m <- as.matrix(pod)


## Rescale each column to range between 0 and 1
# where each c source is a different column

poo <- apply(m, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

head(poo)

# read in metadata
meTa <- read.table("Mresp-metadata.txt", header = TRUE, row.names = 1, sep='\t')

meta1 <- meTa[1:18,]
meta2 <- meTa[19:78,]
meta3 <- meTa[79:150,]

# read in metadata file explaining what treatment each sample had 
# using same sample name as the initial file you read in of course


# whole dataset at once



v.dist <- vegdist(poo, method="euclidean", binary=FALSE, diag=FALSE, upper=FALSE)


NMDS <- metaMDS(v.dist, distance = "euclidean", k = 3, trymax = 20, autotransform =TRUE,
                 noshare = 0.1, wascores = TRUE, expand = TRUE, trace = 1,
                 old.wa = FALSE)

# Copy and paste what's printed to console into notepad for ref.

# have a wee look at what you have. 
# first the stress plot
stressplot(NMDS) #r2 =0.99

# check r2 value and scattered-ness of points from the line
# shouldnt be big scatter. 

# then basic plot of nmds to check it worked
plot(NMDS3, type="t")

## subset into 3 phases


phase1 <- poo[1:18,]
head(phase1)
phase2 <- poo[19:78,]
phase3 <- poo[79:150,]



v.dist1 <- vegdist(phase1, method="euclidean", binary=FALSE, diag=FALSE, upper=FALSE)


NMDS1 <- metaMDS(v.dist1, distance = "euclidean", k = 3, trymax = 20, autotransform =TRUE,
                 noshare = 0.1, wascores = TRUE, expand = TRUE, trace = 1,
                 old.wa = FALSE)
# stress = 0.016

v.dist2 <- vegdist(phase2, method="euclidean", binary=FALSE, diag=FALSE, upper=FALSE)


NMDS2 <- metaMDS(v.dist2, distance = "euclidean", k = 3, trymax = 20, autotransform =TRUE,
                 noshare = 0.1, wascores = TRUE, expand = TRUE, trace = 1,
                 old.wa = FALSE)
# stress = 0.016

v.dist3 <- vegdist(phase3, method="euclidean", binary=FALSE, diag=FALSE, upper=FALSE)


NMDS3 <- metaMDS(v.dist3, distance = "euclidean", k = 3, trymax = 20, autotransform =TRUE,
                 noshare = 0.1, wascores = TRUE, expand = TRUE, trace = 1,
                 old.wa = FALSE)
# stress = 0.04
# have a wee look at what you have. 
# first the stress plot
stressplot(NMDS1) #r2 =0.99
stressplot(NMDS2) #r2 = 0.99
stressplot(NMDS3) # r2 = 0.98
# making a dataframe with NMDS data and metadata in one so can plot
# using ggplot

# first convert metaMDS output into a dataframe

data.scores1 <- as.data.frame(scores(NMDS1)) # have a look at it and check ok


data.scores2 <- as.data.frame(scores(NMDS2)) # have a look at it and check ok


data.scores3 <- as.data.frame(scores(NMDS3)) # have a look at it and check ok


# choose row names (IDS) from metadata file that i read in just now 
# as one of columns.
# a) preflood phase
data.scores1$id<- rownames(data.scores1)

plot(NMDS1$points, col = meTa$timepoint)

# 2) flooded phase
data.scores2$id<- rownames(data.scores2)

plot(NMDS2$points, col = meTa$timepoint)

# 3) recovery phase
data.scores3$id<- rownames(data.scores3)

plot(NMDS3$points, col = meTa$timepoint)



# Phase 1 now add info re treatment and sample day as two extra colums in data.scores
# data frame. Did factor() to keep them in order that I input them. 

data.scores1$treatment<- meta1$Treatment
data.scores1$time<- meta1$timepoint

data.scores1[["time"]] <- setFactorOrder(data.scores1[["time"]], c("T0", "T1", "T2"))

data.scores1[["treatment"]] <- setFactorOrder(data.scores1[["treatment"]], c("Control", "Slurry"))


# now add info re treatment and sample day as two extra colums in data.scores
# data frame. Did factor() to keep them in order that I input them. 

data.scores2$treatment<- meta2$Treatment
data.scores2$time<- meta2$timepoint

data.scores2[["time"]] <- setFactorOrder(data.scores2[["time"]], c("T3", "T4", "T5", "T6", "T7"))

data.scores2[["treatment"]] <- setFactorOrder(data.scores2[["treatment"]], c("Control", "Slurry", "Flood", "Flood+Slurry"))


# now add info re treatment and sample day as two extra colums in data.scores
# data frame. Did factor() to keep them in order that I input them. 

data.scores3$treatment<- meta3$Treatment
data.scores3$time<- meta3$timepoint

data.scores3[["time"]] <- setFactorOrder(data.scores3[["time"]], c("T8", "T9", "T10", "T11", "T12", "T13"))

data.scores3[["treatment"]] <- setFactorOrder(data.scores3[["treatment"]], c("Control", "Slurry", "Flood", "Flood+Slurry"))


##
NMDSplot <- ggplot(data.scores, title='nMDS of MicroResp') +
  geom_point(aes(x=NMDS1, y=NMDS2, color=time, shape=treatment))
NMDSplot

##
NMDSplot <- ggplot(data.scores1, title='nMDS of MicroResp') +
  geom_point(aes(x=NMDS1, y=NMDS2, color=time, shape=treatment))
NMDSplot


## Modify shapes so can display the different phase within the days

data.scores.shape.names = unique(data.scores$phase)
data.scores.shape <- 1:(length(data.scores.shape.names))
names(data.scores.shape) <- data.scores.shape.names
data.scores.shape["samples"] <- 150


# use ggthemr to plot with my WP3 colour theme so 4 treatments
# labelled as in all my other plots. 

library(ggthemr)
# define 
WP3_colsA <- c("black", "chocolate4", "slateblue", "olivedrab")
# add background
WP3_cols <- c("#555555", WP3_colsA)

# define palette
WP3Cols <- define_palette(
  swatch = WP3_cols, # colours for plotting points and bars
  gradient = c(lower = WP3_cols[1L], upper = WP3_cols[2L]), #upper and lower colours for continuous colours
  background = "white" #defining a grey-ish background 
)

# set new col theme as current

ggthemr(WP3Cols)    

# and replot with new color scheme


P <- ggplot(data.scores3, aes(x= NMDS1, y= NMDS2, colour=treatment, label=time))+
  geom_point(size=15, shape=21, stroke =2)  + # size and line thickness of plotted points
  scale_shape_manual(values = c(1, 5, 22)) +
  theme(legend.key.size=unit(0.3,"cm")) +
  theme_bw()
P

# label each point with time. Put colour=control
# as want all these labels in black text, otherwise looks messy.
# play with hjust and vjust to move text around
# h just moves text left to right ( left bigger the number)

P2 <- P +geom_text(aes(label=time, colour="Control"),hjust=0.45, vjust=0.3, 
                   size=4, fontface = "bold", show.legend = FALSE) 


p2 = P2 + theme(axis.text.y=element_text(size=16, colour="black"),
                axis.text.x=element_text(size=16, colour="black"),
                axis.title.x=element_text(size=15, colour="black"),
                legend.text = element_text(size=16),
                legend.title = element_text(size=17),
                axis.title.y=element_text(size=15, colour="black"))+
  labs(colour=" Treatment") 



P2Stress <-  p2 + 
  annotate("text", colour="black", size = 5, x=1, y=-0.3, 
           label= "R^{2}==0.98", parse=T) +
  annotate("text", colour="black",  size = 5, x=1, y=-0.32, 
           label= "Stress==0.04", parse=T)
P2Stress


phase1plot = p2
phase1noLegend = p2 + guides(colour=FALSE)

phase2plot = p2
phase2noLegend = p2 + guides(colour=FALSE)

phase3plot = p2
phase3noLegend = p2 + guides(colour=FALSE)



library("ggpubr")

# create plots as normal in ggplot. 
# name them. Here i have 4 plots:


ggarrange(phase1noLegend, phase2noLegend, phase3noLegend,
          labels = c("A", "B", "C"),
          ncol = 2, nrow=2)




##  ------------------------------------------- bray



v.dist1 <- vegdist(phase1, method="euclidean", binary=FALSE, diag=FALSE, upper=FALSE)


NMDS1 <- metaMDS(v.dist1, distance = "euclidean", k = 3, trymax = 20, autotransform =TRUE,
                 noshare = 0.1, wascores = TRUE, expand = TRUE, trace = 1,
                 old.wa = FALSE)
# stress = 0.016

v.dist2 <- vegdist(phase2, method="euclidean", binary=FALSE, diag=FALSE, upper=FALSE)


NMDS2 <- metaMDS(v.dist2, distance = "euclidean", k = 3, trymax = 20, autotransform =TRUE,
                 noshare = 0.1, wascores = TRUE, expand = TRUE, trace = 1,
                 old.wa = FALSE)
# stress = 0.016

v.dist3 <- vegdist(phase3, method="euclidean", binary=FALSE, diag=FALSE, upper=FALSE)


NMDS3 <- metaMDS(v.dist3, distance = "euclidean", k = 3, trymax = 20, autotransform =TRUE,
                 noshare = 0.1, wascores = TRUE, expand = TRUE, trace = 1,
                 old.wa = FALSE)
# stress = 0.04
# have a wee look at what you have. 
# first the stress plot
stressplot(NMDS1) #r2 =0.99
stressplot(NMDS2) #r2 = 0.99
stressplot(NMDS3) # r2 = 0.98
# making a dataframe with NMDS data and metadata in one so can plot
# using ggplot

# first convert metaMDS output into a dataframe

data.scores1 <- as.data.frame(scores(NMDS1)) # have a look at it and check ok


data.scores2 <- as.data.frame(scores(NMDS2)) # have a look at it and check ok


data.scores3 <- as.data.frame(scores(NMDS3)) # have a look at it and check ok


# choose row names (IDS) from metadata file that i read in just now 
# as one of columns.
# a) preflood phase
data.scores1$id<- rownames(data.scores1)

plot(NMDS1$points, col = meTa$timepoint)

# 2) flooded phase
data.scores2$id<- rownames(data.scores2)

plot(NMDS2$points, col = meTa$timepoint)

# 3) recovery phase
data.scores3$id<- rownames(data.scores3)

plot(NMDS3$points, col = meTa$timepoint)



# Phase 1 now add info re treatment and sample day as two extra colums in data.scores
# data frame. Did factor() to keep them in order that I input them. 

data.scores1$treatment<- meta1$Treatment
data.scores1$time<- meta1$timepoint

data.scores1[["time"]] <- setFactorOrder(data.scores1[["time"]], c("T0", "T1", "T2"))

data.scores1[["treatment"]] <- setFactorOrder(data.scores1[["treatment"]], c("Control", "Slurry"))


# now add info re treatment and sample day as two extra colums in data.scores
# data frame. Did factor() to keep them in order that I input them. 

data.scores2$treatment<- meta2$Treatment
data.scores2$time<- meta2$timepoint

data.scores2[["time"]] <- setFactorOrder(data.scores2[["time"]], c("T3", "T4", "T5", "T6", "T7"))

data.scores2[["treatment"]] <- setFactorOrder(data.scores2[["treatment"]], c("Control", "Slurry", "Flood", "Flood+Slurry"))


# now add info re treatment and sample day as two extra colums in data.scores
# data frame. Did factor() to keep them in order that I input them. 

data.scores3$treatment<- meta3$Treatment
data.scores3$time<- meta3$timepoint

data.scores3[["time"]] <- setFactorOrder(data.scores3[["time"]], c("T8", "T9", "T10", "T11", "T12", "T13"))

data.scores3[["treatment"]] <- setFactorOrder(data.scores3[["treatment"]], c("Control", "Slurry", "Flood", "Flood+Slurry"))


##
NMDSplot <- ggplot(data.scores, title='nMDS of MicroResp') +
  geom_point(aes(x=NMDS1, y=NMDS2, color=time, shape=treatment))
NMDSplot

##
NMDSplot <- ggplot(data.scores1, title='nMDS of MicroResp') +
  geom_point(aes(x=NMDS1, y=NMDS2, color=time, shape=treatment))
NMDSplot


## Modify shapes so can display the different phase within the days

data.scores.shape.names = unique(data.scores$phase)
data.scores.shape <- 1:(length(data.scores.shape.names))
names(data.scores.shape) <- data.scores.shape.names
data.scores.shape["samples"] <- 150


# use ggthemr to plot with my WP3 colour theme so 4 treatments
# labelled as in all my other plots. 

library(ggthemr)
# define 
WP3_colsA <- c("black", "chocolate4", "slateblue", "olivedrab")
# add background
WP3_cols <- c("#555555", WP3_colsA)

# define palette
WP3Cols <- define_palette(
  swatch = WP3_cols, # colours for plotting points and bars
  gradient = c(lower = WP3_cols[1L], upper = WP3_cols[2L]), #upper and lower colours for continuous colours
  background = "white" #defining a grey-ish background 
)

# set new col theme as current

ggthemr(WP3Cols)    

# and replot with new color scheme


P <- ggplot(data.scores3, aes(x= NMDS1, y= NMDS2, colour=treatment, label=time))+
  geom_point(size=15, shape=21, stroke =2)  + # size and line thickness of plotted points
  scale_shape_manual(values = c(1, 5, 22)) +
  theme(legend.key.size=unit(0.3,"cm")) +
  theme_bw()
P

# label each point with time. Put colour=control
# as want all these labels in black text, otherwise looks messy.
# play with hjust and vjust to move text around
# h just moves text left to right ( left bigger the number)

P2 <- P +geom_text(aes(label=time, colour="Control"),hjust=0.45, vjust=0.3, 
                   size=4, fontface = "bold", show.legend = FALSE) 


p2 = P2 + theme(axis.text.y=element_text(size=16, colour="black"),
                axis.text.x=element_text(size=16, colour="black"),
                axis.title.x=element_text(size=15, colour="black"),
                legend.text = element_text(size=16),
                legend.title = element_text(size=17),
                axis.title.y=element_text(size=15, colour="black"))+
  labs(colour=" Treatment") 



P2Stress <-  p2 + 
  annotate("text", colour="black", size = 5, x=1, y=-0.3, 
           label= "R^{2}==0.98", parse=T) +
  annotate("text", colour="black",  size = 5, x=1, y=-0.32, 
           label= "Stress==0.04", parse=T)
P2Stress


phase1plot = p2
phase1noLegend = p2 + guides(colour=FALSE)

phase2plot = p2
phase2noLegend = p2 + guides(colour=FALSE)

phase3plot = p2
phase3noLegend = p2 + guides(colour=FALSE)



library("ggpubr")

# create plots as normal in ggplot. 
# name them. Here i have 4 plots:


ggarrange(phase1noLegend, phase2noLegend, phase3noLegend,
          labels = c("A", "B", "C"),
          ncol = 2, nrow=2)

### thought about trying to do sep nmds per day

meTa <- read.table("Mresp-metadata.txt", header = TRUE, row.names = 1, sep='\t')
typeof(meTa)
metaf = as.data.frame(meTa) # convert to d.frame

pool$tmt<- metaf$Treatment # add tmt col from metadata

pool$time <- metaf$timepoint # add col timepoint

pool$phase <- metaf$Phase # add col phase

## subset by day

day0 <- subset(pool, time=="T0")
day1 <- subset(pool, time=="T1")
day2 <- subset(pool, time=="T2")
day3 <- subset(pool, time=="T3")
day4 <- subset(pool, time=="T4")
day5 <- subset(pool, time=="T5")
day6 <- subset(pool, time=="T6")
day7 <- subset(pool, time=="T7")
day8 <- subset(pool, time=="T8")
day9 <- subset(pool, time=="T9")
day10 <- subset(pool, time=="T10")
day11 <- subset(pool, time=="T11")
day12 <- subset(pool, time=="T12")
day13 <- subset(pool, time=="T13")



day0b = day0[,1:11]




## now do distance and nmds calcs with vegan



v.dist0 <- vegdist(day0b, method="euclidean", binary=FALSE, diag=FALSE, upper=FALSE)



NMDS2 <- metaMDS(v.dist0, distance = "euclidean", k = 2, trymax = 20, autotransform =TRUE,
                 noshare = 0.1, wascores = TRUE, expand = TRUE, trace = 1,
                 old.wa = FALSE)

