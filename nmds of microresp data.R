
## Plotting carbon utilization profile data from microresp
## system as an nMDS 
# data input (head) looks like this where there are four treatments and 
## 14 timepoints

#                  Tmt1_T0   Tmt2_T0    Tmt1_T1   Tmt2_T1   Tmt1_T2    Tmt2_T2
#Cysteine        0.30762150 1.0007201 0.30157851 0.5863990 0.2790756 0.48249941
#GABA            0.16796625 0.9996302 0.18081572 0.4551209 0.1757800 0.38341919
#Lysine          0.04361777 0.2569897 0.04325326 0.1109734 0.0444487 0.07450042
#MalicAcid       0.39896253 0.7568375 0.33224924 0.7058575 0.2775511 0.53305969
#a-ketoglutarate 0.90591633 0.8458032 0.78273238 0.7568823 0.5561732 0.59555168
#OxalicAcid      0.64296843 0.9998756 0.24948304 0.6052436 0.1701977 0.24571969

setwd("C:/Users/Camilla/Dropbox/Data & analysis/WP3 Slurry disturbance/R")

library(vegan)
library(MASS)
library(ggplot2)


####### Note: did this On transformed data - this is what I will present so far
## transformed in excel for vals of each substrate for entire sampling 
## period to be between 0 and 1. Data in Microresp-heatmap-collation.xls
## need to find out if it's correct to use transformed or not in ordination - 3/4/2017. 

pod <- read.table("T13-ExArg-transformed.txt", sep = "\t", header=T, row.names=1)

MR <- t(pod)

v.dist <- vegdist(MR, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)


NMDS2 <- metaMDS(v.dist, distance = "bray", k = 2, trymax = 20, autotransform =TRUE,
                 noshare = 0.1, wascores = TRUE, expand = TRUE, trace = 1,
                 old.wa = FALSE)

# Copy and paste what's printed to console into notepad for ref.

# have a wee look at what you have. 
# first the stress
stressplot(NMDS2) 
# then basic plot of nmds. 
plot(NMDS3, type="t")

# read in metadata file which will give info on each sample for colour
# and shape in nmds. head looks like this

#Tmt timepoint Treatment   Phase
#Tmt1_T0 Tmt1        T0   Control NoFlood
#Tmt2_T0 Tmt2        T0    Slurry NoFlood
#Tmt1_T1 Tmt1        T1   Control NoFlood
#Tmt2_T1 Tmt2        T1    Slurry NoFlood
#Tmt1_T2 Tmt1        T2   Control NoFlood
#Tmt2_T2 Tmt2        T2    Slurry NoFlood

meTa <- read.table("MetaData.txt", header = TRUE, row.names = 1, sep='\t')

plot(NMDS2$points, col = meTa$timepoint)

# Now make a dataframe with NMDS data and metadata in one so can plot
# using ggplot

# first convert metaMDS output into a dataframe
data.scores <- as.data.frame(scores(NMDS2)) # have a look at it and check ok

# choose row names (IDs) from metadata file as one of columns. 
data.scores$id<- rownames(data.scores)

# now add info re treatment and sample day as two extra colums in data.scores
# data frame. Did factor() to keep them in order that I input them when plotted
# This will bring up warnings, dont worry about them.

data.scores$treatment<- meTa$Treatment
data.scores$treatment <- factor(data.scores$treatment, levels = data.scores$treatment)
data.scores$time<- meTa$timepoint
data.scores$time <- factor(data.scores$time, levels = data.scores$time)
data.scores$phase<- meTa$Phase
data.scores$phase <- factor(data.scores$phase, levels = data.scores$phase)

## Modify shapes so can display the different experimental phases within the days

data.scores.shape.names = unique(data.scores$phase)
data.scores.shape <- 1:(length(data.scores.shape.names))
names(data.scores.shape) <- data.scores.shape.names
data.scores.shape["samples"] <- 50 # change as appropriate


# using ggplots default colour scheme:

nMDS.Plot <- ggplot(data.scores) +
  geom_point(aes(x=NMDS1, y=NMDS2, color=treatment, shape = phase),size=8)  +
  scale_shape_manual(values = data.scores.shape) +
  theme(legend.key.size=unit(0.3,"cm")) +
  theme_bw() 
nMDS.Plot
##

# use ggthemr to plot with my WP3 colour theme so 4 treatments
# labelled as in all my other plots. 

library(ggthemr)
# define 4 colours
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

nMDS.Plot <- ggplot(data.scores) +
  geom_point(aes(x=NMDS1, y=NMDS2, color=treatment, shape = phase),size=7, stroke=2)  +
  scale_shape_manual(values = data.scores.shape) +
  theme(legend.key.size=unit(0.3,"cm")) +
  theme_bw() 
nMDS.Plot

## now different way as i want to add labels next to each point
## in order to ID timepoint


P <- ggplot(data.scores, aes(x= NMDS1, y= NMDS2, colour=treatment, shape=phase, label=time))+
  geom_point(size=8, stroke=2)  +
  scale_shape_manual(values = data.scores.shape) +
  theme(legend.key.size=unit(0.3,"cm")) +
  theme_bw() 
P

## actually gonna change geompoint shapes as that cross thing is annoying

P <- ggplot(data.scores, aes(x= NMDS1, y= NMDS2, colour=treatment, shape=phase, label=time))+
  geom_point(size=15, stroke=2)  + # size and line thickness of plotted points
  scale_shape_manual(values = c(1, 5, 22)) +
  theme(legend.key.size=unit(0.3,"cm")) +
  theme_bw() 
P

# label each point with time. Put colour=control
# as want all these labels in black text, otherwise looks messy.
# play with hjust and vjust to move text around
# h just moves text left to right ( left bigger the number)

P2 <- P +geom_text(aes(label=time, colour="Control"),hjust=0.45, vjust=0.3, 
                   size=4, fontface = "bold") 

P2 + labs(shape="Phase", colour="Treatment") + # update legend titles.
  # change legend icon size for Treatment
  guides(color = guide_legend(override.aes = list(size=7))) + 
  # change legend icon size for phase
  guides(shape = guide_legend(override.aes = list(size=6))) +
  # change spacing of legend icons. 
  theme(legend.key.size = unit(1.8,"line"))


# done. Play with export dims to get best. I found around 1200 width
# was good but it depends how big you made points in the end. 

## 
  
