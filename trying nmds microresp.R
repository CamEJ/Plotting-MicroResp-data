
# trying nmds microresp
setwd("C:/Users/Camilla/Dropbox/Data & analysis/WP3 Slurry disturbance/R")

library(vegan)
library(MASS)
library(ggplot2)

pod <- read.table("Mresp-T12-AllUntransformed.txt", sep = "\t", header=T, row.names=1)

MR <- t(pod)
#P.dist <- dist(MR, method="euclidean")

v.dist <- vegdist(MR, method="euclidean", binary=FALSE, diag=FALSE, upper=FALSE)


NMDS2 <- metaMDS(v.dist, distance = "euclidean", k = 2, trymax = 20, autotransform =TRUE,
        noshare = 0.1, wascores = TRUE, expand = TRUE, trace = 1,
        old.wa = FALSE)

plot(NMDS2, type="t")

# read in metadata
meTa <- read.table("MetaData.txt", header = TRUE, row.names = 1, sep='\t')

plot(NMDS2$points, col = meTa$timepoint)

# making a dataframe with NMDS data and metadata in one so can plot
# using ggplot

# first convert metaMDS output into a dataframe
data.scores <- as.data.frame(scores(NMDS2)) # have a look at it and check ok

# choose row names (IDS) from metadata file that i read in just now 
# as one of columns. 

data.scores$id<- rownames(data.scores)
# now add info re treatment and sample day as two extra colums in data.scores
# data frame

data.scores$treatment<- meTa$Treatment
data.scores$time<- meTa$timepoint
data.scores$time <- factor(data.scores$time, levels = data.scores$time )


##
NMDSplot <- ggplot(data.scores, title='nMDS of MicroResp') +
  geom_point(aes(x=NMDS1, y=NMDS2, color=time))
NMDSplot

##Modify shapes so can display the different treatments within the days

data.scores.shape.names = unique(data.scores$treatment)
data.scores.shape <- 1:(length(data.scores.shape.names))
names(data.scores.shape) <- data.scores.shape.names
data.scores.shape["samples"] <- 46




nMDS.Plot <- ggplot(data.scores) +
  geom_point(aes(x=NMDS1, y=NMDS2, color=time, shape = treatment),size=3)  +
  scale_shape_manual(values = data.scores.shape) +
  theme(legend.key.size=unit(0.3,"cm"))
nMDS.Plot


####### now try it on transformed data - this is what I will present so far
## meed to find out if it's correct to use transformed or not - 3/4/2017. 

pod <- read.table("mrespSubsHeatMap-transformedexArg.txt", sep = "\t", header=T, row.names=1)

MR <- t(pod)
#P.dist <- dist(MR, method="euclidean")

v.dist <- vegdist(MR, method="euclidean", binary=FALSE, diag=FALSE, upper=FALSE)


NMDS2 <- metaMDS(v.dist, distance = "euclidean", k = 2, trymax = 20, autotransform =TRUE,
                 noshare = 0.1, wascores = TRUE, expand = TRUE, trace = 1,
                 old.wa = FALSE)

# Copy and paste what's printed to console into notepad for ref.

# have a wee look at what you have. 
# first the stress
stressplot(NMDS2) 
# then basic plot of nmds. 
plot(NMDS3, type="t")

# read in metadata
meTa <- read.table("MetaData.txt", header = TRUE, row.names = 1, sep='\t')

plot(NMDS2$points, col = meTa$timepoint)

# making a dataframe with NMDS data and metadata in one so can plot
# using ggplot

# first convert metaMDS output into a dataframe
data.scores <- as.data.frame(scores(NMDS2)) # have a look at it and check ok

# choose row names (IDS) from metadata file that i read in just now 
# as one of columns. 

data.scores$id<- rownames(data.scores)
# now add info re treatment and sample day as two extra colums in data.scores
# data frame. Did factor() to keep them in order that I input them. 

data.scores$treatment<- meTa$Treatment
data.scores$treatment <- factor(data.scores$treatment, levels = data.scores$treatment)
data.scores$time<- meTa$timepoint
data.scores$time <- factor(data.scores$time, levels = data.scores$time)
data.scores$phase<- meTa$Phase
data.scores$phase <- factor(data.scores$phase, levels = data.scores$phase)


##
NMDSplot <- ggplot(data.scores, title='nMDS of MicroResp') +
  geom_point(aes(x=NMDS1, y=NMDS2, color=time))
NMDSplot


## Modify shapes so can display the different phase within the days

data.scores.shape.names = unique(data.scores$phase)
data.scores.shape <- 1:(length(data.scores.shape.names))
names(data.scores.shape) <- data.scores.shape.names
data.scores.shape["samples"] <- 46

#in the end not actually sure I need to do this as I'm going to specify which
# shapes I want to use. 

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

P2 <- P +geom_text(aes(label=time, colour="Control"),hjust=0.4, vjust=0.3, 
                      size=5, fontface = "bold") 

P2 + labs(shape="Phase", colour="Treatment") + # update legend titles.
  # change legend icon size for Treatment
  guides(color = guide_legend(override.aes = list(size=7))) + 
  # change legend icon size for phase
  guides(shape = guide_legend(override.aes = list(size=6))) +
  # change spacing of legend icons. 
  theme(legend.key.size = unit(1.8,"line"))
   
  
 # done. Play with export dims to get best. I found around 1200 width
# was good but it depends how big you made points in the end. 

  