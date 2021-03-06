## trying to plot microresp data as a heatmap
## like in fig 6 of this paper
## https://link.springer.com/article/10.1007%2Fs00248-014-0511-5

## taken from 
# http://stackoverflow.com/questions/13016022/ggplot2-heatmaps-using-different-gradients-for-categories

# WHERE I AM
## plot below works but as its transformed by column, it is transforming by sample 
# day I think instead of C source
## you need to figure out how to transform by row or something. 
# load necessary libs

library(ggplot2)
library("plyr")
library("reshape2")
library("scales")
library(gplots)

 # simple with dendrogram
 
 data <- read.table("mrespSubsHeatMap-transformed.txt", row.names=1, sep = "\t", header=T)
 x <- as.matrix(data)
 
 heatmap.2(x, tracecol = NA)

# read in data for more complex one
 # this is data that I transformed already. So for each substrate
 # the values are transformed to be within 0 - 1


pod <- read.table("mrespSubsHeatMap-transformed.txt", sep = "\t", header=T)

pod.m <- melt(pod)
# basic plot, no colour coding

ggplot(pod.m, aes(variable, Subs)) + 
  geom_tile(aes(fill = value), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + 
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 9) + 
  theme(legend.position = "none",
        axis.ticks = element_blank(), 
        axis.text.x = element_text(angle = 330, hjust = 0))

#------------------------------------------------------------
# if your data is not transformed already
# load non-transformed data and transform using R

data2 <- read.table("mrespSubsHeatMap.txt", sep = "\t", header=T)

class(data2)

# melt and transform



data2.m <- melt(data2)
data2.m <- ddply(data2.m, .(variable), transform, rescale = rescale(value))

# basic plot, no colour coding
ggplot(data2.m, aes(variable, Subs)) + 
  geom_tile(aes(fill = rescale), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + 
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 9) + 
  theme(legend.position = "none",
        axis.ticks = element_blank(), 
        axis.text.x = element_text(angle = 330, hjust = 0))

--------------------------------------------------------------------
  
# now doing it with colour coding
  
# define the sample id's that you want in each colour category
# 1st did by treatment

data2.m$Category <- data2.m$variable
levels(data2.m$Category) <- 
  list("Tmt1" = c("Tmt1_T0", "Tmt1_T1", "Tmt1_T2", "Tmt1_T3", "Tmt1_T4", "Tmt1_T5", 
                  "Tmt1_T6", "Tmt1_T7", "Tmt1_T8", "Tmt1_T9", "Tmt1_T10", "Tmt1_T11"),       
       "Tmt2" = c("Tmt2_T0", "Tmt2_T1", "Tmt2_T2", "Tmt2_T3", "Tmt2_T4", "Tmt2_T5", 
                       "Tmt2_T6", "Tmt2_T7", "Tmt2_T8", "Tmt2_T9", "Tmt2_T10", "Tmt2_T11"),
       "Tmt3" = c("Tmt3_T3", "Tmt3_T4", "Tmt3_T5", 
                   "Tmt3_T6", "Tmt3_T7", "Tmt3_T8", "Tmt3_T9", "Tmt3_T10", "Tmt3_T11"),
       "Tmt4" = c("Tmt4_T3", "Tmt4_T4", "Tmt4_T5", 
                  "Tmt4_T6", "Tmt4_T7", "Tmt4_T8", "Tmt4_T9", "Tmt4_T10", "Tmt4_T11"))



data2.m$rescaleoffset <- data2.m$rescale + 100*(as.numeric(data2.m$Category)-1)
scalerange <- range(data2.m$rescale)
gradientends <- scalerange + rep(c(0,100,200,300), each=2)
colorends <- c("white", "red", "white", "green", "white", "blue", "white", "plum4" )

##############################
## second did by sample day - it's better I think for seeing differences

# define which samples belong to which sample days

pod.m$Category <- pod.m$variable
levels(pod.m$Category) <- 
  list("T0" = c("Tmt1_T0", "Tmt2_T0"),  
       "T1" = c("Tmt1_T1", "Tmt2_T1"), 
        "T2" = c("Tmt1_T2", "Tmt2_T2"),
       "T3" = c("Tmt1_T3", "Tmt2_T3", "Tmt3_T3", "Tmt4_T3"),
       "T4" = c("Tmt1_T4", "Tmt2_T4", "Tmt3_T4", "Tmt4_T4"),
       "T5" = c("Tmt1_T5", "Tmt2_T5", "Tmt3_T5", "Tmt4_T5"),
       "T6" = c("Tmt1_T6", "Tmt2_T6", "Tmt3_T6", "Tmt4_T6"),
       "T7" = c("Tmt1_T7", "Tmt2_T7", "Tmt3_T7", "Tmt4_T7"),
       "T8" = c("Tmt1_T8", "Tmt2_T8", "Tmt3_T8", "Tmt4_T8"),
       "T9" = c("Tmt1_T9", "Tmt2_T9", "Tmt3_T9", "Tmt4_T9"),
       "T10" = c("Tmt1_T10", "Tmt2_T10", "Tmt3_T10", "Tmt4_T10"),
       "T11" = c("Tmt1_T11", "Tmt2_T11", "Tmt3_T11", "Tmt4_T11"),
       "T12" = c("Tmt1_T12", "Tmt2_T12", "Tmt3_T12", "Tmt4_T12")
      )



# change gradient ends in increments of 100 to = number of categories, in this case 12
# also add extra colours for 12 categories. The "white" between each colour is to give
# 'low end' of heatmap color spectrum
# the ggplot2 colours I chose came from 
# http://sape.inf.usi.ch/quick-reference/ggplot2/colour


pod.m$rescaleoffset <- pod.m$value + 100*(as.numeric(pod.m$Category)-1)
scalerange <- range(pod.m$value)
gradientends <- scalerange + rep(c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200), each=2)
colorends <- c("white", "firebrick", "white", "darkmagenta", "white", "orangered1", "white", "seagreen4", "white",
               "goldenrod2", "white", "royalblue4", "white", "springgreen4", "white", "darkred", "white", 
               "blue4", "white", "darkorange2", "white", "darkgreen", "white", "tomato", "white", "darkturquoise", "white")

# in rainbow order. Looks nice, but not very clear
# colorends <- c("white", "firebrick", "white", "tomato", "white", "orangered1", "white", "darkorange2", "white",
 #              "goldenrod2", "white", "seagreen4", "white", "springgreen4", "white", "darkgreen", "white", 
 #              "steelblue1", "white", "royalblue4", "white", "orchid", "white", "darkmagenta", "white")
#

# now plot it. 
ggplot(pod.m, aes(variable, Subs)) + 
  geom_tile(aes(fill = rescaleoffset), colour = "white") + 
  scale_fill_gradientn(colours = colorends, values = rescale(gradientends)) + 
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 9) + 
  theme(legend.position = "none",
        axis.ticks = element_blank(), 
        axis.text.x = element_text(angle = 310, hjust = 0, size=10))


####################### when samples grouped by sample day, the plot is now complete
##### if you grouped by treatment, you now want to group samples in treatmemt

# reorder
data2.m$variable2 <- reorder(data2.m$variable, as.numeric(data2.m$Category))
# re plot
ggplot(data2.m, aes(variable2, Subs)) + 
  geom_tile(aes(fill = rescaleoffset), colour = "white") + 
  scale_fill_gradientn(colours = colorends, values = rescale(gradientends)) + 
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 9) + 
  theme(legend.position = "none",
        axis.ticks = element_blank(), 
        axis.text.x = element_text(angle = 330, hjust = 0))

# done. 