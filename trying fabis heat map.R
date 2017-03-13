###########

pod <- read.table("mrespSubsHeatMap-transformedexArg.txt", sep = "\t", header=T, row.names=1)
x <- as.matrix(pod)

x2 <- t(x)

###defining the color --> check the package
library(RColorBrewer)
mypalette<-brewer.pal(9,"Blues")
help(brewer.pal)

## just to check the range
image(1:9,1,as.matrix(1:9),col=mypalette,xlab="Blues (sequential)",
      ylab="",xaxt="n",yaxt="n",bty="n")


#### To make the bar next to dendrogam to id tmts via colour
#tmt <- c(1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4)
tmt <- c(1,2,1,2,1,2,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)
f.tmt <- factor(tmt)
f.tmt
vec.tmt <- terrain.colors(nlevels(f.tmt))
# or 
vec.tmt <- rainbow.colors(nlevels(f.tmt)) start=.65,end=0.8)
# chose
vec.tmt <- brewer.pal(4, "Set1")

# or chose four colours from ggplot

library(ggplot2)
vec.tmt <- c("lightsalmon1", "sienna", "darkseagreen", "darkolivegreen")

# cont..
tmt.colour <- rep(0,length(f.tmt))
for(i in 1:length(f.tmt))
  tmt.colour[i] <- vec.tmt[ f.tmt[i]==levels(f.tmt) ]


#####build heatmap --> here I used a postcript to get high quality figure.

#setEPS()

#postscript(file="heatmap2.eps",horizontal=FALSE,onefile=FALSE,paper="special",family="Times",bg="white",width=7.0,height=7.0)


heatmap(x2,Colv=NA,col=mypalette,scale="column",RowSideColors=tmt.colour)

dev.off()
