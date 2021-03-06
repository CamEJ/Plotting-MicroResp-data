# catabolic evenness from microresp data

library(vegan)

pod <- read.table("MResp-BioReps_UnTransformed.txt", sep = "\t", header=T, row.names=1)
meta <- read.table("Mresp-metadata.txt", header = TRUE, row.names = 1, sep='\t')


Shan = diversity(pod, "shannon") # actually think I'll stick with simpson. 

H= diversity(pod, "simpson") # calculate diversity

J <- Shan/log(specnumber(pod)) # calculate evenness 
                                # where evenness = div/(log species no in sample)

# got this from here
# http://www2.uaem.mx/r-mirror/web/packages/vegan/vignettes/diversity-vegan.pdf

# put results into dfs for plotting. 
resDiv = as.data.frame(Shan) # make into df. 

resEven = as.data.frame(J) # make into df. 

MDiv = resDiv[,1]
MDiv = as.data.frame(MDiv) # make into df. 
colnames(MDiv)[1] <- "div" # give this variable a names
rownames(MDiv) = row.names(pod) # assign row names from orignal df
MDiv$even = resEven[,1] # add evenness data

#now add metadata info for plotting

MDiv$treatment = meta$Treatment
MDiv$time = meta$timepoint

write.csv(MDiv, "CatabolicEvenness&Div.csv")

# run setFactorOrder Function
MDiv[["treatment"]] <- setFactorOrder(MDiv[["treatment"]], c("Control", "Slurry", "Flood", "Flood+Slurry"))
MDiv[["time"]] <- setFactorOrder(MDiv[["time"]], c("T0", "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11", "T12", "T13"))




c <- ggplot(MDiv, aes(factor(treatment), div, fill = factor(treatment))) +
  
  ## + geom_boxplot so it knows what type of plot
  # and put colour = black to make lines of box black. 
  
  geom_boxplot(colour="black") +
  scale_fill_manual(values=c("black", "chocolate4", "slateblue", "olivedrab"))
c4 = c + facet_wrap(~time, ncol = 3)



rep <- c4 + labs(fill="    Treatment ", y = " Catabolic diversity") +
  
  ## specify labels for axes and plot title if required
  
  theme_bw() +
  
  
  ## change text size and placing of axes and titles
  ## ensure , at end of each line 
  ## legend.key.size changes size of legend and required 'grid' package to be loaded for unit() to work
  
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(size=16, vjust=0.5, colour = "black"),
        axis.title.y=element_text(size=18, vjust=1, colour="black"),
        legend.text=element_text(size=16, vjust=0.5),
        legend.title=element_blank(),
        legend.key.size=unit(1.25, "cm"),
        axis.title.x=element_blank(),
        #legend.direction = "horizontal",
        strip.text.x = element_text(size = 18, colour = "black"),# change font of facet label
        strip.background =  element_rect(fill = "white"),
        legend.position = c(1, 0), legend.justification = c(1, 0)
        ) # remove grey backgroup of facet label

rep

rep + guides(fill = guide_legend(ncol=2))

# =============== strip plot / dot plot


c <- ggplot(MDiv, aes(factor(treatment), even, fill = factor(treatment))) +
  
  ## + geom_boxplot so it knows what type of plot
  # and put colour = black to make lines of box black. 
  
  geom_point(shape=21, size = 5) +
  scale_fill_manual(values=c("black", "chocolate4", "slateblue", "olivedrab"))
c4 = c + facet_wrap(~time, ncol = 3)

rep <- c4 + labs(fill="    Treatment ", y = " Catabolic evenness\n") +
  
  ## specify labels for axes and plot title if required
  
  theme_bw() +
  
  
  ## change text size and placing of axes and titles
  ## ensure , at end of each line 
  ## legend.key.size changes size of legend and required 'grid' package to be loaded for unit() to work
  
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(size=16, vjust=0.5, colour = "black"),
        axis.title.y=element_text(size=18, vjust=1, colour="black"),
        legend.text=element_text(size=16, vjust=0.5),
        legend.title=element_blank(),
        legend.key.size=unit(1.25, "cm"),
        axis.title.x=element_blank(),
        legend.direction = "horizontal",
        strip.text.x = element_text(size = 18, colour = "black"),# change font of facet label
        strip.background =  element_rect(fill = "white"), # remove grey backgroup of facet label
        legend.position = "top"
  ) # remove grey backgroup of facet label

rep

#rep + guides(fill = guide_legend(ncol=2))




# --------- with transformed 

pod <- read.table("MResp-BioReps_UnTransformed.txt", sep = "\t", header=T, row.names=1)
meta <- read.table("Mresp-metadata.txt", header = TRUE, row.names = 1, sep='\t')

m <- as.matrix(pod)


## Rescale each column to range between 0 and 1
# where each c source is a different column

poo <- apply(m, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

pool <- as.data.frame(poo)


# function(x) x / sum(x) )

sapply(poo, function(cl) list(means=mean(cl,na.rm=TRUE), sds=sd(cl,na.rm=TRUE)))


head(poo)


X2 = function(x){x/sum(x, na.rm=TRUE)}
# apply the function where you change second argument to 2 if you
# want it by column instead of row as in here. 
res = apply(pod,1,X2)

res = as.data.frame(res) # make into df. 
rest = t(res) # transform output from applt
rest = as.data.frame(rest)

Shan = diversity(rest, "shannon") # actually think I'll stick with simpson. 

H= diversity(rest, "simpson") # calculate diversity

J <- Shan/log(specnumber(rest)) # calculate evenness 
# where evenness = div/(log species no in sample)

# got this from here
# http://www2.uaem.mx/r-mirror/web/packages/vegan/vignettes/diversity-vegan.pdf

# put results into dfs for plotting. 
resDiv = as.data.frame(Shan) # make into df. 

resEven = as.data.frame(J) # make into df. 

MDiv = resDiv[,1]
MDiv = as.data.frame(MDiv) # make into df. 
colnames(MDiv)[1] <- "div" # give this variable a names
rownames(MDiv) = row.names(rest) # assign row names from orignal df
MDiv$even = resEven[,1] # add evenness data

#now add metadata info for plotting

MDiv$treatment = meta$Treatment
MDiv$time = meta$timepoint

write.csv(MDiv, "CatabolicEvenness&Div.csv")

# run setFactorOrder Function
MDiv[["treatment"]] <- setFactorOrder(MDiv[["treatment"]], c("Control", "Slurry", "Flood", "Flood+Slurry"))
MDiv[["time"]] <- setFactorOrder(MDiv[["time"]], c("T0", "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11", "T12", "T13"))




c <- ggplot(MDiv, aes(factor(treatment), div, fill = factor(treatment))) +
  
  ## + geom_boxplot so it knows what type of plot
  # and put colour = black to make lines of box black. 
  
  geom_boxplot(colour="black") +
  scale_fill_manual(values=c("black", "chocolate4", "slateblue", "olivedrab"))
c4 = c + facet_wrap(~time, ncol = 3)



rep <- c4 + labs(fill="    Treatment ", y = " Catabolic diversity") +
  
  ## specify labels for axes and plot title if required
  
  theme_bw() +
  
  
  ## change text size and placing of axes and titles
  ## ensure , at end of each line 
  ## legend.key.size changes size of legend and required 'grid' package to be loaded for unit() to work
  
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(size=16, vjust=0.5, colour = "black"),
        axis.title.y=element_text(size=18, vjust=1, colour="black"),
        legend.text=element_text(size=16, vjust=0.5),
        legend.title=element_blank(),
        legend.key.size=unit(1.25, "cm"),
        axis.title.x=element_blank(),
        #legend.direction = "horizontal",
        strip.text.x = element_text(size = 18, colour = "black"),# change font of facet label
        strip.background =  element_rect(fill = "white"),
        legend.position = c(1, 0), legend.justification = c(1, 0)
  ) # remove grey backgroup of facet label

rep

rep + guides(fill = guide_legend(ncol=2))

## didnt make much diff, transforming or not, so went with untransformed

# -------------------- this stuff below is wrong but I will leave it because
# ---------------- might be useful for later, working w formulae etc. --------- ###


# make a function where each substrate (x) is divided by the sum 
# of the respiration responses for all substrates. 
# this gives "pi" value in evennes equation

X2 = function(x){x/sum(x, na.rm=TRUE)}
# apply the function where you change second argument to 2 if you
# want it by column instead of row as in here. 
res = apply(pod,1,X2)

res = as.data.frame(res) # make into df. 
rest = t(res) # transform output from applt
rest = as.data.frame(rest)

# now you need to sum the whole row for each sample
# first make a df to store the data
even = meta[,1]
even = as.data.frame(even)
rownames(even) <- rownames(pod)

# make a function to square the pi value for each substrate
X3 <- function(x){ x^2 }
resty = apply(rest,1,X3) # apply the function and give a name to the output
resty = as.data.frame(resty) # make into df. 
restyt = t(resty)  #transform
restyt = as.data.frame(restyt)

# now make a new column in your df "even" where you sum together all pi2 values
# this gets output to a column called Epi
even$Epi = apply(restyt, 1, function(x) { sum(x, na.rm=TRUE) }) # get row sum
head(even)

# last thing it to make it 1/Epi2
X4 <- function(x){ 1/x }

even2 = even[,2]
even2 = as.data.frame(even2)
even$E = apply(even2,1,X4)


## now add extra sample info to this df for plotting

even$treatment<- meta$Treatment
even$time<- meta$timepoint
even$phase<- meta$Phase.1

# put in corrrect order. 
even[["treatment"]] <- setFactorOrder(even[["treatment"]], c("Control", "Slurry", "Flood", "Flood+Slurry"))
even[["time"]] <- setFactorOrder(even[["time"]], c("T0", "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11", "T12", "T13"))
even[["phase"]] <- setFactorOrder(even[["phase"]], c("NoFlood", "Flooded", "Recovery"))


write.csv(even, "CatabolicEvennessMicroResp.csv")




# ---------------------- and now to plotting --------------------------



c <- ggplot(even, aes(factor(treatment), E, fill = factor(treatment))) +
  
  ## + geom_boxplot so it knows what type of plot
  # and put colour = black to make lines of box black. 
  
  geom_boxplot(colour="black") 
c + facet_wrap(~time, ncol = 3)

c <- ggplot(even, aes(factor(time), E, fill = factor(treatment))) +
  
  ## + geom_boxplot so it knows what type of plot
  # and put colour = black to make lines of box black. 
  
  geom_boxplot(colour="black") 
c + facet_wrap(~treatment, ncol = 2)


evenCut = subset(even, (time %in% c("T0", "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T9", "T10", "T11", "T12", "T13")))

c <- ggplot(evenCut, aes(factor(time), E, fill = factor(treatment))) +
  
  ## + geom_boxplot so it knows what type of plot
  # and put colour = black to make lines of box black. 
  
  geom_boxplot(colour="black") 

re = c + facet_wrap(~treatment, ncol = 2)

rep <- re + labs(fill="    Treatment ", y = " Catabolic evenness") +
  
  ## specify labels for axes and plot title if required
  
  theme_bw() +
  
  
  ## change text size and placing of axes and titles
  ## ensure , at end of each line 
  ## legend.key.size changes size of legend and required 'grid' package to be loaded for unit() to work
  
  theme(axis.text.x=element_text(size=14, vjust=0.5, colour = "black"), 
        axis.text.y=element_text(size=16, vjust=0.5, colour = "black"),
        axis.title.y=element_text(size=16, vjust=1, colour="black"),
        legend.text=element_text(size=16, vjust=0.5),
        legend.title=element_text(size=16, vjust=0.5),
        legend.key.size=unit(1.5, "cm"),
        axis.title.x=element_blank(),
        strip.text.x = element_text(size = 18, colour = "black"),# change font of facet label
        strip.background =  element_rect(fill = "white") ) # remove grey backgroup of facet label

rep

## -------------------- wrap by day

c <- ggplot(evenCut, aes(factor(treatment), E, fill = factor(treatment))) +
  
  ## + geom_boxplot so it knows what type of plot
  # and put colour = black to make lines of box black. 
  
  geom_boxplot(colour="black") 
re = c + facet_wrap(~time, ncol = 3)
rep <- re + labs(fill="    Treatment ", y = " Catabolic evenness") +
  
  ## specify labels for axes and plot title if required
  
  theme_bw() + 
  
  
  ## change text size and placing of axes and titles
  ## ensure , at end of each line 
  ## legend.key.size changes size of legend and required 'grid' package to be loaded for unit() to work
  
  theme(#axis.text.x=element_text(size=14, vjust=0.5, colour = "black"), 
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_text(size=16, vjust=0.5, colour = "black"),
        axis.title.y=element_text(size=16, vjust=1, colour="black"),
        legend.text=element_text(size=16, vjust=0.5),
        legend.title=element_blank(),
        legend.key.size=unit(1.1, "cm"),
        axis.title.x=element_blank(),
        strip.text.x = element_text(size = 18, colour = "black"),# change font of facet label
        strip.background =  element_rect(fill = "white"),
    legend.direction = "horizontal") # remove grey backgroup of facet label

rep +
theme(legend.position = c(1, 0), legend.justification = c(1, 0))

