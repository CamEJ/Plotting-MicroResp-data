
# forest plots representing changes in basal respiration from control

library(ggplot2)

# the data used here is from the 'water only' wells of the microresp,
# with each data point being an average of the three bio reps (microcosms)
# the data file used has 4 colums, here is the head:

#     ID    BasalResp treatment timepoint      pSd          
#1 Tmt1_T0 0.4723870      Tmt1        T0 0.001871706 
#2 Tmt2_T0 1.4472063      Tmt2        T0 0.303400466 
#3 Tmt1_T1 0.4625339      Tmt1        T1 0.003786029 
#4 Tmt2_T1 0.7242768      Tmt2        T1 0.056182498 
#5 Tmt1_T2 0.4819327      Tmt1        T2 0.004009777

# where pSd is the standard dev of the 3 bio reps

# the excel doc from which this data is taken is in microresp data folder &
# is called Microresp-heatmap-collation.xls - and see appropriately labelled
# sheet of that book.

setwd("C:/Users/Camilla/Dropbox/Data & analysis/WP3 Slurry disturbance/Plotting Data/PhenotypicData-R")

D1 <- read.table("BasalResp-19April.txt", sep = "\t", header=T)

# run setFactorOrder function (R basics folder)
# then do the following to ensure in correct order when plotting

# 1 order you want timepoints to occur
D1[["timepoint"]] <- setFactorOrder(D1[["timepoint"]], c("T0", "T1", "T2", "T3", "T4", "T5","T6","T7", "T8", "T9","T10","T11","T12", "T13"))

# order of treatments. 
D1[["Treatment"]] <- setFactorOrder(D1[["Treatment"]], c("Control", "Slurry", "Flood", "Flood+Slurry"))


p <- ggplot(D1, aes(x=timepoint, y=BasalResp, ymin=BasalResp-pSd, ymax=BasalResp+pSd))+
  geom_pointrange(size=0.75)+
  geom_hline(yintercept = 0.535, linetype=2)+ # y intercept= Av of all control resp
  coord_flip(ylim = c(0.12, 1.7))+
  xlab('Time Point')+
  ylab(expression(paste('Basal Respiration (', mu, 'g ', C-CO[2],' ', g^-1, 'soil ', hr^-1, ' )' ))) +
  geom_point(colour="black", shape=21, size = 10) +
  aes(fill = factor(Treatment)) + 
  scale_fill_manual(values=c("black", "chocolate4", "slateblue", "olivedrab"))
p # print and check all ok. 


# now make axis font sizes better etc

p2 <- p + theme_bw() + 
  theme(axis.text.y=element_text(size=17, colour="black"),
  axis.text.x=element_text(size=16, colour="black"),
  axis.title.x=element_text(size=16),
  legend.text = element_text(size=15),
  legend.title = element_text(size=16),
  axis.title.y=element_blank())+
  labs(fill="Treatment") 
p2

# save as pdf

