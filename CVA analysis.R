

# trying CVA analysis

# followed the tutorial found here:
# https://rdrr.io/cran/Morpho/man/CVA.html

# this is paper where i got this idea from 

# https://link.springer.com/article/10.1007/s00374-011-0554-4#Sec8


pod <- read.table("MResp-BioReps_UnTransformed.txt", sep = "\t", header=T, row.names=1)

meta <- read.table("Mresp-metadata.txt", header = TRUE, row.names = 1, sep='\t')


library("Morpho")
install.packages("car")
### Morpho CVA

vari <- pod[,1:11]
facto <- meta[,6] # define factor you want to use to explain data. 

cva.1=CVA(vari, groups=facto)
## get the typicality probabilities and resulting classifications - tagging
## all specimens with a probability of < 0.01 as outliers (assigned to no class)
typprobs <- typprobClass(cva.1$CVscores,groups=facto)
print(typprobs)
## visualize the CV scores by their groups estimated from (cross-validated)
## typicality probabilities:
if (require(car)) {
  scatterplot(cva.1$CVscores[,1],cva.1$CVscores[,2],groups=typprobs$groupaffinCV,
              smooth=FALSE,reg.line=FALSE)
}
# plot the CVA
plot(cva.1$CVscores, col=facto, pch=as.numeric(facto), typ="n",asp=1,
     xlab=paste("1st canonical axis", paste(round(cva.1$Var[1,2],1),"%")),
     ylab=paste("2nd canonical axis", paste(round(cva.1$Var[2,2],1),"%")))

text(cva.1$CVscores, as.character(facto), col=as.numeric(facto), cex=.7)


# add chull (merge groups)
for(jj in 1:length(levels(facto))){
  ii=levels(facto)[jj]
  kk=chull(cva.1$CVscores[facto==ii,1:2])
  lines(cva.1$CVscores[facto==ii,1][c(kk, kk[1])],
        cva.1$CVscores[facto==ii,2][c(kk, kk[1])], col=jj)
}

# add 80% ellipses
if (require(car)) {
  for(ii in 1:length(levels(facto))){
    dataEllipse(cva.1$CVscores[facto==levels(facto)[ii],1],
                cva.1$CVscores[facto==levels(facto)[ii],2], 
                add=TRUE,levels=.80, col=c(1:7)[ii])}
}
# histogram per group
if (require(lattice)) {
  histogram(~cva.1$CVscores[,1]|facto,
            layout=c(1,length(levels(facto))),
            xlab=paste("1st canonical axis", paste(round(cva.1$Var[1,2],1),"%")))
  histogram(~cva.1$CVscores[,2]|facto, layout=c(1,length(levels(facto))),
            xlab=paste("2nd canonical axis", paste(round(cva.1$Var[2,2],1),"%")))
} 
# plot Mahalahobis
dendroS=hclust(cva.1$Dist$GroupdistMaha)
dendroS$labels=levels(facto)
par(mar=c(4,4.5,1,1))
dendroS=as.dendrogram(dendroS)
plot(dendroS, main='',sub='', xlab="Geographic areas",
     ylab='Mahalahobis distance')


# Variance explained by the canonical roots:
cva.1$Var
# or plot it:
barplot(cva.1$Var[,2])


# -------------------- phase 2 ---------------



vari <- pod[19:78,1:11]
facto <- meta[19:78,6] # define factor you want to use to explain data. 

cva.1=CVA(vari, groups=facto)
## get the typicality probabilities and resulting classifications - tagging
## all specimens with a probability of < 0.01 as outliers (assigned to no class)
typprobs <- typprobClass(cva.1$CVscores,groups=facto)
print(typprobs)
## visualize the CV scores by their groups estimated from (cross-validated)
## typicality probabilities:


## typicality probabilities:
if (require(car)) {
  scatterplot(cva.1$CVscores[,1],cva.1$CVscores[,2],groups=typprobs$groupaffinCV,
              smooth=FALSE,reg.line=FALSE)
}

# plot the CVA
plot(cva.1$CVscores, col=facto, pch=as.numeric(facto), typ="n",asp=1,
     xlab=paste("1st canonical axis", paste(round(cva.1$Var[1,2],1),"%")),
     ylab=paste("2nd canonical axis", paste(round(cva.1$Var[2,2],1),"%")))

text(cva.1$CVscores, as.character(facto), col=as.numeric(facto), cex=.7)

# ---------- get the data out to plot somewhere more pleasant. 

data = rownames(meta)
data = as.data.frame(data)

# for phase 2 
dataP2 = data[19:78,]
dataP2 = as.data.frame(dataP2)
dataP2$cv1 = cva.1$CVscores[,1]
dataP2$cv2 = cva.1$CVscores[,2]
dataP2$typ = typprobs$groupaffinCV

write.csv(dataP2, "CVA-outputPhase2.csv")


#------------------------- phase 1 & 2------------------------#



vari <- pod[1:78,1:11]
facto <- meta[1:78,6] # define factor you want to use to explain data. 

cva.1=CVA(vari, groups=facto)
## get the typicality probabilities and resulting classifications - tagging
## all specimens with a probability of < 0.01 as outliers (assigned to no class)
typprobs <- typprobClass(cva.1$CVscores,groups=facto)
print(typprobs)
## visualize the CV scores by their groups estimated from (cross-validated)
## typicality probabilities:


## typicality probabilities:
if (require(car)) {
  scatterplot(cva.1$CVscores[,1],cva.1$CVscores[,2],groups=typprobs$groupaffinCV,
              smooth=FALSE,reg.line=FALSE)
}

# plot the CVA
plot(cva.1$CVscores, col=facto, pch=as.numeric(facto), typ="n",asp=1,
     xlab=paste("1st canonical axis", paste(round(cva.1$Var[1,2],1),"%")),
     ylab=paste("2nd canonical axis", paste(round(cva.1$Var[2,2],1),"%")))

text(cva.1$CVscores, as.character(facto), col=as.numeric(facto), cex=.7)

# -------------- write out to plot more prettily
# for phase 1 &2 
dataP1y2 = data[1:78,]
dataP1y2 = as.data.frame(dataP1y2)
dataP1y2$cv1 = cva.1$CVscores[,1]
dataP1y2$cv2 = cva.1$CVscores[,2]
dataP1y2$typ = typprobs$groupaffinCV



write.csv(dataP1y2, "CVA-outputPhase1&2.csv")

  
  

    # -------------------- phase 3 ---------------
    
    
    
    vari <- pod[79:150,1:11]
    facto <- meta[79:150,6] # define factor you want to use to explain data. 
    
    cva.1=CVA(vari, groups=facto)
    ## get the typicality probabilities and resulting classifications - tagging
    ## all specimens with a probability of < 0.01 as outliers (assigned to no class)
    typprobs <- typprobClass(cva.1$CVscores,groups=facto)
    print(typprobs)
    ## visualize the CV scores by their groups estimated from (cross-validated)
    ## typicality probabilities:
    
    
    ## typicality probabilities:
    if (require(car)) {
      scatterplot(cva.1$CVscores[,1],cva.1$CVscores[,2],groups=typprobs$groupaffinCV,
                  smooth=FALSE,reg.line=FALSE)
    }
    
    # plot the CVA
    plot(cva.1$CVscores, col=facto, pch=as.numeric(facto), typ="n",asp=1,
         xlab=paste("1st canonical axis", paste(round(cva.1$Var[1,2],1),"%")),
         ylab=paste("2nd canonical axis", paste(round(cva.1$Var[2,2],1),"%")))
    
    text(cva.1$CVscores, as.character(facto), col=as.numeric(facto), cex=.7)
    
    # ---------- get the data out to plot somewhere more pleasant. 
    

    dataP3 = data[79:150,]
    dataP3 = as.data.frame(dataP3)
    dataP3$cv1 = cva.1$CVscores[,1]
    dataP3$cv2 = cva.1$CVscores[,2]
    dataP3$typ = typprobs$groupaffinCV
    
    write.csv(dataP3, "CVA-outputPhase3.csv")
    
