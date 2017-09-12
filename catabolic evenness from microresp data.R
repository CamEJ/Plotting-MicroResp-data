# catabolic evenness from microresp data


pod <- read.table("MResp-BioReps_UnTransformed.txt", sep = "\t", header=T, row.names=1)

meta <- read.table("Mresp-metadata.txt", header = TRUE, row.names = 1, sep='\t')


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

