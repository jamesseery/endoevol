library(ape)
library(caper)
library(corHMM)

### To read tree and data in, and to match
tempotree = read.tree("tempo_scrubbed_CONSTRAINT_rooted.dated.tre")
data = read.delim("data.txt") ### Data must in be 2 columns (tab delimited), one labeled 'gs' with names in Genus_species format, and the second 'state' column with numbers (1, 2, 3,...) indicating state

### To be compatible with caper comparative data command, must label nodes
tempotree.labeled = makeLabel(tempotree)

### To match tree with data and output the tree and data table containing only matched data
matched.files = comparative.data(tempotree.labeled,data,gs,vcv=F)
write.tree(matched.files$phy,file="matched.phy")
write.table(matched.files$data, "state.data", sep="\t")

### Read tree and data back into R
matched.tree = read.tree("matched.phy")
state.data = read.delim("state.data")

ard.model.01an = rayDISC(matched.tree,state.data,ntraits=1,charnum=1,model="ARD",node.state="marginal",root.p=c(1,0,0)) 
###root.p command must have the same number of states as data. This command sets state 1 as ancestral, but that can be changed.
###'ard' referes to asymmetric transition rate model (i.e., it allows transitions between states to vary).

ard.model.null = rayDISC(matched.tree,state.data,ntraits=1,charnum=1,model="ARD",node.state="marginal")
###this version has the root.p command missing, which means no assumption is made about the ancestral state. Can compare likelihood scores to see if changing assumptions
###about ancestral state changes model fit.

er.model.01an = rayDISC(matched.tree,state.data,ntraits=1,charnum=1,model="ER",node.state="marginal",root.p=c(1,0,0)) 
###this version makes state 1 ancestral, but forces transition rates between states to be equal. You can compare the likelihood scores of the ARD vs the ER model to see
###if transtion rates are significantly different from equality.

er.model.null = rayDISC(matched.tree,state.data,ntraits=1,charnum=1,model="ER",node.state="marginal")
###as previous but no root state assigned. 


### To fit models where rates between any pair of states are held to equality or zero
### obtain the rate matrix from one of the above models. Let's assume that ard.model.null is the best fit.

ard.model.null$index.mat

#      1  2  3  
# 1    NA 3  5  
# 2    1  NA 6  
# 3    2  4  NA 

###This is the matrix. You have 6 pairwise transition rates between 3 states.
### To constrain transitions between any two states to be equal:

rate.mat = rate.mat.maker(hrm=FALSE,ntraits=1,nstates=3,model="ARD")
rate.mat = rate.par.eq(rate.mat, eq.par=c(1,3)) ##makes the rate between state 1-2 (value 1 on the matrix) and state 2-1 (value 3), equal.

### now run the model
ard.model.null.12eq = rayDISC(matched.tree,state.data,ntraits=1,charnum=1,model="ARD",node.state="marginal",rate.mat=rate.mat)

###You can compare the loglik of this model to the fully asymmetric one to see if making transitions between states 1 and 2 equal results in a poorer fit.
###If its a poorer fit, then transitions between 1 and 2 must be asymmetric. If the fit does not change, then rates between 1 and 2 are not different from equality.
###You can do this iteratively to find which rates in the asymmetric model are significantly asymmetric.


### To constrain transitions between any two states to be zero:
rate.mat = rate.mat.maker(hrm=FALSE,ntraits=1,nstates=3,model="ARD")
rate.mat = rate.par.eq(rate.mat, drop.par=c(1,3)) ##makes the rate between state 1-2 (position 1 on the matrix) and state 2-1 (position 3), zero (i.e., dropping the parameter makes it zero).

### now run the model
ard.model.null.12omit = rayDISC(matched.tree,state.data,ntraits=1,charnum=1,model="ARD",node.state="marginal",rate.mat=rate.mat)

###As with the equal rates comparision, if the model that drops rates between states 1 and 2 is a poorer fit to the data, you can state that the rates between these states are signfiantly
###different from zero. If the model fit is the same with transtions from state 1 to 2 dropped, then you can say that rates between these states are not significantly different from zero.

###You can do this iteratively to find which rates in the model are significantly different from zero. This will help determine what the most likely pathway for transitions between the states are.