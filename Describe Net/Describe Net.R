## ****************************************************
## Created: Adam Cooper, Cetis, Oct 2013
## This source code was produced for The University of
## Edinburgh DEI as part of their MOOC initiative.
## Use saved igraph data (graphml)
## to calculate various network-level statistics
## including distributions of degree
## ****************************************************

# this is meant to be used from RStudio wit knitr but should work separately
# test if courseIDs has already been defined (in Rmd), otherwise default
if(!exists("courseIDs")){
   courseIDs<-c("aiplan","astro","crit","edc","equine","intro")
   tie.type="P-C"
}

## @knitr INIT
library("igraph")
store.dir<-"~/R Projects/Edinburgh MOOC/EdMOOC-SNA/Network Data/" #where the extracted networks were stored to

# load graphml into igraph objects
igraphList<-lapply(courseIDs, function(x){read.graph(paste(store.dir,tie.type," ", x,group,".graphml",sep=""), format="graphml")})
# igraphList<-list()
# for(i in 1:length(courseIDs)){
#    igraphList[[i]]<-read.graph(paste(store.dir,tie.type," ", courseIDs[i],".graphml",sep=""), format="graphml")
# }

# all networks in a given set (tie.type) are assumed to be either directed or undirected and the 
# work done in SINGLES assumes this to create a sensible summary table. Hence the following line is outside the loop
directed.net<-is.directed(igraphList[[1]])

## @knitr SINGLES
# these are single stats for the each graph, rather than distributions
if(directed.net){
   singles.df<-data.frame(rep(numeric(),8))
}else{
   singles.df<-data.frame(rep(numeric(),5))
}
for(i in 1:length(courseIDs)){
   #    #graph level degree centrality is normalised relative to the max possible for #nodes and #vertices
   one.row<-c(nodes=vcount(igraphList[[i]]),
      ecount(igraphList[[i]]),
      sum(degree(igraphList[[i]])==0),
      1000*graph.density(igraphList[[i]], loops=FALSE),
      diameter(igraphList[[i]]),
      centralization.degree(igraphList[[i]], mode="all", loops=FALSE, normalized=TRUE)$centralization,
   centralization.closeness(igraphList[[i]], mode="all", normalized=TRUE)$centralization,
   centralization.betweenness(igraphList[[i]], directed=FALSE, normalized=TRUE)$centralization)
   if(directed.net){
      dyads<-dyad.census(igraphList[[i]])
      one.row<-c(one.row,                 
                 centralization.degree(igraphList[[i]],
                                       mode="in", loops=FALSE, normalized=TRUE)$centralization, #in degree
                 centralization.degree(igraphList[[i]],
                                       mode="out", loops=FALSE, normalized=TRUE)$centralization, #out degree
                 dyads$mut,
                 dyads$asym)
   }
   singles.df<-rbind(singles.df,one.row)
}
if(directed.net){
   colnames(singles.df)<-c("nodes","edges","isolates","graph density*1000","diameter","degree","closeness","betweenness","in degree","out degree","mutual dyads","asymmetric dyads")
}else{
   colnames(singles.df)<-c("nodes","edges","isolates","graph density*1000","diameter", "degree","closeness","betweenness")
}
row.names(singles.df)<-courseIDs

# save the results. Useful for the larger, more dense, networks
if(group!=""){
   fname<-paste(tie.type, " ", group, ".RData", sep="")
}else{
   fname<-paste(tie.type, group, ".RData", sep="")
}

metadata<-list(project=basename(getwd()), created=date())
save(list=c("metadata", "singles.df", "tie.type", "group"), file=fname)
cat(paste("Saved to",fname,"\r\n"))

## ***Made available using the The MIT License (MIT)***
#The MIT License (MIT)
#Copyright (c) 2013 Adam Cooper, University of Bolton
#
#Permission is hereby granted, free of charge, to any person obtaining a copy of
#this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#    
#    The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
## ************ end licence ***************
