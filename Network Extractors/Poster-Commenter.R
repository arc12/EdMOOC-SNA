## ****************************************************
## Created: Adam Cooper, Cetis, Oct 2013
## This source code was produced for The University of
## Edinburgh DEI as part of their MOOC initiative.
## Exctracts edge and node data according to rules for
## what constitutes a tie and saves to graphml and
## RData containing a network class object
## ****************************************************


library("igraph")
#establish connection to MySQL, loading library. contains coursera DB exports from 2013
library("RMySQL")
MySQL.driver <- MySQL()
source("./dbConnect.R")
#helper functions
source("./helpers.R")

courseIDs<-c("aiplan","astro","crit","edc","equine","intro")
#which forum IDs indicate an "introductions and interests" forum
intoductions.forumIDs<-c(10,12,2,25,2,11)#must be in same order as coursIDs.
echo.sql<-TRUE # echo SQL statements
store.dir<-"~/R Projects/Edinburgh MOOC/EdMOOC-SNA/Network Data/" #where to store the extracted network to
do.plot<-F

#group is inserted into output filenames. Used to separate all-forums from introductions-only
worker<-function(group=""){

#Iterate over the course-level list for some cleaning
for(i in 1:length(courseIDs)){
   edgeList1<-edgeList[[i]]
   nodeList1<-nodeList[[i]]
   #remove edges for nodes that cannot be found (i.e. withdrawn people)
   edgeList1<-edgeList1[edgeList1$commenter %in% nodeList1$forum_user_id,] #missing commenter
   edgeList[[i]]<-edgeList1[edgeList1$poster %in% nodeList1$forum_user_id,] #missing poster
   #do some mapping to a definition of region that is more natural for interpretation
   nodeList[[i]]<-cbind(nodeList1,region=apply(nodeList1,1,map.toRegion))
}

# temporarily load igraph to write out the network as graphml
library("igraph")
for(i in 1:length(courseIDs)){
   edgeList1<-edgeList[[i]]
   nodeList1<-nodeList[[i]]
   igraph1<- graph.data.frame(edgeList1, directed=TRUE, vertices=nodeList1[,c("forum_user_id","role","continent","country","region")])
   write.graph(igraph1,paste(store.dir,"P-C ", courseIDs[i],group,".graphml",sep=""), format="graphml")
}

#igraph and network packages conflict
detach("package:igraph")
library("network")

#Iterate over the course-level list to create "network" data structures for ergm
for(i in 1:length(courseIDs)){
   edgeList1<-edgeList[[i]]
   nodeList1<-nodeList[[i]]
   ## build network object, nodes first
   # initialise an empty network with the right number of vertices. Loop edges are not permitted.
   net1<-network.initialize(length(nodeList1[,1]), directed=TRUE, hyper=FALSE, loops=FALSE, multiple=FALSE)
   # add role attribute to the vertices
   set.vertex.attribute(net1, "anon_user_id", as.character(nodeList1[,"anon_user_id"]))
   set.vertex.attribute(net1, "role", as.character(nodeList1[,"role"]))
   set.vertex.attribute(net1, "country", as.character(nodeList1[,"country"]))
   set.vertex.attribute(net1, "continent", as.character(nodeList1[,"continent"]))
   set.vertex.attribute(net1, "region", as.character(nodeList1[,"region"]))
   network.vertex.names(net1)<-as.character(nodeList1$forum_user_id)
   # add edges, noting that the matrix supplied to network.edgelist() must contain the index numbers of
   # the already-created vertices and NOT the node identifiers in nodeList1.
   # Hence some look-ups are needed
   senders<-match(edgeList1$commenter,nodeList1$forum_user_id)
   receivers<-match(edgeList1$poster,nodeList1$forum_user_id)
   edge.mat<-cbind(senders,receivers)
   network.edgelist(edge.mat, net1)

   # saves network object
   name<-"Poster-Commenter"
   notes<-"net1 contains a network package object. *list1 contain separate edge and node data frames. 
            net1 does not allow loops and has binary edges but *list1 may have loops and has edge weights"
   fname<-paste(store.dir,"P-C ", courseIDs[i], group, ".RData", sep="")
   metadata<-list(project=basename(getwd()), origin=name, created=date(), notes=notes)
   save(list=c("metadata","net1","edgeList1","nodeList1"), file=fname)
   cat(paste("Saved to",fname,"\r\n"))
   
   if(do.plot){
      plot(net1)
   }
   
}
}

## For all SQL here. The wildcard ** is for replacement by vpodata_equine etc.

# edges = comment-on-post relationship, nodes = user (person)
# edges use forum_user_id
# edges includes users that have been deleted (no hash-mapping entry) hence do not have the corresponding node
# if I try to join to mapping from both fc and fp to exclude them, execution time is excessive. Not sure why.
edgeList.sql.a <-"SELECT fc.forum_user_id commenter, fp.forum_user_id poster, count(fc.id) weight
FROM **for.forum_comments fc, **for.forum_posts fp
WHERE fc.post_id = fp.id"
edgeList.sql.b<-"GROUP BY fc.forum_user_id, fp.forum_user_id ORDER by fc.forum_user_id"

# Use a union of poster and commenter ids to identify nodes.
# note that the 2nd select in the union does not contain "fc.post_id = fp.id AND", hence will include isolates
#                          (posters who got no comments)
nodeList.sql.a <- "SELECT m.forum_user_id, u.anon_user_id, u.access_group_id role,
                           c.ip_continent continent, c.ip_country country
                     FROM **gen.users u, **map.hash_mapping m, **gen.uoe_ip_country c,
                     ((SELECT m.anon_user_id FROM **for.forum_comments fc, **for.forum_posts fp,
                     **map.hash_mapping m WHERE fc.post_id = fp.id AND fc.forum_user_id = m.forum_user_id"
nodeList.sql.b<- ") UNION DISTINCT
                  (SELECT m.anon_user_id FROM **for.forum_posts fp, **map.hash_mapping m
                     WHERE fp.forum_user_id = m.forum_user_id"
nodeList.sql.c<- ")) i
            WHERE u.anon_user_id =i.anon_user_id AND m.anon_user_id =i.anon_user_id
               AND c.anon_user_id = u.anon_user_id"

#connect to DB
db<-conn()

#this recipe gets from threads in all forums
edgeList.sql<-paste(edgeList.sql.a,edgeList.sql.b)
nodeList.sql<-paste(nodeList.sql.a, nodeList.sql.b, nodeList.sql.c)
edgeList<-list.SELECT(db, courseIDs, edgeList.sql, echo=echo.sql)
nodeList<-list.SELECT(db, courseIDs, nodeList.sql, echo=echo.sql)
worker()

# additional clauses to limit to Introductions forums. ## will be replaced witht an id in list.limit.SELECT()
limitClause<- "AND fp.thread_id IN (SELECT ft.id FROM **for.forum_threads ft WHERE forum_id = ##)"
edgeList.sql<-paste(edgeList.sql.a, limitClause, edgeList.sql.b)
nodeList.sql<-paste(nodeList.sql.a, limitClause, nodeList.sql.b, limitClause, nodeList.sql.c)
edgeList<-list.limit.SELECT(db, courseIDs, edgeList.sql, intoductions.forumIDs, echo=echo.sql)
nodeList<-list.limit.SELECT(db, courseIDs, nodeList.sql, intoductions.forumIDs, echo=echo.sql)
worker("I")

limitClause<- "AND fp.thread_id NOT IN (SELECT ft.id FROM **for.forum_threads ft WHERE forum_id = ##)"
edgeList.sql<-paste(edgeList.sql.a, limitClause, edgeList.sql.b)
nodeList.sql<-paste(nodeList.sql.a, limitClause, nodeList.sql.b, limitClause, nodeList.sql.c)
edgeList<-list.limit.SELECT(db, courseIDs, edgeList.sql, intoductions.forumIDs, echo=echo.sql)
nodeList<-list.limit.SELECT(db, courseIDs, nodeList.sql, intoductions.forumIDs, echo=echo.sql)
worker("noI")

#end tidily
dbDisconnect(db)

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
