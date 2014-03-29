## ****************************************************
## Created: Adam Cooper, Cetis, Nov 2013
## This source code was produced for The University of
## Edinburgh DEI as part of their MOOC initiative.
## Exctracts edge and node data according to rules for
## what constitutes a tie and saves to graphml and
## RData containing a network class object
## ****************************************************

# nodes = user (person). Deleted/withdrawn people are excluded
# edges = between people who contributed to the same thread (the network is UNDIRECTED)
# forum_user_id is the key for nodes
# nodes have attributes: role (access_group_id)

## this shares a lot with Poster-Commenter.R. Refactor at leisure (but watch for directed/undirected)!!!

library("igraph")
#establish connection to MySQL, loading library. contains coursera DB exports from 2013
library("RMySQL")
MySQL.driver <- MySQL()
source("./dbConnect.R")
#helper functions
source("./helpers.R")

# threshold for minimum number of contributions to a thread by a person for them to be included
thd.thresh<-4

courseIDs<-c("aiplan","astro","crit","edc","equine","intro")
#which forum IDs indicate an "introductions and interests" forum
intoductions.forumIDs<-c(10,12,2,25,2,11)#must be in same order as coursIDs.
echo.sql<-TRUE # echo SQL statements
store.dir<-"~/R Projects/Edinburgh MOOC/EdMOOC-SNA/Network Data/" #where to store the extracted network to
tie.type<-"Whole-T" #file prefix for the type of tie

#group is inserted into output filenames. Used to separate all-forums from introductions-only
worker<-function(group=""){
   
   #Iterate over the course-level list to add attributes to nodes and to create edges from the list of thread,user ids
   # NB it would be possible to use table() and %*% and throw a sociomatrix at igraph but
   # the following method is used so that the same graph creation code works for all tie types
   edgeList<-list()
   for(i in 1:length(courseIDs)){
      nodeList1<-nodeList[[i]]
      #do some mapping to a definition of region that is more natural for interpretation
      nodeList[[i]]<-cbind(nodeList1,region=apply(nodeList1,1,map.toRegion))
      
      edges<-data.frame(node1=character(0), node2=character(0))
      tu<-threadUsersList[[i]]
      
      #guard against empty
      if(length(rownames(tu))!=0){
         threadIDs<-unique(tu[,"thread_id"])
         for(threadID in threadIDs){
            fids<-tu$forum_user_id[tu$thread_id == threadID]
            #remove forum user ids for which there are no nodes. This may happen if there is no IP data, hence no geographical attribute info. This is best done BEFORE taking combinations, I think....
            fids<-fids[fids %in% nodeList1$forum_user_id]
            if(length(fids)>1){
               # this avoids self-ties and mirror-image ties
               edges<-rbind(edges, t(combn(fids[order(fids)],2)))
            }
         }
      }
      #de-dupe and store
      colnames(edges)<-c("node1","node2")
      edgeList[[i]]<-unique(edges)
      
   }
   
   # temporarily load igraph to write out the network as graphml
   library("igraph")
   for(i in 1:length(courseIDs)){
      edgeList1<-edgeList[[i]]
      nodeList1<-nodeList[[i]]
      igraph1<- graph.data.frame(edgeList1, directed=FALSE, vertices=nodeList1[,c("forum_user_id","role","continent","country","region")])
      write.graph(igraph1,paste(store.dir,tie.type," ", courseIDs[i],group,".graphml",sep=""), format="graphml")
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
      net1<-network.initialize(length(nodeList1[,1]), directed=FALSE, hyper=FALSE, loops=FALSE, multiple=FALSE)
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
      senders<-match(edgeList1$node1,nodeList1$forum_user_id)
      receivers<-match(edgeList1$node2,nodeList1$forum_user_id)
      edge.mat<-cbind(senders,receivers)
      network.edgelist(edge.mat, net1)
      
      # saves network object
      name<-"Whole-Thread"
      notes<-"net1 contains a network package object. *list1 contain separate edge and node data frames. 
            net1 does not allow loops and has binary edges but *list1 may have loops"
      fname<-paste(store.dir,tie.type," ", courseIDs[i], group, ".RData", sep="")
      metadata<-list(project=basename(getwd()), origin=name, created=date(), notes=notes)
      save(list=c("metadata","net1","edgeList1","nodeList1"), file=fname)
      cat(paste("Saved to",fname,"\r\n"))
      
      summary(net1)
      
   }
}

## For all SQL here. The wildcard ** is for replacement by vpodata_equine etc.
# this gets users per thread. Need to further process to get edges by a "cross product"
# this DOES NOT contain deleted users
threadUsersList.sql.a <-"SELECT thread_id, forum_user_id, thread_contributions FROM
   (SELECT thread_id, forum_user_id, sum(contributions) thread_contributions FROM
      (SELECT ft.id thread_id, fp.forum_user_id, count(1) contributions FROM **for.forum_threads ft
       JOIN  **for.forum_posts fp ON fp.thread_id = ft.id
      JOIN **map.hash_mapping m ON fp.forum_user_id = m.forum_user_id"
threadUsersList.sql.b <-"group by ft.id, fp.forum_user_id
      UNION 
      SELECT ft.id thread_id, fc.forum_user_id, count(1) contributions FROM **for.forum_threads ft
       JOIN  **for.forum_posts fp ON fp.thread_id = ft.id
       JOIN  **for.forum_comments fc ON fc.post_id = fp.id
      JOIN **map.hash_mapping m ON fc.forum_user_id = m.forum_user_id"

threadUsersList.sql.c <-paste("group by  ft.id, fc.forum_user_id) a
                     group by thread_id, forum_user_id )b
                        WHERE thread_contributions >=", thd.thresh)

# threadUsersList.sql.a <-"SELECT DISTINCT ft.id thread_id, fp.forum_user_id FROM **for.forum_threads ft
#                            JOIN  **for.forum_posts fp ON fp.thread_id = ft.id
#                            JOIN **map.hash_mapping m ON fp.forum_user_id = m.forum_user_id"
# threadUsersList.sql.b <-"UNION DISTINCT
#                       SELECT DISTINCT ft.id thread_id, fc.forum_user_id FROM **for.forum_threads ft
#                            JOIN  **for.forum_posts fp ON fp.thread_id = ft.id
#                            JOIN  **for.forum_comments fc ON fc.post_id = fp.id
#                            JOIN **map.hash_mapping m ON fc.forum_user_id = m.forum_user_id"

# Use a union of poster and commenter ids to get attributes
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
nodeList.sql<-paste(nodeList.sql.a, nodeList.sql.b, nodeList.sql.c)
threadUsersList.sql<-paste(threadUsersList.sql.a, threadUsersList.sql.b, threadUsersList.sql.c)
nodeList<-list.SELECT(db, courseIDs, nodeList.sql, echo=echo.sql)
threadUsersList<-list.SELECT(db, courseIDs, threadUsersList.sql, echo=echo.sql)
worker(thd.thresh)

# additional clauses to limit to Introductions forums. ## will be replaced witht an id in list.limit.SELECT()
limitClause<- "AND fp.thread_id IN (SELECT ft.id FROM **for.forum_threads ft WHERE forum_id = ##)"
nodeList.sql<-paste(nodeList.sql.a, limitClause, nodeList.sql.b, limitClause, nodeList.sql.c)
threadUsersList.sql<-paste(threadUsersList.sql.a, limitClause, threadUsersList.sql.b, limitClause, threadUsersList.sql.c)
nodeList<-list.limit.SELECT(db, courseIDs, nodeList.sql, intoductions.forumIDs, echo=echo.sql)
threadUsersList<-list.limit.SELECT(db, courseIDs, threadUsersList.sql, intoductions.forumIDs, echo=echo.sql)
worker(paste("I",thd.thresh,sep=""))

# additional clauses to EXCLUDE Introductions forums.
limitClause<- "AND fp.thread_id NOT IN (SELECT ft.id FROM **for.forum_threads ft WHERE forum_id = ##)"
nodeList.sql<-paste(nodeList.sql.a, limitClause, nodeList.sql.b, limitClause, nodeList.sql.c)
threadUsersList.sql<-paste(threadUsersList.sql.a, limitClause, threadUsersList.sql.b, limitClause, threadUsersList.sql.c)
nodeList<-list.limit.SELECT(db, courseIDs, nodeList.sql, intoductions.forumIDs, echo=echo.sql)
threadUsersList<-list.limit.SELECT(db, courseIDs, threadUsersList.sql, intoductions.forumIDs, echo=echo.sql)
worker(paste("noI",thd.thresh,sep=""))

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
