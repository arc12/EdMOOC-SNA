##
# push calculated centrality measures into MySQL
# *** this DROPS and CREATES tables for each course prior to inserting data
##

library("igraph")
library("RMySQL")
MySQL.driver <- MySQL()
source("./dbConnect.R")

store.dir<-"~/R Projects/Edinburgh MOOC/EdMOOC-SNA/Network Data/" #where to load data from

courseIDs<-c("aiplan","astro","crit","edc","equine","intro")
schemaPrefix="vpodata_"
sql.drop <-"DROP TABLE IF EXISTS uoe_centrality"
sql.create<-"CREATE TABLE `uoe_centrality` (`forum_user_id` varchar(120) NOT NULL,
                     `pc_degree` float, `pc_indegree` float, `pc_outdegree` float, `pc_betweenness` float, `pc_eigenvector` float,
                     `pset_degree` float, `pset_betweenness` float, `pset_eigenvector` float,
                     `wt3_degree` float, `wt3_betweenness` float, `wt3_eigenvector` float,
                     `wt2_degree` float, `wt2_betweenness` float, `wt2_eigenvector` float,
                        PRIMARY KEY (`forum_user_id`)) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4"

schemaName<-function(cID){
   return(paste(schemaPrefix,cID,"gen",sep=""))
}

## deal with tables
db<-conn()
for(i in 1:length(courseIDs)){
   #set schema
   dbSendQuery(db, statement=paste("USE ",schemaName(courseIDs[i])))
   #guarded drop table
   dbSendQuery(db, statement=sql.drop)
   #re-create
   dbSendQuery(db, statement=sql.create)
}
dbDisconnect(db)

## deal with data
for(courseID in courseIDs){
   print(courseID)
   ig<-read.graph(paste(store.dir,"P-C"," ", courseID,".graphml",sep=""), format="graphml")
   #build a dataframe for person-level centrality measures+role (for labelling)
   pc.df<-data.frame(pc_degree=degree(ig, mode="all", loops=FALSE),
                     pc_indegree=degree(ig, mode="in", loops=FALSE),
                     pc_outdegree=degree(ig, mode="out", loops=FALSE),
                     pc_betweenness=betweenness(ig, directed=FALSE),
                     pc_eigenvector=evcent(ig, directed=FALSE)$vector)
   ig<-read.graph(paste(store.dir,"P-Set"," ", courseID,".graphml",sep=""), format="graphml")
   pset.df<-data.frame(pset_degree=degree(ig, mode="all", loops=FALSE),
                       pset_betweenness=betweenness(ig, directed=FALSE),
                       pset_eigenvector=evcent(ig, directed=FALSE)$vector)
   ig<-read.graph(paste(store.dir,"Whole-T"," ", courseID,"3.graphml",sep=""), format="graphml")
   wt3.df<-data.frame(wt3_degree=degree(ig, mode="all", loops=FALSE),
                      wt3_betweenness=betweenness(ig, directed=FALSE),
                      wt3_eigenvector=evcent(ig, directed=FALSE)$vector)
   ig<-read.graph(paste(store.dir,"Whole-T"," ", courseID,"2.graphml",sep=""), format="graphml")
   wt2.df<-data.frame(wt2_degree=degree(ig, mode="all", loops=FALSE),
                      wt2_betweenness=betweenness(ig, directed=FALSE),
                      wt2_eigenvector=evcent(ig, directed=FALSE)$vector)
   #ensure that the forum_user_ids correspond. The nodes are the same but do not rely on ordering.
   pset.df<-pset.df[rownames(pc.df),]
   wt3.df<-wt3.df[rownames(pc.df),]
   wt2.df<-wt2.df[rownames(pc.df),]
   #all.df<-cbind(forum_user_id=rownames(pc.df), pc.df, pset.df)#need an explicit column for forum_user_ids
   #write to database
   db<-conn(schemaName(courseID))
   for(row in rownames(pc.df)){
      sql<-paste("insert into uoe_centrality (forum_user_id, pc_degree, pc_indegree, pc_outdegree, pc_betweenness, pc_eigenvector, pset_degree, pset_betweenness, pset_eigenvector, wt3_degree, wt3_betweenness, wt3_eigenvector, wt2_degree, wt2_betweenness, wt2_eigenvector)   values (",
                 paste(paste("'",row,"'",sep=""),
                       pc.df[row,"pc_degree"],
                       pc.df[row,"pc_indegree"],
                       pc.df[row,"pc_outdegree"],
                       pc.df[row,"pc_betweenness"],
                       pc.df[row,"pc_eigenvector"],
                       pset.df[row,"pset_degree"],
                       pset.df[row,"pset_betweenness"],
                       pset.df[row,"pset_eigenvector"],
                       wt3.df[row,"wt3_degree"],
                       wt3.df[row,"wt3_betweenness"],
                       wt3.df[row,"wt3_eigenvector"],
                       wt2.df[row,"wt2_degree"],
                       wt2.df[row,"wt2_betweenness"],
                       wt2.df[row,"wt2_eigenvector"],    
                       sep=","),")")
      res<-dbSendQuery(db,sql)
   }
   dbDisconnect(db)
   print(res)
}




