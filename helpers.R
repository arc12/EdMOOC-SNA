## ****************************************************
## Created: Adam Cooper, Cetis, Oct 2013
## This source code was produced for The University of
## Edinburgh DEI as part of their MOOC initiative.
## Various functions with utility value
## ****************************************************

# make sure text does not have leading or trailing spaces characters
trimText<-function(text){
   txt<-gsub("\\s*$","", text,perl=T)
   return (gsub("^\\s*","", txt,perl=T))
}

#make some friendly user id labels to length "len"
# gives you A-Z for up to 26, Aa...Az...Bz...Zz otherwise.
niceLabels<-function(len){
   if(len>26^2){
      stop("len too large. max value is 26^2, which is 676 and more than enough for labelling plots")
   }
   if(len<=26){
      labels<-LETTERS[1:len]
   }else{
      ch1<-floor(len/26)
      ch2<-len %% 26
      labels<-character(0)
      for (i in 1:ch1){
         labels<-c(labels, paste(LETTERS[i], letters, sep=""))
      }
      if(ch2>0){
         labels<-c(labels,paste(LETTERS[ch1+1],letters[1:ch2],sep=""))
      }
   }
   return (labels)
}

# map continent and country information (from uoe_ip_country) to a practically-useful concept of region
# Regions are: UK & Ireland, N.America, Continental Europe, HK&China, India, S.America, Africa.
# Also recodes Mexico to Central America (not Northern) and merges the Caribbean with C Am.
# cc is a vector containing fields with names "country" and "continent"
# returns a region string (defaults to continent if no region rule exists)
map.toRegion<-function(cc){
   region<- switch(cc["country"],
                   'UNITED KINGDOM'="UK AND IRELAND",
                   'IRELAND'="UK AND IRELAND",
                   'ISLE OF MAN'="UK AND IRELAND",
                   'CHINA'="CHINA",
                   'HONG KONG'="CHINA",
                   'TAIWAN, PROVINCE OF CHINA'="CHINA",
                   'AUSTRALIA'="AUSTRALIA AND NZ",
                   'NEW ZEALAND'="AUSTRALIA AND NZ",
                   'MEXICO'="CENTRAL AMERICA",
                   'INDIA'="INDIA"
   )
   if(is.null(region)){
      region=cc["continent"]
      if(cc["continent"]=="CARIBBEAN"){
         region<-"CENTRAL AMERICA"
      }
   }
  return (region)
}


# Loop over the vector of schemaIDs, replacing the "**" placeholder in sql with each
# (prefixed by schemaPrefix) and executing on the (already connected) database db.
# Set echo=T to reflect the SQL to output.
# returns a dataframe with rows named by schemaIDs and columns determined by the SQL
tabular.SELECT <- function(db, schemaIDs, sql, echo=FALSE, schemaPrefix="vpodata_"){
   ret.df<-data.frame()
   for(i in 1:length(schemaIDs)){
      sql.1<-gsub("**", sql, fixed=T, replacement=paste(schemaPrefix,schemaIDs[i],sep=""))
      if(echo && i==1){
         cat(sql.1, sep="\r\n")
      }
      ret.df<-rbind(ret.df,dbGetQuery(db,sql.1))
   }
   rownames(ret.df)<-schemaIDs
   return(ret.df)
}

# Similar to tabular.SELECT but returns a list of data-frames.
# Each list item relates to a mamber of schemaIDs.
# The data-frames will have the same column names (same SQL template) but may have differing no. of rows
list.SELECT <- function(db, schemaIDs, sql, echo=FALSE, schemaPrefix="vpodata_"){
   ret.list<-list()
   for(i in 1:length(schemaIDs)){
      sql.1<-gsub("**", sql, fixed=T, replacement=paste(schemaPrefix,schemaIDs[i],sep=""))
      if(echo && i==1){
         cat(sql.1, sep="\r\n")
      }
      ret.list[[schemaIDs[i]]]<-dbGetQuery(db,sql.1)
   }
   return(ret.list)
}

# as list.SELECT but substities one item from a supplied vector (of length schemaIDs) for ## placeholder
list.limit.SELECT <- function(db, schemaIDs, sql, replacements, echo=FALSE, schemaPrefix="vpodata_"){
   ret.list<-list()
   for(i in 1:length(schemaIDs)){
      sql.1<-gsub("**", sql, fixed=T, replacement=paste(schemaPrefix,schemaIDs[i],sep=""))
      sql.1<-gsub("##", sql.1, fixed=T, replacement=replacements[i])
      if(echo && i==1){
         cat(sql.1, sep="\r\n")
      }
      ret.list[[schemaIDs[i]]]<-dbGetQuery(db,sql.1)
   }
   return(ret.list)
}

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