require(stringr);require(RColorBrewer)
#Convert gnarly Bureau of Labor Statistics file for use as a network
jorbs<-read.csv("data/occupation_Table1.7.csv",strip.white=T,stringsAsFactors = F)
#reduce wordiness
jorbs$title<-sub(" occupations", "", jorbs$title,fixed=T,jorbs$title)
#make shorter titles (max 3 in a series:  x, y, z, etc)
jorbs$ttl<-sapply(jorbs$title,function(x) str_replace(x,"(([^,]+,){2}).+","\\1 etc"))

# #There's some stupid pseudohierarchy, so all "line items" should be tips
# sum(jorbs$Hierarchy==4)
# jorbs$Hierarchy<-sapply(1:nrow(jorbs),function(x) ifelse(jorbs$Occupation.type[x]=="Line item",4,jorbs$Hierarchy[x]))
# sum(jorbs$Hierarchy==4)



jorbs$Level0<-"Jobs"
#initiate Level vectors
jorbs$Level1<-jorbs$Level2<-jorbs$Level3<-jorbs$Level4<-NA

#expand the hierarchy to have recursive names
level1vec<-which(jorbs$Hierarchy==1)
level2vec<-which(jorbs$Hierarchy==2)
level3vec<-which(jorbs$Hierarchy==3)
level4vec<-which(jorbs$Hierarchy==4)
#for each 1st level, repeat it, up to one bf next 1st level, unless it's the last one
#in that case, repeat up to end of the dataframe
for (i in 1:length(level1vec)){
  category<-jorbs$ttl[level1vec[i]]
  if(i==length(level1vec)){
  jorbs$Level1[level1vec[i]:nrow(jorbs)]<-category}else{
    jorbs$Level1[level1vec[i]:(level1vec[i+1]-1)]<-category
}}
subset(jorbs,select=c("ttl","Hierarchy","Level1"))[1:20,]#test
  
#Do the same thing for 2nd level
for (i in 1:length(level2vec)){
  category<-jorbs$ttl[level2vec[i]]
  if(i==length(level2vec)){ #If this is the last segment, build to the end of the df
  jorbs$Level2[level2vec[i]:nrow(jorbs)]<-category}else{
    #otherwise, figure out the segment for filling down level hierarchy
      segmentlevels<-jorbs$Hierarchy[(level2vec[i]+1):(level2vec[i+1])]#levels of the segment right after 3 to the next 2
      transition<-level2vec[i]+which.min(segmentlevels) #jorbs row index for when goes up a level or next level 2 category
    if(transition==level2vec[i]+1){jorbs$Level2[level2vec[i]]<-category #if the "segment" is just 1 value, store it
      }else{ #otherwise, fill "category" down
    jorbs$Level2[level2vec[i]:(transition-1)]<-category
  }}}
cbind(jorbs$ttl,jorbs$Hierarchy,paste(jorbs$Level1,jorbs$Level2,sep="/ "))[1:20,]#test

#Do the same thing for 3rd level
for (i in 1:length(level3vec)){
  category<-jorbs$ttl[level3vec[i]]
  if(i==length(level3vec)){ #If this is the last segment, build to the end of the df
  jorbs$Level3[level3vec[i]]<-category}else{
    #otherwise, figure out the segment for filling down level hierarchy
      segmentlevels<-jorbs$Hierarchy[(level3vec[i]+1):(level3vec[i+1])]#levels of the segment right after 3 to the next 2
      transition<-level3vec[i]+which.min(segmentlevels) #jorbs row index for when goes up a level or next level 2 category
    if(transition==level3vec[i]+1){jorbs$Level3[level3vec[i]]<-category #if the "segment" is just 1 value, store it
      }else{ #otherwise, fill "category" down
    jorbs$Level3[level3vec[i]:(transition-1)]<-category
  }}}
cbind(jorbs$ttl,jorbs$Hierarchy,paste(jorbs$Level1,jorbs$Level2,jorbs$Level3,sep="/ "))[1:20,]#test

#Do the same thing for 4th level
for (i in 1:length(level4vec)){
  category<-jorbs$ttl[level4vec[i]]
  if(i==length(level4vec)){ #If this is the last segment, build to the end of the df
  jorbs$Level4[level4vec[i]]<-category}else{
    #otherwise, figure out the segment for filling down level hierarchy
      segmentlevels<-jorbs$Hierarchy[(level4vec[i]):(level4vec[i+1])]#levels of the segment right after 3 to the next 2
      istransition<-try(which(segmentlevels<4))
    if(sum(istransition)==0){jorbs$Level4[level4vec[i]]<-category #if the "segment" is just 1 value, store it
      }else{ #otherwise, fill "category" down
        transition<-level4vec[i]+which.min(istransition) #jorbs row index for when goes up a level or next level 3 category
        jorbs$Level4[level4vec[i]:(transition-1)]<-category
      }}}

cbind(jorbs$ttl,jorbs$Hierarchy,paste(jorbs$Level1,jorbs$Level2,jorbs$Level3,jorbs$Level4,sep="/ "))[29:35,]#test

jorbs$pathString<-paste(jorbs$Level0,jorbs$Level1,jorbs$Level2,jorbs$Level3,jorbs$Level4,sep="/")

#Get rid of NAs
jorbs$pathString<-sapply(jorbs$pathString,function(x) gsub("/NA","",x),USE.NAMES = F)

jorbs2<-jorbs[,c("ttl","Occupation.type","Hierarchy","pathString")]
write.csv(jorbs2,"data/jobtree_just hierarchy2.csv")

require(data.tree)
require(networkD3)
require(collapsibleTree)

#********* COLOURS *********
#Color vector based on category
jobcol.pal<-c(brewer.pal(9,name="Set1"),brewer.pal(8,name="Dark2"),brewer.pal(5,name="Accent"))
joblvls<-unique(jorbs[-1,]$Level1)
jorbs$jobcol<-jobcol.pal[match(jorbs$Level1,joblvls)]
jorbs$jobcol[1:2]<-"white"
#****************************


#*!!!!!!!!!!!!
  #Output Dataframe
write.csv(jorbs,"data/BLSjobdata.csv",row.names=F)

  
