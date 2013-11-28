library(kinship2)
library(pedigree)
#reading of the counted individuals in genealogy
gg=read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION//SELECTING GENEALOGIES//scripts/BuildGen/output_files/ancestries/237329_Ancestors.txt",sep = '\t',h=T)
###########################TEST DE SA MERE
gg=read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION//SELECTING GENEALOGIES//scripts/BuildGen/output_files/ancestries_simple_truncated_6//722860_Ancestors.txt",sep = ' ')
gg2=read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION//SELECTING GENEALOGIES//scripts/BuildGen/output_files/ancestries_simple_truncated//722860_Ancestors.txt",h = T,sep = ' ')

levels(gg$sex) <-c("F","H","1","2")
gg$sex = replace(gg$sex,which(gg$sex == "F"),"2")
gg$sex = replace(gg$sex,which(gg$sex == "H"),"1")
gg$sex = as.numeric(levels(gg$sex))[gg$sex]
gg$famid = rep("A", length(gg$sex))



levels(gg2$sex) <-c("F","H","1","2")
gg2$sex = replace(gg2$sex,which(gg2$sex == "F"),"2")
gg2$sex = replace(gg2$sex,which(gg2$sex == "H"),"1")
gg2$sex = as.numeric(levels(gg2$sex))[gg2$sex]
gg2$famid = rep("B", length(gg2$sex))

lulu = NULL
lulu = rbind(lulu,gg2)
lulu = rbind(lulu,gg)

ped <- pedigree(id=lulu$V1,dadid=lulu$V3,momid= lulu$V2)


ggsimple = gg[,names(gg) %in% c("ind","dad","mom", "sex")]
ggsimple = data.frame(id = ggsimple$ind, dam = ggsimple$mom, sire  = ggsimple$dad, sex = ggsimple$sex )
ggsimpleord = orderPed(gg2)
gg2 = gg2[ggsimpleord,]
pedsimple <- pedigree(id=ggsimple$id,dadid=ggsimple$sire,momid= ggsimple$dam,sex = ggsimple$sex)
ggsimple = ggsimple[,names(ggsimple) %in% c("id","sire","dam")]
write.table(file = "PED.ped", ggsimple, quote = F, row.names = F)

F = calcInbreeding(gg2)

ped <- pedigree(id=gg2$id,dadid=gg2$dad,momid= gg2$mom, sex = gg2$sex)

#reordering of the dataframe to match the order of the tree
cc <-sapply(ped$id,function(x) which(x ==gg2$ind))
colalt = NULL
ggsorted = gg[order(gg$ind),]
gg2sorted = gg2[order(gg2$ind),]
idid = gg2sorted$ind[which(ggsorted$depth!=gg2sorted$depth)]
for ( x in idid)
{
  if (length(which(gg2$ind[cc] == x)) ==1){
    colalt = append(colalt,which(gg2$ind[cc] == x))
  }
}
#gg$ind[cc]
colo = rep("black",length(gg2$ind))
colo[colalt] = "red"
indice = which(colnames(gg2) == "depth")
#par(mar=c(3, 0, 0, 0))
plot(ped,id=paste(gg2[cc,indice]),cex = 0.4,col =colo )
#legend("bottomleft", legend = c("Ego","Ancestors","Founders"),text.col = seq_along(levels(factor(datadata[cc,indice]))))         
}
plotped2(164031,"depth",0.4)
ggsorted = gg[order(gg$ind),]
gg2sorted = gg2[order(gg2$ind),]
plot(ggsorted$depth~gg2sorted$depth)
plot(ggsorted$WFI~gg2sorted$WFI)
gg2sorted$ind[which(ggsorted$depth!=gg2sorted$depth)]
ggsorted[which(ggsorted$depth!=gg2sorted$depth),]
ggsorted[which(ggsorted$WFI!=gg2sorted$WFI),]

gg=read.table("~/scripts/BuildGen/buildgen/ancestries/8121567_Ancestors.txt",h=T)
gg=read.table("~/scripts/BuildGen/buildgen/ancestries/8152530_Ancestors.txt",h=T)
gg=read.table("~/scripts/BuildGen/buildgen/ancestries/169168_Ancestors.txt",h=T)
gg=read.table("~/scripts/BuildGen/buildgen/ancestries2/156890_Ancestors.txt",h=T)
gg=read.table("~/scripts/BuildGen/buildgen/ancestries/237329_Ancestors.txt",h=T)
#Test pour inbreed
gg=read.table("~/scripts/BuildGen/buildgen/ancestries/153918_Ancestors.txt",h=T) 


#Test leveled completeness
gg=read.table("~/scripts/BuildGen/buildgen/ancestries/103168868_Ancestors.txt",h=T) 
gg=read.table("~/scripts/BuildGen/buildgen/ancestries2/103225554_Ancestors.txt",h=T) 

## test for cumWFI diff

plotped <- function(individual_id,variable){
  
  datadata = read.table(paste0("~/scripts/BuildGen/buildgen/ancestries/",individual_id,"_Ancestors.txt"),h=T)
  
  ped <- pedigree(id=datadata$ind, dadid=datadata$dad,momid= datadata$mom, sex = datadata$sex)

  #reordering of the dataframe to match the order of the tree
  cc <-sapply(ped$id,function(x) which(x ==datadata$ind))
  #gg$ind[cc]
 
  indice = which(colnames(datadata) == variable)
  par(mar=c(3, 0, 0, 0))
 plot(ped,id=paste(datadata[cc,indice]),cex = 0.4,packed = T,col =datadata[cc,"count"] +1 )
legend("bottomleft",title = "Count", legend = levels(factor(datadata[cc,"count"])),text.col = seq_along(datadata[cc,"count"])+1)         
         
}

plotped(103225554,"depth")

plotped2 <- function(individual_id,variable,ccex){  
  datadata = read.table(paste0("~/scripts/BuildGen/buildgen/ancestries/",individual_id,"_Ancestors.txt"),h=T) 
  ped <- pedigree(id=datadata$ind, dadid=datadata$dad,momid= datadata$mom, sex = datadata$sex)
  
  #reordering of the dataframe to match the order of the tree
 
  cc <-sapply(ped$id,function(x) which(x ==datadata$ind))
  #gg$ind[cc]
  
  indice = which(colnames(datadata) == variable)
  par(mar=c(3, 0, 0, 0))
  plot(ped,id=paste(datadata[cc,indice]),cex = ccex,col =datadata[cc,indice] +1 )
  #legend("bottomleft", legend = c("Ego","Ancestors","Founders"),text.col = seq_along(levels(factor(datadata[cc,indice]))))         
}
plotped2(205905,"depth",0.4)


gg=read.table("~/scripts/BuildGen/buildgen/ancestries/725266_Ancestors.txt",h=T)
gg$truesex = rep(0,length(gg$sex))
gg$truesex[which(gg$sex=="F") ] = 2
gg$truesex[which(gg$sex=="H") ] = 1
ped <- pedigree(id=gg$ind, dadid=gg$dad,momid= gg$mom, sex = gg$truesex)

#reordering of the dataframe to match the order of the tree
cc <-sapply(ped$id,function(x) which(x ==gg$ind))
#gg$ind[cc]
ttt <- sapply(ped$id,function(x) which(x ==tt$V1))
plot(ped,id=paste(gg$WFI[cc]),cex = 0.4, align = T)
plot(ped,id=paste(tt$V2[ttt]),cex = 0.6)


#function to compute the completeness function of level

completbydepth <- function(individual_id,color){
  gg = read.table(paste0("~/scripts/BuildGen/buildgen/ancestries/",individual_id,"_Ancestors.txt"),h=T)
  
  ratio = NULL
  maxlist = NULL
  currlist = NULL
  dd = NULL
  maxdepth = max(gg$depth)
  for(i in 1:maxdepth){
    nbmax = 2**i
    dd = append(dd,i)
    maxlist = append(maxlist,nbmax)
    currlist = append(currlist,sum(gg$count[which(gg$depth==i)]))
    ratio = append(ratio,(sum(gg$count[which(gg$depth==i)])/nbmax))

  }
  
 # plot(ratio,type = "l",col=color, ylim = c(0,1),xlim = c(1,maxdepth), ylab = "Completeness",xlab = "Depth")

  ret = data.frame(Completeness = ratio,Depth = dd,Genealogy = individual_id)
                   
  return(ret)
  #plot(maxlist,ylim = c(0,max(maxlist)),col = "red")
  #par(new = T)
  #plot(currlist,ylim = c(0,max(maxlist)),col = "blue")
}
ped1 = completbydepth("102395518","red")
par(new = T)
ped2 = completbydepth("102560166","blue")
df = rbind(ped1,ped2)
ggplot(df, aes(y = Completeness,x = Depth, shape = Genealogy, color = Genealogy )) + geom_line() + geom_point()

#Create a way to plot the completeness by depth form all the ancestries _Count
files_names =system("ls ~/scripts/BuildGen/buildgen/ancestries/*_Count.txt", intern = T)

completbydepth <- function(x){
  tt = read.table(x, sep = ";")  
  ratio = NULL
  maxlist = NULL
  currlist = NULL
  maxdepth = 15
  for(i in 1:maxdepth){
    nbmax = 2**i
    maxlist = append(maxlist,nbmax)
    currlist = append(currlist,sum(tt$V2[which(tt$V3==i)]))
    ratio = append(ratio,(sum(tt$V2[which(tt$V3==i)])/nbmax))    
  }
  #plot(ratio)
  #plot(maxlist,ylim = c(0,max(maxlist)),col = "red")
  #par(new = T)
  #plot(currlist,ylim = c(0,max(maxlist)),col = "blue")
  plot(ratio,ylim = c(0,1.2),type = "l")
  par(new = T)
}

for (i in 500:600){
completbydepth(files_names[i])
}
plot(currlist)
mama = matrix(nrow = 2000, ncol = 15)
mama = as.data.frame(mama)
mama = matrix()
mama = sapply(files_names, completbydepth)
tt = read.table("~/scripts/BuildGen/buildgen/ancestries/103168868_Count.txt", sep = ";")

#genealogies with colors
mycol <- rep("black",length(ped$id))
endl <-rep('\n',length(gg$cumWFI))
plot(ped,id=paste(gg$cumWFI,endl,gg$WFI),cex = 0.5)
plot(ped,col = mycol,cex = 0.1)
gg=read.table("~/scripts/BuildGen/buildgen/noflag.ped",h=T)
unique(gg)
ped <- pedigree(id=gg$ind,dadid=gg$pere,momid= gg$mere,sex = gg$sexe)

