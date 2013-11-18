library(plotGoogleMaps)
library(RgoogleMaps)
library(ggmap)
library(ggplot2)
library(plotrix)
library(maptools)
library(maps)
library(mapdata)
library(mapplots)
library(ggsubplot)
library(grid)
library(RColorBrewer)
library(animation)
i = 723406
plotancestry <- function(x){
  gen = NULL
  

  ##Creation of the big matrix of ancestry
  for( i in x){
    
    gentemp = read.table(paste("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION//SELECTING GENEALOGIES//scripts/BuildGen/output_files/ancestries/",i,"_Ancestors.txt", sep = ""),h=T,sep = "\t")
    
    gen = rbind(gen,gentemp)
  }
  gen$lon = rep(0,length(gen$ind))
  gen$lat = rep(0,length(gen$ind))
  j = 0
  for(i in gen$town){
    j = j+1
    indice =   which(URB$NOM==i)
    if(length(indice) != 0) {
      gen$lat[j] = URB$LATITUDE_ADRIEN[indice]
      gen$lon[j] = URB$LONGITUDE_ADRIEN[indice]
    }
  }
  print(dim(gen))
  gen = gen[order(gen$firstwedyear),]
  gen$period = rep(NA, length(gen$ind))
  gen$period[which(gen$firstwedyear > 1605)] = "1605-1635"
  gen$period[which(gen$firstwedyear > 1635)] = "1635-1665"
  
  gen$period[which(gen$firstwedyear > 1665)] = "1665-1695"
  gen$period[which(gen$firstwedyear > 1695)] = "1695-1725"
  
  gen$period[which(gen$firstwedyear > 1725)] = "1725-1755"
  gen$period[which(gen$firstwedyear > 1755)] = "1755-1785"
  
  gen$period[which(gen$firstwedyear > 1785)] = "1785-1815"
  gen$period[which(gen$firstwedyear > 1815)] = "1815-1845"
  
  gen$period[which(gen$firstwedyear > 1845)] = "1845-1905"
  gen$period[which(gen$firstwedyear > 1905)] = "1905-2000"
  
  gen = gen[!is.na(gen$period),]
  print(dim(gen))
  gen = gen[-which(gen$WFI == -1),]
  print(dim(gen))
  gen = gen[-which(gen$lon == 0),]
  print(dim(gen))
  gen$period = as.factor(gen$period)
  return(gen)
}
plotancestryregion <- function(x){
  gen = NULL
  latlong = read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION/SELECTING GENEALOGIES//Data_files/latitude-longitude-quebec-region.csv",h=T,stringsAsFactors=FALSE)
  
  gen$lon = rep(0,length(x$maxDepth))
  gen$lat = rep(0,length(x$maxDepth))
  gen$size = rep(0,length(x$maxDepth))
  j = 0
  tata = table(x$Loc_ego)
  for(i in x$Loc_ego){
    j = j+1
    indice =   which(latlong$region==i)
    if(length(indice) != 0) {
      gen$lat[j] = latlong$latitude[indice]
      gen$lon[j] = latlong$longitude[indice]
      gen$size[j] = tata[i]
    }
  }
  y = x
  y$lon = gen$lon
  y$lat = gen$lat
y$size = gen$size
  return(y)
}

 
quebec <-get_map(location = c(lon = -70.270386,lat = 47.591346),zoom =6, scale = 2, maptype = "terrain", source = 'stamen')

gg = read.table("96-Indiv-list.txt", sep = "\t", row.names = 1, h = T)
gg = read.table("120-Indiv-list.txt", sep = "\t", row.names = 1, h = T)
gglow = gg[which(gg$cond == "Low cWFI"),]
gghigh = gg[-(which(gg$cond == "Low cWFI")),]
latlong = read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION/SELECTING GENEALOGIES//Data_files/latitude-longitude-quebec-region.csv",h=T,stringsAsFactors=FALSE)
latlongtown = read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION/SELECTING GENEALOGIES//Data_files/villes.csv",h=T,stringsAsFactors=FALSE, sep = "\t")
gglow2 = plotancestryregion(gglow)
gghigh2 = plotancestryregion(gghigh)
gg2 =plotancestryregion(gg)
##########################################Locality opening
##########################################
i = 1850
quebec <-get_map(location = c(lon = -70.270386,lat =  47.591346),zoom =6, scale = 2, maptype = "satellite")
toto = read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION/SELECTING GENEALOGIES//scripts/python-toolbox/towns-and-opening.csv",h=T,na.string = "NULL", sep = "\t")
toto$lon = as.numeric(toto$lon)
toto$lat = as.numeric(toto$lat)
toto = toto[!is.na(toto$lat),]
toto = toto[!is.na(toto$opening),]
toto = toto[order(toto$opening),]
colpal = brewer.pal(11,"RdBu")
periode = c("1621-1651",
            "1651-1681",
            "1681-1711",
            "1711-1741",
            "1741-1771",
            "1771-1801",
            "1801-1831",
            "1831-1861",
            "1861-1891",
            "1891-1921",
            "1921-1985")
interval = seq((min(toto$opening)+30),1921,30)
interval
j = 1
toto$col = rep(colpal[j], length(toto$lat))
toto$periode = rep(periode[j],length(toto$lat))
for(i in interval){
  j = j +1 
  print(j)
  toto$col[which(toto$opening > i)] <- colpal[j]
 toto$periode[which(toto$opening > i)] <- periode[j]
  
}
j = 1
saveGIF(movie.name="opening.gif",autobrowse = TRUE,interval = 2,clean = TRUE,ani.width = 800, ani.height = 800,outdir = "/home/foucal/",{
for(i in c(interval,1985)){
temp = toto[which(toto$opening <= i),]

#temp$col[which(temp$opening > (i-30))] = "#053061"
locop <-ggmap(quebec) + geom_point(data = temp ,aes(y=lat,x=lon, col = periode),size = 4, alpha = 0.7) +
  ggtitle(paste("Opened localities until year",i))+scale_colour_manual(name="Time periods",values=colpal[1:j])+
  guides(colour=guide_legend(override.aes=list(size=10)))+
  theme(legend.text = element_text(size=30),
        legend.title = element_text(size=20),legend.position = c(1.2,0.9),
        axis.text.x = element_text(size=18),axis.title.x = element_text(size=30),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=30),
        legend.justification = "top", plot.title = element_text(size=30),plot.margin = unit(c(0,8,0,1), units = "cm")) 
    locop$labels$x = "Longitude"
    locop$labels$y = "Latitude"
    print(locop)
j = j +1
  }
})

locop <-ggmap(quebec) + geom_point(data = toto ,aes(y=lat,x=lon , color = periode))+ 
  ggtitle(paste("Opened localities until year",i))+scale_colour_manual(name="Time periods",values=colpal[1:j])+
  guides(colour=guide_legend(override.aes=list(size=10)))+
  theme(legend.text = element_text(size=30),
        legend.title = element_text(size=20),legend.position = c(1.2,0.9),
        axis.text.x = element_text(size=18),axis.title.x = element_text(size=30),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=30),
        legend.justification = "top", plot.title = element_text(size=30),plot.margin = unit(c(0,8,0,1), units = "cm")) 
locop$labels$x = "Longitude"
locop$labels$y = "Latitude"
locop

#######################################"
######################################Checking the data of towns seriously.
URB <- read.delim("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION/SELECTING GENEALOGIES/Data_files/URB_COORD_COMPLETEES_PAR_ADRIEN.csv", na.strings = "NULL")
###without __


URB$COORD_X = as.numeric(URB$COORD_X)
URB$COORD_Y = as.numeric(URB$COORD_Y)
which(is.na(URB$LONGITUDE_ADRIEN))
quebec <-get_map(location = c(lon = -70.270386,lat = 50),zoom =6, scale = 2, maptype = "terrain", source = 'stamen')

ggmap(quebec) + 
  geom_point(data = URB ,aes(y=LATITUDE_ADRIEN,x=LONGITUDE_ADRIEN),col= "red", alpha = 0.4)

difflon = abs(URB$LONGITUDE_ADRIEN-URB$COORD_X)
difflat = abs(URB$LATITUDE_ADRIEN-URB$COORD_Y)
DIFFURB = URB[URB$NOM %in% union(URB$NOM[which(difflat > 0.2)],URB$NOM[which(difflon > 0.2)]),]
write.table(DIFFURB, "VILLES-DIFFERENTES.csv",sep = "\t", quote=F, row.names=F)

write.table(DIFFURB, "LAT-LONG-TOWN-ADRIEN-DIFFERENT-FROM-ST-HILAIRE.csv",sep = "\t", quote=F, row.names=F)
test = data.frame(lat = DIFFURB$COORD_Y, lon = DIFFURB$COORD_X, NOM = DIFFURB$NOM, Annotation = "Marc")
test2 = data.frame(lat = DIFFURB$LATITUDE_ADRIEN, lon = DIFFURB$LONGITUDE_ADRIEN, NOM = DIFFURB$NOM, Annotation = "Adrien")
test = rbind(test,test2)
quebec <-get_googlemap(center = c(lon = -70.270386,lat = 47),zoom =6, scale = 2, maptype = "roadmap")

ggmap(quebec) + 
  geom_point(data = test ,aes(y=lat,x=lon, colour = NOM, shape = Annotation), size = 10, alpha  = 0.8) 
  
  NEWURB = URB[is.na(URB$COORD_X),]
NEWURB = NEWURB[!is.na(NEWURB$LATITUDE_ADRIEN),]
write.table(NEWURB, "NEW-LAT-LONG-TOWN-FROM-ADRIEN.csv",sep = "\t", quote=F, row.names=F)


plotttt <- function(x){
ggmap(quebec) + 
  geom_point(data = DIFFURB[x,] ,aes(y=LATITUDE_ADRIEN,x=LONGITUDE_ADRIEN), col= "black") +
  
  geom_point(data = DIFFURB[x,] ,aes(y=COORD_Y,x=COORD_X), pch = 17,col = "red") +  facet_wrap(~NOM)

}
plotttt(35)
        
#################################
##################################PIECHART MOTHERFUCKER§§§§§§§§§§§§§§ town per selected individuals
ggll = plotancestryregion(gg)
pie = ggll[,which(names(ggll) %in% c("cond","lat","lon","Loc_ego"))] 
pie$cond2 = rep(0,length(pie$lon))
pie$cond2[which(pie$cond == "High cWFI")] <- 1

by(pie$cond2, pie$Loc_ego, mean)
count = table(pie$Loc_ego)
nondup = pie[!duplicated(pie$Loc_ego),]
ord =order(nondup$Loc_ego)
ultimatepie = data.frame(loc = nondup$Loc_ego[ord],lat = nondup$lat[ord],
                         lon = nondup$lon[ord], size = as.vector(count),
                         frac = as.vector(by(pie$cond2, pie$Loc_ego, mean)))
quebec <-get_googlemap(center = c(lon = -72,lat = 47.2),zoom =7, scale = 2, maptype = "roadmap")

ggmap(quebec, extent = "device") + geom_point(alpha = 0.8,data = ultimatepie ,aes(y=lat,x=lon, color = frac,size = size,na.rm = TRUE)) +
  scale_size_area(max_size = 10) +
  scale_color_gradient(low = "yellow",                       high= "red")

#####################################Diagram to see the selection of individuals
####################################


ggll = plotancestry(gg)
pie = ggll[,which(names(ggll) %in% c("cWFI","lat","lon","Loc_ego"))] 

pie$Loc_ego = factor(pie$Loc_ego)
count = table(pie$Loc_ego)
nondup = pie[!duplicated(pie$Loc_ego),]
ord =order(nondup$Loc_ego)
ultimatepie = data.frame(loc = nondup$Loc_ego[ord],lat = nondup$lat[ord],
                         lon = nondup$lon[ord], size = as.vector(count),
                         cWFI = as.vector(by(pie$cWFI, pie$Loc_ego, mean)))
quebec <-get_googlemap(center = c(lon = -72,lat = 47.2),zoom =7, scale = 2, maptype = "roadmap")

ggmap(quebec, extent = "device") + geom_point(alpha = 0.8,data = ultimatepie ,aes(y=lat,x=lon, color = cWFI,size = size,na.rm = TRUE)) +
  scale_size_area(max_size = 10) +scale_size_identity(guide="legend", trans = "log10")  
  #scale_color_gradient(low = "yellow",high= "red")

#############################
#############################PRINTING ALL THE INFO OF AN AN ANCESTRY

i = "1605-1635"
periode = c("1605-1635", "1635-1665", "1665-1695","1695-1725", "1725-1755", "1755-1785","1785-1815", "1815-1845", "1845-1905", "1905-2000")
periode = c("1621-1651",
            "1651-1681",
            "1681-1711",
            "1711-1741",
            "1741-1771",
            "1771-1801",
            "1801-1831",
            "1831-1861",
            "1861-1891",
            "1891-1921",
            "1921-1985")
quebec <-get_map(location = c(lon = -70.8,lat = 47.3),zoom =7, scale = 2, maptype = "satellite")

####Here say wich gtroup you want to plot the ancestry in a gif
for(individu in "726792"){
  anc = plotancestry(c(individu))
  anc$wfi =rep(NA,length(anc$town))
  anc$size =rep(NA,length(anc$town))
  
  output = NULL
  for(i in levels(anc$period)){
    print(i)
    temp = anc[anc$period %in% i,]
    vv =  by(temp$WFI, factor(temp$town), mean)
    ww =  table(factor(temp$town))
    k = 0
    for(j in names(vv)){
      k = k+1
      temp$wfi[which(temp$town[temp$period %in% i] == j)] = as.vector(vv[k])
      if (as.vector(ww[k]) < 10){
        temp$size[which(temp$town[temp$period %in% i] == j)] = 10
        
      }else{
        temp$size[which(temp$town[temp$period %in% i] == j)] = as.vector(ww[k]) 
      }
    }
    output = rbind(output,temp)  
  }
  anc = output
  
  
  
saveGIF(movie.name=paste0(individu,".gif"),autobrowse = FALSE,clean = TRUE,ani.width = 400, ani.height = 400,outdir = "/home/foucal/Pictures/HIGH",{
for(i in levels(anc$period)){
p2 <-ggmap(quebec) + geom_point(data = anc[anc$period %in% i,] , aes(y=lat,x=lon, color=wfi, size = size),alpha = 0.8, na.rm = TRUE) + 
scale_color_gradient2(name = "WFI",midpoint = 0.5,low =rgb(84, 39, 136, max = 255) , mid = rgb(247, 247, 247,max = 255), high = rgb(230, 97, 1, max = 255), limits = c(0,1))+ggtitle(i)+
         theme(plot.title = element_text(size=15),legend.text = element_text(size=10),legend.title = element_text(size=10))+
  scale_size_area(limits = c(1,60), max_size = 10,breaks =c(10,30,60),labels = c("<10","30","60"), guide  = 'legend',name = "Ancestors number")
p2$labels$x = "Longitude"
p2$labels$y = "Latitude"
print(p2)
#  print(paste(i,"_",k,".png", sep = ""))
#ggsave(paste(i,"_",k,".png", sep = ""),width = 8, height = 8)

#graphics.off()
}
})

}

#########################################
########################################









######################




