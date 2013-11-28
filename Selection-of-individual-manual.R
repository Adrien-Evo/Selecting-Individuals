library(ggplot2)
library(plyr)
library(MASS)
library(rgl)
library(grid)
library(VennDiagram)

library(plotGoogleMaps)
library(RgoogleMaps)
library(ggmap)
library(ggplot2)
library(lattice)

par(mar=c(5.1,4.1,4.1,2.1))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

StartInd <- read.delim("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION//SELECTING GENEALOGIES//scripts/BuildGen/output_files/info.info")
StartInd <- read.delim("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION//SELECTING GENEALOGIES//scripts/BuildGen/output_files/info-added-info-on-nb-of-WFI.info")

per =length(which(StartInd$maxDepth == 15))+ length(which(StartInd$maxDepth == 12)) + length(which(StartInd$maxDepth == 14)) +length(which(StartInd$maxDepth == 13))
per = 100*per/dim(StartInd)[1] 

#Graph avec les densités de WFI pour différentes profondeurs de 11 à 15
Start16 <- StartInd[which(StartInd$maxDepth == 16),]
Start13 <- StartInd[which(StartInd$maxDepth == 13),]
Start14 <- StartInd[which(StartInd$maxDepth == 14),]
Start15 <- StartInd[which(StartInd$maxDepth == 15),]
Start17 <- StartInd[which(StartInd$maxDepth == 17),]

dff2 <- rbind(Start13, Start14,Start15,Start16,Start17)

dff2$nb_wfi <-dff2$nbIndiv -dff2$nb_wfi_default
rownames(dff2) <- dff2$ind
dff2<- dff2[,-(1)]

#########################RESUMEPLOT
############################
supaplot <- function(dat){
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 2)))
a1<- ggplot(dat,aes(x=cWFI)) + geom_histogram(aes(y = ..density.., fill = ..count..),binwidth = 1/5)+
   theme(axis.text.x = element_text(size=25),axis.title.x = element_text(size=30),
         axis.text.y = element_text(size=25),axis.title.y = element_text(size=30),
         legend.text = element_text(size=20),legend.title = element_text(size=20)) +
  coord_cartesian(xlim = c(0, 8), ylim = c(0,0.55))
  
a2 <-  ggplot(dat,aes(x=nb_wfi_12)) + geom_histogram(aes(y = ..density.., fill = ..count..),binwidth = 100)+
    theme(axis.text.x = element_text(size=25),axis.title.x = element_text(size=30),
          axis.text.y = element_text(size=25),axis.title.y = element_text(size=30),
          legend.text = element_text(size=20),legend.title = element_text(size=20)) +
    coord_cartesian(xlim = c(1500,4000), ylim = c(0,0.5))

  pie = data.frame(Loc = names(table(factor(dat$Loc_ego))), value = as.vector(table(factor(dat$Loc_ego))))
  
  a3 <-ggplot(data = pie, aes(x= Loc,y = value, fill = Loc)) + 
    geom_bar(stat = "identity") + geom_text(aes(y = value , label = value), size = 5)+ coord_polar()+ guides(fill = FALSE)+
    theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.text.x = element_text(colour = "black",size = 15),axis.title.x = element_blank(),axis.ticks.x = element_blank())
  
  print(a1, vp = vplayout(1, 1:2))
  print(a2, vp = vplayout(2, 1))
  print(a3, vp = vplayout(2, 2))

}
#supaplot(dff2)
#supaplot(boys1)
#supaplot(boys)
#supaplot(common)

###########################END OF RESUME PLOT
##########################"




select_the_boys <-function(dff2,meandepth1,meandepth2,CC6,CC12_1,CC12_2){

  print(ggplot(dff2,aes(x=as.factor(maxDepth),y = Completeness_6,fill = as.factor(maxDepth))) + geom_violin(scale = "count")+
          scale_fill_discrete(name = "Maximum\n depth") + geom_hline(yintercept = CC6, col = "red",linetype = "dashed",size = 3)+
          scale_x_discrete(name = "") + scale_y_continuous(name = "Completeness at depth 6") +  
          theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=30),
                axis.text.y = element_text(size=18),axis.title.y = element_text(size=30),
                legend.text = element_text(size=20),legend.title = element_text(size=20)))
 

  print(ggplot(dff2,aes(x=as.factor(maxDepth),y = meanGenDepth,fill = as.factor(maxDepth))) + geom_violin(scale = "count")+
          geom_hline(yintercept = meandepth1, col = "red",linetype = "dashed",size = 3)+geom_hline(yintercept = meandepth2, col = "red",linetype = "dashed",size = 3)+ 
         # stat_summary(fun.y = mean, geom = "point", color = "black", fill = "black", pch = 21, size = 2) +
         # stat_summary(fun.y = median, geom = "point", color = "black", fill = "white", pch = 23, size = 2) +
          scale_fill_discrete(name = "Maximum\n depth") + 
          scale_x_discrete(name = "") + scale_y_continuous(name = "Mean Genealogical Depth") +  
          theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=30),
                axis.text.y = element_text(size=18),axis.title.y = element_text(size=30),
                legend.text = element_text(size=20),legend.title = element_text(size=20)))


  print( ggplot(dff2,aes(x=as.factor(maxDepth),y = Completeness_12,fill = as.factor(maxDepth))) + geom_violin(scale = "count") +
           geom_hline(yintercept = CC12_2, col = "red",linetype = "dashed",size = 3) + geom_hline(yintercept = CC12_1, col = "red",linetype = "dashed",size = 3) +
           scale_fill_discrete(name = "Maximum\n depth") +
           scale_x_discrete(name = "") + scale_y_continuous(name = "Completeness at depth 12") +  
           theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=30),
                 axis.text.y = element_text(size=18),axis.title.y = element_text(size=30),
                 legend.text = element_text(size=20),legend.title = element_text(size=20))
           )

  
  subset  =intersect(which(dff2$meanGenDepth > meandepth1),which(dff2$meanGenDepth < meandepth2))
  print(length(subset))
  subset = intersect(subset,which(dff2$Completeness_6 >= CC6))
  print(  length(subset))
  subset = intersect(intersect(which(dff2$Completeness_12 > CC12_1),which(dff2$Completeness_12 < CC12_2)),subset) 
          print(length(subset))
  return (dff2[subset,])
}
cc6 = NULL
mgd = NULL
cc12 = NULL
meandepth1 = 8
meandepth2 = 10.5
CC12_1 = 0.325
CC12_2 = 0.75
seuil = 0.95
boys1 =   select_the_boys(dff2,meandepth1,meandepth2,seuil,CC12_1,CC12_2) 

###############################Where is the max depth where the cWFI doesnt count more
###############################

ComputeCompletenessAtDepth <-function(datadata){
  lolo = NULL
  
  for(i in rownames(datadata)){
    gen = read.table(paste("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION//SELECTING GENEALOGIES//scripts/BuildGen/output_files/ancestries/",i,"_Ancestors.txt", sep = ""),h=T,sep = "\t")
    lili =by(gen$WFI,gen$depth,sum)
    for(i in seq(2,length(lili),1)){
      lili[i] = lili[i]*0.5**strtoi(rownames(lili)[i])
    }
    sumlili = sum(lili[-1])
    
    lolo = append(lolo ,lili/sumlili)
    
  }
  return(lolo)
}

#ggg = ComputeCompletenessAtDepth(boys)
#ggggraph  = data.frame(contrib = ggg,depth = as.factor(strtoi(names(ggg))))
#toplot = ggggraph[-which(ggggraph$depth == 0),]
#ggplot(toplot,aes(y = contrib,x= depth, fill = depth)) + geom_boxplot() + 
  

#tt = ComputeCompletenessAtDepth(boys1)
#ttt = tt
#for( i in seq(1,13,1)){
#ttt[i] = tt[i]*(0.5**strtoi(rownames(tt)[i]))
#}
#ttt = ttt[-1]
#ttfinal = ttt/sum(ttt)
#ttgraph = NULL
#ttgraph$contribcWFI = unlist(ttfinal)
#ttgraph$depth = unlist(strtoi(rownames(ttfinal)))

#ttgraph = as.data.frame(ttgraph)
#plot(ttgraph$contribcWFI)
#ggplot(toplot,aes(y = contrib,x= depth, fill = depth)) + geom_boxplot() + 
 # scale_fill_discrete(name = "Generations") + scale_y_continuous(name = "% of contribution to the cWFI")+
  #scale_x_discrete(name = "Generations") +  
  #theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=30),
  #      axis.text.y = element_text(size=18),axis.title.y = element_text(size=30),
   #     legend.text = element_text(size=20),legend.title = element_text(size=20)
    #    )





########################
########################

dim(boys1)
boys = boys1[which(boys1$wfi_completeness_6==1),]
dim(boys)
inbreeding = read.table("../../Python/1558-Inbreeding-Coefficient", sep = " ", row.name = 1)
boys$inbreeding = inbreeding$V2
F.lm = lm(inbreeding~cWFI, data = boys)
plot(inbreeding~cWFI, data = boys, pch = 15, cex = 0.5, ylab = "Inbreeding Coefficient", xlab = "Cumulated WFI")
abline(F.lm, col = "red", lwd = 2)
F.lmr =rstudent(F.lm)
plot(F.lmr,pch = 15, cex = 0.5)

FIRSTQUANTILE = 3
LASTQUANTILE = 18

breakquantile = seq(0,1,0.05)
#############################
#####FIRST : WFI COMPLETENESS + NB WFI
#############################
###
###
###
ccwfi_1 = 0.65
ccwfi_2 = 0.92
nbwfi_1 = 2200
nbwfi_2 = 3300

WFI_CC = ggplot(boys,aes(x = wfi_completeness_12)) + geom_histogram(binwidth = 1/90) +
  geom_vline(xintercept = ccwfi_1, linetype = "dashed", col = "red", size = 3) +
  geom_vline(xintercept = ccwfi_2, linetype = "dashed", col = "red",size = 3) +
  scale_x_continuous(name = "WFI Completeness until depth 12") +
  scale_y_continuous(name = "") + 
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=28),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=28),
        legend.text = element_text(size=20),legend.title = element_text(size=20)) 
jpeg("./figure2/WFI-CC-until-12.jpg",width = 600, height = 600)
print(WFI_CC)
dev.off()

subset = intersect(which(boys$wfi_completeness_12 > ccwfi_1),which(boys$wfi_completeness_12 < ccwfi_2))
sub1 = boys[subset,]
dim(sub1)
jpeg("./figure2/WFI-NB-until-12.jpg",width = 600, height = 600)
ggplot(sub1,aes(x = nb_wfi_12)) + geom_histogram(binwidth = 60) +
  geom_vline(xintercept = nbwfi_1, linetype = "dashed",  col = "red", size = 3) +
  geom_vline(xintercept = nbwfi_2, linetype = "dashed", col = "red", size = 3) +
  scale_x_continuous(name = "Number of individuals with WFI attributed\n until depth 12")  +
  scale_y_continuous(name = "") +  
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=28),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=28),
        legend.text = element_text(size=20),legend.title = element_text(size=20))
dev.off()
tronc = sub1[intersect(which(sub1$nb_wfi_12 > nbwfi_1),which(sub1$nb_wfi_12< nbwfi_2)),]
standard = sub1[intersect(which(sub1$nb_wfi_12 > nbwfi_1),which(sub1$nb_wfi_12< nbwfi_2)),]


tronc$cond = rep("Standard cWFI",length(tronc$cWFI_tronc_no_default))
tronc$cond[which(tronc$cWFI_tronc_no_default < quantile(tronc$cWFI_tronc_no_default,probs = breakquantile)[FIRSTQUANTILE])] = "Low cWFI"
tronc$cond[which(tronc$cWFI_tronc_no_default > quantile(tronc$cWFI_tronc_no_default,probs = breakquantile)[LASTQUANTILE])] = "High cWFI"
jpeg("./figure2/cWFI-Distrib-truncated.jpg",width = 600, height = 600)
ggplot(tronc,aes(cWFI_tronc_no_default,fill = cond)) + geom_histogram(binwidth = 0.2) +
        geom_vline(xintercept = quantile(tronc$cWFI_tronc_no_default,probs = breakquantile)[FIRSTQUANTILE], linetype = "dashed", col = "red") +
        geom_vline(xintercept = quantile(tronc$cWFI_tronc_no_default,probs = breakquantile)[LASTQUANTILE], linetype = "dashed", col = "red") + 
        scale_x_continuous(name="     Cumulated truncated WFI distribution") + 
  scale_fill_discrete(name = "Types of cWFI") +
  scale_y_continuous(name = "") + 
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=30),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=30),
        legend.text = element_text(size=20),legend.title = element_text(size=20))
dev.off()

Truncated_LOW  = tronc[which(tronc$cond == "Low cWFI"),]
Truncated_HIGH = tronc[which(tronc$cond == "High cWFI"),]

trunc = Truncated_LOW
trunc = rbind(trunc,Truncated_HIGH)

#####Stardard WFI
standard$cond = rep("Standard cWFI",length(standard$cWFI_no_default))
standard$cond[which(standard$cWFI_no_default < quantile(standard$cWFI_no_default,probs = breakquantile)[FIRSTQUANTILE])] = "Low cWFI"
standard$cond[which(standard$cWFI_no_default > quantile(standard$cWFI_no_default,probs = breakquantile)[LASTQUANTILE])] = "High cWFI"

jpeg("./figure2/cWFI-Distrib.jpg",width = 600, height = 600)
print(ggplot(standard,aes(cWFI_no_default,fill = cond)) + geom_histogram(binwidth = 0.2) +
        geom_vline(xintercept = quantile(standard$cWFI_no_default,probs = breakquantile)[FIRSTQUANTILE], linetype = "dashed", col = "red") +
        geom_vline(xintercept = quantile(standard$cWFI_no_default,probs = breakquantile)[LASTQUANTILE], linetype = "dashed", col = "red") + 
        scale_x_continuous(name="  Cumulated WFI distribution") +  
        scale_fill_discrete(name = "Types of cWFI") +
        scale_y_continuous(name = "") + 
        theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=30),
              axis.text.y = element_text(size=18),axis.title.y = element_text(size=30),
              legend.text = element_text(size=20),legend.title = element_text(size=20)))
dev.off()

standard_LOW  = standard[which(standard$cWFI_no_default < quantile(standard$cWFI_no_default,probs = breakquantile)[FIRSTQUANTILE]),]
standard_HIGH = standard[which(standard$cWFI_no_default > quantile(standard$cWFI_no_default,probs = breakquantile)[LASTQUANTILE]),]
standard_HIGH$cond = "High cWFI"
standard_LOW$cond = "Low cWFI"

stand = standard_LOW
stand = rbind(stand,standard_HIGH)



venn.diagram(x = list(I = rownames(stand),
                      II =rownames(trunc)),
filename = "./figure2/INTERSECTION.tiff",
lwd = 4,
             col = "transparent",
fill = c("red", "green"),  
alpha = 0.75,
label.col = "black",  
cex = 4,
fontfamily = "serif",
fontface = "bold",
cat.col = c("black", "black"),
cat.cex = 3,

cat.fontfamily = "serif",
cat.fontface = "bold",
cat.dist = c(0.03, 0.03),
cat.pos = c(-20, 14)
);


#######
#######Get the common occurence between all
######


common = trunc[intersect(rownames(trunc),rownames(stand)),]
dim(common)


dens_wfi_final = ggplot(common,aes(x = cWFI_no_default,fill =as.factor(cond))) + geom_histogram(binwidth = 1/5) +
  scale_x_continuous(name = "cWFI") + scale_y_continuous(name = "")+scale_fill_discrete(name = "Types of cWFI") +  
  theme(axis.text.x = element_text(size=20),axis.title.x = element_text(size=30),
        axis.text.y = element_text(size=20),axis.title.y = element_text(size=30),
        legend.text = element_text(size=20),legend.title = element_text(size=20))  

jpeg("./figure2/dens_final.jpg",width = 600, height = 600)
print(dens_wfi_final)
dev.off()
#########
########
#######
checkwilcox = function(df,variable){
  dat <- df
  dd = wilcox.test(y = df[which(df$cond=="Low cWFI"),variable],x = df[which(df$cond=="High cWFI"),variable])
  jpeg(paste("./figure2/",variable,"-WILCOX.jpg",sep = ""),width = 600, height = 600)
  print(ggplot(dat, aes(y = dat[,variable],x = cond, fill = cond),environment = environment()) + geom_boxplot()+
          scale_fill_discrete(name = "Types of cWFI") +
          scale_x_discrete(name = "cWFI") +scale_y_continuous(name = variable)+  
          theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=30),
                axis.text.y = element_text(size=18),axis.title.y = element_text(size=30),
                legend.text = element_text(size=20),legend.title = element_text(size=20),
                plot.title = element_text(size=20))+
          ggtitle(paste("p-value for wilcoxon test :",signif(dd$p.value,digit = 3))))
  dev.off()
}
dim(common)
length(which(common$cond == "High cWFI"))


checkwilcox(common,"meanGenDepth")

checkwilcox(common,"Completeness_12")
checkwilcox(common,"Completeness_6")
checkwilcox(common, "wfi_completeness_6")
checkwilcox(common,"wfi_completeness_12")
checkwilcox(common,"nb_wfi_12")
checkwilcox(common,"nb_wfi_6")
checkwilcox(common,"nb_wfi_7")
checkwilcox(common,"nb_wfi_8")
checkwilcox(common,"nb_wfi_9")
checkwilcox(common,"nb_wfi_10")
checkwilcox(common,"nb_wfi_11")
checkwilcox(common,"nb_wfi_12.1")

##############################
##############################INBREEDING

inbreed = read.table("~/Downloads/RE__Selection_of_the_individuals/p2013_135_Fs.csv", sep = "\t", h = T, row.names = 1)
inbreed
common$gen6 = inbreed$GEN6
common$gen12 = inbreed$GEN12
common$gen20 = inbreed$GEN20

ggplot(common, aes(y = gen20,x = cond, fill = cond),environment = environment()) + geom_violin()+
  scale_fill_discrete(name = "Types of cWFI") +
  scale_x_discrete(name = "cWFI") +scale_y_continuous(name = "Inbreeding_6")+  
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=30),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=30),
        legend.text = element_text(size=20),legend.title = element_text(size=20),
        plot.title = element_text(size=20))

row.names(common)
##############################
##############################

#################################LOCATION
###############################"


table(common$Loc_ego[which(common$cond == "Low cWFI")])
table(common$Loc_ego[which(common$cond == "High cWFI")])
jpeg("./figure2/REPARTITIONREGION.jpg",width = 1000, height = 1000)
ggplot(data = common, aes(x = factor(1),fill = cond)) + geom_histogram() + facet_wrap( ~Loc_ego ) + xlab('') + 
  coord_polar(theta = "y") + theme(strip.text.x = element_text(size = 15))+ scale_fill_discrete(name = "Types of cWFI")+ 
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=30),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=30),
        legend.text = element_text(size=20),legend.title = element_text(size=20))
dev.off()
write.table(row.names(common[which(common$cond == "Low cWFI"),]),"LOW-test",quote = F,row.names = F, col.names = F)
write.table(row.names(common[which(common$cond == "High cWFI"),]),"HIGH-test",quote = F,row.names = F, col.names = F)
write.table(row.names(common),"COMMON",quote = F,row.names = F, col.names = F)





################################COMMON ANCESTORS
##################################"

cc = read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION/SELECTING GENEALOGIES/scripts/python-toolbox/COMMON-MAT",
                sep = ';',row.names = 1)
highmat = read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION/SELECTING GENEALOGIES/scripts/python-toolbox/HIGH-MAT",
                sep = ';',row.names = 1)
lowmat = read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION/SELECTING GENEALOGIES/scripts/python-toolbox/LOW-MAT",
                sep = ';',row.names = 1)
highold = read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION/SELECTING GENEALOGIES/scripts/python-toolbox/old_HIGH",
                     sep = ';',row.names = 1)
 highrecent= read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION/SELECTING GENEALOGIES/scripts/python-toolbox/recent_HIGH",
                    sep = ';',row.names = 1)

lowold = read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION/SELECTING GENEALOGIES/scripts/python-toolbox/old_LOW",
                     sep = ';',row.names = 1)
lowrecent= read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION/SELECTING GENEALOGIES/scripts/python-toolbox/recent_LOW",
                       sep = ';',row.names = 1)

haut= read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION/SELECTING GENEALOGIES/scripts/python-toolbox/HIGH-AFTER-CLEAN-MAT",
                      sep = ';',row.names = 1)


hightest= read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION/SELECTING GENEALOGIES/scripts/python-toolbox/HIGH-test-MAT",
                       sep = ';',row.names = 1)
plotmat<- function(x){
colnames(x) =rownames(x)
x= as.matrix(x)

return(x)
}
col.l <- colorRampPalette(c( rgb(5, 48, 97, max = 255),rgb(146, 197, 222,max = 255),rgb(247, 247, 247,max = 255),rgb(244, 165, 130,max = 255),rgb(103, 0, 31, max = 255)))(30)

#########################
#########################SELECTION OF FINALE STUFF

highmat = read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION/SELECTING GENEALOGIES/scripts/R-scripts/Selecting-Individuals//param-3-output/High-kinship",
                     sep = ' ',row.names = 1, na.string = "NA")
lowmat = read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION/SELECTING GENEALOGIES/scripts/R-scripts/Selecting-Individuals//param-3-output/LOW-kinship",
                    sep = ' ',row.names = 1,na.string = "NA")
high = plotmat(highmat)
low = plotmat(lowmat)

repair = read.table("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION//SELECTING GENEALOGIES//scripts//Python//outputtemp", sep = " ")
lowrepair = intersect(row.names(lowmat),as.character(repair$V2))
lowrep = repair[which(as.character(repair$V2) %in% lowrepair),]
highrepair = intersect(row.names(highmat),as.character(repair$V2))
highrep = repair[which(as.character(repair$V2) %in% highrepair),]

out = NULL
for(i in seq(1,dim(lowrep)[1])){
low[as.character(lowrep$V2[i]),as.character(lowrep$V3[i])]  = lowrep$V1[i]
}
out
for(i in seq(1,dim(highrep)[1])){
  high[as.character(highrep$V2[i]),as.character(highrep$V3[i])]  = highrep$V1[i]
}

high[which(high == 0.5)] = rep(0,length(high[which(high == 0.5)]))
low[which(low == 0.5)] = rep(0,length(low[which(low == 0.5)]))

levelplot(high,ylab = '',col.regions = col.l, xaxt='n',xlab = "", ann=FALSE,yaxt = "n", main = " KINSHIP HIGH cWFI")

levelplot(low,ylab = '',col.regions = col.l,xaxt='n',xlab = "", ann=FALSE,yaxt = "n", main = " KINSHIP LOW cWFI")


length(unique(rownames(which(low>0.005 ,arr.ind = TRUE))))

length(unique(rownames(which(high>0.015 ,arr.ind = TRUE))))

lowremove = unique(rownames(which(low>0.005 ,arr.ind = TRUE)))
highremove = unique(rownames(which(high>0.015 ,arr.ind = TRUE)))
write(lowremove,"LOWREMOVE")
write(highremove,"HIGHREMOVE")
###########################################"""



levelplot(plotmat(cc),ylab = '',col.regions = col.l, main = " % of Common ancestors of selected individuals")
levelplot(plotmat(lowrecent),ylab = '',col.regions = col.l, main = "Recent Common ancestors of Low cWFI individuals")
levelplot(plotmat(highrecent),ylab = '',col.regions = col.l, main = "Recent Common ancestors of High cWFI individuals")
levelplot(plotmat(lowold),ylab = '',col.regions = col.l, main = "Old Common ancestors of Low cWFI individuals")
levelplot(plotmat(highold),ylab = '',col.regions = col.l, main = "Old Common ancestors of High cWFI individuals")

levelplot(plotmat(haut),ylab = '',col.regions = col.l, main = "Common ancestors of High cWFI individuals")
levelplot(plotmat(highmat),ylab = '',col.regions = col.l, main = "Common ancestors of High cWFI individuals")
levelplot(plotmat(hightest),ylab = '',col.regions = col.l, main = "Common ancestors of High cWFI individuals")

jpeg("./figure2/COMMON-MAT.jpg",width = 800, height = 800)

levelplot(highmat,ylab = '',col.regions = gray(0:100/100))
dev.off()
levelplot(lowmat,ylab = '',col.regions = gray(0:100/100))
dim(common)

length(unique(rownames(which(lowmat>0.444 ,arr.ind = TRUE))))

length(unique(rownames(which(hightest>0.2 ,arr.ind = TRUE))))

mean(lowmat,na.rm = T)
mean(highmat,na.rm = T)

rmrm = unique(rownames(which(lowmat>0.51 ,arr.ind = TRUE)))
common1 = common[!rownames(common) %in% rmrm,]
common2 = common1[!rownames(common1) %in% unique(rownames(which(highmat>0.74 ,arr.ind = TRUE))),]

common_96_1 = common[!rownames(common) %in% unique(rownames(which(lowmat>0.442 ,arr.ind = TRUE))),]
common_96_2 = common_96_1[!rownames(common_96_1) %in% unique(rownames(which(highmat>0.627 ,arr.ind = TRUE))),]
table(common_96_2$cond)

checkwilcox(common,"meanGenDepth")

checkwilcox(common2,"Completeness_12")
checkwilcox(common2,"Completeness_6")
checkwilcox(common2, "wfi_completeness_6")
checkwilcox(common2,"wfi_completeness_12")
checkwilcox(common_96_2,"nb_wfi_12")
checkwilcox(common2,"nb_wfi_6")

jpeg("./figure2/REPARTITIONREGIONFINAL.jpg",width = 1000, height = 1000)
ggplot(data = common_96_2, aes(x = factor(1),fill = cond)) + geom_histogram() + facet_wrap( ~Loc_ego ) + xlab('') + 
  coord_polar(theta = "y") + theme(strip.text.x = element_text(size = 10)) +
  scale_fill_discrete(name = "Types of cWFI")+
theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=34),
      axis.text.y = element_text(size=18),axis.title.y = element_text(size=34),
      legend.text = element_text(size=20),legend.title = element_text(size=20))
dev.off()

write.table(common2,"120-Indiv-list.txt", sep = "\t", quote = FALSE)
write.table(row.names(common2[which(common2$cond == "High cWFI"),]),"60-Indiv-HIGH", quote = FALSE, row.names = F, col.names = F)
write.table(common_96_2,"96-Indiv-list.txt", sep = "\t", quote = FALSE)