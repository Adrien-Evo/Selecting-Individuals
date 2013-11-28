library(ggplot2)
library(plyr)
library(MASS)
library(grid)
library(VennDiagram)
library(RgoogleMaps)
library(ggmap)
library(ggplot2)
library(lattice)

par(mar=c(5.1,4.1,4.1,2.1))
###TESTING PURPOSE
args  = NULL
args[1] = "param-3.txt"
################"

args <- commandArgs(trailingOnly = TRUE)



###Settings the parameters
print("Reading arguments from parameters file")
parameters = read.table(paste("./",args[1],sep = ""), h = F, row.names = 1 , sep = "=")

meandepth1 = parameters["meandepth1",]
meandepth2 = parameters["meandepth2",]
CC12_1 = parameters["CC12_1",]
CC12_2 =parameters["CC12_2",]
seuil = parameters["seuil",]
breaks = parameters["breaks",]
breakquantile = seq(0,1,breaks)
FIRSTQUANTILE = parameters["FIRSTDUODECILE",]+1
LASTQUANTILE = 20 - parameters["LASTDUODECILE",]+1
ccwfi_1 = parameters["ccwfi_1",]
ccwfi_2 = parameters["ccwfi_2",]
nbwfi_1 =parameters["nbwfi_1",]
nbwfi_2 = parameters["nbwfi_2",]

##Setting the output directory
splitlist = strsplit(args[1],"/")[[1]][length(strsplit(args[1],"/")[[1]])]
dirname = strsplit(splitlist,"[.]")[[1]][1]

print(paste("Creation of the output directory :",dirname))
dir.create(paste("./",dirname,"-output", sep = ""))
setwd(paste("./",dirname,"-output", sep = ""))

##Setting the Log file
write("Selection steps\n###############\n###############",file = "LOG.txt")
######Start of analysis

StartInd <- read.delim("~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION//SELECTING GENEALOGIES//scripts/BuildGen/output_files/info-added-info-on-nb-of-WFI.info")

per =length(which(StartInd$maxDepth == 15))+ length(which(StartInd$maxDepth == 12)) + length(which(StartInd$maxDepth == 14)) +length(which(StartInd$maxDepth == 13))
per = 100*per/dim(StartInd)[1] 

#####Total Inbreeding coefficient
inb = read.table(row.names = 1,"~/PROJECTS/RANGE EXPANSION IN HUMAN POPULATION//SELECTING GENEALOGIES//Data_files/Inbreeding-coefficient-3916.txt")
StartInd$Inbreeding = inb$V2

#Graph avec les densités de WFI pour différentes profondeurs de 11 à 15
Start16 <- StartInd[which(StartInd$maxDepth == 16),]
Start13 <- StartInd[which(StartInd$maxDepth == 13),]
Start14 <- StartInd[which(StartInd$maxDepth == 14),]
Start15 <- StartInd[which(StartInd$maxDepth == 15),]
Start17 <- StartInd[which(StartInd$maxDepth == 17),]

#dff2 <- rbind(Start13, Start14,Start15,Start16,Start17)
dff2 <- rbind(Start13, Start14,Start15,Start16,Start17)
dff2$nb_wfi <-dff2$nbIndiv -dff2$nb_wfi_default
rownames(dff2) <- dff2$ind
dff2<- dff2[,-(1)]





select_the_boys <-function(dff2,meandepth1,meandepth2,CC6,CC12_1,CC12_2){
  jpeg("./cc6.jpg",width = 600, height = 600)
  print(ggplot(dff2,aes(x=as.factor(maxDepth),y = Completeness_6,fill = as.factor(maxDepth))) + geom_violin(scale = "count")+
          scale_fill_discrete(name = "Maximum\n depth") + geom_hline(yintercept = CC6, col = "red",linetype = "dashed",size = 3)+
          scale_x_discrete(name = "") + scale_y_continuous(name = "Completeness at depth 6") +  
          theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=30),
                axis.text.y = element_text(size=18),axis.title.y = element_text(size=30),
                legend.text = element_text(size=20),legend.title = element_text(size=20)))
  dev.off()
  jpeg("./mgd.jpg",width = 600, height = 600)
  print(ggplot(dff2,aes(x=as.factor(maxDepth),y = meanGenDepth,fill = as.factor(maxDepth))) + geom_violin(scale = "count")+
          geom_hline(yintercept = meandepth1, col = "red",linetype = "dashed",size = 3)+geom_hline(yintercept = meandepth2, col = "red",linetype = "dashed",size = 3)+ 
          # stat_summary(fun.y = mean, geom = "point", color = "black", fill = "black", pch = 21, size = 2) +
          # stat_summary(fun.y = median, geom = "point", color = "black", fill = "white", pch = 23, size = 2) +
          scale_fill_discrete(name = "Maximum\n depth") + 
          scale_x_discrete(name = "") + scale_y_continuous(name = "Mean Genealogical Depth") +  
          theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=30),
                axis.text.y = element_text(size=18),axis.title.y = element_text(size=30),
                legend.text = element_text(size=20),legend.title = element_text(size=20)))
  dev.off()
  jpeg("./cc12.jpg",width = 600, height = 600)
  print( ggplot(dff2,aes(x=as.factor(maxDepth),y = Completeness_12,fill = as.factor(maxDepth))) + geom_violin(scale = "count") +
           geom_hline(yintercept = CC12_2, col = "red",linetype = "dashed",size = 3) + geom_hline(yintercept = CC12_1, col = "red",linetype = "dashed",size = 3) +
           scale_fill_discrete(name = "Maximum\n depth") +
           scale_x_discrete(name = "") + scale_y_continuous(name = "Completeness at depth 12") +  
           theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=30),
                 axis.text.y = element_text(size=18),axis.title.y = element_text(size=30),
                 legend.text = element_text(size=20),legend.title = element_text(size=20))
  )
  dev.off()
  
  subset  =intersect(which(dff2$meanGenDepth > meandepth1),which(dff2$meanGenDepth < meandepth2))
  write(paste("Selection on the mean genealogical depth :",length(subset)),file = "LOG.txt",append = TRUE)
  subset = intersect(subset,which(dff2$Completeness_6 >= CC6))
  write(paste("Selection on the Completeness at 6 :",length(subset)),file = "LOG.txt",append = TRUE) 
  subset = intersect(intersect(which(dff2$Completeness_12 > CC12_1),which(dff2$Completeness_12 < CC12_2)),subset) 
  write(paste("Selection on the Completeness at 12 :",length(subset)),file = "LOG.txt",append = TRUE)
  
  return (dff2[subset,])
}

boys1 =   select_the_boys(dff2,meandepth1,meandepth2,seuil,CC12_1,CC12_2) 


power2 = c(126,254,510,1022,2046,4094,8190)
mean(boys$nb_wfi_7/(2**7))
mean(boys$nb_wfi_8/(2**8))
mean(boys$nb_wfi_9/(2**9))
mean(boys$nb_wfi_10/(2**10))
mean(boys$nb_wfi_11/(2**11))


boys = boys1[which(boys1$wfi_completeness_6==1),]

write(paste("Selection on total wfi completeness until 6 :",dim(boys)[1]),file = "LOG.txt",append = TRUE)

#############################
#####FIRST : WFI COMPLETENESS + NB WFI
#############################
###
###
###


WFI_CC = ggplot(boys,aes(x = wfi_completeness_12)) + geom_histogram(binwidth = 1/90) +
  geom_vline(xintercept = ccwfi_1, linetype = "dashed", col = "red", size = 3) +
  geom_vline(xintercept = ccwfi_2, linetype = "dashed", col = "red",size = 3) +
  scale_x_continuous(name = "WFI Completeness until depth 12") +
  scale_y_continuous(name = "") + 
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=28),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=28),
        legend.text = element_text(size=20),legend.title = element_text(size=20)) 
jpeg("./WFI-CC-until-12.jpg",width = 600, height = 600)
print(WFI_CC)
dev.off()

subset = intersect(which(boys$wfi_completeness_12 > ccwfi_1),which(boys$wfi_completeness_12 < ccwfi_2))
sub1 = boys[subset,]
write(paste("Selection on the highest WFI Completeness until depth 12 :",dim(sub1)[1]),file = "LOG.txt",append = TRUE)

jpeg("./WFI-NB-until-12.jpg",width = 600, height = 600)
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

write(paste("Selection on an interval of nb of WFI until 12:",dim(tronc)[1]),file = "LOG.txt",append = TRUE)

tronc$cond = rep("Standard cWFI",length(tronc$cWFI_tronc_no_default))
tronc$cond[which(tronc$cWFI_tronc_no_default < quantile(tronc$cWFI_tronc_no_default,probs = breakquantile)[FIRSTQUANTILE])] = "Low cWFI"
tronc$cond[which(tronc$cWFI_tronc_no_default > quantile(tronc$cWFI_tronc_no_default,probs = breakquantile)[LASTQUANTILE])] = "High cWFI"
jpeg("./cWFI-Distrib-truncated.jpg",width = 600, height = 600)
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

jpeg("./cWFI-Distrib.jpg",width = 600, height = 600)
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
             filename = "./INTERSECTION.tiff",
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

write(paste("Selection on the low cWFI and high cWFI :",dim(common)[1]),file = "LOG.txt",append = TRUE)
write(paste("Individuals with low cWFI =", length(which(common$cond == "Low cWFI"))),file = "LOG.txt",append = TRUE)
write(paste("Individuals with high cWFI = ", length(which(common$cond == "High cWFI"))),file = "LOG.txt",append = TRUE)

dens_wfi_final = ggplot(common,aes(x = cWFI_no_default,fill =as.factor(cond))) + geom_histogram(binwidth = 1/5) +
  scale_x_continuous(name = "cWFI") + scale_y_continuous(name = "")+scale_fill_discrete(name = "Types of cWFI") +  
  theme(axis.text.x = element_text(size=20),axis.title.x = element_text(size=30),
        axis.text.y = element_text(size=20),axis.title.y = element_text(size=30),
        legend.text = element_text(size=20),legend.title = element_text(size=20))  

jpeg("./cWFI_distribution_final.jpg",width = 600, height = 600)
print(dens_wfi_final)
dev.off()

#print(str(common))
#########
########
#######
checkwilcox = function(df,variable){
  dat <- df
  dd = wilcox.test(y = df[which(df$cond=="Low cWFI"),variable],x = df[which(df$cond=="High cWFI"),variable])
  jpeg(paste("./",variable,"-WILCOX.jpg",sep = ""),width = 600, height = 600)
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



checkwilcox(common,'Inbreeding')



write.table(row.names(common[which(common$cond == "Low cWFI"),]),"LOW-indiv",quote = F,row.names = F, col.names = F)
write.table(row.names(common[which(common$cond == "High cWFI"),]),"HIGH-indiv",quote = F,row.names = F, col.names = F)
write.table(row.names(common),"COMMON",quote = F,row.names = F, col.names = F)

##############################
##############################Kinship computation :

#cmd <-"python ~/PROJECTS/RANGE\\ EXPANSION\\ IN\\ HUMAN\\ POPULATION/SELECTING\\ GENEALOGIES/scripts/Python/Merge-Pedigree.py COMMON"

#system(cmd)

#####################"
####################"Kinship matrix between groups


lowremove = read.table("LOWREMOVE")
highremove = read.table("HIGHREMOVE")
write(paste("Removing individuals with the highest kinship:",length(lowremove), 'of Low cWFI and',length(highremove),'of High cWFI'),file = "LOG.txt",append = TRUE)

common1 = common[!rownames(common) %in% as.character(lowremove$V1),]
dim(common1)
common2 = common1[!rownames(common1) %in% as.character(highremove$V1),]
dim(common2)

nblow = length(which(common2$cond == "Low cWFI"))-60
nbhigh = length(which(common2$cond == "High cWFI"))-60

write(paste("Removing individuals to have 60 60 with extrem values :", nblow,"low cWFI and",nbhigh,"high cWFI to remove"),file = "LOG.txt",append = TRUE)

low.df = common2[which(common2$cond == "Low cWFI"),]
high.df = common2[which(common2$cond == "High cWFI"),]

low.df = low.df[order(low.df$cWFI),]
high.df = high.df[order(high.df$cWFI),]
low.df.final = low.df[1:(length(which(common2$cond == "Low cWFI"))-nblow),]
high.df.final = high.df[(nbhigh+1):(length(which(common2$cond == "High cWFI"))),]

