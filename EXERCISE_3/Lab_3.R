## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
require(oligo)
require(Biobase)
require(arrayQualityMetrics)
require(genefilter)
require(Biobase)
require(hgu219.db)
require(limma)
require(gplots)
require(ReactomePA)


## ------------------------------------------------------------------------
workingDir <-getwd()


## ------------------------------------------------------------------------
targets <-read.table("./Data/Targets.txt", header = TRUE, sep="\t") 
#targets
summary(targets[2:5])


## ---- include=FALSE------------------------------------------------------
CELfiles <- list.celfiles(file.path("./Data/Cell_files"),full.names=TRUE)
data_CEL=read.celfiles(CELfiles)
my_targets<- read.AnnotatedDataFrame(file.path("./Data/Targets.txt"),header=TRUE, row.names = 1, sep = "\t")
rawData <- read.celfiles(CELfiles, phenoData = my_targets)


## ------------------------------------------------------------------------
#CELfiles <- list.celfiles(file.path("./Data/Cell_files"),full.names=TRUE)

#data_CEL=read.celfiles(CELfiles)

#my_targets<- read.AnnotatedDataFrame(file.path("./Data/Targets.txt"),header=TRUE, row.names = 1, sep = "\t")

#rawData <- read.celfiles(CELfiles, phenoData = my_targets)
rawData



## ---- dEFINE SOME VARIABLES FOR PLOTS------------------------------------

sampleNames <- as.character(targets$SHORT_NAME)
sampleColor <- as.character(targets$COLORS)
IDs <- as.character(targets$ID)
#sampleNames
#sampleColor
#IDs


## ------------------------------------------------------------------------
#arrayQualityMetrics(rawData, outdir = file.path("./Results/QCdir.raw"), force=TRUE)


## ---- BOXPLOT,PCA--------------------------------------------------------

boxplot(rawData, which="all",las=2, main="Intensity distribution of RAW data", 
        cex.axis=0.6, col=sampleColor, names=sampleNames)



plotPCA <- function ( X, labels=NULL, colors=NULL, dataDesc="", scale=FALSE, formapunts=NULL, myCex=0.8,...)
{
  pcX<-prcomp(t(X), scale=scale) # o prcomp(t(X))
  loads<- round(pcX$sdev^2/sum(pcX$sdev^2)*100,1)
  xlab<-c(paste("PC1",loads[1],"%"))
  ylab<-c(paste("PC2",loads[2],"%"))
  if (is.null(colors)) colors=1
  plot(pcX$x[,1:2],xlab=xlab,ylab=ylab, col=colors, pch=formapunts, 
       xlim=c(min(pcX$x[,1])-100000, max(pcX$x[,1])+100000),ylim=c(min(pcX$x[,2])-100000, max(pcX$x[,2])+100000))
  text(pcX$x[,1],pcX$x[,2], labels, pos=3, cex=myCex)
  title(paste("Plot of first 2 PCs for expressions in", dataDesc, sep=" "), cex=0.8)
}

plotPCA(exprs(rawData), labels=sampleNames, dataDesc="raw data", colors=sampleColor,
        formapunts=c(rep(16,4),rep(17,4)), myCex=0.6)


## ---- HIERARQUICAL CLUSTERING, include=FALSE-----------------------------

clust.euclid.average <- hclust(dist(t(exprs(rawData))),method="average")
plot(clust.euclid.average, labels=sampleNames, main="Hierarchical clustering of RawData", cex=0.7,  hang=-1)


## ----include=FALSE-------------------------------------------------------
eset <- rma(rawData)


## ------------------------------------------------------------------------
#eset <- rma(rawData)
eset


## ------------------------------------------------------------------------
#arrayQualityMetrics(eset, outdir = file.path("./Results/QCdir.norm"), force=TRUE)


## ------------------------------------------------------------------------
boxplot(eset, which="all",las=2, main="Intensity distribution of NORM data",cex.axis=0.6, col=sampleColor, names=sampleNames)


plotPCA <- function ( X, labels=NULL, colors=NULL, dataDesc="",
scale=FALSE, formapunts=NULL, myCex=0.8,...)
{
  pcX<-prcomp(t(X), scale=scale) # o prcomp(t(X))
  loads<- round(pcX$sdev^2/sum(pcX$sdev^2)*100,1)
  xlab<-c(paste("PC1",loads[1],"%"))
  ylab<-c(paste("PC2",loads[2],"%"))
  if (is.null(colors)) colors=1
  plot(pcX$x[,1:2],xlab=xlab,ylab=ylab, col=colors, pch=formapunts, 
       xlim=c(min(pcX$x[,1])-100, max(pcX$x[,1])+100),ylim=c(min(pcX$x[,2])-100, max(pcX$x[,2])+100))
  text(pcX$x[,1],pcX$x[,2], labels, pos=3, cex=myCex)
  title(paste("Plot of first 2 PCs for expressions in", dataDesc, sep=" "), cex=0.8)
}

plotPCA(exprs(eset), labels=sampleNames, dataDesc="Norm data", colors=sampleColor,
        formapunts=c(rep(16,4),rep(17,4)), myCex=0.6)


## ------------------------------------------------------------------------
annotation(eset) <- 'hgu219.db'
filter <- nsFilter(eset, var.func=IQR,
         var.cutoff=0.75, var.filter=TRUE,
         filterByQuantile=TRUE)
eset_filtered <- filter$eset
eset_filtered


## ---- DESIGNMATRIX-------------------------------------------------------
names_fac <- factor(sampleNames, levels = unique(sampleNames))
#names_fac

design_matrix <- model.matrix(~0+names_fac, pData(eset_filtered))
colnames(design_matrix)<- colnames(design_matrix)<- c('E_UC','dE_UC','E_NC','dE_NC')
rownames(design_matrix)<- rownames(design_matrix)<- IDs
design_matrix



## ----contrastsMatrix-----------------------------------------------------
cont.matrix <- makeContrasts (
  diff1 = E_UC - E_NC,
  diff2 = dE_UC - dE_NC,
  diff3 = (E_UC - E_NC) - (dE_UC - dE_NC),
  levels=design_matrix)
cont.matrix


## ----fitModel------------------------------------------------------------
fit<-lmFit (eset_filtered,design_matrix)
fit.main<-contrasts.fit(fit, cont.matrix)
fit.main<-eBayes(fit.main)


## ----extractResults------------------------------------------------------
first_comparison <- topTable (fit.main, number=nrow(fit.main), coef="diff1", adjust="fdr"); head(first_comparison[1:5])
second_comparison <- topTable (fit.main, number=nrow(fit.main), coef="diff2", adjust="fdr"); head(second_comparison[1:5])
third_comparison <- topTable(fit.main, number=nrow(fit.main), coef="diff3", adjust="fdr"); head(third_comparison[1:5])


## ------------------------------------------------------------------------
volcanoplot(fit.main, coef="diff1", highlight=10, main=paste('Differentially expressed genes \n E_UC and E_NC'))


## ------------------------------------------------------------------------
volcanoplot(fit.main, coef="diff2", highlight=10,main=paste('Differentially expressed genes \n dE_UC and dE_NC'))


## ------------------------------------------------------------------------
volcanoplot(fit.main, coef="diff3", highlight=10,main=paste('Diff. expr. genes \n type of ePoc (E, dE) and type of patient (UC, NC)'))


## ---- include=FALSE------------------------------------------------------

annotatedTopTable <- function(topTab, anotPackage)
  {
    topTab<-cbind(PROBEID=rownames(topTab), topTab)
    myprobes <- rownames(topTab)
    thePackage <- eval(parse(text=anotPackage))
    geneAnots <- select(thePackage, myprobes, c('SYMBOL','ENTREZID','GENENAME'))
    annotatedTopTab <- merge (x=geneAnots, y = topTab, by.x='PROBEID', by.y='PROBEID')
    return (annotatedTopTab)
}

topAnnotated_diff1 <- annotatedTopTable(first_comparison, anotPackage = "hgu219.db"); head(topAnnotated_diff1)
topAnnotated_diff2 <- annotatedTopTable(second_comparison, anotPackage = "hgu219.db"); head(topAnnotated_diff2)
topAnnotated_diff3 <- annotatedTopTable(third_comparison, anotPackage = "hgu219.db"); head(topAnnotated_diff3)

topAnnotated_diff1_ <- annotatedTopTable(first_comparison[1:10,1:5], anotPackage = "hgu219.db"); head(topAnnotated_diff1_)
topAnnotated_diff2_ <- annotatedTopTable(second_comparison[1:10,1:5], anotPackage = "hgu219.db"); head(topAnnotated_diff2_)
topAnnotated_diff3_ <- annotatedTopTable(third_comparison[1:10,1:5],anotPackage = "hgu219.db"); head(topAnnotated_diff3_)


## ------------------------------------------------------------------------

annotatedTopTable <- function(topTab, anotPackage)
  {
    topTab<-cbind(PROBEID=rownames(topTab), topTab)
    myprobes <- rownames(topTab)
    thePackage <- eval(parse(text=anotPackage))
    geneAnots <- select(thePackage, myprobes, c('SYMBOL','ENTREZID','GENENAME'))
    annotatedTopTab <- merge (x=geneAnots, y = topTab, by.x='PROBEID', by.y='PROBEID')
    return (annotatedTopTab)
  }

#topAnnotated_diff1 <- annotatedTopTable(first_comparison[0], anotPackage = "hgu219.db")
head(topAnnotated_diff1_[1:4])
#topAnnotated_diff2 <- annotatedTopTable(second_comparison[0], anotPackage = "hgu219.db")
head(topAnnotated_diff2_[1:4])
#topAnnotated_diff3 <- annotatedTopTable(third_comparison[0], anotPackage = "hgu219.db")
head(topAnnotated_diff3_[1:4])



## ------------------------------------------------------------------------
res <- decideTests(fit.main, method = 'separate', adjust.method='fdr', p.value=0.1, lfc = 1)

sum.res.rows <- apply(abs(res),1,sum)
res.selected <- res[sum.res.rows != 0,]
print(summary(res))

vennDiagram(res.selected[,1:3], cex=0.9)
title("Genes in common between three comparisons \n genes selected with FDR <0.5 and log(LFC) <1")


## ---- include=FALSE------------------------------------------------------
probesInHeatmap <- rownames(res.selected)
HMdata <- exprs(eset_filtered)[rownames(exprs(eset_filtered)) %in% probesInHeatmap,]

geneSymbols <- select(hgu219.db, rownames(HMdata), c("SYMBOL"))
SYMBOLS<- geneSymbols$SYMBOL
rownames(HMdata) <- SYMBOLS
write.csv(HMdata, file = file.path("./Results/data4Heatmap.csv"))

## ------------------------------------------------------------------------
probesInHeatmap <- rownames(res.selected)
HMdata <- exprs(eset_filtered)[rownames(exprs(eset_filtered)) %in% probesInHeatmap,]

#geneSymbols <- select(hgu219.db, rownames(HMdata), c("SYMBOL"))
SYMBOLS<- geneSymbols$SYMBOL
rownames(HMdata) <- SYMBOLS
write.csv(HMdata, file = file.path("./Results/data4Heatmap.csv"))


## ------------------------------------------------------------------------
my_palette <- colorRampPalette(c("blue", "red"))(n = 299)

heatmap.2(HMdata,
          Rowv = TRUE,
          Colv = TRUE,
          main = "Differentially expressed genes \n FDR < 0,1, logFC >=1",
          scale = "row",
          col = my_palette,
          sepcolor = "white",
          sepwidth = c(0.05,0.05),
          cexRow = 0.5,
          cexCol = 0.9,
          key = TRUE,
          keysize = 1.5,
          density.info = "histogram",
          ColSideColors = c(sampleColor),
          tracecol = NULL,
          dendrogram = "both",
          srtCol = 30)


## ------------------------------------------------------------------------
listOfTables <- list(diff1 = first_comparison, 
                     diff2  = second_comparison, 
                     diff3 = third_comparison)
listOfSelected <- list()
for (i in 1:length(listOfTables)){
  # select the toptable
  topTab <- listOfTables[[i]]
  # select the genes to be included in the analysis
  whichGenes<-topTab["adj.P.Val"]<0.5
  selectedIDs <- rownames(topTab)[whichGenes]
  # convert the ID to Entrez
  EntrezIDs<- select(hgu219.db, selectedIDs, c("ENTREZID"))
  EntrezIDs <- EntrezIDs$ENTREZID
  listOfSelected[[i]] <- EntrezIDs
  names(listOfSelected)[i] <- names(listOfTables)[i]
}
sapply(listOfSelected, length)



## ------------------------------------------------------------------------
EntrezUni <- c(topAnnotated_diff1$ENTREZID, topAnnotated_diff2$ENTREZID)


listOfData <- listOfSelected[1:2]
comparisonsNames <- names(listOfData)

organisme <- "human"
universe <- as.character(EntrezUni)

for (i in 1:length(listOfData)){
  data <- listOfData[[i]]
  genesIn <- listOfSelected[[i]]
  comparison <- comparisonsNames[i]
  enrich.result <- enrichPathway(gene = genesIn,
                                 pvalueCutoff = 0.5,
                                 readable = T,
                                 organism =  organisme,
                                 universe = universe,
                                 minGSSize = 5,
                                 maxGSSize = 500,
                                 pAdjustMethod = "BH")
  
  if (length(rownames(enrich.result@result)) != 0) {
  write.csv(as.data.frame(enrich.result), 
             file =paste0("./Results/","ReactomePA.Results.",comparison,".csv"), 
             row.names = FALSE)
  
  pdf(file=paste0("./Results/","ReactomePABarplot.",comparison,".pdf"))
    print(barplot(enrich.result, showCategory = 15, font.size = 4, 
            title = paste0("Reactome Pathway Analysis for ", comparison,". Barplot")))
  dev.off()
  
  pdf(file = paste0("./Results/","ReactomePAcnetplot.",comparison,".pdf"))
    print(cnetplot(enrich.result, categorySize = "geneNum", schowCategory = 15, 
         vertex.label.cex = 0.75))
  dev.off()
  }
}


## ------------------------------------------------------------------------
Tab.react <- read.csv(file.path("./Results/ReactomePA.Results.diff1.csv"), 
                       sep = ",", header = TRUE, row.names = 2)
head(Tab.react[,3:4])


Tab.react2 <- read.csv(file.path("./Results/ReactomePA.Results.diff2.csv"), 
                       sep = ",", header = TRUE, row.names = 2)
head(Tab.react2[,3:4])




## ------------------------------------------------------------------------
library("knitr")
purl("Lab_3.Rmd")

