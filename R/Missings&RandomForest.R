#install.packages("randomForest")
install.packages("VIM")
rm(list = ls()); options(warn=-1)
library(randomForest)
library(VIM)
library(ggplot2)
library(foreign)

setwd("C:/Users/jmartinez/Desktop/JC/OneDrive - El Colegio de México A.C/CM_1/2. Estadística/0. Articulo")
ae<- data.frame(read.table("Datos_22102018.csv", header=TRUE, sep=","))
ap <- aggr(ae[,c(7,11:21)], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(ae[,c(7,11:21)]), cex.axis=.7, gap=3, ylab=c("Valores perdidos","Distribución de los missings"))
ae.1<-ae[,c(7,11:21)]; for(i in 1:dim(ae.1)[2]) ae.1[,i]<-as.numeric(as.character(ae.1[,i]))

ae.imp <- rfImpute(hombres ~ ., ae.1)
ae.imp.nw<-ae.imp[c("evmun2018","hom_doloso","hom_culposo","inc_delic15")]
names(ae.imp.nw)<-c("evmun2018.IMP","hom_doloso.IMP","hom_culposo.IMP","inc_delic15.IMP")
ae.final<-data.frame(ae,ae.imp.nw); ae.final$id<-1:nrow(ae.final)
val.ae.final<-ae.final[c("id","evmun2018","evmun2018.IMP")]

ggplot(val.ae.final, na.rm = TRUE, aes(x=id,y=evmun2018)) + geom_point()
is.na(ae.final$evmun2018)

write.table(ae.final, "ae.final.csv", sep=",")
