library(e1071) 
library(lattice)


shapiro.test(environmental$ozone)
library(fitdistrplus)
library(gamlss)
library(gamlss.mx)
library(factoextra)

?gamlss
fit <-  fitdistr(environmental$temperature,'normal')
summary(fit)


summary(environmental)
str(environmental)

#separting variables
radiation <- environmental$radiation
temperature <- environmental$temperature
wind <- environmental$wind
ozone <- environmental$ozone


# Radiation
summary(radiation)
boxplot(radiation)
skewness(radiation)
kurtosis(radiation)
x11()
hist(radiation,breaks=30,freq = FALSE)
lines(density(radiation))



radiation.fit.exp <-histDist(radiation,family = EXP,nbins = 30,main = "Exponential Distribution")
radiation.fit.lognorm <- histDist(radiation,family = LOGNO,nbins=30,main="Log Normal Distribution")
radiation.fit.GA <- histDist(radiation,family = GA,nbins = 30,main="Gamma Distribution")
radiation.fit.IG <- histDist(radiation,family = IG,nbins = 30,main="Inverse Gaussian Distribution")
radiation.fit.WEI <- histDist(radiation,family=WEI,nbins=30,main = "Weibull Distribution")

data.frame(row.names = c("Exponential",'Log Normal','Gamma ',"Inverse Gussian","Weibull"),
           AIC = c(AIC(radiation.fit.exp),AIC(radiation.fit.lognorm),AIC(radiation.fit.GA),AIC(radiation.fit.IG),AIC(radiation.fit.WEI)),
           SBC = c(radiation.fit.exp$sbc,radiation.fit.lognorm$sbc,radiation.fit.GA$sbc,radiation.fit.IG$sbc,radiation.fit.WEI$sbc))


LR.test(radiation.fit.GA,radiation.fit.WEI)

logLik(radiation.fit.WEI)

radiation.mix.WEI <- gamlssMXfits(n=5,radiation~1,family=WEI,K=2,data=environmental) 

radiation.mix.WEI$aic
radiation.mix.WEI$sbc

mu.hat1 <- exp(radiation.mix.WEI[["models"]][[1]][["mu.coefficients"]])    
sigma.hat1 <- exp(radiation.mix.WEI[["models"]][[1]][["sigma.coefficients"]])



mu.hat2 <- exp(radiation.mix.WEI[["models"]][[2]][["mu.coefficients"]])    
sigma.hat2 <- exp(radiation.mix.WEI[["models"]][[2]][["sigma.coefficients"]])

#mu.hat3 <- exp(radiation.mix.GA[["models"]][[3]][["mu.coefficients"]])    
#sigma.hat2 <- exp(radiation.mix.GA[["models"]][[3]][["sigma.coefficients"]])

radiation.mix.WEI
#done with k=2
x11()
hist(radiation, breaks = 30,freq = FALSE, xlab = "Radiation" ,main=" Mixture of Weibull distributions")
lines(seq(min(radiation),max(radiation),length=length(radiation)),
      radiation.mix.WEI[["prob"]][1]*dWEI(seq(min(radiation),max(radiation),length=length(radiation)),
                                     mu = mu.hat1, sigma = sigma.hat1),lty=2,lwd=3,col=1)
lines(seq(min(radiation),max(radiation),length=length(radiation)),
      radiation.mix.WEI[["prob"]][2]*dWEI(seq(min(radiation),max(radiation),length=length(radiation)),
                                     mu = mu.hat2, sigma = sigma.hat2),lty=2,lwd=3,col=2)
lines(seq(min(radiation),max(radiation),length=length(radiation)),
      radiation.mix.WEI[["prob"]][1]*dWEI(seq(min(radiation),max(radiation),length=length(radiation)),
                                     mu = mu.hat1, sigma = sigma.hat1)+
        radiation.mix.WEI[["prob"]][2]*dWEI(seq(min(radiation),max(radiation),length=length(radiation)),
                                       mu = mu.hat2, sigma = sigma.hat2),lty=1,lwd=3,col=4)
radiation.mix.WEI[["prob"]]

#Temperature

summary(temperature)
boxplot(temperature)
#ONE WAY TO FIND THE DISTRIBUTION
# FIRST FIND THE HISTOGRAM AND DENSITY GRAPH
hist(temperature,freq=FALSE,breaks =30 )
lines(density(temperature),col='red')
skewness(temperature)
kurtosis(temperature)

#USING EXPONENTIAL DISTRIBUTION
temp.fit.exp <- histDist(temperature,family=EXP,nbins = 30,main="Exponential Distribution")
temp.fit.lognorm <- histDist(temperature,family=LOGNO,nbins = 30,main = "Log Normal Distribution")
temp.fit.GA <- histDist(temperature,family=GA,nbins = 30,main = "Gamma Distribution")
temp.fit.IG <- histDist(temperature,family=IG,nbins = 30,main = "Inverse Gaussian Distribution")
temp.fit.WEI <- histDist(temperature,family=WEI,nbins = 30, main = "Weibull Distribution")


data.frame(row.names = c("Exponential",'Log Normal','Gamma ',"Inverse Gussian","Weibull"),
                            AIC = c(AIC(temp.fit.exp),AIC(temp.fit.lognorm),AIC(temp.fit.GA),AIC(temp.fit.IG),AIC(temp.fit.WEI)),
                            SBC = c(temp.fit.exp$sbc,temp.fit.lognorm$sbc,temp.fit.GA$sbc,temp.fit.IG$sbc,temp.fit.WEI$sbc))



temp.mix.WEI <- gamlssMXfits(n=5,temperature~1,family=WEI,K=3,data=environmental) 

temp.mix.WEI$aic
temp.mix.WEI$sbc

mu.hat1 <- exp(temp.mix.WEI[["models"]][[1]][["mu.coefficients"]])    
sigma.hat1 <- exp(temp.mix.WEI[["models"]][[1]][["sigma.coefficients"]])

#?exp
#coef(temp.mix.WEI,distribution=2)["mu.hat1"]
# estimate of mu and sigma in group 2

mu.hat2 <- exp(temp.mix.WEI[["models"]][[2]][["mu.coefficients"]])    
sigma.hat2 <- exp(temp.mix.WEI[["models"]][[2]][["sigma.coefficients"]])

#checking the lenght of the 
length(temp.mix.WEI[["models"]])

mu.hat3 <- exp(temp.mix.WEI[["models"]][[3]][["mu.coefficients"]])    
sigma.hat3 <- exp(temp.mix.WEI[["models"]][[3]][["sigma.coefficients"]])


hist(temperature, breaks = 30,freq = FALSE, xlab = "Temperature" ,main="Mixture of three Weibull distributions")
lines(seq(min(temperature),max(temperature),length=length(temperature)),
      temp.mix.WEI[["prob"]][1]*dWEI(seq(min(temperature),max(temperature),length=length(temperature)),
                                     mu = mu.hat1, sigma = sigma.hat1),lty=2,lwd=3,col=3)
lines(seq(min(temperature),max(temperature),length=length(temperature)),
      temp.mix.WEI[["prob"]][2]*dWEI(seq(min(temperature),max(temperature),length=length(temperature)),
                                     mu = mu.hat2, sigma = sigma.hat2),lty=2,lwd=3,col=4)
lines(seq(min(temperature),max(temperature),length=length(temperature)),
      temp.mix.WEI[["prob"]][3]*dWEI(seq(min(temperature),max(temperature),length=length(temperature)),
                                     mu = mu.hat3, sigma = sigma.hat3),lty=2,lwd=3,col=1)
lines(seq(min(temperature),max(temperature),length=length(temperature)),
      temp.mix.WEI[["prob"]][1]*dWEI(seq(min(temperature),max(temperature),length=length(temperature)),
                                     mu = mu.hat1, sigma = sigma.hat1)+
      temp.mix.WEI[["prob"]][2]*dWEI(seq(min(temperature),max(temperature),length=length(temperature)),
                                     mu = mu.hat2, sigma = sigma.hat2)+
        temp.mix.WEI[["prob"]][3]*dWEI(seq(min(temperature),max(temperature),length=length(temperature)),
                                       mu = mu.hat3, sigma = sigma.hat3),lty=1,lwd=3,col=2)





?dWEI
temp.mix.WEI[["models"]][[1]][["mu.coeficients"]]


#wind
summary(wind)
boxplot(wind)
skewness(wind)
kurtosis(wind)

x11()
hist(wind,freq = FALSE,breaks = 30)
lines(density(wind))

wind.fit.exp <- histDist(wind,family=EXP,nbins=30,xlab = "wind",main = "Exponential Distribution")
wind.fit.lognorm <- histDist(wind,family = LOGNO,nbins=30,xlab="wind", main="Log Normal Distribution")
wind.fit.GA <- histDist(wind,family = GA,nbins=30,xlab = "wind", main="Gamma Distribution")
wind.fit.IG <- histDist(wind,family = IG,nbins=30,xlab = "wind", main="Inverse Gaussian Distribution")
wind.fit.WEI <- histDist(wind,family = WEI,nbins = 30,xlab = "wind",main = "Weibull Distribution")


data.frame(row.names = c("Exponential",'Log Normal','Gamma ',"Inverse Gussian","Weibull"),
           AIC = c(AIC(wind.fit.exp),AIC(wind.fit.lognorm),AIC(wind.fit.GA),AIC(wind.fit.IG),AIC(wind.fit.WEI)),
           SBC = c(wind.fit.exp$sbc,wind.fit.lognorm$sbc,wind.fit.GA$sbc,wind.fit.IG$sbc,wind.fit.WEI$sbc))





wind.mix.GA <- gamlssMXfits(n=6,wind~1,family = GA,K=3,data = environmental)

wind.mix.GA$aic

wind.mix.GA$sbc

library(fitdistrplus)
descdist(temperature)
kurtosis(temperature)
#description
#wind.mix.GA

mu.hat1 <- exp(wind.mix.GA$models[[1]][["mu.coefficients"]])
sigma.hat1 <- exp(wind.mix.GA$models[[1]][["sigma.coefficients"]])

mu.hat2 <- exp(wind.mix.GA[["models"]][[2]][["mu.coefficients"]])    
sigma.hat2 <- exp(wind.mix.GA$models[[2]][["sigma.coefficients"]])                       

mu.hat3 <- exp(wind.mix.GA[["models"]][[3]][["mu.coefficients"]])    
sigma.hat3 <- exp(wind.mix.GA$models[[3]][["sigma.coefficients"]])

wind.mix.GA[["prob"]][[1]]
wind.mix.GA[["prob"]][[2]]
wind.mix.GA[["prob"]][[3]]
str(wind.mix.GA)

hist(wind,freq = FALSE,breaks = 30, main = "Mixture of Gamma Distribution")
lines(seq(min(wind),max(wind),length=length(wind)),
      wind.mix.GA[["prob"]][1]*dGA(seq(min(wind),max(wind),length=length(wind)),
                                     mu = mu.hat1, sigma = sigma.hat1),lty=2,lwd=3,col=3)
lines(seq(min(wind),max(wind),length=length(wind)),
      wind.mix.GA[["prob"]][2]*dGA(seq(min(wind),max(wind),length=length(wind)),
                                     mu = mu.hat2, sigma = sigma.hat2),lty=2,lwd=3,col=4)
lines(seq(min(wind),max(wind),length=length(wind)),
      wind.mix.GA[["prob"]][3]*dGA(seq(min(wind),max(wind),length=length(wind)),
                                   mu = mu.hat3, sigma = sigma.hat3),lty=2,lwd=3,col=1)
lines(seq(min(wind),max(wind),length=length(wind)),
      wind.mix.GA[["prob"]][1]*dGA(seq(min(wind),max(wind),length=length(wind)),
                                     mu = mu.hat1, sigma = sigma.hat1)+
        wind.mix.GA[["prob"]][2]*dGA(seq(min(wind),max(wind),length=length(wind)),
                                       mu = mu.hat2, sigma = sigma.hat2)+
      wind.mix.GA[["prob"]][3]*dGA(seq(min(wind),max(wind),length=length(wind)),
                                   mu = mu.hat3, sigma = sigma.hat3),lty=1,lwd=3,col=2)

#Ozone
summary(ozone)
boxplot(ozone)


hist(ozone,breaks=30,freq = FALSE)
lines(density(ozone))

skewness(ozone)
kurtosis(ozone)

ozone.fit.exp <-histDist(ozone,family = EXP,nbins = 30,main = "Exponential Distribution")
ozone.fit.lognorm <- histDist(ozone,family = LOGNO,nbins=30,main="Log Normal Distribution")
ozone.fit.GA <- histDist(ozone,family = GA,nbins = 30,main="Gamma Distribution")
ozone.fit.IG <- histDist(ozone,family = IG,nbins = 30,main="Inverse Gaussian Distribution")
ozone.fit.WEI <- histDist(ozone,family=WEI,nbins=30,main = "Weibull Distribution")


data.frame(row.names = c("Exponential",'Log Normal','Gamma ',"Inverse Gussian","Weibull"),
           AIC = c(AIC(ozone.fit.exp),AIC(ozone.fit.lognorm),AIC(ozone.fit.GA),AIC(ozone.fit.IG),AIC(ozone.fit.WEI)),
           SBC = c(ozone.fit.exp$sbc,ozone.fit.lognorm$sbc,ozone.fit.GA$sbc,ozone.fit.IG$sbc,ozone.fit.WEI$sbc),
           logLik= c(logLik(ozone.fit.exp),logLik(ozone.fit.lognorm),logLik(ozone.fit.GA),
                     logLik(ozone.fit.IG),logLik(ozone.fit.WEI)))

AIC(ozone.fit.exp,ozone.fit.lognorm,ozone.fit.GA,ozone.fit.IG,ozone.fit.WEI)

LR.test(ozone.fit.WEI,ozone.fit.GA)

ozone.mix.GA[["prob"]]

ozone.mix.GA <- gamlssMXfits(n=5,ozone~1,family=GA,K=2,data=environmental) 

ozone.mix.GA$aic
ozone.mix.GA$sbc


mu.hat1 <- exp(ozone.mix.GA[["models"]][[1]][["mu.coefficients"]])    
sigma.hat1 <- exp(ozone.mix.GA[["models"]][[1]][["sigma.coefficients"]])



mu.hat2 <- exp(ozone.mix.GA[["models"]][[2]][["mu.coefficients"]])    
sigma.hat2 <- exp(ozone.mix.GA[["models"]][[2]][["sigma.coefficients"]])


#done with k=2
x11()
hist(ozone, breaks = 30,freq = FALSE, xlab = "Ozone" ,main=" Mixture of two Gamma distributions")
lines(seq(min(ozone),max(ozone),length=length(ozone)),
      ozone.mix.GA[["prob"]][1]*dGA(seq(min(ozone),max(ozone),length=length(ozone)),
                                         mu = mu.hat1, sigma = sigma.hat1),lty=2,lwd=3,col=1)
lines(seq(min(ozone),max(ozone),length=length(ozone)),
      ozone.mix.GA[["prob"]][2]*dGA(seq(min(ozone),max(ozone),length=length(ozone)),
                                         mu = mu.hat2, sigma = sigma.hat2),lty=2,lwd=3,col=2)
lines(seq(min(ozone),max(ozone),length=length(ozone)),
      ozone.mix.GA[["prob"]][1]*dGA(seq(min(ozone),max(ozone),length=length(ozone)),
                                         mu = mu.hat1, sigma = sigma.hat1)+
        ozone.mix.GA[["prob"]][2]*dGA(seq(min(ozone),max(ozone),length=length(ozone)),
                                           mu = mu.hat2, sigma = sigma.hat2),lty=1,lwd=3,col=4)


##
library(psych)
x11()
pairs.panels(environmental, hist.col="blue", density = TRUE, ellipses = FALSE)

#plots <- pairs.panels(environmental)
scaled.data <- apply(environmental,2,scale)

pairs(scaled.data,pch=20,gap=0,col="navy")

library(corrplot)
corMatrix <- cor(environmental)
corrplot(corMatrix,method="number",type="upper",tl.col = "black", tl.srt = 45,tl.cex = 0.7)

#covariance
covar <-cov(scaled.data)
eigen(covar)

pc<- prcomp(environmental,scale=TRUE)
summary(pc)
pc$rotation[,1:4]
x11()
biplot(pc,scale=0)
sdev <- pc$sdev
pve <- sdev^2/sum(sdev^2)
pve
pc.var <- pc$sdev^2
pc.var
eigen_value = pc$sdev^2
eigen_value
# since there are 4 pcs, so how many we want to extract
#we can do cumulative sum, whatever pcs make 80, that are the number of pcs we use

cumsum(pve)
# only two pcs make 80, so we use only two pcs.....

plot(cumsum(pve),ylim = c(0,1),type = 'b',main = "Cumulative Scree Plot",
     ylab = "cumulative PVE",
     xlab= "Prinicipal Components")
# checking
plot(pve,ylim = c(0,1),type = 'b',main = "Cumulative Scree Plot",
     ylab = "cumulative PVE",
     xlab= "Prinicipal Components")
#
Scree.Plot(corMatrix)
fviz_eig(pc)
summary(pc)
pc
#clustering
# distance type affects the dissimalrity b/w units, hence also the shape of clusters
# Scaling is neccessay steps as distance and dissimalirty directly depends on the scale of measurement
# the goal is to make comparable
# here finding the euclidean distance

#Accessing Clustering 
library(factoextra)
library(ggplot2)
#step 1 accessing cluster tendency
x11()
pairs(scaled.data,gap=0,pch=16)
# below line, not neccessary, can look after or skip
#fviz_pca_ind(prcomp(scaled.data),geom="point",pallete="jco",ggtheme=theme_classic())

# below 3 lines are also skipping for time being
km.res <- kmeans(environmental,4)
cl1 <-  km.res$cluster
pairs(environmental,gap=0,pch=cl1,col =c("black","orange","red","blue")[cl1])

#step 2 statistics method
library(clustertend)
set.seed(123)
hopkins(environmental,n=nrow(environmental)-1)

#step 3 visualization method
x11()
fviz_dist(dist(environmental),show_labels=FALSE)+labs(title="Environmental")


#optimal number of clusters
x11()
fviz_nbclust(environmental,kmeans, method="wss")+geom_vline(xintercept=4,linetype=2)

fviz_nbclust(environmental,kmeans,method = "silhouette")
set.seed(123)
x11()
fviz_nbclust(environmental,kmeans,nstart=50, method = "gap_stat",nboot = 50)


library(NbClust)
x11()
nb <- NbClust(environmental,distance="euclidean",min.nc=2,
              max.nc=10,method="kmeans")
fviz_nbclust(nb)

#Cluster validation statitics
#silhoutte
km.res <- eclust(scaled.data,"kmeans",k=4,nstart=50)
fviz_silhouette(km.res,palette="jco",ggtheme=theme_classic())
silinfo <- km.res$silinfo
head(silinfo$widths[,1:3],10)
sil <- km.res$silinfo$widths[,1:3]
neg_sil_index <-which(sil[,"sil_width"]<0)
sil[neg_sil_index,,drop=FALSE]

#Dunn index
library(fpc)
km_stats <- cluster.stats(dist(scaled.data),km.res$cluster)
km_stats$dunn
km_stats$min.separation # inter-cluster separation

km_stats$max.diameter # intra-cluster compactness

#Dunn for mediods

pam.res <- eclust(scaled.data,"pam",k=4,nstart=50)
fviz_silhouette(pam.res,palette="jco",ggtheme=theme_classic())
silinfo <- pam.res$silinfo
head(silinfo$widths[,1:3],10)
sil <- pam.res$silinfo$widths[,1:3]
neg_sil_index <-which(sil[,"sil_width"]<0)
sil[neg_sil_index,,drop=FALSE]

pam_stats <- cluster.stats(dist(scaled.data),pam.res$cluster)
pam_stats$dunn
pam_stats$min.separation # inter-cluster separation

pam_stats$max.diameter # intra-cluster compactness


dist.euclidean <- dist(scaled.data,method="euclidean")
# for visulaization of distance
round(as.matrix(dist.euclidean)[1:20,1:20],2)


library(factoextra)
#x11()
#fviz_dist(dist.euclidean) # not useful for my data, not useful visulization

dist.manhattan <- dist(scaled.data,method = "manhattan")
round(as.matrix(dist.manhattan)[1:10,1:10],2)

# Agglomerative Hierarchical clustering 
# ward method

res.hc <- hclust(d=dist.euclidean,method = "ward.D2")

res.coph <- cophenetic(res.hc)
cor(res.coph,dist.euclidean)

grp <- cutree(res.hc,k=4)
head(grp)
table(grp)
#rownames(df)[grp==1]
x11()
fviz_dend(res.hc,k=4,cex = 0.5, k_colors =c("#2E95DF","#00AFBB","#E7B800","#FC4E07"),
          color_labels_by_k = TRUE, rect = TRUE)
x11()
pairs(scaled.data,gap=0,pch=grp,col=c("#2E95DF","#00AFBB","#E7B800","#FC4E07")[grp])

fviz_cluster(list(data=scaled.data,cluster=grp),
             pallete=c("#2E95DF","#00AFBB","#E7B800","#FC4E07"), 
             ellipse.type = "convex",
             repel=TRUE)

# Agglomerative hierarchical Clustering - euclidean dist and average linkage method
res.hc2 <- hclust(d=dist.euclidean,method = "average")

grp2 <- cutree(res.hc2,k=4)
head(grp2)
table(grp2)
x11()
fviz_dend(res.hc2,k=4,cex = 0.5, k_colors =c("#2E95DF","#00AFBB","#E7B800","#FC4E07"),
          color_labels_by_k = TRUE, rect = TRUE)

res.coph2 <- cophenetic(res.hc2)
cor(res.coph2,dist.euclidean)

pairs(scaled.data,gap=0,pch=grp2,col=c("#2E95DF","#00AFBB","#E7B800")[grp2])
fviz_cluster(list(data=scaled.data,cluster=grp2),
             pallete=c("#2E95DF","#00AFBB","#E7B800"), 
             ellipse.type = "convex",
             repel=TRUE)

# Agglomerative - euclidean distance

# agglomerative -- manhattan distance
dist.manhattan <- dist(scaled.data,method = "manhattan")
round(as.matrix(dist.manhattan)[1:10,1:10],2)

res.hc.man <- hclust(d=dist.manhattan,method = "ward.D2")

res.coph.man <- cophenetic(res.hc.man)
cor(res.coph.man,dist.manhattan)

grp <- cutree(res.hc.man,k=4)
head(grp)
table(grp)
x11()
fviz_dend(res.hc.man,k=4,cex = 0.5, k_colors =c("#2E95DF","#00AFBB","#E7B800","#FC4E07"),
          color_labels_by_k = TRUE,rect = TRUE)
x11()
pairs(scaled.data,gap=0,pch=grp,col=c("#2E95DF","#00AFBB","#E7B800","#FC4E07")[grp])
fviz_cluster(list(data=scaled.data,cluster=grp),
             pallete=c("#2E95DF","#00AFBB","#E7B800","#FC4E07"), 
             ellipse.type = "convex",
             repel=TRUE)
# Agglomerative HC based on average linkage

res.hc.man2 <- hclust(d=dist.manhattan,method = "average")

res.coph2 <- cophenetic(res.hc.man2)
cor(res.coph2,dist.manhattan)

grp <- cutree(res.hc.man2,k=4)
head(grp)
table(grp)
x11()
fviz_dend(res.hc.man2,k=4,cex = 0.5, k_colors =c("#2E95DF","#00AFBB","#E7B800","#FC4E07"),
          color_labels_by_k = TRUE,rect=TRUE)
x11()
pairs(scaled.data,gap=0,pch=grp,col=c("#2E95DF","#00AFBB","#E7B800","#FC4E07")[grp])
fviz_cluster(list(data=environmental,cluster=grp),
             pallete=c("#2E95DF","#00AFBB","#E7B800","#FC4E07"), 
             ellipse.type = "convex",
             repel=TRUE)


#Partition clustering
# K means


#fviz_nbclust(environmental,kmeans,nstart=25,method="wss")+
#  geom_vline(xintercept = 4,linetype=2)

km.res <- kmeans(scaled.data,4,nstart=25)
km.res$size
c1 <- km.res$cluster
x11()
pairs(scaled.data,gap=0,pch=c1,col=c("#2E95DF","#00AFBB","#E7B800","#FC4E07")[c1])

fviz_cluster(km.res,data= scaled.data,pallete=c("#2E95DF","#00AFBB","#E7B800","#FC4E07"),
             ellipse.type = "euclid", star.plot=TRUE,
             repel = TRUE,ggtheme=theme_minimal())

# K mediods - we can find using PAM algorithm

pam.res <- pam(scaled.data,4)
pam.res$medoids
clust <- pam.res$clustering
pairs(scaled.data,gap = 0,pch=clust,col=c("#2E95DF","#00AFBB","#E7B800","#FC4E07")[clust])
fviz_cluster(pam.res,data= scaled.data,pallete=c("#2E95DF","#00AFBB","#E7B800","#FC4E07"),
             ellipse.type="euclid",star.plot=TRUE,repel = TRUE,ggtheme=theme_minimal())

# Model based clustering
library(mclust)
mod <-Mclust(scaled.data,G=1:9,modelNames=NULL)
summary(mod$BIC)
x11()

plot(mod, what = "BIC", ylim = range(mod$BIC, na.rm = TRUE), 
     legendArgs = list(x = "bottomleft"))
fviz_mclust(mod, "BIC", palette = "jco")


summary(mod)
head(round(mod$z, 6), 15)
head(mod$classification, 15)
pairs(scaled.data, gap=0, pch = 16, col = mod$classification)

fviz_cluster(mod,data= scaled.data,pallete=c("#2E95DF","#00AFBB","#E7B800","#FC4E07"),
             ellipse.type = "euclid", star.plot=TRUE,
             repel = TRUE,ggtheme=theme_minimal())

library(cluster)
res.agnes <- agnes(x=environmental, stand=TRUE, metric="eculidean",method="ward")
res.agnes
x11()
fviz_dend(res.agnes,cex=0.6,k=5)
pairs(environmental,gap=0,pch=grp,col=c("#2E95DF","#00AFBB","#E7B800","FC4E07")[grp])
