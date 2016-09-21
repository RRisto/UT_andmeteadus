numbrid=read.csv("./data/numbrid.csv")

#Visualiseeri näiteid nii nullide kui ka ühtede seast.
#Näpunäited:
#Abiks on ette antud funktsioon plot_digit, mille argumendiks sobib andmestiku
#üks rida (NB! ilma viimase veeruta)
#Alamjooniste tegemisel on kasuks käsk par(mfrow = c(mitu_rida, mitu_veergu))
#Ääriseid saad muuta par(mar = c(bottom, left, top, right))

plot_digit = function(digit, ...){
    cols = grey(seq(1, 0, length = 256))
    image(t(matrix(as.numeric(digit), nrow=28, ncol=28)[28:1, ]), col = cols, ...)
}

par(mfrow=c(2, 5), mar=c(1, 1, 1, 1) + 0.1)

for (i in 1:10) {
    plot_digit(numbrid[i,])
}


# Pärast taasta esialgsed graafilised parameetrid
par(mfrow=c(1, 1), mar=c(5, 4, 4, 2) + 0.1)

#Tutvu ka paketi pheatmap funktsionaalsusega ja visualiseeri nulle ja ühtesid. 
#Abiks on ette antud funktsioon plot_digit_pheatmap.
#Näiteks nullide korral võiksid joonised välja näha umbes järgnevad. (Aga 
#joonise mitmeks jagamine mfrow abil ei tööta siin. Raportisse saad väiksemad 
#pildid, kui muudad koodiploki sätteid fig.width, fig.height.)
library(pheatmap)
plot_digit_pheatmap = function(digit){
    mat = matrix(as.numeric(digit), nrow=28, ncol=28)
    pheatmap(mat, cluster_cols=FALSE, cluster_rows=FALSE)
}

#siin ei saa mitut pilti ühele joonisele
for (i in 1:10) {
    plot_digit_pheatmap(numbrid[i,])
}

#sorteerin nii, et nullid üleval ja 1 all
numbrid_sort <- numbrid[order(numbrid$label),] 

#Visualiseeri kogu andmestikku kasutadaes pheatmap funktsiooni. Kasuta argumente 
#cluster_rows=FALSE, cluster_cols=FALSE. Abiks on 
pheatmap(numbrid_sort, cluster_rows=FALSE, cluster_cols=FALSE)

#Tee andmestikul PCA (kontrolli, et oled eelnevalt andmestikust eemaldanud 
#tunnuse label). PCA tegemiseks kasuta funktsiooni prcomp
pca = prcomp(numbrid[, -785])
#Küsimus: Eelmisel korral vaatasid videot, kus näidati, et vahel on andmestiku 
#“efektiivne dimensionaalsus” väiksem kui tunnuste arv. Milline on sinu arvates 
#andmestiku “efektiivne dimensionaalsus” praegusel juhul?
summary(pca)

#Tee hajuvusdiagramm PC1 vs PC2. Seejärel märgi joonisele, millised punktid 
#kujutavad numbrit 0 ja millised numbrit 1 (võid kasutada värvi või argumenti 
#pch).
pc1 = pca$x[, 1]
pc2 = pca$x[, 2]
plot(pc1, pc2, pch=16, col=as.factor(numbrid$label))
#Küsimus: Mida võiks selle joonise põhjal tähistada PC1?
#numbrit 0 ja 1

#Visualiseeri PCA kaalusid.
pca_rotate=pca$rotation
plot_digit_pheatmap(pca_rotate[,1])

#Küsimus: Millistel pikslitel on absoluutväärtuselt suured kaalud? Interpreteeri 
#selle abil PC1 tähendust (milliste pikslite intensiivsus peab olema suur ja 
#milliste pikslite intensiivsus madal, et PC1 väärtus oleks võimalikult suur).

#Tee nüüd PCA andmestikul, mis koosneb ainult nullidest. Lisaks tee 
#hajuvusdiagramm PC1 vs PC2.

numbrid_null=subset(numbrid, label==0)
pca_null = prcomp(numbrid_null[, -785])
pc1_null = pca_null$x[, 1]
pc2_null = pca_null$x[, 2]
plot(pc1_null, pc2_null, pch=16, col=as.factor(numbrid_null$label))

#Küsimus: Kas oskad selle joonise põhjal tõlgendada peakomponente PC1 ja PC2?
# rida1_null=numbrid_null[1,]
# rida2_null=numbrid_null[2,]
# 
# plot_digit_pheatmap(rida1_null)
# plot_digit_pheatmap(rida2_null)

#Vali üheksa numbrit tasandi erinevatest nurkadest ja visualiseeri neid 
#funktsiooni plot_digit abil. (Järgmisel joonisel on tähistatud punasena 25 
#punkti tasandi erinevatest nurkadest. Paremal pool on näidatud number 3 jaoks 
#võimalik tulemus. Sinul piisab võtta analoogilisest võrestikust 9 punktikest ja
#visualiseerida neid numbreid.)
#Näpunäide: Kasuks võib tulla funktsioon identify, mis aitab joonisele vajutades 
#leida lähimad punktid. Uuri mida teeb järgmine koodiblokk:

x = c(1,2,3)
y = c(2,5,3)
plot(x, y)
identify(c(1,2,3), c(2,5,3), n=1)
# kliki joonisel ja jälgi konsooli
#minu kood
identify(pc1_null, pc2_null, n=9)
#numbrid mis märkisin 353 452 522 588 653 782 806 866 931
arvud=c(353, 452, 522, 588, 653, 782, 806, 866, 931)

#plotime
par(mfrow=c(3, 3), mar=c(1, 1, 1, 1) + 0.1)

for (i in 1:length(arvud)) {
    plot_digit(numbrid_null[i,])
}

# Pärast taasta esialgsed graafilised parameetrid
par(mfrow=c(1, 1), mar=c(5, 4, 4, 2) + 0.1)

#Küsimus: Kuidas tõlgendad selle joonise põhjal peakomponente PC1 ja PC2?

#Punktikeste asemel visualiseeri numbreid. 

#Näpunäited:
#Toimi sarnaselt nagu NBA joonise tegemisel.Kõigepealt joonista tühi aken.
#for tsükli abil lisa numbrid. Sul ei ole vaja joonistada kõiki numbreid, 
#sest numbrid võivad kattuda. Võid joonistada näiteks 500 numbrit. Kui numbrid 
#tulevad liiga väikesed, siis muuda argumenti scale. Siin oleme ette andnud 
#täiendatud plot_digit funktsiooni, mis muudab valge läbipaistvaks (mis on 
#kasulik, kui numbrid joonistuvad üksteise peale).
plot_digit = function(digit, x=NA, y=NA, scale=1, add=FALSE, transparency=FALSE, ...){
    if(is.na(x)){
        x = 0
    }
    if(is.na(y)){
        y = 0
    }
    
    x_id = seq(x, x + scale, length=28)
    y_id = seq(y, y + scale, length=28)
    
    if(transparency==TRUE){
        tmp = as.character(round(seq(0, 0.99, length=256)*100))
        tmp[nchar(tmp) == 1] = paste("0", tmp[nchar(tmp) == 1], sep="")
        cols = colorRampPalette(c("#FFFFFF", "#000000"))(256)
        cols = paste(cols, tmp, sep="")
    }
    else{
        cols = grey(seq(1, 0, length = 256))
    }
    
    image(x_id, y_id, t(matrix(as.numeric(digit), nrow=28, ncol=28)[28:1, ]),
          col = cols, axes=F, asp=1, add=add, ...)
}

#plotime nullid
plot(0, 0, type = "n", xlim=c(-1500, 1500), ylim=c(-1500, 1000), asp=1)

for(i in 1:500){
    x = pc1_null[i] 
    y = pc2_null[i] 
    
    plot_digit(numbrid_null[i, -785], x, y, scale=100, add=TRUE, transparency = T)
}

#Tee samasugune joonis nagu eelmises punktis esialgse nullide ja ühtede
#andmestiku peal.
plot(0, 0, type = "n", xlim=c(-1500, 1500), ylim=c(-1500, 1000), asp=1)

for(i in 1:500){
    x = pc1[i] 
    y = pc2[i] 
    
    plot_digit(numbrid[i, -785], x, y, scale=100, add=TRUE, transparency = T)
}

#Boonusülesanne 1 (2 punkti) - implementeeri PCA
#Selle ülesande eesmärk on implementeerida omaenda PCA. Võid kasutada järgnevat 
#pseudokoodi. PCA pseudokood:
#Tsentreeri andmestik (ehk lahuta igast tunnusest tema keskmine).
#Tähistame tsentreeritud andmemaatriksit X (vaatlused ridades, tunnused 
#veergudes).
#võtan irise adnmetest
andmed=iris
andmed=andmed[, -5]

#leiame keskmised
keskmised=list()
for (i in 1:ncol(andmed)) {
    keskmised[i]=mean(as.numeric(andmed[,i]))
}
#lahutame keskmised maha
andmed2=andmed

for(i in 1:ncol(andmed2)) {
    andmed2[,i]=as.numeric(andmed2[,i])-as.numeric(keskmised[i])
}

#Arvuta kovariatsioonimaatriks XTX. (Näpunäide: maatriksite korrutamiseks on 
#R-is operaator %*%, transponeerimiseks funktsioon t().)
matrix_cov=data.matrix(andmed2)%*%data.matrix(t(andmed2))

#Leia kovariatsioonimaatriksi omaväärtused ja omavektorid. (Näpunäide: Abiks on 
#funktsioon eigen)
omavaartused=eigen(matrix_cov)

#Esimese omavektori elemendid on PC1 kordajad, teise omavektori elemendid PC2 
#kordajad jne. Kasuta nüüd neid kordajaid, et leida kõigi andmepunktide jaoks 
#PC1 ja PC2 väärtus. Selleks tuleb vastavad lineaarkombinatsioonid arvutada 
#kõigi X ridade jaoks.
#võtsin aluseks hoopis selle:
#http://www.bioinformaticstutorials.com/?p=124

x <- c(1,3,6,8,9,10,15,12,17)
y <- c(10,14,12,17,19,20,20,24,19)
m <- cbind(x, y)
plot(m, pch=seq(9))

#alguses kasutame prcompi
pca_proc <- prcomp(m)
plot(pca_proc$x, col=seq(9)) #the pca plot
plot(pca_proc) #the proportion of variance capture by each PC

#Now to do PCA manually, first step, center data around zero, then plot results
x1 <- x - mean(x)
y1 <- y - mean(y)
plot(x1, y1, xlim=c(-40, 40), ylim=c(-40, 20))

#Next, calculate the covariance matrix for vectors x1 and y1 and plot the 
#column vectors, these column vectors describe the direction of variability 
#in the data, i.e how similar is cov(x, x) with cov(x, y) and how similar is 
#cov(y, y) with cov(y, x). Note that the covariance of x with itself is the 
#same as the variance of x (i.e. var(x) == cov(x, x)).
m1 <- cbind(x1, y1)
cM <- cov(m1)
lines(x=c(0, cM[1,1]), y=c(0, cM[2,1]), col="blue")
lines(x=c(0, cM[1,2]), y=c(0, cM[2,2]), col="blue")
#As the cov(x, y) can never be greater than cov(x,x), one of the lines plotted 
#above will always be above the diagonal and one will be below, plot 
#the diagonal (i.e. a line through the origin with a slope of 1).
abline(0, 1, col="grey")
#While the vectors we have plotted above describe the direction of the 
#variability in the data, they do not do so in a way that is particularly 
#useful. By finding the eigenvectors of the covariance matrix, we can describe 
#the variability in terms of two orthogonal vectors which capture the direction 
#of variation, instead of the two vectors that we are currently plotting.
eigenVecs <- eigen(cM)$vectors
lines(x=c(0, eigenVecs[1,1]), y=c(0, eigenVecs[2,1]), col="red")
lines(x=c(0, eigenVecs[1,2]), y=c(0, eigenVecs[2,2]), col="red")
#As the eigenvectors are unit vectors (i.e. of length 1) it may be easier to 
#visualize how much each of them influences the data if we multiply them by 
#their corresponding eigenvalues, which represent the proportion of variability 
#explained by each eigenvector.
eVal1 <- eigen(cM)$values[1]
eVal2 <- eigen(cM)$values[2]
lines(x=c(0, eVal1*eigenVecs[1,1]), y=c(0, eVal1*eigenVecs[2,1]), col="red")
lines(x=c(0, eVal2*eigenVecs[1,2]), y=c(0, eVal2*eigenVecs[2,2]), col="red")
#This matrix contains the eigenvectors and we want to display the data in 
#terms of these eigenvectors. In this case we will select both eigenvectors, 
#but on high dimensional datasets, it is normal to chose a subset of 
#eigenvectors.
rowFeatVec <- t(eigenVecs)
rowDataAdj <- t(m1)
#Finally use matrix multiplcation to get the dot product between each 
#eigenvector and each point in the original centered data matrix (m1). This 
#operation describes to what degree each point is influenced by each 
#eigenvector Plot this and that’s the final plot.
#Note, %*% is the matrix multiplication operator in R.
transFData <- rowFeatVec %*% rowDataAdj
finalData <- rowFeatVec %*% rowDataAdj
plot(t(finalData), col=seq(9))
#Finally, to plot the equivalent of the scree plot we made above, simply plot 
#the eigen values.
barplot(eigen(cM)$values)

#Paku välja moodus, kuidas pikslite põhjal eristada numbreid 0 ja 1. Leia mitmel 
#juhul sinu meetod prognoosib õigesti, mitmel juhul valesti ja raporteeri 
#täpsus (õigete klassifitseerimiste arv koguarvust). Võiksid täpsuseks saada 
#vähemalt 90%.
#proovin randomforestit
library(caret)
inTrain <- createDataPartition(y=numbrid$label,
                               p=0.7, list=FALSE)
inTrain.train <- numbrid[inTrain,]
inTrain.test <- numbrid[-inTrain,]
inTrain.train$label=factor(inTrain.train$label)
inTrain.test$label=factor(inTrain.test$label)

library(randomForest)
model2 <- randomForest(factor(label) ~ ., data=inTrain.train, method="class", 
                       do.trace=TRUE)

rfcv(inTrain.train, factor(inTrain.train$label), cv.fold=3)

#ennustame mudeliga
prediction2 <- predict(model2, inTrain.test)
#vaatame tulemust, väga täpne
confusionMatrix(prediction2, factor(inTrain.test$label))

#muidugi on lihtne ka jooniselt vaadata, kus on 
#pc1 järgi jagunemise koht
plot(pc1, pc2, pch=16, col=as.factor(numbrid$label))

#paneme pc1-d ja labelid kokku
ennustus=data.frame(pc1, numbrid$label)
nullid=subset(ennustus, pc1<=300)
sum(nullid$numbrid.label)
#tundub, et 6  1-te on nullide tabelis, väga täpne:
sum(nullid$numbrid.label)/nrow(nullid)*100
#ühed
uhed=subset(ennustus, pc1>300)
sum(uhed$numbrid.label)/nrow(uhed)*100
#vigade arv mõlemas
viga_uhed=nrow(uhed)-sum(uhed$numbrid.label)
viga_nullid=sum(nullid$numbrid.label)
#koguviga
(viga_uhed+viga_nullid)/nrow(numbrid)*100

