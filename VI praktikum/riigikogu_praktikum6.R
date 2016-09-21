andmed=read.csv("./data/riigikogu_xii.csv", as.is = TRUE)

#Muuda tunnused numbriliseks. Selleks kodeeri hääletused järgmiselt:
#vastu tähista arvuga -1, poolt tähista arvuga 1, puudub, erapooletu, ei 
#hääletanud, NA tähista arvuga 0. Pane tähele, et tunnused oleksid numbrilised 
#mitte sõned.
library(car)
data <- apply(andmed, 2, function(x) {x <- recode(x,"'vastu'=-1; 'poolt'=1; NA=0; 
                                                  'puudub'=0; 'erapooletu'=0;
                                                  'ei hääletanud'=0"); x})

#teeme numericuks
data_numeric <- apply(data[, 3:1847], 2, function(x) {x <- as.numeric(x); x})
data[,3:1847]=data_numeric

#Sorteeri andmestiku read fraktsiooni põhjal ja visualiseeri neid heatmapi abil.
#Tõlgenda tulemusi.
data_order=data[order(data$fraktsioon),]

library(pheatmap)
pheatmap(data_order[, 3:1847], cluster_rows=FALSE, cluster_cols=FALSE)
pheatmap(data_order[, 3:1847], cluster_rows=FALSE, cluster_cols=FALSE, 
         gaps_row = c(24, 68, 73,106))

#Visualiseeri andmestikku heatmapi abil, aga seekord klasterda ka read. Tõlgenda 
#tulemusi.
pheatmap(data_order[, 3:1847], cluster_rows=T, cluster_cols=FALSE)

#Visualiseeri andmestikku heatmapi abil, aga seekord klasterda nii read kui 
#veerud. Tõlgenda tulemusi.
pheatmap(data_order[, 3:1847], cluster_rows=T, cluster_cols=T)

#Riigikogu 2011. aasta valimised võitis Reformierakond ning koalitsioonilepe 
#sõlmiti REF ja IRL vahel. See koalitsioon tegi tööd 3 aastat enne kui purunes. 
#26. märtsil 2014 algas Taavi Rõivase valitsus, kus koalitsioonis olid REF ja SDE.
#Jaga sisseloetud andmestik data kaheks alamandmestikuks data1 ja data2. Esimene 
#neist sisaldagu hääletusi kuni 2014-03-26. Andmestik data2 sisaldagu hääletusi 
#alates kuupäevast 2014-03-26.
grepl("2014.03.26",names(data_order))
data1=data_order[, 1:1526]
data2=data_order[, c(1,2, 1527:1847)]


#Tee PCA esialgsele andmestikule. Visualiseeri kahte esimest peakomponenti 
#hajuvusdiagrammi abil. Tõlgenda tulemusi.
pca_algne=prcomp(data[, -(1:2)])

pc1_algne = pca_algne$x[, 1]
pc2_algne = pca_algne$x[, 2]
plot(pc1_algne, pc2_algne, pch=16, col=as.factor(data$fraktsioon))
title("Algne")

legend("center","groups", legend = unique(data2$fraktsioon),
       text.width = 5,cex=0.45, ncol=1,
       fill = 1:5, bty ="n")

#data1
pca_data1=prcomp(data1[, -(1:2)])

pc1_data1 = pca_data1$x[, 1]
pc2_data1 = pca_data1$x[, 2]
plot(pc1_data1, pc2_data1, pch=16, col=as.factor(data1$fraktsioon))

legend("bottom","groups", legend = unique(data2$fraktsioon),
       text.width = 5,cex=0.45, ncol=1,
       fill = 1:5, bty ="n")

#data2
pca_data2=prcomp(data2[, -(1:2)])

pc1_data2 = pca_data2$x[, 1]
pc2_data2 = pca_data2$x[, 2]
plot(pc1_data2, pc2_data2, pch=16, col=as.factor(data2$fraktsioon))

legend("bottom","groups", legend = unique(data2$fraktsioon),
       text.width = 5,cex=0.45, ncol=1,
       fill = 1:5, bty ="n")

#ühele pildile
par(mfrow=c(3, 1), mar=c(1, 1, 1, 1) + 0.1)
plot(pc1_algne, pc2_algne, pch=16, col=as.factor(data$fraktsioon))
title("Kogu periood kokku")
plot(pc1_data1, pc2_data1, pch=16, col=as.factor(data1$fraktsioon))
title("Vana koalitsioon")
plot(pc1_data2, pc2_data2, pch=16, col=as.factor(data2$fraktsioon))
title("Uus koalitsioon")

legend(x = "bottom",inset = 0,
       legend = unique(data2$fraktsioon), 
       fill = 1:5, lwd=1, cex=.7, ncol=1)

# Pärast taasta esialgsed graafilised parameetrid
par(mfrow=c(1, 1), mar=c(5, 4, 4, 2) + 0.1)

#Eelmises ülesandes rakendasid PCA-d andmestikule data1 ja visualiseerisid 
#seda kasutades kahte esimest komponenti. Kas piisab ka ühest komponendist?
#ei
summary(pca_data1)
#Visualiseeri data1 esimest peakomonenti selliselt, et näha oleksid ka saadikute
#nimed. Kas esimest peakomonenti võib tõlgendada kui vasak- ja parempoolsust?
junn=as.data.frame(pc1_data1)
junn$nimi=data_order$nimi
junn$fraktsioon=data_order$fraktsioon

library(ggplot2)
ggplot(junn, aes(x=pc1_data1,  colour=fraktsioon))+
    geom_point(aes(y=nimi),size=3)+
    #coord_flip()+
    facet_wrap(~fraktsioon,scales="free_y")+
    geom_vline(xintercept=0)+
    #facet_grid(.~ fraktsioon,  scales="free_x")+
    theme_minimal()

#XII Riigikogu ajal vahetasid fraktsiooni järgmised saadikud:
#Aivar Riisalu - Eesti Keskerakonna fraktsioon;Fraktsiooni mittekuuluvad 
#Riigikogu liikmed
#Andres Herkel - Isamaa ja Res Publica Liidu fraktsioon;Fraktsiooni mittekuuluvad
#Riigikogu liikmed
#Deniss Boroditš - Fraktsiooni mittekuuluvad Riigikogu liikmed;Eesti 
#Keskerakonna fraktsioon
#Inara Luigas - Fraktsiooni mittekuuluvad Riigikogu liikmed;Eesti Keskerakonna 
#fraktsioon;
#Kalle Laanet - Fraktsiooni mittekuuluvad Riigikogu liikmed;Eesti Keskerakonna 
#fraktsioon;
#Lembit Kaljuvee - Fraktsiooni mittekuuluvad Riigikogu liikmed;Eesti 
#Keskerakonna fraktsioon;
#Olga Sõtnik - Eesti Keskerakonna fraktsioon;Fraktsiooni mittekuuluvad 
#Riigikogu liikmed
#Rainer Vakra - Fraktsiooni mittekuuluvad Riigikogu liikmed;Eesti Keskerakonna 
#fraktsioon
#(Fraktsioonid ei ole ajalises järjestuses.)
#Uuri, kuidas muutusid erakonnavahetajate otsused. Kas suudad andmetest 
#tuvastada, millal nad fraktsiooni vahetasid?
vahet_nimed_kesk=c("Aivar Riisalu", "Deniss Boroditš", 
                   "Inara Luigas", "Kalle Laanet", 
                   "Lembit Kaljuvee", "Olga Sõtnik",
                   "Rainer Vakra")
vahetajad_kesk=subset(data, nimi %in% vahet_nimed_kesk
                      |fraktsioon=="Eesti Keskerakonna fraktsioon")

pheatmap(vahetajad_kesk[, 3:1847], cluster_rows=FALSE, cluster_cols=FALSE)
#märgime ära read, kus on vahetajad ning vaatame, kas leiame nad üles
#st kas muster muutus
gaps=which(vahetajad_kesk$nimi%in%vahet_nimed_kesk)
#järjestame ka
vahetajad_kesk_order=vahetajad_kesk[order(vahetajad_kesk$fraktsioon, 
                                          decreasing = TRUE),]
#siit on näha, kui keski omad läksid välja 09.04.2012, siis 
#hakkavad eristuma ja ühtselt käituma võrreldes teiste keskidega
pheatmap(vahetajad_kesk_order[, 250:350], cluster_rows=F, cluster_cols=FALSE)
#vaatan kas paistab silma ka kogu plotilt
pheatmap(vahetajad_kesk_order[, 3:1847], cluster_rows=F, cluster_cols=FALSE)

#paneme ka Riisalu ja Sõtniku, kes pole märgitud mittekeski liiketetena
gaps=c(7, 20)
pheatmap(vahetajad_kesk_order[, 3:1847], cluster_rows=F, cluster_cols=FALSE, 
         gaps_row=gaps)


