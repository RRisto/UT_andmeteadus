#Laadi ÕISist alla andmestik doping.csv ja loe töökeskkonda. Andmestikus on 
#järgmised tunnused:
#ethnicity - etnilisus: kas african või caucasian
#kit - dopingutesti erinevad variandid (kit1 ja kit2)
#rec - kasvuhormooni isovormi rec kontsentratsioon
#pit - kasvuhormooni isovormi pit kontsentratsioon
#Ülesanne 1 (1 punkt) - andmetega tutvumine
#Lisa andmetabelisse tunnus ratio, mis näitab tunnuste rec ja pit suhet. 
#Visualiseeri tunnuste rec, pit ja ratio jaotusi.

doping=read.csv("./data/doping.csv")
#lisame ratio
doping$ratio=doping$rec/doping$pit
#rec jaouts
library(ggplot2)
ggplot(doping, aes(x=rec))+
    geom_density()
#pit
ggplot(doping, aes(x=pit))+
    geom_density()
#ratio
ggplot(doping, aes(x=ratio))+
    geom_histogram(aes(y= ..density..))+
    stat_function(fun=dnorm)

ggplot(doping, aes(x=ratio))+
    geom_density()+
    stat_function(fun=dnorm)


#log-normaalse jaoutse tihedus
library(ggplot2)
library(dplyr)

i = 1
df_list = list()
for(m in c(0, 0.5, 1, 1.5)){
    for(s in c(0.25, 0.5, 1, 2)){
        # tiheduse graafiku jaoks x ja y koordinaadid
        x = seq(0, 5, 0.01)
        y = dlnorm(x, meanlog = m, sdlog = s)
        
        df_list[[i]] = data.frame(x=x, y=y, 
                                  group = paste("meanlog =", m, ", sdlog =", s))
        i = i + 1
    }
}
df = rbind_all(df_list)
ggplot(df, aes(x, y)) + geom_area() + facet_wrap(~ group) + 
    coord_cartesian(ylim = c(0, 1))

#Joonista ka gammajaotuse tihedusfunktsioonid Γ(α,β) erinevate α∈{1,2,3,4,5} j
#a β∈{1,2,3,4,5} jaoks. Selgita, kuidas muutub jaotus, kui muudame kumbagi p
#arameetrit.

i = 1
df_list_gamma = list()
for(m in c(1, 2, 2, 4, 5)){
    for(s in c(1, 2, 2, 4, 5)){
        # tiheduse graafiku jaoks x ja y koordinaadid
        x = seq(0, 5, 0.01)
        y = dgamma(x, shape = m, rate = s)
        
        df_list_gamma[[i]] = data.frame(x=x, y=y, 
                                  group = paste("shape =", m, ", rate =", s))
        i = i + 1
    }
}
df_gamma = rbind_all(df_list_gamma)
ggplot(df_gamma, aes(x, y)) + geom_area() + facet_wrap(~ group) + 
    coord_cartesian(ylim = c(0, 1))

#Tihti eeldatakse statistikas, et tunnus on normaaljaotusega. Kas isovormide 
#suhe võiks põhimõtteliselt olla normaaljaotusega? Põhjenda
#Ei, vaata joonist ratio jaotusest

#Ülesanne 3 (2 punkti) - andmete filtreerimine
#Mis sa arvad, miks jäeti analüüsist välja andmepunktid, kus rec või pit 
#kontsentratsioon oli väiksem kui 0.05?
#Visualiseeri hajuvusdiagrammi abil, millised andmepunktid jäid analüüsist välja.
valjajaanud=subset(doping, rec<0.05| pit<0.05)

ggplot(valjajaanud, aes(x=rec))+
    geom_density()

ggplot(valjajaanud, aes(x=pit))+
    geom_density()

ggplot(valjajaanud, aes(x=ratio))+
    geom_density()
#andmed, mis jäid alles
doping_subset=subset(doping, rec>0.05& pit>0.05)
ggplot(doping_subset, aes(x=rec))+
    geom_density()

ggplot(doping_subset, aes(x=pit))+
    geom_density()

ggplot(doping_subset, aes(x=ratio))+
    geom_density()

#Log-normaaljaotusel on kaks parameetrit. Kuidas leiad sellised parameetrite 
#väärtused, mille korral jaotus sobiks andmetega kõige paremini? Leia need 
#parameetrid “kit1 - valged” osapopulatsiooni jaoks.
library(MASS)
log_norm_param=fitdistr(x=doping_subset[c(doping_subset$kit==1&doping_subset$ethnicity=="caucasian"), c("ratio")], 
         densfun="log-normal")

meanlog1=log_norm_param$estimate[1]
sdlog1=log_norm_param$estimate[2]

#gamma
gamma_param=fitdistr(x=doping_subset[c(doping_subset$kit==1&doping_subset$ethnicity=="caucasian"), c("ratio")], 
                        densfun="gamma")

shape1=gamma_param$estimate[1]
rate1=gamma_param$estimate[2]

#Leia kummagi jaotuse 99.99% kvantiil.
#log-normal
qlnorm(0.9999, meanlog = meanlog, sdlog = sdlog)
#gamma
qgamma(0.9999, shape=shape, rate = rate)

#Visualiseeri ühel joonisel koos andmetega nii sobitatud log-normaal kui ka 
#gammajaotust. Lisa joonisele 99.99% kvantiil.
kit1_caucasian=doping_subset[c(doping_subset$kit==1&doping_subset$ethnicity=="caucasian"),]

ggplot(kit1_caucasian, aes(x=ratio))+
    geom_histogram(aes(y=..density..), binwidth=0.06, fill="white", colour="black" )+
    stat_function(fun=dlnorm, args = list(meanlog=meanlog1, sdlog=sdlog1))+
    stat_function(fun=dgamma, args=list(shape=shape1, rate=rate1), colour="blue")+
    geom_vline(xintercept=qlnorm(p=0.9999, meanlog = meanlog1, sdlog = sdlog1))+
    geom_vline(xintercept=qgamma(p=0.9999, shape = shape1, rate = rate1), colour="blue")+
    coord_cartesian(xlim=c(0, 1.8))
    
#Kas eelnevalt sobitatud log-normaaljaotus võiks sobida andmetega? Mille alusel 
#otsustad?
#Aga kas gammajaotus võiks sobida andmetega?
#Praktikumis arutasime märksõnu QQplot ja Kolmogorov-Smirnovi test.
qqnorm(kit1_caucasian$ratio)
qqline(kit1_caucasian$ratio)
ks.test(kit1_caucasian$ratio,"pgamma", shape=shape1, rate=rate1)
ks.test(kit1_caucasian$ratio,"dlnorm", meanlog=meanlog1, sdlog=sdlog1)
#log-normal pole, kolmogorov smirnovi test lükkb ümber, gamma aga on

#Sobita nüüd kõigile neljale alamgrupile log-normaaljaotus ja leia selle 99.99% 
#kvantiil.
kit2_caucasian=doping_subset[c(doping_subset$kit==2&doping_subset$ethnicity=="caucasian"),]
kit1_african=doping_subset[c(doping_subset$kit==1&doping_subset$ethnicity=="african"),]
kit2_african=doping_subset[c(doping_subset$kit==2&doping_subset$ethnicity=="african"),]
#valged kit2
log_norm_param2=fitdistr(x=kit2_caucasian$ratio, densfun="log-normal")

meanlog2=log_norm_param2$estimate[1]
sdlog2=log_norm_param2$estimate[2]

qlnorm(0.9999, meanlog = meanlog2, sdlog = sdlog2)
#musatd kit1
log_norm_param3=fitdistr(x=kit1_african$ratio, densfun="log-normal")

meanlog3=log_norm_param3$estimate[1]
sdlog3=log_norm_param3$estimate[2]

qlnorm(0.9999, meanlog = meanlog3, sdlog = sdlog3)
#mustad kit2
log_norm_param4=fitdistr(x=kit2_african$ratio, densfun="log-normal")

meanlog4=log_norm_param4$estimate[1]
sdlog4=log_norm_param4$estimate[2]

qlnorm(0.9999, meanlog = meanlog4, sdlog = sdlog4)
#kittide lõikes, kit1
kit1=doping_subset[c(doping_subset$kit==1),]
log_norm_param5=fitdistr(x=kit1$ratio, densfun="log-normal")

meanlog5=log_norm_param5$estimate[1]
sdlog5=log_norm_param5$estimate[2]

qlnorm(0.9999, meanlog = meanlog5, sdlog = sdlog5)
#kit2
kit2=doping_subset[c(doping_subset$kit==2),]
log_norm_param6=fitdistr(x=kit2$ratio, densfun="log-normal")

meanlog6=log_norm_param6$estimate[1]
sdlog6=log_norm_param6$estimate[2]

qlnorm(0.9999, meanlog = meanlog6, sdlog = sdlog6)

#Ametlik piirmäär kit1 korral oli 1.81 ja kit2 korral 1.68. Kas said sarnased 
#tulemused?
#Kit1 enam vähem sama (1.88), kit2 1.92



#Esmased otsustuspiirid on määratud. Nüüd tuleb neid verifitseerida. Kuidas 
#tegi seda WADA?
#Rutiinsete dopingukontrollide käigus koguti aastatel 2009-2011 kit1 kohta 3547 
#mõõtmist ja kit2 kohta 617 mõõtmist.Nendes andmetes puudub tunnus ethnicity.
#Nüüd filtreeriti välja andmepunktid, kus rec kontsentratsioon oli väiksem 
#kui 0.1 ja pit kontsentratsioon oli väiksem kui 0.05.Lognormaaljaotus ei 
#sobinud. Kasutati gammajaotust. Visati välja 10 imelikku (liiga kõrget) 
#andmepunkti.Selle andmestiku põhjal arvutatud kvantiilid tulid väiksemad kui 
#esmase uuringu kvantiilid. Järeldati, et esmased piirmäärad on verifitseeritud.
#Laadi ÕISist alla andmestik doping_verification.csv.

verification=read.csv("./data/doping_verification.csv.")

#Boonusülesanne (kuni 5 punkti)
#Uuri ise midagi põnevat. Näiteks testi, kas tõesti enam log-normaaljaotus ei 
#sobi, või uuri, milline mõju oli imelike andmepunktide väljaviskamisel.
#arvutame ratio
verification$ratio=verification$rec/verification$pit
#pean ka miinused ja nullid välja võtma, muidu ei tööta!!!!
param_verif1=fitdistr(x=(verification$ratio[!is.infinite(verification$ratio)&!is.na(verification$ratio)&verification$ratio>0]), densfun="log-normal")

meanlog_verif1=param_verif1$estimate[1]
sdlog_verif1=param_verif1$estimate[2]

#viskame välja sodi
verif_subset=subset(verification, rec>=0.01 & pit>=0.05)
param_verif2=fitdistr(x=verif_subset$ratio, densfun="log-normal")

meanlog_verif2=param_verif2$estimate[1]
sdlog_verif2=param_verif2$estimate[2]

ks.test(verif_subset$ratio,"dlnorm", meanlog=meanlog_verif2, sdlog=sdlog_verif2)

#mis juhtus, kui viskasime välja osad vaatused
    ggplot(verification, aes(ratio))+
        geom_histogram(aes(y=..density..), binwidth=0.01, fill="blue", colour="blue", alpha=0.1 )+
        #geom_density()+
        geom_vline(xintercept=qlnorm(p=0.9999, meanlog = meanlog_verif1, sdlog = sdlog_verif1), colour="blue")+    
        geom_vline(xintercept=qlnorm(p=0.9999, meanlog = meanlog_verif2, sdlog = sdlog_verif2))+
        stat_function(fun=dlnorm, args = list(meanlog=meanlog_verif1, sdlog=sdlog_verif1), colour="blue")+
        stat_function(fun=dlnorm, args = list(meanlog=meanlog_verif2, sdlog=sdlog_verif2))+
        coord_cartesian(xlim=c(0, 3))+
        geom_histogram(data=verif_subset,aes(y=..density..), binwidth=0.01, fill="grey", colour="grey", alpha=0.1)
    
#Leia bootstrap 95% usaldusintervall dopingutesti piirmäärale (ehk 99.99% 
#kvantiilile). Visualiseeri saadud tulemust.
piir=numeric()

for (i in 1:10000) {
    param=fitdistr(x=sample(verif_subset$ratio, replace=T), densfun="log-normal")
    meanlog=param$estimate[1]
    sdlog=param$estimate[2]
    piir[i]=qlnorm(0.9999, meanlog = meanlog, sdlog = sdlog) 
}

hist(piir)
#piirid 95% jagunemise järgi
quantile(piir, c(.05, .5, .95))
#teine variant
se=sd(piir)/sqrt(length(piir))
keskmine=mean(piir)
keskmine+c(-1,1)*1.65*se

#Boonusülesanne (3 punkti)
#Kasvuhormooni dopingutesti üks eeldustest oli, et isovormide suhe on konstantne.
#
library(ggplot2)
#rec vs ratio
ggplot(verification, aes(x=rec, y=ratio))+
    geom_point()

#pit vs ratio
ggplot(verification, aes(x=pit, y=ratio))+
    geom_point()

#mõlemad liidame
ggplot(verification, aes(x=pit+rec, y=ratio))+
    geom_point()

#korrelatsioon
verification_puhas=verification[!is.infinite(verification$ratio)&!is.na(verification$ratio),]
cor(verification_puhas$pit, verification_puhas$ratio)

#proovin sama algsete andmete pealt
ggplot(doping, aes(x=rec, y=ratio))+
    geom_point()

#pit vs ratio
ggplot(doping, aes(x=pit, y=ratio))+
    geom_point()

#mõlemad liidame
ggplot(doping, aes(x=pit+rec, y=ratio))+
    geom_point()
#korrelatsioon
doping_puhas=doping[!is.infinite(doping$ratio)&!is.na(doping$ratio),]
cor(doping_puhas$pit, doping_puhas$ratio)


