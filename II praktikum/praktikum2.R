andmed=read.csv("./data/skp_ja_volg.csv")
str(andmed)
#jätame koopia algsest andmestikust
andmed_alg=andmed

#Lisa uus tunnus, mis näitab vola_skp_suhe kategooriat 
#(< 30%, 30-60%, 60-90%, > 90%). Näpunäide: kasuks tuleb käsk cut.
miinimum=min(andmed$vola_skp_suhe, na.rm=T)
maksimum=max(andmed$vola_skp_suhe, na.rm=T)
andmed$vola_skp_suhe_klass=cut(andmed$vola_skp_suhe, 
                               breaks=c(miinimum,30,60,90, maksimum), include.lowest=T,
                               labels=c("< 30%", "30-60%", "60-90%", "> 90%"))

#kaalumisviisid
#HAP kaalub igas kategoorias andmepunkte võrdselt (ehk võtab tavalise 
#aritmeetilise keskmise), RR arvutab igas grupis riikide keskmise ja võtab 
#neist aritmeetilise keskmise.
#HAPi viis
library(dplyr)
andmed_grupp= group_by(andmed, vola_skp_suhe_klass)

HAP  <- summarise(andmed_grupp, HAP_mean = round(mean(skp_kasv, na.rm=T),1),
                  HAP_median=round(median(skp_kasv, na.rm=T),1))

#lühem versioon HAP
HAP_tulem=andmed  %>% 
    group_by(vola_skp_suhe_klass)%>%
    summarise(HAP_mean = round(mean(skp_kasv, na.rm=T),1),
              HAP_median=round(median(skp_kasv, na.rm=T),1))

#RR-i tulemuste reprodutseerimiseks jäta arvutustest välja andmepunktid, 
#mis jäid välja Exceli vea tõttu (vt tunnus exceli_viga) ja andmete puudumise 
#tõttu (vt tunnus valikuline).
RR=subset(andmed_grupp,  exceli_viga==0)
RR=subset(RR,  valikuline==0)
RR= group_by(RR, vola_skp_suhe_klass,riik)
#riigi keskmine grupis
RR_vahe  <- summarise(RR, SKP_keskmine_kasv= round(mean(skp_kasv, na.rm=T),1),
                      SKP_mediaan_kasv=round(median(skp_kasv, na.rm=T),1))
#eelneva põhjal keskmine grupis
RR_klassid= group_by(RR_vahe, vola_skp_suhe_klass)

# RR_tulem=summarise(RR_klassid, RR_mean=round(mean(SKP_keskmine_kasv, na.rm=T),1),
#                    RR_median=round(median(SKP_keskmine_kasv, na.rm=T),1))

RR_tulem=summarise(RR_klassid, RR_mean=round(mean(ifelse(riik=="New Zealand" 
                        & SKP_keskmine_kasv==-7.6,-7.9,SKP_keskmine_kasv),  na.rm=T),1),
                   RR_median=round(median(SKP_mediaan_kasv),1))

#lühem versioon RR
RR_tulem=andmed_grupp%>% 
    subset( exceli_viga==0)%>% 
    subset(valikuline==0)%>% 
    group_by(vola_skp_suhe_klass,riik)%>% 
    summarise(SKP_keskmine_kasv= round(mean(skp_kasv, na.rm=T),1),
          SKP_mediaan_kasv=round(median(skp_kasv, na.rm=T),1))%>% 
    group_by(vola_skp_suhe_klass)%>% 
    summarise(RR_mean=round(mean(ifelse(
        riik=="New Zealand" & SKP_keskmine_kasv==-7.6,-7.9,SKP_keskmine_kasv),na.rm=T),1),
        RR_median=round(median(SKP_mediaan_kasv),1))

#tulemid kokku
tulem=cbind(RR_tulem[1:4,], HAP_tulem[1:4,])

#Visualiseeri, kuidas võlakoorem on aastate jooksul muutunud riikide lõikes
library(ggplot2)
volakoorem_plot=ggplot(andmed, aes(x=aasta, y=vola_skp_suhe))+
    geom_point()+
    facet_wrap(~riik)+
    theme_minimal()

volakoorem_plot

#Ülesanne 3 (1 punkt) - millised vaatlused jäid RR analüüsist välja
pole=subset(andmed, exceli_viga==1| valikuline==1)

#Boonusülesanne (2 punkti) - kuidas erinesid RR ja HAP analüüside tulemused
#HAPi raportis kasutati vasakpoolset joonist, et visualiseerida RR ja HAP tulemuste 
#erinevusi. NY times pani samale joonisele aga mediaani ja keskmise (parempoolne joonis)! 
#Paku välja parem visualiseerimise idee, kuidas muuta arusaadavaks tulemuste erinevus.
library(reshape2)
tulem_melt=melt(tulem, id=c("vola_skp_suhe_klass"))
tulem_melt_median=tulem_melt[grepl("median", tulem_melt$variable),]
tulem_melt_mean=tulem_melt[grepl("mean", tulem_melt$variable),]

ggplot(tulem_melt_median, aes(x=vola_skp_suhe_klass, y=value, fill=variable)) + 
    geom_bar(position="dodge", stat="identity")

ggplot(tulem_melt_mean, aes(x=vola_skp_suhe_klass, y=value, fill=variable)) + 
    geom_bar(position="dodge", stat="identity")

#Ülesanne 4 (2 punkti) - kas võlakoormus suurem kui 90% on maagilise tähtsusega
#Selgitage välja, kas täpselt 90% on just see piir, millest suurem võlakoormus on 
#seotud madalama SKP kasvuga, või on see suhteliselt suvaliselt valitud arv?
#Üks võimalik lahendusviis: Tekitage uus kategooria, kus võlg jaotatakse 5 gruppi: 
#< 30%, 30-60%, 60-90%, 90-120%, > 120%). Arvutage iga grupi kohta mediaanid ja 
#keskmised kasutades RR kaalumisviisi.
andmed2=andmed_alg
andmed2$vola_skp_suhe_klass=cut(andmed2$vola_skp_suhe, 
                               breaks=c(miinimum,30,60,90,120, maksimum), include.lowest=T,
                               labels=c("< 30%", "30-60%", "60-90%", "90-120%", "> 120%"))

andmed_grupp2= group_by(andmed2, vola_skp_suhe_klass)

RR_tulem2=andmed_grupp2%>% 
    subset( exceli_viga==0)%>% 
    subset(valikuline==0)%>% 
    group_by(vola_skp_suhe_klass,riik)%>% 
    summarise(SKP_keskmine_kasv= round(mean(skp_kasv, na.rm=T),1),
              SKP_mediaan_kasv=round(median(skp_kasv, na.rm=T),1))%>% 
    group_by(vola_skp_suhe_klass)%>% 
    summarise(RR_mean=round(mean(ifelse(
        riik=="New Zealand" & SKP_keskmine_kasv==-7.6,-7.9,SKP_keskmine_kasv),na.rm=T),1),
        RR_median=round(median(SKP_mediaan_kasv),1))

#Ülesanne 5 (4 punkti) - Kuidas visuaalselt uurida, milline on seos SKP ja 
#võlakoormuse vahel? Kõigepealt, tehke joonis, kus oleks näha seos SKP ja 
#võlakoormuse vahel. Seose iseloomustamiseks võite kasutada stat_smooth() abil 
#leitavat joont. Näinud seost andmestikus, tekib küsimus, ega see seos ei ole 
#lihtsalt juhuslik. 
library(ggplot2)

ggplot(andmed, aes(x=vola_skp_suhe, y=skp_kasv))+
    geom_point()+
    stat_smooth()

#Ehk kas vaadeldud seos erineb oluliselt seostest sellistes 
#andmestikes, kus tegelikult SKP ja võlakoormuse vahel mingisugust seost ei eksisteeri. 
#Selle visuaalseks kontrollimiseks võime kasutada järgmist skeemi. 
#Selle visuaalseks kontrollimiseks võime kasutada järgmist skeemi. See põhineb 
#permutatsioonitestil, mille kohta vaadake esmalt kodutöö osa III videot 
#https://www.youtube.com/watch?v=5Dnw46eC-0o  Skeem: Nullhüpotees on, et SKP ja 
#riigivõla vahel seos puudub. Genereerime meie andmetest permuteerimise teel 
#sellise andmestiku, mis vastab nullhüpoteesile. Näiteks võib fikseerida SKP 
#väärtused ning neile vastavusse seatavad riigivõla väärtused permuteerida. 
#(Näpunäide: permuteerimisel on kasuks funktsioon sample.)

#jätame alles read, kus pole NAd SKP kasvus ja võla SKP suhtes
andmed_permut=andmed[!is.na(andmed$vola_skp_suhe)&!is.na(andmed$skp_kasv),]

#teeme sellega ka graafiku
plot_permut=ggplot(andmed_permut, aes(x=vola_skp_suhe, y=skp_kasv))+
    geom_point()+
    stat_smooth()

#permuteerime vola_skp_suhe väärtuseid
andmed_permut$iter1=sample(andmed_permut$vola_skp_suhe, replace=T)
plot_permut+stat_smooth(aes(x=iter1, y=skp_kasv), colour="grey")

plot_permut=ggplot(andmed_permut, aes(x=vola_skp_suhe, y=skp_kasv))+
    geom_point()+
    stat_smooth(aes(x=iter1, y=skp_kasv), colour="grey")

#jätame ainult vajaliku alles
andmed_permut=andmed_permut[, c("vola_skp_suhe","skp_kasv","iter1" )]
andmed_permut_iter=andmed_permut

#graafik itereerimiseks
plot_permut_iter=ggplot(andmed_permut_iter, aes(x=vola_skp_suhe, y=skp_kasv))+
    geom_point()+
    stat_smooth()

plot_permut_iter=plot_permut_iter+stat_smooth(aes(x=iter1, y=skp_kasv), colour="grey")

#nüüd tuleb seda 100 korda teha
#library(ggplot2) lisaks tuleb esimene graafik valmis teha
j=4
for (i in 1:100) {
    
    andmed_permut_iter[,j]=sample(andmed_permut_iter$vola_skp_suhe, replace=T)
    plot_permut_iter=plot_permut_iter+stat_smooth(aes(x=andmed_permut_iter[,j], 
                                                      y=skp_kasv), colour="grey")
    j+1
}
    







#jätame alles read, kus pole NAd SKP kasvus ja võla SKP suhtes
andmed_permut=andmed[!is.na(andmed$vola_skp_suhe)&!is.na(andmed$skp_kasv),]
andmed_permut$iter1=sample(andmed_permut$vola_skp_suhe, replace=T)
#jätame vajaliku alles
andmed_permut=andmed_permut[, c("vola_skp_suhe","skp_kasv","iter1" )]
andmed_permut_iter=andmed_permut
#graafik itereerimiseks
plot_permut_iter=ggplot(andmed_permut_iter, aes(x=vola_skp_suhe, y=skp_kasv))+
    geom_point()+
    stat_smooth()
plot_permut_iter=plot_permut_iter+stat_smooth(aes(x=iter1, y=skp_kasv), 
                                              colour="grey")

#nüüd tuleb seda 100 korda teha
#library(ggplot2) lisaks tuleb esimene graafik valmis teha
j=4
for (i in 1:100) {
    
    andmed_permut_iter[,j]=sample(andmed_permut_iter$vola_skp_suhe, replace=T)
    plot_permut_iter=plot_permut_iter+stat_smooth(aes(x=andmed_permut_iter[,j], 
                                                      y=skp_kasv), colour="grey")
    j+1
}
