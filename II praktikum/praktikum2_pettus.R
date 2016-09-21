maksud=read.csv2("./data/tolliamet_maksud.csv")
maksud$riiklikud_maksud=as.numeric(as.character(maksud$riiklikud_maksud))
maksud$toojoumaksud_ja_maksed=as.numeric(as.character(
    maksud$toojoumaksud_ja_maksed))
#Tee 3 joonist, mis iseloomustavad hästi seda andmestikku.
#sitt graafik
library(ggplot2)
# ggplot(maksud, aes(x=riiklikud_maksud, y=toojoumaksud_ja_maksed))+
#     geom_point(alpha)
#sitt
# ggplot(maksud, aes(x=liik, y=riiklikud_maksud))+
#     geom_boxplot()

# ggplot(maksud, aes(x=maakond, y=riiklikud_maksud/100000))+
#         geom_boxplot()
# 
# ggplot(maksud, aes(x=kaibemaksukohustlane, y=riiklikud_maksud))+
#     geom_boxplot()
# ggplot(maksud, aes(x=riiklikud_maksud))+
#     geom_histogram(binwidth=100000)
# 
# ggplot(maksud, aes(x=toojoumaksud_ja_maksed))+
#     geom_histogram(binwidth=1000000)

#maakonna keskmised
library(dplyr)
library(reshape2)
maksud_maakond_group=maksud %>%
    group_by(maakond)%>%
summarise(kesk_riiklikud_maksud= mean(riiklikud_maksud, na.rm=T),
          mediaan_riiklikud_maksud=median(riiklikud_maksud, na.rm=T),
          kesk_tööjõumaksud= mean(toojoumaksud_ja_maksed, na.rm=T),
          mediaan_tööjõumaksud=median(toojoumaksud_ja_maksed, na.rm=T))%>%
    melt(id.vars="maakond")

ggplot(maksud_maakond_group, aes(x=maakond, y=value))+
    geom_point()+
    facet_wrap(~variable)+
    theme(axis.text.x  = element_text(angle=90, vjust=0.5))

#jur vormi keskmised
maksud_liik_group=maksud %>%
    group_by(liik)%>%
    summarise(kesk_riiklikud_maksud= mean(riiklikud_maksud, na.rm=T),
              mediaan_riiklikud_maksud=median(riiklikud_maksud, na.rm=T),
              kesk_tööjõumaksud= mean(toojoumaksud_ja_maksed, na.rm=T),
              mediaan_tööjõumaksud=median(toojoumaksud_ja_maksed, na.rm=T))%>%
    melt(id.vars="liik")

ggplot(maksud_liik_group, aes(x=liik, y=value/1000))+
    geom_point()+
    facet_wrap(~variable)+
    theme_minimal()+
    theme(axis.text.x  = element_text(angle=90, vjust=0.5))
    
#plotime esimesed 10 suurimat maksumaksjat
maksud_sort=maksud%>%
    mutate(summa=riiklikud_maksud+toojoumaksud_ja_maksed)%>%
    arrange(-summa)
#see on selleks, et ggplot kuvaks maksude järjekorras, mitte tähestikulises 
#järjekorras
maksud_sort$nimi <- as.character(maksud_sort$nimi)
#Then turn it back into an ordered factor
maksud_sort$nimi <- factor(maksud_sort$nimi, levels=unique(maksud_sort$nimi))

ggplot(subset(maksud_sort, liik!="Valitsus- ja riigiasutus")[1:20,], 
       aes(x=nimi, y=summa/100000))+
    geom_bar(stat="identity")+
    scale_y_reverse()+
    theme(axis.text.x  = element_text(angle=90, vjust=0.5))+
    coord_flip()
    
koik_maksud=sum(maksud_sort$summa)
top_enim=sum(maksud_sort$summa[1:100])
top_enim/koik_maksud

#Kontrolli visuaalselt Benfordi seaduse kehtimist tunnustel riiklikud_maksud ja 
#toojoumaksud_ja_maksed. Selleks tekita esinumbrite histogramm. Nulliga võrduvad 
#väärtused jäta kõrvale. Tee vastav joonis ka FIE-de, äriühingute jne lõikes 
#(vt tunnus liik).

#lõikame esimese numbri maha
maksud_sort$esimene_nr_riikl=substr(maksud_sort$riiklikud_maksud, 1, 1)
maksud_sort$esimene_nr_tooj=substr(maksud_sort$toojoumaksud_ja_maksed, 1, 1)

#riiklike maksude osas
library(ggplot2)
kokku_count=nrow(subset(maksud_sort,esimene_nr_riikl!=0))
maksud_sort2=subset(maksud_sort,esimene_nr_riikl!=0)
library(dplyr)
maksud_sort2=maksud_sort2 %>%
    group_by(esimene_nr_riikl) %>%
    summarise(osakaal=n()/kokku_count)

library(BenfordTests)
#Benfordi seadusele vastav jaotus
benford=as.data.frame(pbenf(digits = 1))

ggplot(maksud_sort2, aes(x=factor(esimene_nr_riikl), y=osakaal, group=1))+
    geom_point(colour="red")+
    geom_line()+
    geom_line(data=benford, aes(x=Var1, y=Freq,group=1), colour="red")

#sama ka tööjõumaksude kohta
kokku_count_too=nrow(subset(maksud_sort,esimene_nr_tooj!=0))
maksud_sort3=subset(maksud_sort,esimene_nr_tooj!=0)
library(dplyr)
maksud_sort3=maksud_sort3 %>%
    group_by(esimene_nr_tooj) %>%
    summarise(osakaal=n()/kokku_count_too)

ggplot(maksud_sort3, aes(x=factor(esimene_nr_tooj), y=osakaal, group=1))+
    geom_point(colour="red")+
    geom_line()+
    geom_line(data=benford, aes(x=Var1, y=Freq,group=1), colour="red")

#liikide lõikes tööjõumaksud
kokku_count_too=nrow(subset(maksud_sort,esimene_nr_tooj!=0))
maksud_sort4=subset(maksud_sort,esimene_nr_tooj!=0)
#arvutame iga liigi counti ja siis iga liigi iga algusnumbri count ja 
#osakaalu
library(dplyr)
maksud_sort4=maksud_sort4 %>%
    group_by(liik) %>%
    mutate(count=n())%>%
    group_by(liik,esimene_nr_tooj) %>%
    mutate(count_grupp=n(), 
           osakaal_grupp=count_grupp/count)

library(BenfordTests)
#Benfordi seadusele vastav jaotus
benford=as.data.frame(pbenf(digits = 1))

ggplot(subset(maksud_sort4, liik!="Eestis asuv rahvusvaheline organisatsioon"), 
       aes(x=factor(esimene_nr_tooj), y=osakaal_grupp, group=1))+
    geom_point(colour="red")+
    geom_line()+
    xlab("Algusnumber")+
    ylab("Esinemissagedus")+
    geom_line(data=benford, aes(x=Var1, y=Freq,group=1), colour="red")+
    facet_wrap(~liik)

##liikide riiklikud maksud
kokku_count_riikl=nrow(subset(maksud_sort,esimene_nr_riikl!=0))
maksud_sort5=subset(maksud_sort,esimene_nr_riikl!=0)
#arvutame iga liigi counti ja siis iga liigi iga algusnumbri count ja 
#osakaalu
library(dplyr)
maksud_sort5=maksud_sort5 %>%
    group_by(liik) %>%
    mutate(count=n())%>%
    group_by(liik,esimene_nr_riikl) %>%
    mutate(count_grupp=n(), 
           osakaal_grupp=count_grupp/count)

ggplot(subset(maksud_sort5, liik!="Eestis asuv rahvusvaheline organisatsioon"), 
              aes(x=factor(esimene_nr_riikl), y=osakaal_grupp, group=1))+
    geom_point(colour="red")+
    geom_line()+
    geom_line(data=benford, aes(x=Var1, y=Freq,group=1), colour="red")+
    facet_wrap(~liik)

