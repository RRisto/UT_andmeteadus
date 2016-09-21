biomarkerid=read.csv("./data/biomarkerid.csv")

biomarkerid=biomarkerid[complete.cases(biomarkerid),]
#Eralda andmestikust alamandmestik, mis sisaldaks tunnuseid sugu, vanusgrupp, s5, 
#hyp, suits ning järgmisi biomarkereid:
#Serum_C - üldkolesterool
#HDL_C - HDL kolesterool (“hea”)
#LDL_C - LDL kolesterool (“halb”)

biom_subset=biomarkerid[, c("sugu", "vanusegrupp", "s5", "hyp", "suits", 
                            "Serum_C", "HDL_C", "LDL_C")]

#Ülesanne 1 (2 punkti) - kolesterool soo ja vanusegruppide lõikes
#Tutvu andmestikuga ja selgita välja, kuidas on kodeeritud tunnus sugu (kas 
#0 tähistab meest või naist)? Visualiseeri, kuidas nende 3 biomarkeri jaotused 
#erinevad soo ja vanusegruppide lõikes.
#Serum_C
library(ggplot2)
ggplot(biom_subset, aes(x=factor(sugu), y=Serum_C))+
    geom_boxplot()
#HDL_C
ggplot(biom_subset, aes(x=factor(sugu), y=HDL_C))+
    geom_boxplot()
#LDL_C
ggplot(biom_subset, aes(x=factor(sugu), y=LDL_C))+
    geom_boxplot()

#vanusegrappide lõikes
ggplot(biom_subset, aes(x=factor(sugu), y=Serum_C))+
    geom_boxplot()+
    facet_wrap(~vanusegrupp)

#HDL_C
ggplot(biom_subset, aes(x=factor(sugu), y=HDL_C))+
    geom_boxplot()+
    facet_wrap(~vanusegrupp)
#LDL_C
ggplot(biom_subset, aes(x=factor(sugu), y=LDL_C))+
    geom_boxplot()+
    facet_wrap(~vanusegrupp)

#Ülesanne 2 (4 punkti) - surma prognoosimine kolesterooli abil?
#Uuri, kas kolesterool võimaldab prognoosida surma. Selleks tuleb andmetele 
#sobitada mudel. Visualiseeri, kas kolesterooli (Serum_C, HDL_C, LDL_C) abil 
#võiks saada prognoosida surma.
ggplot(biom_subset, aes(x=factor(s5), y=Serum_C))+
    geom_boxplot()
#HDL_C
ggplot(biom_subset, aes(x=factor(s5), y=HDL_C))+
    geom_boxplot()
#LDL_C
ggplot(biom_subset, aes(x=factor(s5), y=LDL_C))+
    geom_boxplot()

#Tundub, et joonisest ei piisa ning tuleb pöörduda statistiliste mudelite juurde.
#Kas kasutad lineaarset või logistilist regressiooni? Miks?
#Sobitasime mudeli glm(s5 ~ HDL_C, family=binomial, data=data) ning selgus, 
#et HDL_C on oluline surma prognoosimisel. Seejärel aga sobitasime mudeli 
#glm(s5 ~ HDL_C + sugu, family=binomial, data=data), siis miskipärast HDL_C 
#enam ei ole oluline. Selgita, mis värk on. Kas siis kokkuvõttes on oluline või 
#mitte?
mudel1=glm(s5 ~ HDL_C, family=binomial, data=biom_subset)
summary(mudel1)

mudel2=glm(s5 ~ HDL_C + sugu, family=binomial, data=biom_subset)
summary(mudel2)

#Sobita kolm mudelit, et uurida kolesterooli (tunnuste Serum_C, HDL_C ja LDL_C) 
#seost surmaga. Muide, kas lisad mudelitesse ka tunnused sugu, vanusegrupp, 
#suits ja hyp? Põhjenda oma otsust.
mudel3=glm(s5 ~ Serum_C+ HDL_C + LDL_C, family=binomial, data=biom_subset)
summary(mudel3)

#Ülesanne 3 (1 punkt) - korrelatsioonid biomarkerite vahel
#Tee joonis, mis annaks hästi edasi, kas ja millised biomarkerid on omavahel 
#korreleeritud. (Näpunäide: Arvuta korrelatsioonimaatriks käsuga cor ning 
#visualiseeri seda.) Interpreteeri, milliseid mustreid ja seoseid näed?
korrel_maatriks=cor(biomarkerid[6:111])
library(corrplot)
corrplot(korrel_maatriks, method = "number")

library(pheatmap)
pheatmap(korrel_maatriks, cluster_rows=FALSE, cluster_cols=FALSE)

#Ülesanne 4 (1 punkt) - Oluliste biomarkeri tuvastamine
#Milline biomarker aitab kõige paremini ennustada surma kui võtame arvesse 
#vanuse ja soo mõju?
#Selleks sobita mudelid
#s5 ~ sugu + vanusegrupp + biomarker_1
#s5 ~ sugu + vanusegrupp + biomarker_2
#…
#s5 ~ sugu + vanusegrupp + biomarker_106
#ja iga biomarkeri korral eralda mudelist selle p-väärtus ja kordaja.
#Kui sa ei soovi 106 korda glm mudelit käsitsi jooksutada ja manuaalselt 
#p-väärtuseid välja noppida, siis automatiseeri see (näiteks for tsükli abil).
#Näide, kuidas ühe biomarkeri korral saada kätte p-väärtus ja kordaja:
biomarker = "LDL_D"
formula0 = "s5 ~ sugu + vanusegrupp"
formula = paste(formula0, biomarker, sep=" + ")
model = glm(formula, family=binomial, data=biomarkerid)
summary_table = coef(summary(model))
pvalue = summary_table[nrow(summary_table), 4]
estimate = summary_table[nrow(summary_table), 1]

print(paste("Biomarker ", biomarker, 
            ", kordaja: " , estimate,
            ", pväärtus: ", pvalue,
            sep=""))


#teeme asja for tsükliga iga markeri kohta
biomarkers = names(biomarkerid)[6:111]
formula0 = "s5 ~ sugu + vanusegrupp"

for (i in 1:length(biomarkers)) {
    formula = paste(formula0, biomarkers[i], sep=" + ")
    model = glm(formula, family=binomial, data=biomarkerid)
    summary_table = coef(summary(model))
    pvalue[i] = summary_table[nrow(summary_table), 4]
    estimate[i] = summary_table[nrow(summary_table), 1] 
}
#vähem arve pärast komakohti
pvalue=round(pvalue, 4)
estimate=round(estimate, 4)
tulemid=data.frame(biomarkers, pvalue, estimate)

#Ülesanne 5 (1 punkt)
#Kirjuta eelnev kood funktsiooniks. 

estimate_significance = function(formula0, biomarkers, data){
    coefs=list()
    pvalues=list()
    for (i in 1:length(biomarkers)) {
        formula = paste(formula0, biomarkers[i], sep=" + ")
        model = glm(formula, family=binomial, data=data)
        summary_table = coef(summary(model))
        pvalues[i] = summary_table[nrow(summary_table), 4]
        coefs[i] = summary_table[nrow(summary_table), 1] 
    }
    #vähem arve pärast komakohti
    pvalues=round(as.numeric(pvalues), 10)
    coefs=round(as.numeric(coefs), 6)
    tulemid=data.frame(biomarkers, pvalues, coefs)
    
    return(tulemid)
}
#testin
estimate_significance(formula0="s5 ~ sugu + vanusegrupp", 
                      biomarkers=c("LDL_D", "HDL_C"), data=biomarkerid)

#Kasuta nüüd eelnevalt kirjutatud funktsiooni kõigi 106 biomarkeri jaoks. 
#Prindi välja iga biomarkeri p-väärtus ning kordaja hinnang.
hinnangud=estimate_significance(formula0="s5 ~ sugu + vanusegrupp", 
                      biomarkers=names(biomarkerid)[6:111], data=biomarkerid)

#Visualiseeri saadud tulemust.
library(ggplot2)
#leaime õige järjekorra
jrk=with(hinnangud, reorder(biomarkers, pvalues, mean))
#plotime
ggplot(hinnangud, aes(x=jrk, y=log(pvalues)))+
    geom_point()+
    geom_hline(yintercept=log(0.05), colour="red")+
    theme(axis.text.x=element_text(angle=90))
#kui aga kasutada bonferroni korrektsiooni
ggplot(hinnangud, aes(x=jrk, y=log(pvalues)))+
    geom_point()+
    geom_hline(yintercept=log(0.0005), colour="red")+
    theme(axis.text.x=element_text(angle=90))


#(2 punkti) Veendumaks, et p < 0.05 kasutamisel võime tõepoolest saada liiga 
#palju valepositiivseid tulemusi, tekita andmestik, kus puudub seos tunnuse s5 
#ja biomarkerite vahel. Selleks tekita uus tunnus, kus oleks s5 väärtuseid 
#permuteeritud suvaliselt. Sobita nüüd mudelid, kus prognoosiksid permuteeritud 
#s5 väärtuseid biomarkerite põhjal (selleks võid kasutada ülesandes 5 kirjutatud
#funktsiooni).
#permuteerime s5-e
set.seed(100)
biomarkerid$s5_permut=sample(biomarkerid$s5, replace=T)
#names(biomarkerid)

hinnangud_permut=estimate_significance(formula0="s5_permut ~ sugu + vanusegrupp", 
                                biomarkers=names(biomarkerid)[6:111], 
                                data=biomarkerid)
#leiame faktorite õige järjekorra
jrk_permut=with(hinnangud_permut, reorder(biomarkers, pvalues, mean))
#plotime
ggplot(hinnangud_permut, aes(x=jrk_permut, y=log(pvalues)))+
    geom_point()+
    geom_hline(yintercept=log(0.05), colour="red")+
    theme(axis.text.x=element_text(angle=90))

#Mitme biomarkeri p-väärtused tulid väiksemad kui 0.05?
nrow(subset(hinnangud_permut,pvalues<0.05)) #53 kui seed 200, 100-ga 27
#Aga mitu tükki olid olulised Bonferroni korrektsiooni järgi?
ggplot(hinnangud_permut, aes(x=jrk_permut, y=log(pvalues)))+
    geom_point()+
    geom_hline(yintercept=log(0.0005), colour="red")+
    theme(axis.text.x=element_text(angle=90))

nrow(subset(hinnangud_permut,pvalues<0.0005)) #0

#Mitut olulist p-väärtust oleksid oodanud kummalgi juhul? Selgita.
#esimesel juhul 106*0,05=5.3
#teisel juhul 106*0.0005=0.053

#(1 boonuspunkt) Korda permuteerimist 100 korral ning tee kokkuvõte tulemustest.
tavaline=list()
bonf=list()
#aeglane, 20min teeb!
for (i in 1:100) {
    
    biomarkerid$s5_permut=sample(biomarkerid$s5, replace=T)
    pvaartused=estimate_significance(formula0="s5_permut ~ sugu + vanusegrupp", 
                                           biomarkers=names(biomarkerid)[6:111], 
                                           data=biomarkerid)[,2]
    tavaline[i]=summary(pvaartused<0.05)[3] 
    bonf[i]=summary(pvaartused<0.0005)[3] 
}

hist(as.numeric(tavaline),  freq=FALSE, breaks=40)
hist(as.numeric(bonf), freq=FALSE, breaks=40)

simul=data.frame(as.numeric(tavaline),as.numeric(bonf))
summary(simul)

#Ülesanne 8 (1 punkt) - alternatiiv Bonferroni korrektsioonile
#Ülesandes 3 nägime, et mitmed biomarkerid on omavahel tugevalt korreleeritud. 
#Niisiis võib Bonferroni korrektsioon osutuda praegu liiga rangeks. 
#Alternatiivselt võiksime leida, kui suur on meie andmestikus mittekorreleeritud 
#tunnuste arv, ning teha nende arvu järgi Bonferroni korrektsiooni. Selleks, et 
#leida andmestiku nn “efektiivne dimensionaalsus”, kasuta PCA-d.
#Juhised:
#Rakenda andmestikul PCA-d ning leia, mitu peakomponenti seletavad näiteks 99% 
#variatsioonist. Leitud peakomponentide arv näitabki ligikaudu sõltumatute 
#tunnuste arvu meie andmestikus. Tee Bonferroni korrektsioon selle arvu järgi.
#Millise p-väärtuse piiri saad?
pca = prcomp(biomarkerid[, 6:111])
pca_sum=summary(pca)
#u 24 muutujat selgitab 99% variatsioonist ära
#sellel juhul saan bonferroni korrektsiooni p-väärtuseks 0.05/24=0.00208

#Boonusülesanne 1 (2 punkti) - usaldusintervallid kordajate jaoks
#(1 boonuspunkt) Muuda funktsiooni estimate_significance selliselt, et iga 
#biomarkeri kordajale arvutad ka 95% usaldusintervalli. Võid kasutada 
#normaaljaotusel põhinevat lähendit ning arvutada selle 
#kordaja_hinnang +- 1.96 * SE, kus SE on summary(model) väljundis toodud
#Std. Error. Funktsiooni tagastatavas andmetabelis peaksid nüüd olema ka 
#veerud lower ja upper.

estimate_significance = function(formula0, biomarkers, data){
    coefs=list()
    pvalues=list()
    upper=list()
    lower=list()
    for (i in 1:length(biomarkers)) {
        formula = paste(formula0, biomarkers[i], sep=" + ")
        model = glm(formula, family=binomial, data=data)
        summary_table = coef(summary(model))
        pvalues[i] = summary_table[nrow(summary_table), 4]
        coefs[i] = summary_table[nrow(summary_table), 1] 
        upper[i]=summary_table[nrow(summary_table), 1]+summary_table[nrow(summary_table), 2]*1.96
        lower[i]=summary_table[nrow(summary_table), 1]-summary_table[nrow(summary_table), 2]*1.96
        
    }
    pvalues=round(as.numeric(pvalues), 10)
    coefs=round(as.numeric(coefs), 6)
    upper=round(as.numeric(upper), 6)
    lower=round(as.numeric(lower), 6)
    tulemid=data.frame(biomarkers, pvalues, coefs, upper, lower)
    return(tulemid)
}

#test
proov=estimate_significance(formula0="s5 ~ sugu + vanusegrupp", 
                      biomarkers=c("LDL_D", "HDL_C"), data=biomarkerid)

ggplot(proov, aes(x=biomarkers, ymax=upper, ymin=lower, y=coefs))+
    geom_pointrange(aes(fill=coefs), shape=21)+
    guides(fill=FALSE)
    
#Ülesanne 9 (1 punkt) - forward selection
#Artiklis on kirjeldatud mudeli koostamist järgnevalt:
#For biomarker discovery in the Estonian Biobank cohort, a multivariate model 
#was derived in a forward stepwise fashion (Figure 2). First, the biomarker 
#leading to the smallest p-value in the model adjusted for age and sex only was 
#included as a predictor. Subsequently, the biomarker leading to the smallest 
#p-value in the multivariate model adjusted for age, sex, and the first 
#biomarker was included in the prediction model. The process was repeated until 
#no additional biomarkers were significant at the Bonferroni-corrected threshold
#of p<0.0005, accounting for testing of 106 candidate biomarkers.
#Eelmistes ülesannetes leidsid kõige olulisema p-väärtusega biomarkeri. 
#Jätka nüüd forward selection-iga:
#Lisa leitud biomarker mudelisse ning lähtu mudelist s5 ~ sugu + vanusegrupp +
#kõige_olulisem_biomarker
#Kasuta funktsiooni estimate_significance ning leia nüüd järgmine biomarker, 
#mis mudelisse lisada. Jätka senikaua, kuni mudelisse lisatavad biomarkerite 
#p-väärtused on väiksemad kui sinu määratud piir.
#Artiklis saadi sellise protsessi tulemusena 4 olulist biomarkerit: 
#Alb, VLDL_D, Gp, Cit. Kas said samasugused?
#leaime väikseima p-väärtusega markeri
esimene=estimate_significance(formula0="s5 ~ sugu + vanusegrupp", 
                      biomarkers=names(biomarkerid)[6:111], data=biomarkerid)
marker1=esimene[order(esimene$pvalues),][1,1]
teine=estimate_significance(formula0="s5 ~ sugu + vanusegrupp + Alb", 
                              biomarkers=names(biomarkerid)[6:111], data=biomarkerid)
marker2=teine[order(teine$pvalues),][2,1]
kolmas=estimate_significance(formula0="s5 ~ sugu + vanusegrupp + Alb + Gp", 
                            biomarkers=names(biomarkerid)[6:111], data=biomarkerid)
marker3=kolmas[order(kolmas$pvalues),][2,1]
neljas=estimate_significance(formula0="s5 ~ sugu + vanusegrupp + Alb + Gp + Cit", 
                             biomarkers=names(biomarkerid)[6:111], data=biomarkerid)
marker4=neljas[order(neljas$pvalues),][2,1]


#Ülesanne 10 (1 punkt) - surma tõenäosuse prognoosimine
#Eelmise ülesande tulemusena on sul nüüd olemas lõplik mudel, mis võtab 
#arvesse kõik, mis on oluline surma tõenäosuse prognoosimiseks. Prognoosi 
#iga andmestikus oleva inimese kohta tema tõenäosust surra 5 aasta jooksul 
#ja visualiseeri tulemust (näiteks histogrammi abil).
#Näpunäide: Uuri, mida teeb järgnev kood
model = glm(s5 ~ sugu + vanusegrupp + Alb + Gp + Cit, 
            family=binomial, data=biomarkerid)
newdata = biomarkerid[,c("s5", "sugu", "vanusegrupp", "Alb", "Gp", "Cit")]
# On oluline, et newdata sisaldaks kindlasti kõik need veerud, mida on 
#vaja prognoosimisel
predicted_probabilities = predict(model, newdata=newdata, type = "response")
hist(predicted_probabilities)

#Boonusülesanne 2 (2 punkti) - prognooside täpsus
#Eelmises ülesandes prognoosisid surma tõenäosust. Aga mida hakkab tavainimene 
#peale tõenäosusega? Olgem ikka konkreetsed, kas siis sureb 5 aasta jooksul 
#või mitte.Selleks otsusta piir, millisest väiksemad tõenäosused klassifitseerid 
#ei sure ja suuremad tõenäosused sureb. Kasutades seda piiri ning eelmises
#ülesandes kirjutatud funktsiooni, arvuta kõigi andmestikus olnud inimeste 
#jaoks 5 aasta jooksul suremise prognoos (justkui meil poleks olnud teada 
#tunnuse s5 väärtus).Milline on sinu prognooside täpsus (st kui suur osa 
#prognoosidest langes kokku tunnuse s5 väärtusega)?
#teeme 0.5 peale
proov=ifelse(predicted_probabilities<0.5, 0, 1)
vordlus=data.frame(biomarkerid$s5, proov)
#kui palju ma puusse panin
summary(factor(vordlus$biomarkerid.s5-vordlus$proov)) #pole paha, 4.158% viga
#kui palju surnute osas puusse panin
surnud=subset(vordlus, biomarkerid.s5==1)
summary(factor(surnud$biomarkerid.s5-surnud$proov)) #väga perses mudel
#teeme confusionmatrixi
library(caret)
confusionMatrix(vordlus$proov, biomarkerid$s5)
#Võrdlusmomendi saamiseks paku välja veel mingi teine, naiivne 
#klassifitseerija (see võib põhineda ükskõik kui lihtsal reeglil). 
#Milline on selle täpsus?
#kõige lihtsam, kuna surijaid oli vähe, ennustan kõigile, et jäävad elama
naive=data.frame(rep.int(0, length(vordlus$proov)))
naive$s5=biomarkerid$s5
summary(factor(naive$s5-naive$rep.int.0..length.vordlus.proov..)) #4,4% viga

