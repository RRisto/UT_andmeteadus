#Eralda html koodis sinisena olev tekst muutujasse tekst. Kasuta paketti rvest.
library(rvest)
html_source ="http://andmeteadus.github.io/examples/html1.html"
page = html(html_source)
tekst=page %>% 
    html_node("p#p01") %>%
    html_text()

#Eralda html koodis punaselt olev tekst muutujasse tekst. Kasuta paketti rvest.
html_source ="http://andmeteadus.github.io/examples/html2.html"
page = html(html_source)
tekst=page %>% 
    html_nodes("p.error") %>%
    html_text()

#Eralda Riigikogu hääletamistulemuste veebilehe html lähtekoodist, mitu saadikut 
#hääletas kooseluseaduse eelnõu:
html_source ="http://www.riigikogu.ee/tegevus/tooulevaade/haaletused/haaletustulemused-kohalolekukontroll/a85129ed-4873-4b9d-ac37-4788b6587fa0/"
page = html(html_source)
tekst=page %>% 
    html_nodes("li a span") %>%
    html_text()

andmed=as.data.frame(tekst)
#või
sodi=page %>% 
    html_nodes("a span") %>%
    html_text()

#Eralda kooseluseaduse eelnõu hääletamistulemuste veebilehe html lähtekoodist 
#andmetabel, kus on 101 rida ning tunnused nr, nimi, otsus, fraktsioon.
# #
# tabel=page %>% 
#     html_nodes("#koik") %>%
#     html_text()
# #sellega saan nimed, kuid need on topelt
# tekst2=page %>% 
#     html_nodes(".w-1") %>%
#     html_text()
# #siin on kogu tabel, kuid seda tuleb töödelda
# tekst3=page %>% 
#     html_nodes("tr td") %>%
#     html_text()
# 
# tekst4=page %>% 
#     html_nodes("td , .w-1") %>%
#     html_text()

#sellega saab juba kätte
tekst5=page %>% 
    html_nodes(".table.table.table-striped.full-bars") %>%
    html_table()
#siit saan data frame 101 liikme, erkonna ja hööletustulemusega, nr-d võtan
#zipitud tabelitest, kuna vahepeal on kodulehte muudetud
sub=tekst5[1]
sub2=as.data.frame(sub)
#puhastan otsuse poolt ja asendan tabelisse
junn=gsub( " ", "",sub2$Otsus)
junn2=gsub( "\n\n", "",junn)
junn3=gsub( "^Poolt", "",junn2)
junn3=gsub( "^Vastu", "",junn3)
junn3=gsub( "^EiHääletanud", "",junn3)
junn3=gsub( "^Puudub", "",junn3)
junn3=gsub( "EiHääletanud", "Ei Hääletanud",junn3)
sub2$Otsus=junn3

#Kirjuta vastav kood funktsiooniks extract_table (seda funktsiooni läheb vaja 
#järgmises ülesandes, kus eraldad kõigi Riigikogu XII hääletuste kohta vastava 
#tabeli). Sisendiks on kas veebilehe url, faili lokaalne asukoht või sõne. 
#Funktsioon peab tagastama vastava data.frame-i (pane tähele, et su funktsioon 
#ei tagastaks listi, milles on üks data.frame).
#kuna kodukat on muudetud teen funktsiooni zipitud falide pealt (neid on vaja
#massiga sisse lugeda)
#Failis htmls.zip on olemas veebilehed kõigi Riigikogu XII hääletuste kohta. 
#Sinu ülesandeks on koostada andmetabel, kus ridades on Riigikogu saadiku 
#nimi ja veergudes kõik hääletamiskorrad. Seda andmestikku läheb 
#vaja järgmises praktikumis, kus uurime hääletamismustreid.
#Kõigepealt paki lahti zip fail ning loe R-i sisse kõigi html failide nimed.
#Näpunäide: Järgnev kood aitab kätte saada kõigi antud kaustas olevad 
#csv-failide nimed (seejuures argument full.names võimaldab tagastada terve 
#failitee).
filenames =list.files("./data/data", pattern = "*.html", full.names=TRUE)

#Esialgu loe sisse umbes 5 erinevat html faili.
#NB! Alles siis, kui oled täiesti kindel, et sinu kood töötab korrektselt, 
#võta kasutusele kõik html failid.Näpunäide: Järgnev kood loeb sisse kõik 
#muutujas filenames olevad csv andmestikud ning tekitab neist listi.
list_of_dataframes = list()
for(i in 1:length(filenames)){
    temp = read.csv(filenames[i])
    list_of_dataframes[[i]] = temp
}

#Praegu pole sul read.csv käsuga midagi peale hakata, sest tegeleme html 
#failidega. Kasuta ülesandes 4 kirjutatud funktsiooni extract_table.
#Eelneva for-tsükli asemel võid kasutada funktsiooni lapply.
#Lisa igal tsükli sammul andmestikule hääletuse indeks või muu identifikaator. 
#Näiteks temp$haaletus = i.
#teen siis kõigepealt funktsiooni
library(rvest)
extract_table=function(url) {
    html_source =url
    page = html(html_source)
    tabel=page %>% 
        html_nodes("table.List") %>%
        html_table()%>%
        as.data.frame()
    tabel
}
#proovin viie tabeli peal
filenames =list.files("./data/data",pattern = "*.html",  full.names=TRUE)

list_of_dataframes = list()
for(i in 1:5){
    temp = extract_table(filenames[i])
    temp$haaletus = i
    list_of_dataframes[[i]] = temp
}

#töötab, loen kõik sisse
filenames =list.files("./data/data", pattern = "*.html", full.names=TRUE)

list_of_dataframes = list()
for(i in 1:length(filenames)){
    temp = extract_table(filenames[i])
    temp$haaletus = i
    list_of_dataframes[[i]] = temp
}

#Nüüdseks peaksid olema saanud listi, mille elementideks on erinevad 
#andmetabelid (kõiki faile kasutades peaks nende koguarv olema 1845). Tee 
#nendest andmetabelitest üks suur (pikk) andmetabel, paigutades need üksteise 
#otsa. Seda aitab teha paketi dplyr funktsioon rbind_all. Tulemuseks peaksid 
#saama andmetabeli, mille ridade arv on 101 * “sinu kasutatud failide arv”.
library(dplyr)
andmed=rbind_all(list_of_dataframes)

#Muuda pikk andmetabel laiaks. Seda aitab teha paketi reshape2 käsk dcast. 
library(reshape2)
andmed_lai=dcast(andmed, Nimi~haaletus, value.var = "Otsus")

save(andmed_lai, file="riigikogu.RData")
#Lõpptulemuseks võiksid saada järgneva andmestiku, kus on 143 rida ning 
#1846 veergu. Klapib

#Teine osa
#Tagasta kõik Postimehe esilehe uudiste pealkirjad (joonisel näidatud kollasega).
#Ära kurvasta, kui sa ei saa absoluutselt kõiki pealkirju, 97% on praegu piisav.
#Kui sulle ei meeldi Postimehe veebilehe hiiglaslikku lähtekoodi inspekteerida 
#brauseris vaikimisi olevate vahenditega, siis abiks on praktikumis tutvustatud 
#tööriist selectorgadget.Vaata, et sinu tagastatud pealkirjade hulgas poleks 
#tühju sõnesid või arve.
url="http://www.postimees.ee/"
page = html(url)
tekst=page %>% 
    html_nodes(".frontHeading") %>%
    html_text()
#puhastame
tekst_puhas=gsub("\n", "", tekst)
#palkirjade lõppu korjab ka kommentaaride arvu, puhastame need välja
tekst_puhas=gsub("\\d*$", "", tekst_puhas)
#eemaldame tühjad stringid
tekst_puhas=tekst_puhas[tekst_puhas != ""] 


#Eva “Usin” Masinal on suur huvi ilmaandmete vastu. Kümme minutit pärast iga 
#täistundi märgib ta Ilmateenistuse vaatlusandmeid oma märkmikku, et hiljem 
#analüüsi teha. Automatiseeri seesama protsess. Juhised:
#Riigi Ilmateenistus pakub värskeid ilmaandmeid XML faili kujul. Meie tegeleme 
#Eesti vaatlusandmete XML failiga. Saa XML failist kätte iga ilmajaama õhurõhk.
#Saa XML failist kätte iga ilmajaama tuule kiirus.
#Tee neist õhurõhu ja tuule kiiruse scatterplot.
url="http://www.ilmateenistus.ee/ilma_andmed/xml/observations.php"
page = html(url)

ohurohk=page %>% 
    html_nodes("airpressure")%>%
    html_text()

tuulekiirus=page %>% 
    html_nodes("windspeed") %>%
    html_text()

# ilm=page %>% 
#     html_nodes("name, airpressure, windspeed")%>% 
#     html_text()

nimi=page %>% 
    html_nodes("name")%>%
    html_text()

#teeme numericuks
ohurohk=as.numeric(as.character(ohurohk))
tuulekiirus=as.numeric(as.character(tuulekiirus))
ilm=data.frame(nimi, tuulekiirus, ohurohk)
#teeme graafiku
library(ggplot2)

ggplot(ilm, aes(x=tuulekiirus, y=ohurohk))+
    geom_point()

#Eva “Usin” Masin on lotohuviline, aga ta pole aastaid Viking Lottoga võitnud. 
#Ta arvab, et lototulemused pole päris juhuslikud ning lotos on võimalik 
#statistiline eelis saada. Seepärast märgib ta iga lotokolmapäev Viking Lotto 
#loositud numbrid üles ja uurib, kas number kahtesid loositakse rohkem välja, 
#kui juhus lubaks.
#Õpeta tehis-Eva tegema seda sama.
#Eesti Loto veebilehel on toodud statistika loositud pallide sagedusest.
#Eralda vastav tabel, kus veergudes on tunnused number, sagedus ja sagedus 
#protsentides.selectorgadget veab sind siin alt ning kergem on lähtekoodi 
#inspekteerida brauseris olevate tööriistadega (Chrome’s vajuta Ctrl + Shift + I 
#või tee parem klikk ja vajuta inspekteeri elementi). Visualiseeri saadud 
#andmetabelit. Tee näiteks tulpdiagramm, kus x-teljel on arvud 1-48 ning y-telg 
#tähistab sagedust.
html_source ="https://www.eestiloto.ee/osi/stats.do?lastDraws=250&gameCode=11&sort=frq0&action=searchNumbers"
#lingi osas tegin ise uue päringu ja võtsin siis lingi, algne peksis segast
page = html(html_source)
loto=page %>% 
    html_nodes("table") %>%
    html_table(fill=T)

numbrid=as.data.frame(loto[5])
#graafik
library(ggplot2)
ggplot(numbrid, aes(x=factor(Number), y=Sagedus))+
    geom_bar(stat="identity")

#(2 boonuspunkti + lisaboonuspunkt) Viimase 250 loosiga on pall 35 tulnud 28 
#korral, pall 34 aga 59 korral. Uuri, kas on alust arvata, et Viking Lotto 
#süsteem on kallutatud. Selleks mõtle välja, kuidas seda kontrollida 
#(näiteks võid kasutada simulatsioonidel põhinevat lähenemist). Selgita 
#lühidalt oma lähenemist ja raporteeri, millise tulemuse said. Lisaboonuspunkti 
#saamiseks visualiseeri seda tulemust.
#teen dataframe, kuhu hakkan itereerima
# tulem=data.frame(c(1:250))
# names(tulem)="järjekord"
# 
# for(i in 1:100) 
# { 
#     tulem[,i+1]=sample(1:48, 250, replace=T)
#     names(tulem)[i+1] <- paste("iter", i, sep = "")
# }
# 
# library(reshape2)
# tulem_melt=melt(tulem, id=c("järjekord"))
# #arvutame iga iteratsioonis iga numbri sageduse
# tulem_sagedus=data.frame(table(tulem_melt$value, tulem_melt$variable))
# #hoiame alles ainult 34 ja 35 sagedused, kuna need huvitavad
# tulem_vaja=subset(tulem_sagedus, Var1%in% c(34,35))
# 
# ggplot(tulem_vaja, aes(x=Freq, colour=Var1))+
#     geom_density()

#siiski minu oma ei anna sama tulemust, pean itereerima ikka 8 numbri kaupa
library(reshape2)
tulem=data.frame(c(1:8))
names(tulem)="järjekord"
list34=list()
list35=list()
j=1
for (j in 1:100)  {
    
    tulem=data.frame(c(1:8))
    names(tulem)="järjekord"
    
        for (i in 1:250) 
        { 
    tulem[,i+1]=sample(1:48, 8, replace=F)
    names(tulem)[i+1] <- paste("iter", i, sep = "")
        }
    
    tulem_melt=melt(tulem, id=c("järjekord"))
    #arvutame iga iteratsioonis iga numbri sageduse
    tulem_sagedus=data.frame(table(tulem_melt$value, tulem_melt$variable))
    #hoiame alles ainult 34 ja 35 sagedused, kuna need huvitavad
    tulem_vaja=subset(tulem_sagedus, Var1%in% c(34,35))
    
    list34[j]=sum(tulem_vaja$Freq[tulem_vaja[,1]==34])
    list35[j]=sum(tulem_vaja$Freq[tulem_vaja[,1]==35])
    j=j+1
}

#teeme dataframeiks listid ja numericuks et ggplottida
simulatsioonid=as.data.frame(rbind(list34, list35))
simulatsioonid=as.data.frame(t(simulatsioonid)) #transpose
simulatsioonid$list34=as.numeric(simulatsioonid$list34)
simulatsioonid$list35=as.numeric(simulatsioonid$list35)
simulatsioonid_melt=melt(simulatsioonid)
simulatsioonid_melt$value=as.numeric(simulatsioonid_melt$value)
#sit on hästi näha, mis on 34 ja 35 esinemissageduste jaotus 250 numrbite
#võtmise korral lotos
ggplot(simulatsioonid_melt, aes(x=variable, y=value))+
    geom_boxplot()

#Alusta suvaliselt veebilehelt. Eralda kõik väljuvad lingid. Vali üks neist 
#linkidest suvaliselt. Hüppa sellele lingile. Kui sellel leheküljel pole 
#ühtegi väljuvat linki, mine tagasi. Kui väljuvaid linke on mitmeid, vali 
#jälle välja suvaline ja hüppa sinna. Kui jõudsid Facebooki, on katse lõppenud. 
#Korda seda protsessi mitu korda ja erinevate alglehtedega. Uuri, mitmel juhul 
#jõudsid FBsse. Näpunäide: Abiks on paketi rvest funktsioon follow_link().



s <- html_session("http://www.postimees.ee/")
s %>% 
    follow_link(sample(1:3, 1))%>%
    follow_link("elu")%>%
    follow_link("face")


s <- html_session("http://had.co.nz")
s %>% jump_to("thesis/")
s %>% follow_link("vita")
s %>% follow_link(3)
s %>% follow_link("vita")


#(1 boonuspunkt) Mis on olnud Tartu Ülikooli Facebooki lehe kõige populaarsem 
#postitus? Mis on olnud matemaatika-informaatikateaduskonna Facebooki lehe 
#kõige populaarsem postitus? (5 boonuspunkti :-)) Kasuta R-i, et uuendada 
#oma staatust tekstiga ‘Teen aine “Statistiline andmeteadus ja visualiseerimine” 
#kodutööd. Väga põnev! :-)’. Abiks on käsk updateStatus.
#(3 boonuspunkti) Kui sulle ei meeldi Facebooki algoritm, mille põhjal 
#ta postitusi ja pilte uudisvoos järjestab, mõtle välja algoritm, mis selle 
#parandab. Implementeeri selle prototüüp (kasuks tuleb käsk getNewsfeed).
library(devtools)
install_github("pablobarbera/Rfacebook/Rfacebook")
library(Rfacebook)

fb_oauth <- fbOAuth(app_id="687303151396669", 
                    app_secret="7b256b7abcebbe38430fb44a5db08576")

save(fb_oauth, file="fb_oauth")

load("fb_oauth")
me <- getUsers("me",token=fb_oauth)
my_likes <- getLikes(user="me", token=fb_oauth)
updateStatus("Statistics are used much like a drunk uses a lamppost: for support, not illumination", token=fb_oauth)
