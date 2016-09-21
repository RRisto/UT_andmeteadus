andmed=read.csv2("aktsiad.csv", sep=";")

#Kõigepealt leia kolm kõige suurema päevase tootlusega aktsiat ning väljasta nende kohta:
#ettevõtte sümbol, ettevõtte nimi, tootlus.
library(dplyr)
parim_tootlus=andmed %>%
    select(symbol,nimi, tootlus)%>%
    arrange(desc(tootlus))%>%
    head(3)

andmed$hind=as.numeric(as.character(andmed$hind))
andmed2=mutate(andmed, kaive=hind*kogus)

sektori_grupp=group_by(andmed2, sektor)

sektori_loikes <- summarise(sektori_grupp,
                   keskmine_tootlus=mean(tootlus, na.rm=T),
                   kogukaive=sum(kaive, na.rm=T),
                   keskmine_kaive=mean(kaive, na.rm=T),
                   ettevotete_arv=n())
sektori_loikes=filter(sektori_loikes, ettevotete_arv>=30)
#facotierst numericuks
andmed$kasum_aktsia_kohta=as.numeric(as.character(andmed$kasum_aktsia_kohta))
andmed$dividend_aktsia_kohta=as.numeric(as.character(andmed$dividend_aktsia_kohta))


andmed=mutate(andmed, 
              kasum_aktsiahinna_suhe=kasum_aktsia_kohta/hind,
              dividendi_maar=dividend_aktsia_kohta/hind)

andmed=mutate(andmed, atraktiivsus_skoor=0.5*kasum_aktsiahinna_suhe+0.5*dividendi_maar)

andmed=arrange(andmed, atraktiivsus_skoor)

andmed2=andmed
andmed2=filter(andmed2, symbol %in% c("AAPL", "FB", "GOOG", "IBM", "TSLA"))
kauplemisalgoritm(andmed2)

#andmete võtmine yahoo financest
symbols=unique(andmed$symbol)
symbols=as.character(symbols)
data=getQuote(symbols,src = "yahoo",
              what=yahooQF(c("Last Trade (Price Only)","Symbol","Volume", "Earnings/Share", "Dividend/Share")))
#algtabelist võtame symoli, nime, sektori ja aktsiate arvu
andmed_select=select(andmed, symbol, nimi, sektor, aktsiate_arv)
#väiketähtdeks
names(data)=tolower(names(data))
#mergime kokku
andmed_merge=merge(andmed_select, data, by="symbol")

#funktsioon, et andmedyahoost tõmmata ja kokku mergida
andmed_yahoost = function(algfail){
    library(dplyr)
    #võtame symbolid, mille kohta infot tõmmata
    symbols=unique(algfail$symbol)
    symbols=as.character(symbols)
    #tõmbame andmed yahoost
    data=getQuote(symbols,src = "yahoo",
                  what=yahooQF(c("Last Trade (Price Only)",
                                 "Symbol","Change in Percent",
                                 "Volume", 
                                 "Earnings/Share", 
                                 "Dividend/Share")))
    #võtame algfailist ainult vajalikud andmed
    andmed_select=select(algfail, symbol, nimi, sektor, aktsiate_arv)
    #väiketähtdeks columnite nimed (vaja et kokku mergida)
    names(data)=tolower(names(data))
    #mergime kokku
    andmed_merge=merge(andmed_select, data, by="symbol")
    andmed_merge
}
    
proov=andmed_yahoost(andmed)

names(proov)=c("symbol", "nimi", "sektor","aktsiate_arv", "trade time", "hind",
               "tootlus", "kogus", "kasum_aktsia_kohta","dividend_aktsia_kohta")

#puhastame tootluse ära

proov$tootlus=gsub("\\+","", proov$tootlus)
proov$tootlus=gsub("\\-","", proov$tootlus)
proov$tootlus=gsub("\\%","", proov$tootlus)
#teeme numericuks
proov$tootlus=as.numeric(as.character(proov$tootlus))

