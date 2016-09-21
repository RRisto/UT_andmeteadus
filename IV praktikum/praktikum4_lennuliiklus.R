airports=read.csv("./data/airports.csv")
flights=read.csv("./data/flights.csv")

#Näide, kuidas joonistada suurringjoont R-is. Ühendame Liivi 2 maja Facebooki 
#peakontoriga.
library(maps)
library(geosphere)

map("world", col="#f2f2f2", fill=TRUE, border=NA)

lat_liivi = 58.379491
lon_liivi = 26.713199
lat_fb = 37.485245
lon_fb = -122.148807

gc_points = gcIntermediate(c(lon_liivi, lat_liivi), c(lon_fb, lat_fb), n=50, 
                           addStartEnd=TRUE)
lines(gc_points)

#Visualiseeri suurringjoonte abil ühendusi, kuhu saab Tallinna lennujaamast 
#otselennuga.
#mergime tabelid
allikas=airports[, c("id", "lat", "lon")]
names(allikas)=c("id", "source_airport_lat", "source_airport_lon")
data=merge(flights, allikas, by.x="source_airport_id", by.y="id", all.x=T, all.y=F)
names(allikas)=c("id", "dest_airport_lat", "dest_airport_lon")
data=merge(data, allikas, by.x="dest_airport_id", by.y="id", all.x=T, all.y=F)
#teeme subseti ainult neist andmetest, kus alguspunkt on tallinn
data2=subset(data, source_airport_id==415)
#teeme kaardi
map(xlim = c(-10, 40), ylim = c(30, 70),col="#f2f2f2", fill=TRUE, border=NA)
#itereerime punktid peale
for (i in 1:nrow(data2)) {
    points = gcIntermediate(c(data2$source_airport_lon[i], 
                              data2$source_airport_lat[i]),
                               c(data2$dest_airport_lon[i], 
                                 data2$dest_airport_lat[i]), n=50, 
                               addStartEnd=TRUE)
    lines(points)
}

#Visualiseeri ühendusi, kuhu saab Tallinna lennujaamast otselennu või ühe 
#vahepeatusega.
airports=unique(data2$dest_airport_id)
data3=subset(data, source_airport_id==415| source_airport_id %in% airports)
#teeme mapi
map("world",col="#c7e9c090", fill=TRUE, border=NA,mar = c(1, 1, par("mar")[3], 0.1))
#itereerime punktid peale
#Et värve muuta läbipaistvaks, lisa värvikoodile lõppu läbipaistvuse intensiivsus.
#Näiteks kasuta funktsiooni lines korral argumenti col="#00000010", et muuta 
#mustade (värvikood #000000) joonte läbipaistvus 10%-ni.
for (i in 1:nrow(data3)) {
    points = gcIntermediate(c(data3$source_airport_lon[i], 
                              data3$source_airport_lat[i]),
                            c(data3$dest_airport_lon[i], 
                              data3$dest_airport_lat[i]), n=50, 
                            addStartEnd=TRUE)
    lines(points, col="#00000008")
}

#Alusta Tallinna lennujaamast. Vali kõikvõimalikest lennujaamadest juhuslikult 
#üks ja lenda sinna. Vali nüüd juhuslikult järgmine sihtpunkt ning lenda sinna. 
#Jätka seda protsessi 100 korral ja visualiseeri läbitud teekonda.
map("world",col="#c7e9c090", fill=TRUE, border=NA,mar = c(1, 1, par("mar")[3], 0.1))
#teeme alguse ära, tallinnast esimene liin
data_subset=subset(data, source_airport_id==415)
destination=sample(data_subset$dest_airport_id,1)
data_subset=subset(data_subset, source_airport_id==415&
                       dest_airport_id==destination )[1,] #võtame esimese rea
#kuna võib olla mitu liini ühte kohta samast lennujaamast

point=gcIntermediate(c(data_subset$source_airport_lon, 
                       data_subset$source_airport_lat),
                     c(data_subset$dest_airport_lon, 
                       data_subset$dest_airport_lat), n=50, 
                     addStartEnd=TRUE)
lines(point, col="#00000050")
#kordame 100 korda
for (i in 1:100) {
    data_subset=subset(data, source_airport_id==destination)
    destination=sample(data_subset$dest_airport_id,1)
    data_subset=subset(data_subset, dest_airport_id==destination )[1,]
    points=gcIntermediate(c(data_subset$source_airport_lon, 
                           data_subset$source_airport_lat),
                         c(data_subset$dest_airport_lon, 
                           data_subset$dest_airport_lat), n=50, 
                         addStartEnd=TRUE)
    lines(points, col="#00000050")  
}

#Sama asi aga suvalised lennujaamad (ei pea liini omavahel olema)
map("world",col="#c7e9c090", fill=TRUE, border=NA,mar = c(1, 1, par("mar")[3], 0.1))

alg=subset(airports, id==415)
samp=sample(airports$id,1)
dest=subset(airports, id==samp)
point=gcIntermediate(c(alg$lon, 
                       alg$lat),
                     c(dest$lon, 
                       dest$lat), n=50, 
                     addStartEnd=TRUE)
lines(point, col="#00000050")

for (i in 1:100) {
    samp1=sample(airports$id,1)
    alg=subset(airports, id==samp1)
    samp2=sample(airports$id,1)
    dest=subset(airports, id==samp2)
    point=gcIntermediate(c(alg$lon, 
                           alg$lat),
                         c(dest$lon, 
                           dest$lat), n=50, 
                         addStartEnd=TRUE)
    lines(point, col="#00000050")
}
