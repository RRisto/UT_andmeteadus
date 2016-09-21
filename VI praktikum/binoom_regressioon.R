#Joonista R-is logistiliste kõverate
#y=eβ0+β1x1+eβ0+β1x
#graafikud:Ühel joonisel β0∈{−1,0,1} ja β1=1.
#Teisel joonisel β0=0 ja β1∈{0.5,1,2}.
#Selgita, millist mõju omab β0 ja millist β1 muutmine.

#tegin tabeli
binoom=read.csv("./data/kodutoo_binoom_regressioon.csv")

#arvutame y-d
library(dplyr)
binoom2=binoom %>%
    mutate(y = (exp(b_null+b_yks*x))/(1+exp(exp(b_null+b_yks*x))))

library(ggplot2)

ggplot(binoom2, aes(x=x, y=y))+
    geom_point()+
    geom_line()+
    facet_wrap(~nimi2)
   


