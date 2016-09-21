#tk=teaduskond
#nr=mitu ainet topist kuvatakse välja
#
plot_oppeained = function(data, tk, sem, nr, jrk){
    library(dplyr)
    data_subset=data %>%
        subset(teaduskond==tk & semester==sem) %>%
        mutate(keskmine = (A*5+B*4+C*3+D*2+E*1)/(registreerunuid-mi)) %>%
        subset(keskmine<=5& keskmine>=0)
    
    #paneme järjekorda
    if (jrk==0) {
        data_subset=data_subset[order(-data_subset$keskmine),]
    } else {
        data_subset=data_subset[order(data_subset$keskmine),]
        
    }
    
    #hoiame ainult nr arvu ridu alles
    if (nrow(data_subset)>=nr) {
    data_subset=data_subset[1:nr,]
    }
    #teeme kitsaks andmetabeli
    library(reshape2)
    data_subset$mi=NULL
    data_subset_melt=melt(data_subset, 
                          measure.vars = c("A", "B", "C", "D", "E", "F"))
    names(data_subset_melt)[7:8]=c("hinne", "opilaste_arv")
    #teeme ploti  
    library(ggplot2)
    ggplot(data_subset_melt, aes(x=reorder(paste(aine, kood),keskmine, median), 
                                 y=opilaste_arv,fill=hinne))+
        geom_bar(position="fill", stat="identity")+
        scale_fill_brewer()+
        xlab("")+
        ylab("")+
        coord_flip()
}
