# Koostame näidismaatriksi, mis sisaldaks teatud mustrit
test = matrix(rnorm(200), 20, 10)
test[1:10, seq(1, 10, 2)] = test[1:10, seq(1, 10, 2)] + 3
test[11:20, seq(2, 10, 2)] = test[11:20, seq(2, 10, 2)] + 2
test[15:20, seq(2, 10, 2)] = test[15:20, seq(2, 10, 2)] + 4
# Kui anname maatriksi ridadele/veergudele nimed, näeme neid heatmapil
colnames(test) = paste("Veerg", 1:10, sep = "")
rownames(test) = paste("Rida", 1:20, sep = "")

library(pheatmap)
pheatmap(test, cluster_rows=FALSE, cluster_cols=FALSE)

pheatmap(test, cluster_rows=FALSE)

pheatmap(test)