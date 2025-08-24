data <- as.matrix(heatmap[,2:12])
dim(data)
rownames<-as.matrix(heatmap[,1])
dim(rownames)
rownames(data) <-rownames
# Default Heatmap
pheatmap(data, scale = "column", cellwidth =11,cellheight =18)
