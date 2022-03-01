library("factoextra")
library("cluster")
library("ggsci")
library("pheatmap")
library("kknn")
library(dendextend)
library(corrplot)

data <- USArrests
data <- scale(USArrests)

or_dis <- dist(data,method = "euclidean")
#distance heatmap
fviz_dist(dist(data,method = "euclidean"))

#optimal number of clusters
fviz_nbclust(data,kmeans,method = "gap_stat")

res <- kmeans(data,centers = 4,nstart = 25)
pam(data,4,metric = "euclidean",stand = FALSE)
clara(data, 4, samples = 50, pamLike = TRUE)

#cluster Plot
fviz_cluster(res,data = data,palette = "jco") 

pheatmap(t(data), cutree_cols = 4) 
#t(data) transpose a matrix or data.frame

rec_hc <- hclust(dist(data),method = "ward.D2")
fviz_dend(rec_hc,cex = 0.5,k=4,palette = "jco")
as.dendrogram(rec_hc) #summary of dendrogram 
cop_dis <- cophenetic(rec_hc)
cor(or_dis,cop_dis)

#Agglomerative Nesting (Hierarchical Clustering) 
res.agnes <- agnes(x = USArrests,# data matrix 
                   stand = TRUE, # Standardize the data 
                   metric = "euclidean", # metric for distance matrix       
                   method = "ward" # Linkage method 
                  )
fviz_dend(res.agnes,cex = 0.6,k=4)                   

#DIvisive ANAlysis Clustering 
res.diana <- diana(x = USArrests, # data matrix 
                   stand = TRUE, # standardize the data 
                   metric = "euclidean" # metric for distance matrix 
                   )
fviz_dend(res.diana,cex = 0.6,k=4)                   

# creating a dendrogram using chaining operator
dend <- USArrests[1:10,] %>% # data 
  scale %>% # Scale the data 
  dist %>% # calculate a distance matrix, 
  hclust(method = "ward.D2") %>% # Hierarchical clustering
  as.dendrogram # Turn the object into a dendrogram. 
plot(dend)

#customize dendrograms set(object, what, value)
mycols <- c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07") 
dend <- as.dendrogram(rec_hc) %>% 
  set("branches_lwd", 1) %>% # Branches line width 
  set("branches_k_color", mycols, k = 4) %>% # Color branches by groups 
  set("labels_colors", mycols, k = 4) %>% # Color labels by groups 
  set("labels_cex", 0.5) # Change label size
  
  fviz_dend(dend)

#Create multiple dendrograms by chaining 
dend1 <- data %>% dist %>% hclust("complete") %>% as.dendrogram 
dend2 <- data %>% dist %>% hclust("single") %>% as.dendrogram 
dend3 <- data %>% dist %>% hclust("average") %>% as.dendrogram 
dend4 <- data %>% dist %>% hclust("centroid") %>% as.dendrogram 
  
#Compute correlation matrix 
dend_list <- dendlist("Complete" = dend1, 
                      "Single" = dend2, 
                      "Average" = dend3, 
                      "Centroid" = dend4) 
cors <- cor.dendlist(dend_list) 

#Print correlation matrix round(cors, 2)
corrplot(cors, "pie", "lower")







