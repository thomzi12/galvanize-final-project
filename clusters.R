library(flexclust) 
library(dummy)
library(MASS)
library(ggplot2)
library(pvclust)
library(plyr)
library(reshape2)
library(plot3D)
library(dbscan)

# Functions ----
cosine <- function(x) {
  x <- as.matrix(x)
  y <- t(x) %*% x
  res <- 1 - y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
  res <- as.dist(res)
  attr(res, "method") <- "cosine"
  return(res)
}

set_clusters <- function(df, clusters, column_name){
  df[column_name] <- 0 
  for (i in 1:length(clusters$edges)){
    for (j in 1:length(clusters$clusters[[i]])){
      df[clusters$clusters[[i]][j],][column_name] = i  
    }
  }
  return(df)
}

# Read in Data ----
data <- read.csv('Documents/Galvanize/capstone-project-files/assessment_data.csv')

# Clean data ----
data[data$recent_degree =="",] <- NA 
data <- data[!is.na(data$recent_degree),]
data<- data[!is.na(data$fa.ml),]
data <- data[data$recent_degree != 'na',]

# Zero down to data with assessment data 
data$time_to_job <- NULL 
data[is.na(data$fa.python),'fa.python'] = median(data$fa.python, na.rm =TRUE)
data[data$recent_degree == 'psychology','recent_degree'] = 'social_science'
data = droplevels(data)

# Answer basic questions using scatterplots, etc. ----

# Are people who are relatively good at coding also good at math?
data$math_total = data$a1.math + data$a3.math 
data$coding_total = data$a1.code + data$a2.code + data$a3.code + data$fa.python + data$fa.sql
data$ds_total = data$a4.ds + data$a5.ds + data$fa.ds + data$fa.ml 

# See if people who scored highly in one area also did in the other, and vice-versa 
# g <- ggplot(data, aes(coding_total, math_total)) + geom_point(color="firebrick")

# Make it even more clear: after centering and scaling, see how people did at different subjects 
data_scaled = scale(data[28:30], center = TRUE, scale = TRUE)
colnames(data_scaled) <- c('math_scaled','coding_scaled', 'ds_scaled')
data= cbind(data, data_scaled)

# (Uncomment for plot)
# g2 <- ggplot(data, aes(coding_scaled, math_scaled)) + geom_point(color="firebrick") + 
#       ggtitle('Relative Performance on Math and Coding') + 
#       theme(plot.title = element_text(size=12, face="bold", hjust = 0.5, 
#                                       margin = margin(10, 0, 10, 0)))

# What about different levels of experience? 

# hist(data$years_exp, breaks = 20)
# boxplot(data$years_exp)
# ^ Not very aesthetically pleasing ... 

# (Uncomment for plot)
# g3 <- ggplot(data, aes('years_exp', years_exp)) + geom_boxplot() +
#       ggtitle('Boxplot: Years of Experience') +
#       theme(plot.title = element_text(hjust = 0.5))

# median  = 6, 1st quartile = 3, 3rd quart = 9

# What about different industry backgrounds?

# (Uncomment for plot)
# g4 <- ggplot(data, aes(recent_degree ,fill = recent_degree)) + geom_bar() +
#   ggtitle('Bar chart: Student Background') +
#   theme(plot.title = element_text(hjust = 0.5))

# Restructure data for clustering ----

# Dummify subject matter 
columns = dummy(data.frame(data$recent_degree), int = TRUE)
data = cbind(data, columns)

# Only look at numeric columns for clustering 
data_hc_num = data.frame(data[,-3], row.names = data[,3]) #Make github usernames row values 
data_hc_num = cbind(data_hc_num[,6:18], data_hc_num[,33:45])

# Create Dataframe of scaled data 
data_hc_scaled = scale(data_hc_num[1:13], center = TRUE, scale = TRUE)
data_hc_scaled = cbind(data_hc_scaled, data_hc_num[14:26])
data_hc_scaled = cbind(data_hc_scaled, data[31:33]) #Add on columns with agg. math, coding, ds scores 

# Heirarchical clustering ----
# Use pvclust to get clusters with p-values attached to them. 
# source : http://www.sthda.com/english/wiki/how-to-compute-p-value-for-hierarchical-clustering-in-r-unsupervised-machine-learning

{
# #correlation = not helpful
# data_hc_num.pv.corr <- pvclust(data = t(data_hc_num), method.hclust = "average", method.dist = "correlation", 
#                           nboot = 1000, parallel = TRUE)
# plot(data_hc_num.pv.corr)
# 
# data_hc_num.pv <- pvclust(data = t(data_hc_num), method.hclust = "average", method.dist = "euclidean", 
#         nboot = 1000, parallel = TRUE)
# plot(data_hc_num.pv)
# pvrect(data_hc_num.pv) # Highlight best looking clusters 
# pvpick(data_hc_num.pv)# Get labels for the points
#OK -- about 14 clusters. Try complete linkage?

# data_hc_num.pvc <- pvclust(data = t(data_hc_num), method.hclust = "complete", method.dist = cosine, 
#                            nboot = 1000, parallel = TRUE)
# plot(data_hc_num.pvc)
# pvrect(data_hc_num.pvc) # Highlight best looking clusters 
# pvpick(data_hc_num.pvc)# Get labels for the points
# #Basically, 2 clusters. Not that useful, haha. 

# Check if results change if you use scaled data
# data_hc_num.pvs <- pvclust(data = t(data_hc_scaled), method.hclust = "complete", method.dist = "euclidean", 
#                            nboot = 1000, parallel = TRUE)
# plot(data_hc_num.pvs)
# pvrect(data_hc_num.pvs) # Highlight best looking clusters 
# pvclusterss <- pvpick(data_hc_num.pvs)# Get labels for the points
#OK -- 11 clusters. 

} # Failed attempts at Hierarchical clustering 

data_hc_num.pv <- pvclust(data = t(data_hc_num), method.hclust = "complete", method.dist = "euclidean",
                          nboot = 1000, parallel = TRUE)

plot(data_hc_num.pv)
pvrect(data_hc_num.pv) # Highlight best looking clusters 
pvclusters <- pvpick(data_hc_num.pv)# Get labels for the points
#OK -- about 8 clusters. Better. 

# Bar Plots for Hierarchical Clustering ----
data_hc_scaled <- set_clusters(df = data_hc_scaled, clusters = pvclusters, column_name = 'pv_cluster') #custom function
data_hc_scaled$pv_cluster <- as.factor(data_hc_scaled$pv_cluster)

# reformat data to get ready for graphing 
melted <- melt(data_hc_scaled[,c(1,2,27,28,29,30)], id.vars = ('pv_cluster'))
means <- ddply(melted, c('pv_cluster', 'variable'), summarise, mean=mean(value))

# bar plots for different clusters!
for (level in levels(means$pv_cluster)){
  means_temp <- means[means$pv_cluster ==level,]
  print(ggplot(means_temp, aes(x=variable, y=mean, fill = pv_cluster)) + 
    geom_bar(stat="identity") + 
    ggtitle(paste("Bar plot of cluster", level)) +
    theme(axis.text.x = element_text(face="bold", color="#993333", 
                                     size=10, angle=10), 
          plot.title = element_text(hjust = 0.5)) + 
    scale_x_discrete(name = NULL, labels=c("years_exp" = "Experience", 
                              "years_exp_rel" = "Technical Experience",
                              "coding_scaled" = "Coding",
                              "math_scaled" = "Math",
                              "ds_scaled" = "Data Science (ML)")))
}

print(summary(data_hc_scaled$pv_cluster)/98)
# Problem with the above approach: Two clusters account for roughly 80% of points. 
# Subtle distictions in above groupings may not be meaningful.

# Other takeaway: Solid 60% of students fall into bucket of less experience but 
# above average technical abilities. Do more to offer early career counseling?

# Other major cluster: people slightly further along in careers, aren't doing as good.
# Maybe think about what could cater to them?

# Clustering with Multidimensional Scaling ----

# Create column to anonymize data 
data_hc_scaled$anon_names <- 0
for (i in 1:nrow(data_hc_scaled)){
  data_hc_scaled[i, 'anon_names'] = paste('s_', toString(i), sep = "")
}

# Get distance matrix
data_MDS = data_hc_scaled[,1:26] #exclude agg columns, pvclust cluster labels 
dist_dhs = dist(data_MDS) #euclidean 
Mdists_dhs <- round(as.matrix(dist_dhs),3)
colnames(Mdists_dhs) <- row.names(data_MDS)

# 
max(Mdists_dhs) #10.386
which.max(Mdists_dhs)s # 5643 
sort(Mdists_dhs[,5643 %% 96]) # The most dissimilar people are student_75 and student_59
min(Mdists_dhs[Mdists_dhs> 0]) #1.676
sort(Mdists_dhs[,which(Mdists_dhs == min(Mdists_dhs[Mdists_dhs> 0]))[1] %% 96]) 
# The most similar people are student_93 and student_43

# Plot distances in different dimensions 
QBs1 <- isoMDS(Mdists_dhs, k = 1)
QBs1$stress
plot(cbind(0,QBs1$points), type = "n", ylab = "Distance", xlab = "Github username")
text(0, QBs1$points, rownames(Mdists_dhs), col = 4)

# Non-metric Multidimensional scaling 
QBs2 <- isoMDS(Mdists_dhs, k = 2)
QBs3 <- cmdscale(Mdists_dhs, k=2)

save(QBs2, file= 'QBs2.RData')
QBs2$points
QBs2$stress
z <- ggplot(x = QBs2$points[,1], y =  QBs2$points[,2], aes = (points)) + 
      geom_point() + 
      geom_text(x = QBs2$points[,1], y =  QBs2$points[,2], 
                data = data_hc_scaled, aes(label = anon_names), hjust=0, vjust=0, 
                color="firebrick", size = 7)+
      ggtitle("Multi-dimensional Scaling, Labeled by K-Means Clustering") +
      ylim(-3.3, 5) + xlim(-6, 5.3)+
      theme(plot.title = element_text(hjust = 0.5))
    

plot(QBs2$points, type = "n", main = "Multi-dimensional Scaling, Labeled by OPTICS", 
     ylab = "Score 2", xlab = "Score 1", 
     ylim = c(-3.3, 5), xlim = c(-6, 5.3))

plot(QBs3, type = "n", main = "Classical Multi-dimensional Scaling, Labeled by K-Means Clustering", 
     ylab = "Score 2", xlab = "Score 1", 
     ylim = c(-3.3, 5), xlim = c(-6, 5.3))
text( QBs3, labels = data_hc_scaled$anon_names, col = 4)

text( QBs2$points, labels = db2$cluster, col = db2$cluster+1, cex = 2)
text( QBs2$points, labels = res2$cluster, col = res2$cluster+1, cex = 2)
text( QBs2$points, labels = data_hc_scaled$anon_names, col = 4, cex = 2)

# text( QBs2$points, rownames(Mdists_dhs), col = 4, cex = 0.6)
text( QBs2$points, labels = data_hc_scaled$km_cluster, 
      col = data_hc_scaled$km_cluster, cex = 3)

# Could use DBSCAN to tease out clusters ... or a dendrogram 

# K-Means Clustering ----
# Use `data_MDS` dataframe 

SFg <- stepFlexclust(data_MDS, nrep=10, k = 2:10) 
plot(SFg, type = "l", ylab = "sum of within cluster distances", 
     main = 'K-means clustering scree plot')
# Pretty vague scree plot -- most of the sowcd gains happen with splitting the data in two.
# Other plausible k values include 3 and 7. 
# Theoretically, this is where you go to your admissions team and ask if 7 or 3 students 
# profiles is more useful. I imagine they say 3 ... 

# What if we just look at aggregate coding, etc. ability?
# data_tall_scaled = data_hc_scaled[,c(1,2,27,28,29)]
# SFg <- stepFlexclust(data_tall_scaled, nrep=10, k = 2:10) 
# plot(SFg, type = "l", ylab = "sum of within cluster distances", 
#      main = 'K-means clustering scree plot')
# Seems to add weight for 3 clusters. Let's do that on the above data. 

k3_clust <- cclust(data_MDS,k=3)
save(data_MDS, file = "data_for_kmeans.RData")
plot(k3_clust, project = princomp(data_MDS), main = "k=3 k-means clusters, projected using PCA")
# ... OK

#Below doesn't work yet ... 
plot3D(data_MDS, main = "data_MDS 3D scatterplot, with labeled clusters", type = "n")
text3D(data_MDS, texts = clusters(k3_clust), color = clusters(k3_clust)+1, cex = 0.8)

# Obtain barplot for the cclust result 
barplot(k3_clust) 
# Ok, we get similar results to before. 3rd cluster (15%) consists of younger than average 
# people who are better at coding than the math/data science of the course 

round(clusterSim(k3_clust, symmetric = TRUE), 3)
round(clusterSim(k3_clust, method = "centers", symmetric = FALSE), 3)
# Essentially, the 2nd and 3rd clusters are more similar than 1 & 2 or 1 & 3 

# [,1]  [,2]  [,3]
# [1,] 1.000 0.130 0.468
# [2,] 0.130 1.000 0.606
# [3,] 0.468 0.606 1.000

km_labels <- clusters(k3_clust)
data_hc_scaled$km_cluster = km_labels

#OK, time to launch this on a web app. 

# DBSCAN and OPTICS ----
kNNdistplot(data_MDS, k =  3) #Knee at 3.6, similar result if k=3. So set eps to that value 
db <- dbscan(data_MDS, eps = 2.5, minPts = 3)

op <- optics(data_MDS, eps = 2.5)

# Try with reduced dimensional data 

kNNdistplot(QBs2$points, k =  5) #Knee at eps = 1
db2 <- dbscan(QBs2$points, eps = 1.5, minPts = 5)

op2 <- optics(QBs2$points, eps = 1.5)
op2$order #get order 
plot(op2) # Gee, there are a lot of points far from each other 
res2 <- optics_cut(op2, eps_cl = 1)
  
