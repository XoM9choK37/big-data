#  Пример классификации с помошью кластерного анализа

#  Шаг 1.  Чтение данных. Обратите внимание на опцию: dec=','
protein.01 <- read.table("D:/BigData/Practical_R/Protein Consumption in Europe.csv", header = TRUE,";", dec=',')

#   Шаг 2.  Удаление пропущенных значений
#   В данной задаче пропущенных значений нет.
#   Удалять нечего.

#   Шаг 3.  Стандартизация переменных.
#   В данной задаче переменные существенно различны.
#   Стандартизировать надо.
#protein.02 <- scale(protein.01[,2:10], center = TRUE, scale = TRUE)
#   Вариант 2 - к минимуму 0 и максимуму 1

protein.02<-protein.01[,-1] #Исключим колонку "Страна"
maxs <- apply(protein.02, 2, max)
mins <- apply(protein.02, 2, min)
str(protein.02)

protein.02 <- scale(protein.02, center = mins, scale = maxs - mins)

#Вернем колонку "Страна"
Countries<-protein.01$Country
protein.02<-data.frame(Countries,protein.02)

#  Создаем матрицу попарных расстояний (по умолчанию - Евклидово расстояние)
dist.protein <- dist(protein.02 [,2:10])

#  Проводим кластерный анализ, 
#  результаты записываем в список clust.protein
# hclust ожидает матрицу расстояния, а не исходные данные.

clust.protein <- hclust(dist.protein, "ward.D")

#  Смотрим краткую сводку результатов анализа
clust.protein 

#  Шаг 4.  Построение дендрограммы c 5-ю кластерами

plot(clust.protein, labels = protein.02$Countries)
rect.hclust(clust.protein, k=5, border="red")
abline(h = 1.5, col = "blue", lwd='3') # h - horizontal line, col - color

 plot(1:24, clust.protein$height, type='b') # каменная осыпь
#  Разделим Страны на 5 кластеров
#  Вектор groups содержит номер кластера, в который попал классифицируемый объект 
groups <- cutree(clust.protein, k=5) 
# Разрезает дерево, например, полученное в результате hclust, на несколько групп 
# путем указания желаемого количества групп или высоты среза.
groups

dend <- as.dendrogram(clust.protein)
if(!require(dendextend)) install.packages("dendextend"); 
library(dendextend)
dend <- color_branches(dend, k = 5) 
plot(dend)
#dev.off() 


#  Выведем страны соответсвенно сформированным кластерам 
protein.01[groups==1, 1]
protein.01[groups==2, 1]
protein.01[groups==3, 1]
protein.01[groups==4, 1]
protein.01[groups==5, 1]

#  Для каждого вида еды определяем, 
#  какая доля потребителей в среднем кластере приобретала этот вид еды

#  в 1-ом кластере
g1<-colMeans(protein.01[groups==1, 2:10])
#  во 2-ом кластере
g2<-colMeans(protein.01[groups==2, 2:10])
#  в 3-ем кластере
g3<-colMeans(protein.01[groups==3, 2:10])
#  во 4-ом кластере
g4<-colMeans(protein.01[groups==4, 2:10])
#  в 5-oм кластере
g5<-colMeans(protein.01[groups==5, 2:10])

df<-data.frame(g1,g2,g3,g4,g5); df
df1<-t(df); df1
barplot(df1, ylim=range(pretty(c(0, 160))), main="Распредление пищевых предпочтенй в странах Европы", col=c("magenta","red","yellow","blue","green"),legend=rownames(df1))

#Построим боксплот, который поможет нам убедиться, что мы 
#действительно имеем 5 классов существенно отличных друг от друга:

boxplot(df, col=c("magenta","red","yellow","blue", "green"))
# Для датасета assess
#barplot(df1, ylim=range(pretty(c(0, 60))), main="Распредление претендентов", col=c("magenta","red","yellow","blue"),legend=rownames(df1))

#############################################################

#  Шаг 4.  Построение дендрограммы c 3-мя кластерами

plot(clust.protein, labels = protein.02$Countries)
rect.hclust(clust.protein, k=3, border="red")
abline(h = 2.4, col = "blue", lwd='3') # h - horizontal line, col - color

plot(1:24, clust.protein$height, type='b') # каменная осыпь
#  Разделим Страны на 5 кластеров
#  Вектор groups содержит номер кластера, в который попал классифицируемый объект 
groups3 <- cutree(clust.protein, k=3) 
# Разрезает дерево, например, полученное в результате hclust, на несколько групп 
# путем указания желаемого количества групп или высоты среза.
groups3

dend <- as.dendrogram(clust.protein)
if(!require(dendextend)) install.packages("dendextend"); 
library(dendextend)
dend <- color_branches(dend, k = 3) 
plot(dend)
#dev.off() 


#  Выведем страны соответсвенно сформированным кластерам 
protein.01[groups==1, 1]
protein.01[groups==2, 1]
protein.01[groups==3, 1]

#  Для каждого вида еды определяем, 
#  какая доля потребителей в среднем кластере приобретала этот вид еды

#  в 1-ом кластере
g1<-colMeans(protein.01[groups==1, 2:10])
#  во 2-ом кластере
g2<-colMeans(protein.01[groups==2, 2:10])
#  в 3-ем кластере
g3<-colMeans(protein.01[groups==3, 2:10])


df<-data.frame(g1,g2,g3); df
df1<-t(df); df1
barplot(df1, ylim=range(pretty(c(0, 160))), main="Распредление пищевых предпочтенй в странах Европы", col=c("magenta","red","yellow","blue","green"),legend=rownames(df1))

#Построим боксплот, который поможет нам убедиться, что мы 
#действительно имеем 5 классов существенно отличных друг от друга:

boxplot(df, col=c("magenta","red","yellow"))
# Для датасета assess
#barplot(df1, ylim=range(pretty(c(0, 60))), main="Распредление претендентов", col=c("magenta","red","yellow","blue"),legend=rownames(df1))

##############################################################
#############             K-means     ########################

install.packages("factoextra")

library (factoextra)
library (cluster)

protein.02 <- na.omit(protein.02)
rownames(protein.02)<-protein.02[,1]
protein.03 <- protein.02[,-1] # убираем названия
head(protein.03)
fviz_nbclust( protein.03, kmeans, method = "wss") 
#calculate gap statistic based on number of clusters
gap_stat <- clusGap(protein.03,
                    FUN = kmeans,
                    nstart = 5,
                    K.max =10,
                    B = 5)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat) 
# Silhouette method
fviz_nbclust(protein.03, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

#Алгоритм на основе консенсуса
install.packages('parameters')
library(parameters)

n_clust <- n_clusters(protein.03,
                      package = c("easystats", "NbClust", "mclust"),
                      standardize = FALSE)
n_clust
plot(n_clust)

set.seed(123)

#perform k-means clustering with k =3  clusters
km <- kmeans(protein.03, centers = 5, nstart = 30)


#view results
fviz_cluster(km, data = protein.03)

pairs(protein.03)

########################################### iris ####################
set.seed(123)

# Data preparation for iris
# +++++++++++++++
data("iris")
head(iris)
ir<-iris[,-5]
fviz_nbclust( ir, kmeans, method = "wss") 
#calculate gap statistic based on number of clusters
gap_stat <- clusGap(ir,
                    FUN = kmeans,
                    nstart = 5,
                    K.max =10,
                    B = 5)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat) 
# Remove species column (5) and scale the data
iris.scaled <- scale(iris[, -5])

# Silhouette method
fviz_nbclust(ir, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

#Алгоритм на основе консенсуса
install.packages('parameters')
library(parameters)

n_clust <- n_clusters(iris[, -5],
                      package = c("easystats", "NbClust", "mclust"),
                      standardize = FALSE)
n_clust
plot(n_clust)



# K-means clustering
# +++++++++++++++++++++
km.res <- kmeans(iris.scaled, 3, nstart = 10)

# Visualize kmeans clustering
# use repel = TRUE to avoid overplotting
fviz_cluster(km.res, iris[, -5], ellipse.type = "norm")


# Change the color palette and theme
fviz_cluster(km.res, iris[, -5],
             palette = "Set2", ggtheme = theme_minimal())

# Show points only
fviz_cluster(km.res, iris[, -5], geom = "point")
# Show text only
fviz_cluster(km.res, iris[, -5], geom = "text")

# PAM clustering
# ++++++++++++++++++++
require(cluster)
pam.res <- pam(iris.scaled, 3)
# Visualize pam clustering
fviz_cluster(pam.res, geom = "point", ellipse.type = "norm")

# Hierarchical clustering
# ++++++++++++++++++++++++
# Use hcut() which compute hclust and cut the tree
hc.cut <- hcut(iris.scaled, k = 3, hc_method = "complete")
# Visualize dendrogram
fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
# Visualize cluster
fviz_cluster(hc.cut, ellipse.type = "convex")

packages <- c('ggplot2', 'dplyr', 'tidyr', 'tibble')
# install.packages(packages)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

iris %>% 
  ggplot(aes(Petal.Length, Petal.Width, color = Species))+geom_point()

plot(iris[,-5])

plot(iris[,-5],main= "Ирисы", col = c("red","green","blue"))
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(iris[,-5],main= "Ирисы по сортам",pch = 19,  cex = 0.8,
      col = my_cols[iris$Species], 
      lower.panel=NULL)

#install.packages("scatterplot3d")
library("scatterplot3d")
colors <-  c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(iris$Species)]

s3d <- scatterplot3d(iris[,1:3], main= "Ирисы по сортам", pch = 16, color=colors)
legend(s3d$xyz.convert(7.5, 3, 4.5), legend = levels(iris$Species),
       col =  c("#999999", "#E69F00", "#56B4E9"), pch = 16)

library(mclust)
m <- Mclust(iris[,-5])
plot(m, what = "classification")
m <- Mclust(iris[,-5], G=3)
summary(m)      

