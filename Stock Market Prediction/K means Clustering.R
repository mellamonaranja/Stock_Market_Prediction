#3.
#K means Clustering
df=sp500_px[row.names(sp500_px)>'2011-01-01', c('XOM', 'CVX')]
km=kmeans(df,centers =4, nstart=1)
df$cluster=factor(km$cluster)
head(df)

centers=data.frame(cluster=factor(1:4),km$centers)
centers
##Cluster1,2 means a bear market, on the other hands 3,4 means a rising market.

ggplot(data=df, aes(x=XOM, y=CVX, color=cluster, shape=cluster))+
    geom_point(alpha=.3)+
    scale_shape_manual(values = 1:4,
                       guide = guide_legend(override.aes=aes(size=1)))+
    geom_point(data=centers, aes(x=XOM, y=CVX),size=2, stroke=2)+
    theme_bw()+
    scale_x_continuous(expand=c(0,0), lim=c(-2, 2))+
    scale_y_continuous(expand=c(0,0), lim=c(-2.5, 2.5))

#K-means Algorithm
syms=c('AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 'XOM', 'SLB', 'COP',
       'JPM', 'WFC', 'USB', 'AXP', 'WMT', 'TGT', 'HD', 'COST')
df=sp500_px[row.names(sp500_px)>='2011-01-01',syms]
set.seed(10010)
km=kmeans(df,centers=5,nstart = 10)
##In order to find 5clusters, implement 10 times.
km$size
centers=km$centers
centers=as.data.frame(t(centers))
names(centers)=paste('Cluster',1:5)
centers$Symbol=row.names(centers)
centers=gather(centers, 'Cluster','Mean',-Symbol)
centers$Color=centers$Mean>0
ggplot(centers,aes(x=Symbol, y=Mean, fill=Color))+
    geom_bar(stat='identity', position = "identity", width=.75)+
    facet_grid(Cluster ~ ., scales='free_y')+
    guides(fill=FALSE) +
    ylab('Component Loading')+
    theme_bw()+
    theme(axis.title.x = element_blank(),
          axis.text.x  = element_text(angle=90, vjust=0.5))
##Cluster1,2 represent rise and fall on stock market.
##Cluster3,5 feature consumer goods stock rises and energy stock falls.
##Cluster4 represents energy stock rise and consumer goods stock falls.

#Elbow Method
pct_var=data.frame(pct_var=0,num_clusters=2:14)
totalss=kmeans(df,centers=14,nstart=50, iter.max = 100)$totss
for (i in 2:14) {
    pct_var[i-1,'pct_var']=kmeans(df,centers = i,
                                  nstart=50, iter.max = 100)$betweenss/totalss
}
ggplot(pct_var,aes(x=num_clusters, y=pct_var))+
    geom_line()+
    geom_point()+
    labs(y='% Variance Explained', x='Number of Clusters')+
    scale_x_continuous(breaks=seq(2, 14, by=2))+
    theme_bw()
