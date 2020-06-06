#4.
#Hierarchical Clustering
syms1=c('GOOGL', 'AMZN', 'AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 
        'XOM', 'SLB', 'COP', 'JPM', 'WFC', 'USB', 'AXP',
        'WMT', 'TGT', 'HD', 'COST')
df=sp500_px[row.names(sp500_px)>='2011-01-01', syms1]
d=dist(t(df))
##Calculate the distance
hcl=hclust(d)
##Hierarchical Clustering implement with hclust

#Dendrogram
plot(hcl)
##Every leaves are records each.
##The length of branch is distance gap between each clusters.
##The profit of Google and Amazon is significantly different with other stocks.
##Other stocks create group natually.
##Energy, finance, consumer goods were sorted by sub tree.

cutree(hcl,k=4)
##Set up the number as 4 that want to extract.
##Google and Amazon belong to independent cluster each. 
##Oil stocks(XOM,CVX,SLB,COP) are belong to one another cluster.
##The rest of them are belong to one another cluster.
