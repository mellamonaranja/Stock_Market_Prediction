#5.
#Mixture Models
df=sp500_px[row.names(sp500_px)>='2011-01-01', c('XOM', 'CVX')]
mcl=Mclust(df)
summary(mcl)

cluster=factor(predict(mcl)$classification)
ggplot(data=df, aes(x=XOM, y=CVX, color=cluster, shape=cluster))+
    geom_point(alpha=.8)+
    theme_bw()+
    scale_shape_manual(values = c(46, 3),
                       guide = guide_legend(override.aes=aes(size=2))) 
##Get two clusters about stock price earning datas.

summary(mcl,parameters = T)$mean
summary(mcl,parameters=T)$variance

#BIC(Bayesian Information Criteria)
plot(mcl,what = 'BIC',ask=F)
##Select the most optimized model with BIC.
##More cluster, more increase the number of parameter, better goodness of fit.
