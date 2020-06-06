#2.
library(dplyr)
library(tidyr)
library(ggplot2)
library(ascii)
install.packages('lubridate')
install.packages('base')
library(lubridate)
install.packages('ellipse')
install.packages('graphics')
library(ellipse)
install.packages('mclust')
library(mclust)
library(cluster)

sp500_px=read.csv(file.path(PSDS_PATH, 'data', 'sp500_px.csv'), row.names = 1)
sp500_sym=read.csv(file.path(PSDS_PATH, 'data', 'sp500_sym.csv'), stringsAsFactors = FALSE)

#PCA(Principal Components Analysis)
oil_px=as.data.frame(scale(oil_px, scale=FALSE))
oil_px=sp500_px[, c('CVX', 'XOM')]
pca=princomp(oil_px)
pca$loadings
##Component 1 is mean of CVX, XOM reflected correlation between two companies.
##Component 2 is the point when stock price is change.
loadings=pca$loadings

ggplot(data=oil_px, aes(x=CVX, y=XOM))+
    geom_point(alpha=.3)+
    scale_shape_manual(values=c(46))+
    stat_ellipse(type='norm', level=.99, color='grey25')+
    geom_abline(intercept=0, slope=loadings[2,1]/loadings[1,1], color='grey25', linetype=2)+
    geom_abline(intercept=0, slope=loadings[2,2]/loadings[1,2], color='grey25', linetype=2)+
    scale_x_continuous(expand=c(0,0), lim=c(-3, 3))+
    scale_y_continuous(expand=c(0,0), lim=c(-3, 3))+
    theme_bw()  
##Stock price tends to move as a group.

#Principal Components Interpret 
syms=c( 'AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 'XOM', 'SLB', 'COP',
        'JPM', 'WFC', 'USB', 'AXP', 'WMT', 'TGT', 'HD', 'COST')    
top_cons=sp500_px[row.names(sp500_px)>='2011-01-01', syms]
sp_pca=princomp(top_cons)    
screeplot(sp_pca)    
##First component relatively high volitality.

loadings=sp_pca$loadings[,1:5]
loadings=as.data.frame(loadings)    
loadings$Symbol=row.names(loadings)    
loadings=gather(loadings, 'Component','Weight',-Symbol)
head(loadings)

loadings$Color=loadings$Weight>0
ggplot(loadings,aes(x=Symbol, y=Weight, fill=Color))+
    geom_bar(stat='identity', position = "identity", width=.75)+
    facet_grid(Component ~ ., scales='free_y')+
    guides(fill=FALSE) +
    ylab('Component Loading')+
    theme_bw()+
    theme(axis.title.x = element_blank(),
          axis.text.x  = element_text(angle=90, vjust=0.5))
##Comp1 presents every components affected by stock market.  
##Comp2 catches price fluctuation on oil related stocks.
##Comp3 presents the motion between AAPL and COST is opposite.
##Comp4 presents the motion between SLB and rest of them is opposite.
##Comp5 presents financial companies are featured.

