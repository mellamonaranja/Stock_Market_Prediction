#6.
#Variable Scailing
syms <- c('GOOGL', 'AMZN', 'AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 'XOM', 
          'SLB', 'COP', 'JPM', 'WFC', 'USB', 'AXP', 'WMT', 'TGT', 'HD', 'COST')
top_15 <- sp500_px[row.names(sp500_px)>='2011-01-01', syms]
sp_pca1 <- princomp(top_15)
screeplot(sp_pca1)
##The gap between Comp1 and Comp2 is bigger than other components.
##Because it means one or two variable control all loading.

round(sp_pca1$loading[,1:2],3)
##The motion of Google and Amazon control all volatility.
