library(readr)
library(tidyr)
library(ggplot2)
library(caret)
library(reshape2)
library(plotly)
library(gganimate)

data <- read_csv("/mnt/raw/csvs/2016-02.csv")

data_sub <- data

data_pivot <- gather(data_sub[,c("Date","3311HS181A.PV")], "feature", "value", -Date)

ggplot(data_pivot) + geom_line(aes(x=Date, y=value, col=feature))

nzv <- nearZeroVar(data_sub, saveMetrics= TRUE)
nzv_col <- !nzv$nzv

data_nzv <- data_sub[,nzv_col]

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

cormat <- reorder_cormat(round(cor(data_nzv[,c(2:55,57,60:103)]),2))

cormat[lower.tri(cormat)] <- NA
melted_cormat <- melt(cormat)

gg <-              
  ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
ggplotly(gg)


james <- data_nzv[,c('3311WI671.PV',
                     'SY23015.MV',
                     'SY23015.MV',
                     '1101047CL1.CPV2',
                     'WIC22026.SV',
                     'SIC22371.MV')]
cormat <- reorder_cormat(round(cor(james[!is.na(james$`3311WI671.PV`),]),2))

cormat[lower.tri(cormat)] <- NA
melted_cormat <- melt(cormat)

gg <-              
  ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
ggplotly(gg)

preProcValues <- preProcess(data_nzv[,c(2:48,50:56,58:111)], method = c("medianImpute"))
data_impute <- predict(preProcValues, data_nzv[,c(2:48,50:56,58:111)])

comboInfo <- findLinearCombos(data_impute)


data.pca <- prcomp(scale(data_impute))

plot(data_nzv$Date, data.pca$x[,1])

plot.df <- data.frame(
  Date  = data_nzv$Date,
  PC1   = data.pca$x[,1],
  PC2   = data.pca$x[,3],
  Vomit = ifelse(data_nzv$`3311WI671.PV` > 300,"Vomit","Smooth Sailing")
)

p <- ggplot(plot.df[1:1000,], aes(x=PC1, y=PC2, frame=Date)) +
  geom_point(aes(cumulative=T)) 

gg_animate(p, interval=.05)

ggplot(plot.df, aes(x=PC1, y=PC2, col=Vomit)) +
  geom_point() 
