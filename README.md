# Automatic-Data-Quality-Analysis-
Algorithm for Automatic Data Quality Analysis (NUmeric)
---
title: "Data distribution report"
author: "RaviShankar"
date: "Monday, June 27, 2016"
output: pdf_document
---
```{r, include=FALSE}
Tempturquoise <- rgb(42/255,210/255,201/255,1)
Temppurple <- rgb(97/255,71/255,103/255,1)
Temporange <- rgb(255/255,141/255,105/255,1)
Tempdsteel <- rgb(95/255,122/255,118/255,1)
Tempgray <- rgb(198/255,201/255,202/255,1)
Tempdgray <- rgb(128/255,130/255,133/255,1)
Tempgreen <- rgb(1/255,169/255,130/255,1)
Tempbronze <- rgb(128/255,116/255,110/255,1)
Tempslate <- rgb(66/255,85/255,99/255,1)
Temp_palete <- c(Tempturquoise,Temppurple,Temporange,Tempdsteel,Tempgray,Tempdgray,Tempgreen,Tempbronze,Tempslate)
Temp_palete2 <- colorRampPalette(c(Tempgreen, Tempgray, Temporange))
```

```{r, message=F, warning=F, include=FALSE}
list.of.packages <- c("ggplot2","tabplot","PerformanceAnalytics","corrplot","dplyr","psych","mice","VIM","fitdistrplus","DescTools","RColorBrewer","gplots","plotrix", "forecast", "qtlcharts", "AppliedPredictiveModeling", "digest", "stringi", "gridExtra", "grid")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages, repos = 'http://cran.rstudio.com')
list.of.packages.add <- c("highr","printr")
new.packages.add <- list.of.packages.add[!(list.of.packages.add %in% installed.packages()[,"Package"])]
if(length(new.packages.add)>0) install.packages(c('highr', 'printr'), type = 'source', repos = c('http://yihui.name/xran', 'http://cran.rstudio.com'))
list.of.packages.add.1 <- c("ClassDiscovery")
new.packages.add.1 <- list.of.packages.add.1[!(list.of.packages.add.1 %in% installed.packages()[,"Package"])]
if(length(new.packages.add.1)>0) install.packages(c('ClassDiscovery'), type = 'source', repos = c('http://R-Forge.R-project.org'))
list.of.packages.add.2 <- c("ggplus")
new.packages.add.2 <- list.of.packages.add.2[!(list.of.packages.add.2 %in% installed.packages()[,"Package"])]
if(length(new.packages.add.2)>0) install_github("guiastrennec/ggplus")
pacman::p_load(ggplot2,tabplot,PerformanceAnalytics,corrplot,dplyr,psych,mice,VIM,fitdistrplus,DescTools,RColorBrewer,gplots,plotrix, forecast, qtlcharts, highr, printr, AppliedPredictiveModeling, ClassDiscovery, digest, stringi, ggplus, gridExtra, grid)
data(segmentationOriginal)
table = segmentationOriginal
```

```{r, message=F, warning=F, include=FALSE}
options(scipen=999)
varlist <- function(df = NULL, type = c("numeric","integer","factor","character"), pattern = "", exclude = NULL){
  vars <- character(0)
  if (any(type %in% "numeric")) {vars <- c(vars, names(df)[sapply(df,is.numeric)])}
  if (any(type %in% "integer")) {vars <- c(vars, names(df)[sapply(df,is.integer)])}
  if (any(type %in% "factor")) {vars <- c(vars, names(df)[sapply(df,is.factor)])}
  if (any(type %in% "character")) {vars <- c(vars, names(df)[sapply(df,is.character)])}
  vars[(!vars %in% exclude) & grepl(vars,pattern=pattern)]
}
num_all = table[,varlist(table, type = "numeric")]
char_only = table[,varlist(table, type = "character")]
factor_only = table[,varlist(table, type = "factor")]
# Are integer variables really factors?
mean_freq_int = vector("numeric", ncol(num_all))
mean_freq_ratio_int = vector("numeric", ncol(num_all))
mean_freq_logical_int = vector("logical", ncol(num_all))
for(i in 1:ncol(num_all)){
	mean_freq_int[i] = mean(table(num_all[,i]))
	mean_freq_ratio_int[i] = mean_freq_int[i]/length(num_all[,i])
	mean_freq_logical_int[i] = ifelse(mean_freq_ratio_int[i] > 0.10,T,F)
}
num_only = num_all[,!mean_freq_logical_int]
int_only = num_all[,mean_freq_logical_int]
```

## Correlation Matrix

The plot below is called a correlation plot - a graphical display of a correlation matrix which is used to investigate the pairwise dependence between multiple variables. The correlation coefficients are based on the Pearson's approach.
```{r, message=F, warning=F, comment="", fig.width = 11, fig.height = 9, echo=FALSE}
m <- cor(num_only, use="pairwise.complete.obs")
if (dim(m) > 50) {
hc.rows = hclust(dist(m))
height = round(0.70 * max(hc.rows$height))
ct = cutree(hc.rows, h = height)
pl0 = vector("list", height*height)
n = 0
for (i in 1:height) {
for (j in 1:height) {
n = n + 1
pl0[[n]] <- ggplot(melt(assign(paste("m",i,j,sep = ""),m[ifelse(ct == i,T,F),ifelse(ct == j,T,F)])), aes(Var2, Var1, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "#01A982FF", high = "#FF8D69FF", mid = "#C6C9CAFF", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation", guide=FALSE) + theme(axis.text.x = element_text(angle = 90, hjust = 1,size=6), axis.text.y = element_text(size=6), axis.title.x=element_blank(), axis.title.y=element_blank()) + coord_fixed()
}
}
ml0 <- marrangeGrob(grobs=pl0, nrow=2, ncol=1)
ml0
} else{
  corrplot(m, order = "hclust", cl.pos = "b", cl.cex = 0.5, tl.cex = 0.5)
}
```
\pagebreak

## Box-plots

Boxplot is a non-parametric graphical depiction of numerical data through quartiles. The bottom and top of the box are the first and third quartiles, and the band inside the box is the median. The spacing in the box indicates the degree of dispersion (spread) in the data. The outliers appear as individual points.
```{r, message=F, warning=F, comment="", fig.width = 11, fig.height = 12, echo=FALSE}
meltData <- melt(num_only)
pl <- lapply(1:ncol(num_only), function(x) ggplot(meltData[meltData$variable == colnames(num_only)[x],], aes(variable, value)) + geom_boxplot(aes(fill=variable)) + theme(legend.position="none"))
ml <- marrangeGrob(grobs=pl, nrow=4, ncol=4)
ml
```
\pagebreak

## Density plots

Histogram is a poor method for determining distributions because it is strongly affected by the number of bins used. Density plots are a much more effective way to view the distribution of a variable.
```{r, message=F, warning=F, comment="", fig.width = 11, fig.height = 12, echo=FALSE}
meltData <- melt(num_only)
pl1 <- lapply(1:ncol(num_only), function(x) ggplot(meltData[meltData$variable == colnames(num_only)[x],], aes(variable)) + geom_density(aes(fill=variable)) + theme(legend.position="none"))
ml1 <- marrangeGrob(grobs=pl1, nrow=4, ncol=4)
ml1
```
\pagebreak

## Moments

Moment is a specific quantitative measure, used to summarize the distribution of data. The various moments presented below are Mean, Variance, Skewness and Kurtosis.
```{r, message=F, warning=F, comment="", fig.width = 11, fig.height = 9, echo=FALSE}
desc_num_only = describe(num_only)
moments_num_only = cbind(rownames(desc_num_only), round(desc_num_only$mean,4), round((desc_num_only$sd*desc_num_only$sd),4), round(desc_num_only$skew,4), round(desc_num_only$kurtosis,4))
colnames(moments_num_only) = c("Variable", "Mean", "Variance", "Skewness", "Kurtosis")
moments_num_only_out = as.data.frame(moments_num_only)
suppressMessages(library(knitr))
kable(moments_num_only_out, digits = 2)
```
\pagebreak

## Skewness-Kurtosis plots

A skewness-kurtosis plot proposed by Cullen and Frey (1999) is given for the empirical distribution. On this plot, values for common distributions are also displayed as a tool to help the choice of distributions to fit to data.
```{r, message=F, warning=F, comment="", fig.width = 11, fig.height = 12, echo=FALSE}
lyt <- rbind(c(1, 2), c(3, 4), c(5, 6), c(7, 8))
layout(lyt)
for (i in 1:ncol(num_only)) {
  descdist(num_only[complete.cases(num_only[,i]),i],discrete = FALSE,boot=1000)
  mtext(colnames(num_only[i]))
}
```
\pagebreak

## Box-Cox transformations

Box-Cox transformations suggest a statistical approach to resolving skewness in data. It constitutes a family of transformations that are indexed by a parameter, denoted by $\lambda$:

$$
\textit{x}^{*} = \left\{
        \begin{array}{ll}
            \frac{x^\lambda-1}{\lambda} & \lambda \neq 0 \\
             log(x) & \lambda = 0
        \end{array}
    \right.
$$

In addition to the log transformation, this family can identify square transformation ($\lambda$ = 2), square root ($\lambda$ = 0.5), inverse ($\lambda = {-1}$), and others in-between. Using the training data, $\lambda$ is estimated as below.
```{r, message=F, warning=F, comment="", fig.width = 11, fig.height = 9, echo=FALSE}
lambdas = vector("numeric", ncol(num_only))
for(i in 1:ncol(num_only)) {
  lambdas[i] = BoxCox.lambda(num_only[,i],lower=0)
}
lambda_table = cbind(data.frame(variable = colnames(num_only)), data.frame(lambdas))
kable(lambda_table, digits = 10)
```

Note: When the $\lambda$ is almost close to zero, it is a recommendation to use a log transformation of the variable.
