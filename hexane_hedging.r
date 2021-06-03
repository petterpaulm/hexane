
packages <- c("devtools", "quantmod", "dataframes2xls", "curl", "data.table", "dataframes2xls", 
                "openxlsx", "XML", "readxl", "zoo", "svDialogs", "class", "openxlsx",
                "xts","zoo", "TSA", "MTS", "ggplot2", "ggthemes", "cowplot", "moments",
                "plotmo", "plotrix", "TeachingDemos","mgcv","nlme","rpart","party","earth",
                "DALEX", "broom","breakDown", "recipes", "caret","TSA", "MTS", "ggplot2", "ggthemes", 
                "cowplot", "moments", "urca", "vars", "tsDyn", "scatterplot3d", "stargazer", 
                "data.table", "xtable", "lmtest", "strucchange", "stats", "e1071", "car", "lmtest", "readxl", 
                "lmtest","structchange","sandwich","stats","e1071","fBasics","qrmtools","fDMA",
                "roll","MTS","forecast","utils","tseries")
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.library(x, character.only = TRUE)
  }
})


setwd("G:/STRATEGICSOURCING/C.O.E. - Center Of Excellence/Indicators and Analisys/Packaging/00. CARGILL/54. PYTHON_PROJECTS/hexane/")
#dados <- read.xlsx('hexane_rbob_base.xlsx', colNames=TRUE, detectDates = T, na.strings = '#N/A')
dados <- read.xlsx('hexane_rbob_20210527.xlsx', sheet=1, colNames=TRUE, detectDates = T, na.strings = '#N/A')
head(dados)


par(mforw=c(1,2))
histogram_chart <- function(serie1, titleName) {
    barfill <- "#4271AE"
    barlines <- "#1F3552"
    datasim <- data.frame(serie1)

       e <- ggplot(datasim, aes(x = get(colnames(datasim)))) + 
            geom_histogram(aes(y = ..density..), fill = barfill, alpha = 0.6, bins=100)

        height_avg <- max(ggplot_build(e)$data[[1]]$y)/2
        horizontal_avg <- max(ggplot_build(e)$data[[1]]$x)/2

       p <- ggplot(datasim, aes(x = get(colnames(datasim)))) + 
            geom_histogram(aes(y = ..density..), fill = barfill, alpha = 0.6, bins=100) + 
            geom_density(colour = barlines, size=1.3) +
            ggtitle(paste0('Histogram for ', titleName)) +
                theme(plot.title = element_text(hjust = 0.5)) +
                    xlab(expression(bold('Returns'))) + 
                    ylab(expression(bold('Density'))) +
            geom_vline(xintercept = mean(datasim[,colnames(datasim)]), size = 1.3, colour ='#a40000', linetype = "dashed") +
            geom_text(aes(x=mean(datasim[,colnames(datasim)]), 
                        label=paste("\nAvg. Returns: ", round(mean(datasim[,colnames(datasim)])*100,4), "%"), y=height_avg), 
                        colour='#a40000', angle=90, text=element_text(size=2)) +
            geom_label(aes(x=horizontal_avg, y=height_avg),
                            label=paste0('Skewness/Assimetry ', ':', round(skewness(datasim),4), 
                                        '\n','Kurtosis ', ': ', round(kurtosis(datasim),4)),
                        colour=barlines, text=element_text(size=2)) + 
            stat_function(fun = dnorm, args = list(mean(serie1), sd(serie1)),
                      colour = "red", size=1.3) +
            geom_vline(xintercept = tail(datasim[,colnames(datasim)],1)[1], size = 1.3, colour = barlines, linetype = "dashed")

    p + theme_economist() + scale_color_economist() 
}



#gasoline_93_close <- na.omit(dados[0:dim(dados)[1],c(1,2,5,8,11)])
gasoline_93_close <- dados[,c('NYMEX.RBOB','NYMEX.RBOB.Future.curve','WTI','WTI.Future.curve')]
colnames(gasoline_93_close) <- c('DATE', 'gasoline_93_close','gasoline_87_close','Hexane_Special','Nat_Gas','Nymex')
#gasoline_93_close['DATE'] <- as.Date(gasoline_93_close[,1], format='%Y-%m-%d')

#gasoline_93_close['gasoline_93_close'] <- as.numeric(gasoline_93_close[,2])
#gasoline_93_close['gasoline_87_close'] <- as.numeric(gasoline_93_close[,3])
#gasoline_93_close['Hexane_Special'] <- as.numeric(gasoline_93_close[,4])
#gasoline_93_close['Nat_Gas'] <- as.numeric(gasoline_93_close[,5])
#gasoline_93_close['Nymex'] <- as.numeric(gasoline_93_close[,6])

nymex <- as.numeric(na.omit(dados[,'NYMEX.RBOB']))
nymex_future <- as.numeric(na.omit(dados[,'NYMEX.RBOB.Future.curve']))
wti <- as.numeric(na.omit(dados[,'WTI']))
wti_future <- as.numeric(na.omit(dados[,'WTI.Future.curve']))

return_nymex <- diff(nymex)/nymex[1:(length(nymex)-1)]
return_nymex_future <- diff(nymex_future)/nymex_future[1:(length(nymex_future)-1)]
return_wti <- diff(wti)/wti[1:(length(wti)-1)]
return_wti_future <- diff(wti_future)/wti_future[1:(length(wti_future)-1)]




#head(gasoline_93_close)
#tail(gasoline_93_close)

#ts_gasoline_93_close <- gasoline_93_close
#ts_gasoline_93_close <- ts(gasoline_93_close)
#head(ts_gasoline_93_close)


#ret_gasoline_93_close <- diff(gasoline_93_close[,2])/gasoline_93_close[1:(dim(gasoline_93_close)[1]-1),2]

#dados <- read.xlsx('hexane_rbob_base.xlsx', sheet=2, startRow = 3, colNames=TRUE, detectDates = T, na.strings = '#N/A')
#head(dados)
#dados['DATE'] <- as.Date(dados[,1], format='%Y-%m-%d')
#dados['PHACZ00'] <- as.numeric(dados[,4])

#hex_month <- data.frame(matrix(0, nrow=dim(dados)[1], ncol=4))
#hex_month$X1 <- dados[,1]
#hex_month$Month <- months(hex_month$X1) 
#hex_month$Year <- format(hex_month$X1, format = '%y') 
#hex_month$X2 <- dados[,4]
#hex_month_avg_2 <- aggregate(X2 ~ Month + Year, hex_month, mean)
#ret_hexane_special_close <- diff(hex_month_avg_2$X2)/hex_month_avg_2[1:(dim(hex_month_avg_2)[1]-1), c("X2")]

#ret_gasoline_87_close <- diff(gasoline_93_close[,3])/gasoline_93_close[1:(dim(gasoline_93_close)[1]-1),3]
#ret_nat_gas_close <- diff(gasoline_93_close[,5])/gasoline_93_close[1:(dim(gasoline_93_close)[1]-1),5]
#ret_nymex_close <- diff(gasoline_93_close[,6])/gasoline_93_close[1:(dim(gasoline_93_close)[1]-1),6]





histogram_chart_2 <- function(serie1, titleName, hist_bins, left_coord, right_coord) {
    barfill <- "#4271AE"
    barlines <- "#1F3552"
    serie1 <- as.numeric(na.omit(serie1))
    return_serie1 <- na.omit(diff(serie1)/serie1[1:(length(serie1)-1)])
    datasim <- data.frame(return_serie1)
    colnames(datasim) <- c('serie1')

       e <- ggplot(datasim, aes(x = get(colnames(datasim)))) + 
            geom_histogram(aes(y = ..density..), fill = barfill, alpha = 0.6, bins=hist_bins) + 
            geom_density(colour = barlines, size=1.3, fill = barlines)

        height_avg <- max(ggplot_build(e)$data[[1]][1])/2
        horizontal_avg <- max(ggplot_build(e)$data[[1]]$x)/3

        datasim_q <- quantile(datasim$serie1, 
                            probs=c(0, length(datasim[datasim$serie1 > tail(datasim[,colnames(datasim)],1)[1],])/dim(datasim)[1]))
        
        datasim$quant <- factor(findInterval(datasim$serie1,datasim_q))

        datasim$central <- datasim$serie1 < datasim_q[2]
        datasim$central <- datasim$serie1 < tail(datasim$serie1,1)[1]
    
        ggplot_dt_aux <- data.frame(ggplot_build(e)$data[[1]])

        ggplot_dt_aux[ggplot_dt_aux$x > tail(datasim$serie1,1)[1], c('y')] <- 0


       p <- ggplot(datasim, aes(x = get(colnames(datasim)[1]))) + 
            geom_histogram(aes(y = ..density..), fill = barfill, alpha = 0.6, bins=hist_bins) + 
            coord_cartesian(xlim = c(left_coord, right_coord)) +

            geom_density(colour = barlines, size=1.3, fill = "transparent") +

            ggtitle(paste0('Histogram for ', titleName)) +
                theme(plot.title = element_text(hjust = 0.5)) +
                    xlab(expression(bold('Returns'))) + 
                    ylab(expression(bold('Density'))) +
            geom_vline(xintercept = mean(datasim[,colnames(datasim)[1]]), size = 1.3, colour ='#a40000', linetype = "dashed") +
            geom_text(aes(x=mean(datasim[,colnames(datasim)[1]]), 
                        label=paste("\nAvg. Returns: ", round(mean(datasim[,colnames(datasim)[1]])*100,3), "%"), y=height_avg), 
                        colour='#a40000', angle=90) +
            geom_label(aes(x=horizontal_avg + tail(datasim[,colnames(datasim)[1]],1)[1]*5, y=height_avg*1.4),
                            label=paste0('Skewness/Assimetry ', ':', round(skewness(datasim[,colnames(datasim)[1]]),4), 
                                        '\n','Kurtosis ', ': ', round(kurtosis(datasim[,colnames(datasim)[1]]),4),
                                        '\n -----------------------------------------------------------------------',
                                        '\n', "Max. Potential Loss in 12mo (256days): USD ", round(sd(datasim[,colnames(datasim)[1]])*(1.29)*(256**0.5)*2.5,3),'mm [80% conf.level]',
                                        '\n', "Max. Potential Loss in 6mo (128days): USD ", round(sd(datasim[,colnames(datasim)[1]])*(1.29)*(128**0.5)*2.5,3),'mm [80% conf.level]', 
                                        '\n', "Max. Potential Loss in 3mo (64days): USD ", round(sd(datasim[,colnames(datasim)[1]])*(1.29)*(64**0.5)*2.5,3),'mm [80% conf.level]'),
                        colour=barlines) + 
             stat_function(fun = dnorm, args = list(mean(serie1), sd(serie1)),
                      colour = "red", size=1.3) +

            geom_vline(xintercept = tail(datasim[,colnames(datasim)[1]],1)[1], size = 1.3, colour = barlines, linetype = "dashed")

    p + theme_economist() + scale_color_economist()
}

dados <- read.xlsx('hexane_rbob_20210527.xlsx', sheet=2, colNames=TRUE, detectDates = T, na.strings = '#N/A')

histogram_chart_2(na.omit(dados[,3]), "Gasoline Unl.87 Daily Returns", hist_bins=100, left_coord = -0.15 , right_coord = 0.25)


setwd("c:/Users/p119124/Documents/")
dados <- read.xlsx('C5.xlsx', sheet=1, colNames=TRUE, detectDates = T, na.strings = '#N/A', startRow=3)
tail(dados)

#par(mforw=c(1,2))
histogram_chart_2(na.omit(dados[,2]), "C5 Daily Returns", hist_bins=100, left_coord = -0.15 , right_coord = 0.25)
histogram_chart_2(na.omit(dados[,3]), "Ethanol Daily Returns", hist_bins=100, left_coord = -0.15 , right_coord = 0.35)


dados <- read.xlsx('tabelaCompleta - ANP.xlsx', sheet=1, colNames=TRUE, detectDates = T, na.strings = '#N/A')




histogram_chart_2(na.omit(return_wti), "WTI Daily Returns", left_coord = 0.05 , right_coord = 1.25)




#length(gasoline_93_close[,2])
#tail(gasoline_93_close[,2])

#ma7_gasoline_93_close <- na.omit(filter(gasoline_93_close[,2], rep(1/5, 5)))
#ret_ma7_gasoline_93_close <- diff(ma7_gasoline_93_close)/ma7_gasoline_93_close[1:(length(ma7_gasoline_93_close)-1)]

#histogram_chart_2(ret_ma7_gasoline_93_close, "Gasoline 93 - Moving Average")


dt <- data.frame(x=c(1:200),y=rnorm(200))
dens <- density(dt$y)
df <- data.frame(x=dens$x, y=dens$y)
probs <- c(0, 0.25, 0.5, 0.75, 1)
quantiles <- quantile(dt$y, prob=probs)
df$quant <- factor(findInterval(df$x,quantiles))
ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + scale_x_continuous(breaks=quantiles) + scale_fill_brewer(guide="none")