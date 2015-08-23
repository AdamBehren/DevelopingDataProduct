library(shiny)
library(Quandl)
library(ggplot2)
library(qualityTools)


function(input, output) {

  #Quandl.auth("Auth token")
  sp_data <- Quandl("YAHOO/ASX_RIO_AX")
  #sp_data <- read.csv("C:/Users/Adam/Documents/School/Coursera/DataProducts/RIO.csv")
  
  #daily return
  for (i in 1:(length(sp_data$Close)-1)) {
    sp_data$previous[i] <- sp_data$Close[i+1]
  }
  sp_data$returns <- 100*(sp_data$Close - sp_data$previous)/sp_data$previous
  
  #45 day moving average
  for (i in 1:length(sp_data$Close)){
    sp_data$mvavg[i] <- mean(sp_data[i:(i+45),]$Close)
  }
  
  #subset data based on date
  sp_data$Date <- as.Date(sp_data$Date)
  date_min <- as.Date("12/01/2015", "%d/%m/%Y")
  date_max <- as.Date("18/07/2015", "%d/%m/%Y")
  
  x <- getDatePointer(date_min,sp_data)
  y <- getDatePointer(date_max,sp_data)
  sp_data <- sp_data[x:y,]

  #risk metrics
  #CDF - Var
  sp_cdf <- ggplot(sp_data, aes(returns)) + stat_ecdf()
  sp_cdf <- sp_cdf + labs(title = "Cumulative Distribution Function of Past Returns", x = "Returns(percent)", y = "Probability")
  
  #QQPLOT returns vs normal
  qq_norm<- ggplot(sp_data, aes(sample=returns)) + stat_qq(distribution=qnorm) + geom_abline(intercept=0,slope=1)
  qq_norm <- qq_norm + labs(title = "Normal Q-Q Plot", x = "Normal Theoretical Quantiles", y = "Normal Sample Quantiles")
  sp_data$logreturns <- log(abs(sp_data$returns))
  qq_lognorm<- ggplot(sp_data, aes(sample=logreturns)) + stat_qq(distribution=qnorm) + geom_abline(intercept=0,slope=1)
  qq_lognorm <- qq_lognorm + labs(title = "Log Normal Q-Q Plot", x = "Log-Normal Theoretical Quantiles", y = "Log-Normal Sample Quantiles")
  
  #Returns plot
  mean_returns <- mean(sp_data$returns)
  sp_returns <- ggplot(sp_data, aes(y=returns, x=Date)) + geom_line(colour = "red") + geom_vline(xintercept=date_max,linetype="dashed", colour="blue",size=1)
  
  #TAB Returns
  output$tab1A <- renderPlot({
    sp_plot <- ggplot(sp_data) + geom_line(aes(x=Date, y=Close))
    sp_plot <- sp_plot + labs(title = "Stock Close Price with 45 Day Moving Average", x = "Date", y = "Price($)")
    if (input$check == TRUE) {
      sp_plot <- sp_plot + geom_line(aes(x=Date, y=mvavg), colour = "blue", linetype="dashed", size=1)
    }
    else {}
    sp_plot})
  output$tab1B <- renderPlot({
    #histogram of returns
    sp_hist <- ggplot(sp_data, aes(x=returns, y = ..count../sum(..count..))) 
    sp_hist <- sp_hist +  geom_histogram(binwidth=0.5,
                                         fill = "#003399",
                                         col= "#FF0000",
                                         alpha= I(.2),
                                         xlim=c(-10,10))
    sp_hist <- sp_hist + labs(title = "Occurance of Past Returns", x = "Returns(percent)", y = "Density") +  xlim(c(-10,10))
    if (input$check2 == TRUE){
      sp_hist <- sp_hist + geom_vline(aes(xintercept=mean(returns)), colour="blue", linetype = "dashed", size=1)
    }
    sp_hist}) 
  output$tab1C <- renderPlot({sp_returns})
  
  #TAB Return Distribution
  output$plot2A <- renderPlot({sp_plot <- ggplot(sp_data) + geom_line(aes(x=Date, y=Close))
  sp_plot <- sp_plot + labs(title = "Stock Close Price with 45 Day Moving Average", x = "Date", y = "Price($)")
  if (input$check == TRUE) {
    sp_plot <- sp_plot + geom_line(aes(x=Date, y=mvavg), colour = "blue", linetype="dashed", size=1)
  }
  else {}
  sp_plot})
  output$plot2B <- renderPlot({plot(sp_cdf)})
  output$plot2C <- renderPlot({qq_norm})
  output$plot2D <- renderPlot({qq_lognorm})
  
}