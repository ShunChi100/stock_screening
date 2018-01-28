#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)


# Define server logic required to draw a histogram
shinyServer(function(input, output, stock) {
  
  stock <- read.csv("./stock_data_clean.csv")
  # data filtering, remove outliers
  stock <- filter(stock, stock$ROE_5Y<0.5, 
                  stock$ROE_5Y> -0.25, 
                  stock$DEratio_5Y>-0.5, 
                  stock$DEratio_5Y<3, 
                  stock$PEratio > 0, 
                  stock$PEratio < 40, 
                  stock$Profit_Margin_5Y >-0.2, 
                  stock$Profit_Margin_5Y < 0.4)
  
  stock_orig = stock

  Net_ave <- 10000*(1+mean(stock$Median_Q_Growth))^6
  
  Net <- data.frame(Screened = 0, Average = Net_ave)
  rownames(Net) <- "Net_Asset"
  
  
  output$Net <- renderTable({
    stock <- filter(stock, stock$ROE_5Y<input$ROE[2], 
                    stock$ROE_5Y> input$ROE[1], 
                    stock$DEratio_5Y>input$DEratio[1], 
                    stock$DEratio_5Y<input$DEratio[2], 
                    stock$PEratio > input$PERatio[1], 
                    stock$PEratio < input$PERatio[2], 
                    stock$Profit_Margin_5Y >input$Profit_Margin[1], 
                    stock$Profit_Margin_5Y < input$Profit_Margin[2])
    Net_new <- 10000*(1+mean(stock$Median_Q_Growth))^6
    
    Net$Screened = Net_new
    
    Net
    })
  
  output$distPlot <- renderPlot({
    
    stock <- filter(stock, stock$ROE_5Y<input$ROE[2], 
                    stock$ROE_5Y> input$ROE[1], 
                    stock$DEratio_5Y>input$DEratio[1], 
                    stock$DEratio_5Y<input$DEratio[2], 
                    stock$PEratio > input$PERatio[1], 
                    stock$PEratio < input$PERatio[2], 
                    stock$Profit_Margin_5Y >input$Profit_Margin[1], 
                    stock$Profit_Margin_5Y < input$Profit_Margin[2])
    
    
    if (input$graphtype == "Scatter"){
      # choose the x axis scale from the input
      if (input$regression == "Linear") {
        scatters <- scatter_plot(stock, "lm")
      }else{
        scatters <- scatter_plot(stock, "auto")
      }
      # show graphs in a grid
      grid.arrange(scatters[[1]],scatters[[2]],scatters[[3]],scatters[[4]], ncol=2)
    }else{
      
      lay <- rbind(c(1,2,5,5),
                   c(3,4,5,5))
      bars <- bar_plot(stock, stock_orig)
      grid.arrange(bars[[1]],bars[[2]],bars[[3]],bars[[4]],bars[[5]],  layout_matrix = lay)
    }
    
    
  }, height = 700)
  
})



scatter_plot <- function(stock, fitmethod = "lm"){
  g2 =  stock %>%
    ggplot(aes(x = ROE_5Y, y = Median_Q_Growth))+
    geom_point(alpha = 0.2, color = "darkgreen")+
    geom_smooth(method = fitmethod)+
    scale_x_continuous("Return on Equity (past 5 years mean)",limits = c(-0.25 ,0.5))+
    scale_y_continuous("Price growth rate",limits = c(-0.5, 0.5))+
    ggtitle("Price Growth v.s. Return on Equity")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14), 
          plot.title = element_text(size=18,face="bold"))
  
  g3 =  stock %>%
    ggplot(aes(x = DEratio_5Y, y = Median_Q_Growth))+
    geom_point(alpha = 0.2, color = "darkgreen")+
    geom_smooth(method = fitmethod)+
    scale_x_continuous("Debt to equity ratio (past 5 years mean)",limits = c(-0.25 , 3))+
    scale_y_continuous("Price growth rate", limits = c(-0.5, 0.5))+
    ggtitle("Price Growth v.s. Debt to Equity Ratio")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14), 
          plot.title = element_text(size=18,face="bold"))
  
  g4 =  stock %>%
    ggplot(aes(x = Profit_Margin_5Y, y = Median_Q_Growth))+
    geom_point(alpha = 0.2, color = "darkgreen")+
    geom_smooth(method = fitmethod)+
    scale_x_continuous("Profit margin (past 5 years mean)",limits = c(-0.2 , 0.4))+
    scale_y_continuous("Price growth rate", limits = c(-0.5, 0.5))+
    ggtitle("Price Growth v.s. Profit Margin")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14), 
          plot.title = element_text(size=18,face="bold"))
  
  g5 = stock %>%
    ggplot(aes(x = PEratio, y = Median_Q_Growth))+
    geom_point(alpha = 0.2, color = "darkgreen")+
    geom_smooth(method = fitmethod)+
    scale_x_continuous("Price to earning ratio",limits = c(0 , 40))+
    scale_y_continuous("Price growth rate", limits = c(-0.5, 0.5))+
    ggtitle("Price Growth v.s. Price to Earning Ratio")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14), 
          plot.title = element_text(size=18,face="bold"))
  
  return(list(g2, g3, g4, g5))
}


bar_plot <- function(stock,stock_orig){
  g1 =  stock %>%
    ggplot(aes(x = Median_Q_Growth*100))+
    geom_density(data = stock_orig, aes(x = Median_Q_Growth*100), color = "black",size = 1)+
    geom_density(color = "darkblue", fill = "darkblue", alpha = 0.5)+
    scale_x_continuous("Yearly Growth Percentage",limits = c(-50 ,50))+
    scale_y_continuous("Density")+
    ggtitle("Stock Price Yearly Growth Percentage")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14), 
          plot.title = element_text(size=18,face="bold"))
  
  g2 =  stock %>%
    ggplot(aes(x = ROE_5Y))+
    geom_density(data = stock_orig, aes(x = ROE_5Y), color = "black",size = 1)+
    geom_density(color = "darkblue", fill = "darkgreen", alpha = 0.3)+
    scale_x_continuous("Return on Equity (past 5 years mean)",limits = c(-0.25 ,0.5))+
    scale_y_continuous("Density")+
    ggtitle("Return on Equity")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14), 
          plot.title = element_text(size=18,face="bold"))
  
  g3 =  stock %>%
    ggplot(aes(x = DEratio_5Y))+
    geom_density(data = stock_orig, aes(x = DEratio_5Y), color = "black",size = 1)+
    geom_density(color = "darkblue", fill = "darkgreen", alpha = 0.3)+
    scale_x_continuous("Debt to equity ratio (past 5 years mean)",limits = c(-0.25 , 3))+
    scale_y_continuous("Density")+
    ggtitle("Debt to Equity Ratio")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14), 
          plot.title = element_text(size=18,face="bold"))
  
  g4 =  stock %>%
    ggplot(aes(x = Profit_Margin_5Y))+
    geom_density(data = stock_orig, aes(x = Profit_Margin_5Y), color = "black",size = 1)+
    geom_density(color = "darkblue", fill = "darkgreen", alpha = 0.3)+
    scale_x_continuous("Profit margin (past 5 years mean)",limits = c(-0.2 , 0.4))+
    scale_y_continuous("Density")+
    ggtitle("Profit Margin")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14), 
          plot.title = element_text(size=18,face="bold"))
  
  g5 = stock %>%
    ggplot(aes(x = PEratio))+
    geom_density(data = stock_orig, aes(x = PEratio), color = "black",size = 1)+
    geom_density(color = "darkblue", fill = "darkgreen", alpha = 0.3)+
    scale_x_continuous("Price to earning ratio",limits = c(0 , 40))+
    scale_y_continuous("Density")+
    ggtitle("Price to Earning Ratio")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14), 
          plot.title = element_text(size=18,face="bold"))
  
  return(list(g2, g3, g4, g5, g1))
}

