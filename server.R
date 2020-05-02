library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect)
library(caret)
# Select columns to be used in the analysis
#step1 - Data preprocessing
#select the columns which impact the price of diamonds
diam <- diamonds[,c(1,2,3,4,7)]
head(diam,2)
# Define server logic required to draw a plot
shinyServer(function(input, output) {
    
    output$distPlot <- renderPlot({
        # Select diamonds depending of user input
        diam <- filter(diamonds, grepl(input$cut, cut), grepl(input$col, color), grepl(input$clar, clarity))
        # build linear regression model
        fit <- lm( price ~carat, diam)
       #predict= train(formula=price~carat,data=diam,method='lm')
        # predicts the price
        pred <- predict(fit, newdata = data.frame(carat = input$car,
                                                  cut = input$cut,
                                                  color = input$col,
                                                  clarity = input$clar))
        # Draw the plot using ggplot2
        plot <- ggplot(data=diam, aes(x=carat, y = price))+
            geom_point(aes(color = cut), alpha = 0.3)+
            geom_smooth(method = "lm")+
            geom_vline(xintercept = input$car, color = "red")+
            geom_hline(yintercept = pred, color = "green")
        plot
    })
    output$result <- renderText({
        # Renders the text for the prediction below the graph
        diam <- filter(diamonds, grepl(input$cut, cut), grepl(input$col, color), grepl(input$clar, clarity))
        fit <- lm( price~carat, diam)
        pred <- predict(fit, newdata = data.frame(carat = input$car,
                                                  cut = input$cut,
                                                  color = input$col,
                                                  clarity = input$clar))
        res <- paste(round(pred, digits = 1.5),"$" )
        res
    })
    
})