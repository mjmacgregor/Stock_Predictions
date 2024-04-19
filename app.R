
library(shiny)
library(shinydashboard)
library(bslib)
library(graphics)

stocks = read.csv('stocks.csv')
stocks$Change = stocks$Close - stocks$Open
stocks$Percent_Change = (stocks$Change/stocks$Open)*100

set.seed(1)
test = sample(1:3872, 774)
stocks_test = stocks[test, ]
stocks_train = stocks[-test, ]

lm_closing_price = lm(Close ~ Open + High + Low + Volume, data = stocks_train)




# Define UI ---

ui <- page_sidebar(
  title = "Predicting a Stock's Closing Price",
  sidebar = sidebar(
    #helpText(
    mainPanel(
      p("Here is an interactive program that works to predict a stock's closing price on a particular day based on other evaluation metrics from that day."),
      p("Modify the values for the opening, minimum, and maximum price of the stock for the day as well as the volume of stocks traded. Then click the calculate button to see the predicted closing price and percent change for the day based on the provided parameters."),
      p("Note that there are no restrictions on the values for each of the parameters but it would not make sense to provide, for example, a higher minimum value than the maximum value."),
      width = 10
    ),
    sliderInput(
      "open_price",
      label = "What is the stock's opening price for the day?",
      min = 3.00, max = 410.00, value = 100 
    ),
    sliderInput(
      "low_val",
      label = "What is the stock's lowest value for the day?",
      min = 3.00, max = 400.00, value = 90
    ),
    sliderInput(
      "high_val",
      label = "What is the stock's highest value for the day?",
      min = 3.00, max = 420.00, value = 120
    ),
    sliderInput(
      "trade_volume",
      label = "Can you estimate the volume of stocks traded on the day?",
      min = 500000.00, max = 400000000.00, value = 5000000.00 
    )
  ),
  
  textOutput("opening"),
  textOutput("min"),
  textOutput("max"),
  textOutput("vol"),
  actionButton("cal", "Calculate Closing Price"),
  textOutput("closing"),
  textOutput("percent"),
  textOutput("explanation"),
  textOutput("logic")
  
)

# Define server logic

server <- function(input, output) {
  
  react <- reactiveValues(result = NULL)
  
  observeEvent(input$cal, {
    df <- data.frame(Open = input$open_price, High = input$high_val, Low = input$low_val, Volume = input$trade_volume)
    react$result <- predict(lm_closing_price, df)
    react$percent_change <- ((react$result-input$open_price)/input$open_price)*100
  })
  
  output$opening <- renderText({
    paste("Today your stock had an opening value of ", input$open_price)
  })
  
  output$min <- renderText({
    paste("The lowest value of your stock today was ", input$low_val)
  })
  
  output$max <- renderText({
    paste("The highest value of your stock today was ", input$high_val)
  })
  
  output$vol <- renderText({
    paste("An estimated ", input$trade_volume, " of your stocks were traded today")
  })
  
  output$closing <- renderText({
    paste("Based on the provided metrics for your stock from today, it is predicted to have a closing price of ", react$result)
  })
  
  output$percent <- renderText({
    paste("This means that there was a ", react$percent_change, "% change between your stock's opening and closing price today")
  })
  
  output$explanation <- renderText({
    "Being able to predict the closing price for a stock, especially relative to its opening price, can have interesting and important applications in determining whether to trade the specific stock."
  })
  
  output$logic <- renderText({
    "For example, if your stock is predicted to have a very positive percent change for the day, you might consider holding it or trading it near the end of the day where it is predicted to have grown considerably from its opening price. However, if your stock is predicted to have a very negative percent change for the day, it might be wiser to trade it before it loses a lot of its value."
  })
  
}

# Run the application

shinyApp(ui = ui, server = server)




