# Loading Libararies

library(shiny)
library(shinythemes)
library(plotly)
library(shinydashboard)
library(lubridate)
library(dplyr)
library(viridis)

load("../Data_Files/TCS")
load("../Data_Files/BSE")
load("../Data_Files/HDFC")
load("../Data_Files/JSW")
load("../Data_Files/INFOSYS")

big = rbind(tcs[[1]],bse,hdfc[[1]],jsw[[1]],infosys[[1]])
big['Comp'] = rep(c('TCS','BSE','HDFC Bank','JSW Steel','Infosys'),rep(86,5))


# Function To Plot Candlestick Chart

candle <- function(dat)
{
  candlestick_trace <- plot_ly(data = dat, type = "candlestick", x = ~Date, open = ~Open,
                               high = ~High, low = ~Low, close = ~Adj_Close,
                               text = ~paste("Volume: ", Volume), name = "Price"
  )
  
  # Create a volume bar chart
  volume_trace <- plot_ly(data = dat, type = "bar",
                          x = ~Date,
                          y = ~Volume,
                          name = "Volume",
                          marker = list(color = "rgba(0,0,0,0.1)")
  )
  
  # Combine both charts into a subplot
  plot <- subplot(candlestick_trace, volume_trace, nrows = 2, shareX = TRUE) %>%
    layout(
      title = "Candlestick Chart with Volume",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Price", domain = c(0.3, 1)),
      yaxis2 = list(title = "Volume", overlaying = "y", side = "right", showgrid = F, domain = c(0, 0.25)),
      showlegend = TRUE
    )
  return(plot)
}

# Function To Plot Monthly Returns 

monthly <- function(dat)
{  
  returns_plot <- plot_ly(data = dat, x = ~Date, y = ~Change, type = 'bar', marker = list(color = 'royalblue')) %>%
    layout(
      title = "Monthly Returns in %",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Return"),
      showlegend = FALSE
    )
  returns_plot
}

# Function To Plot Yearly Returns

yearly <- function(dat)
{
  bd <- arrange(dat[seq(6,86,12),],Date)
  bd <- mutate(bd,Change = c(0,diff(Close))/c(1,bd$Close[-length(bd$Close)])*100)
  last <- (dat[dat$Date == as.Date('23/09/01',"%y/%m/%d"),5]-bd[7,5])/bd[7,5]
  last <- as.numeric(last*100)
  bd$Change <- c(bd$Change[-1],last)
  returns_plot <- plot_ly(data = bd, x = ~Date, y = ~Change, type = 'bar', marker = list(color = 'teal')) %>%
    layout(
      title = "Yearly Returns in %",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Return"),
      showlegend = FALSE
    )
  returns_plot
}

# BoxPlot For Best Time For Investing in a given year

best_month <- function(dat)
{
  values_list = list()
  m = c()
  for(i in 1:12)
  {
    bd <- arrange(dat[seq(i,86,12),],Date)
    bd <- mutate(bd,Change = c(0,diff(Close))/c(1,bd$Close[-length(bd$Close)])*100)
    values_list[[i]] <- bd
    b <-  month(as.data.frame(bd)[1,1])
    m[i] <- b
  }
  
  names(values_list) <- m
  
  data_frames <- lapply(1:length(values_list), function(i) {
    data.frame(Group = m[i], Value = values_list[[i]])
  })
  
  # Combine all data frames into a single data frame
  data <- do.call(rbind, data_frames)
  data <- data[order(data$Group),]
  data
  #data$Group <- paste(data$Group,month.name[data$Group])
  
  # Define a custom color palette with 12 distinct colors

  # Create a side-by-side boxplot using Plotly with a custom color scheme
  boxplot_plot <- plot_ly(data, x = ~Group, y = ~Value.Change, type = 'box', boxpoints = "outliers")%>%
    layout(
      title = "Yearly Change(Month Wise)",
      xaxis = list(title = "Months", ticktext=list('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'),tickvals=1:12,tickmode='array'),
      yaxis = list(title = "Returns"),
      boxmode = "group",
      boxgroupgap = 0.5
    )
  
  # Add boxplots for each group with custom colors
  
  
  # Display the side-by-side boxplot with custom colors for 12 groups
  boxplot_plot
  
}

# Intermediate Calculations 

beta <- function(stock,bench)
{
  b <- cov(stock$Change,bench$Change)/var(bench$Change)
  return(b)
}

# Function To Plot Financial Metrics For The Stock

radar <- function(stock,index)
{
  
  stock[[2]]['Beta'] <- beta(stock[[1]],index)*10
  
  fig <- plot_ly(
    type = 'scatterpolar',
    r = stock[[2]],
    theta = names(stock[[2]]),
    fill = 'toself'
  ) 
  
  fig <- fig %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,30)
        )
      ),
      showlegend = F
    )
  
  fig
}

# Function To Compute Yearly Change in The Stock Price

comp <- function(data)
{
  ploty <- list()
  plot <- plot_ly()
  for(i in 1:length(data))
  {
    plot <- add_trace(plot, x = data[[i]]$Date, y = data[[i]]$Changem, type = 'scatter', mode = 'lines',name=data[[i]]$Comp[1])
  }
  plot <- layout(plot,xaxis=list(title='Date'),yaxis=list(title='Change(%)'))
  plot
}

change <- function(data)
{
  foo <- list()
  unq <- unique(data$Comp)
  for(i in 1:length(unq))
  {
    dat <- subset(as.data.frame(data),Comp == unq[i])
    point <- as.data.frame(dat)[1,5]
    dat$Changem <- (dat$Close-point)/point*100
    foo[[i]] <- dat
  }
  comp(foo)
}

# Function To Calculate Avg Positive Monthly Return

monpos <- function(foo,state)
{
  df <- as.data.frame(foo[[1]])
  pos <- df[df$Change>0,]
  if(state == 1)
  {
    return(mean(pos$Change))
  }else{
    return(var(pos$Change))
  }
}

# Function To Plot Avg Monthly Positive Return

monthcompp <- function(foo)
{
  unq <- unique(foo$Comp)
  bar_plot <- plot_ly()
  for(i in 1:length(unq))
  {
    dat <- subset(as.data.frame(foo),Comp == unq[i])
    bar_plot <- add_trace(bar_plot, x = monpos(list(dat),1), y = dat$Comp[1], type = 'bar', orientation = 'h', marker = list(color =viridis(5)[4] ))
  }
  bar_plot <- layout(
    bar_plot,
    title = "Avg Monthly Profit (%)",
    xaxis = list(title = "Profit(%)",range=c(0,10)),
    yaxis = list(title = "Stock"),
    showlegend = F
  )
  bar_plot
}

# Function To Calculate Avg Negative Monthly Return

monneg <- function(foo,state)
{
  df <- as.data.frame(foo[[1]])
  pos <- df[df$Change<0,]
  if(state == 1)
  {
    return(mean(abs(pos$Change)))
  }else{
    return(var(pos$Change))
  }
}

# Function To Plot Avg Monthly Neagtive Return

monthcompn <- function(foo)
{
  unq <- unique(foo$Comp)
  bar_plot <- plot_ly()
  for(i in 1:length(unq))
  {
    dat <- subset(as.data.frame(foo),Comp == unq[i])
    bar_plot <- add_trace(bar_plot, x = monneg(list(dat),1), y = dat$Comp[1], type = 'bar', orientation = 'h', marker = list(color =magma(5)[4] ))
  }
  bar_plot <- layout(
    bar_plot,
    title = "Avg Monthly Loss (%)",
    xaxis = list(title = "Loss(%)",range=c(0,10)),
    yaxis = list(title = "Stock"),
    showlegend = F
  )
  bar_plot
}


# Ui For the Application

ui <- fluidPage(theme = shinytheme('flatly'),
                tags$head(
                  tags$style(HTML('.avgpos { background-color: #317e61; color: #FFFFFF;margin-right: 10px; margin-bottom:10px;border-radius:10px;}')),
                  tags$style(HTML('.avgneg { background-color: #ff4136; color: #FFFFFF;margin-right: 10px;border-radius:10px;}')),
                  tags$style(HTML('.margin{margin-top: 50px; }')),
                  tags$style(HTML('.top{ background-color: #efefef;margin-bottom:40px;border: 10px solid white; border-radius:10px;}')),
                  tags$style(HTML('.head{ background-color: #c5d6ff;color: #0f161c;padding-top: 10px; padding-right: 10px; padding-bottom: 10px; padding-left: 10px}')),
                  tags$style(HTML('.back{background-color: #c5d6ff}'))),
                
    # Application title
    titlePanel("Stock Pulse"),
    
    navbarPage('',
      tabPanel('Stocks',
             
               sidebarLayout(
                 
                 sidebarPanel(
                   selectInput("dataset", "Select The Stock To Analyze", 
                               choices = c("Infosys", "JSW Steel", "TCS","HDFC Bank")),
                              width = 3,selected='TCS',
                   br(),br(),br(),br(),br()
                 ),
                 
                 mainPanel(
                   h2('Glimpse Into The Stock',class='head'),
                   
                   fluidRow(
                     column(4,class='top',box(title='Price',height='150px',h6('Sep-2023'),h3(textOutput('price')))),
                     column(4,class='top',box(title='High',height='150px',h6('08/2016-09/2023'),h3(textOutput('high')))),
                     column(4,class='top',box(title='Low',height='150px',h6('08/2016-09/2023'),h3(textOutput('low'))))
                   ),
                   
                   
                   plotlyOutput('candle',width='1100px',height='600px'),
                   
                   h2('Technical Indicators',class='head'),
                   
                   fluidRow(class='margin',
                     column(7,plotlyOutput('monthly')),
                     column(5,
                            fluidRow(
                              column(5,class='avgpos',
                                     box(
                                       width='300px',
                                       height='150px',
                                       title='Avg Profit',
                                       'Monthly',
                                       h3(textOutput('phigh_value'))
                                       
                                     )),
                              column(5,class='avgpos',
                                     box(
                                       width='300px',
                                       height='150px',
                                       title='Variance',
                                       'Monthly',
                                       h3(textOutput('plow_value'))
                                       
                                     ))
                            ),
                            fluidRow(
                              column(5,class='avgneg',
                                     box(
                                       width='300px',
                                       height='150px',
                                       title='Avg Loss',
                                       'Monthly',
                                       h3(textOutput('nhigh_value'))
                                       
                                     )),
                              column(5,class='avgneg',
                                     box(
                                       width='300px',
                                       height='150px',
                                       title='Variance',
                                       'Monthly',
                                       h3(textOutput('nlow_value'))
                                       
                                     ))
                            ))
                   ),
                   
                   fluidRow(class='margin',
                     column(6,plotlyOutput('yearly')),
                     column(6,plotlyOutput('radar'))
                   ),
                   
                   h2('Best Month To Invest',class='head'),
                   
                   fluidRow(class='margin',plotlyOutput('monthlybox'))
                 )
               )),

      tabPanel('Comparision',value="Comparision",
               
               sidebarLayout(
                 sidebarPanel(
                   
                   h4('Select Stock'),
                   
                   checkboxGroupInput('comp',label='',
                                      choices=c('TCS','HDFC Bank','JSW Steel','Infosys'),
                                      selected='TCS'),
                   
                   br(),
                   
                   h4('Select Benchmark'),
                   

                   checkboxInput('bse',label='BSE'),
                   
                   width = 3,
                   br()
                 ),
                 mainPanel(
                   h2('Glimpse Into The Past Performance',class='head'),
                   
                   plotlyOutput('compare_line'),
                   
                   h2('Glimpse Into The Monthly Returns',class='head'),
                   
                   fluidRow(
                     column(6,plotlyOutput('monthly_cp')),
                     column(6,plotlyOutput('monthly_cn'))
                   )
                   
                   
                 )
               )
               
               )
    )
    # Sidebar with a slider input for number of bins 
    
)

# Server For The Application

server <- function(input, output) 
{
  
  data <- reactive({
    switch(input$dataset,
           "Infosys" = infosys,
           "JSW Steel" = jsw,
           "TCS" = tcs,
           "HDFC Bank" = hdfc)
  })
  
  datac <- reactive(
    {
      stock <- input$comp
      index <- input$bse
      if(index)
      {
        subset(as.data.frame(big), Comp %in% c(stock,'BSE'))
      }else
      {
        subset(as.data.frame(big), Comp %in% stock)
      }
        
    }
  )
  
  
  output$candle <- renderPlotly({candle(data()[[1]])})
  output$monthly <- renderPlotly({monthly(data()[[1]])})
  output$yearly <- renderPlotly({yearly(data()[[1]])})
  output$monthlybox <- renderPlotly({best_month(data()[[1]])})
  output$radar <- renderPlotly({radar(data(),bse)})
  output$phigh_value <- renderText(paste(round(monpos(data(),1),2),"%"))
  output$plow_value <- renderText(paste(round(monpos(data(),2),2),"%"))
  output$nhigh_value <- renderText(paste(round(monneg(data(),1),2),"%"))
  output$nlow_value <- renderText(paste(round(monneg(data(),2),2),"%"))
  output$high <- renderText(max(as.data.frame(data()[[1]])$Close))
  output$low <- renderText(min(as.data.frame(data()[[1]])$Close))
  output$price <- renderText(as.data.frame(data()[[1]])$Close[86])
  output$compare_line <- renderPlotly(change(datac()))
  output$monthly_cn <- renderPlotly(monthcompp(datac()))
  output$monthly_cp <- renderPlotly(monthcompn(datac()))
  
}

# Run the application 
shinyApp(ui = ui, server = server)

