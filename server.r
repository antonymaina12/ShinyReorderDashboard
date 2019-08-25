library(shiny)
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)

### title: GetExponentialModel
### content:
GetExponentialModel <- function(data, alpha, beta, phi) {
          alpha0 <- as.numeric(data[1,2])
          beta0 <- 0 # Estimate of Trend
          
          alphaList <- c()
          betaList <- c()
          for (i in seq(1, nrow(data))) {
                    if (i == 1) {
                              alphaElement <- alpha*alpha0 + (1-alpha)*(alpha0+phi*beta0)
                              betaElement <- beta*(alphaElement-alpha0) + (1-beta)*beta0
                              alphaList <- c(alphaList, alphaElement)
                              betaList <- c(betaList, betaElement)
                    } else {
                              alphaElement <- alpha*data[i, 2] + (1-alpha)*(alphaElement+phi*betaElement)
                              alphaList <- c(alphaList, alphaElement)
                              
                              betaElement <- beta*(alphaList[i]- alphaList[i-1]) + (1-beta)*phi*betaList[i-1] #i=2
                              betaList <- c(betaList, betaElement)  
                    }
          }
          
          data$Alpha <- alphaList
          data$Beta <- betaList
          data$Prediction <- alphaList + betaList
          data$Error <- data$Demand - data$Prediction
          data$MSE <- data$Error^2
          
          data
}

### title: GetEOQ
### content: it is a function that returns the economic order quantity.
GetEOQ <- function(Demand,PurchaseCost, MOQ = 100, CarryingCost=0.2, fixedCost = 2000) {
          EOQ = sqrt(2*fixedCost*Demand/PurchaseCost*CarryingCost)
          EOQ = MOQ*ceiling(EOQ/MOQ)
          EOQ
}
    
shinyServer(func = function(input, output, session) {

          ### title: GetMainData
          ### content: it is a function that returns the dataset by extracting the information to display, apart from the monthly sales values.
          GetMainData <- reactive({
                    # Load Inventory Data File
                    tryCatch(
                              {
                                        df = read_excel(input$file$datapath, sheet = 1, skip = 4)
                              },
                              error = function(e) {
                                        # return a safeError if a parsing error occurs
                                        stop(safeError(e))
                              }
                    )
                    
                    mainData = data.frame(df[, c(2, 3, 6, 8, 9, 10)])
                    mainData[is.na(mainData)] = 0
                    colnames(mainData) = c("Code", "Description", "Available", "BackOrder", "Ordered", "MinQty")
                    mainData$currentQty = mainData$Available+mainData$Ordered-mainData$BackOrder
                    
                    mainData
          })
          
          ### title:
          ### content:
          GetTimeSeriesData <- reactive({
                    # Load Inventory Data File
                    tryCatch(
                              {
                                        df = read_excel(input$file$datapath, sheet = 1, skip = 4)
                              },
                              error = function(e) {
                                        # return a safeError if a parsing error occurs
                                        stop(safeError(e))
                              }
                    )
                    
                    # Cleaning Data
                    TimeSeriesData = data.frame(df[,17:(ncol(df)-1)])
                    TimeSeriesData = t(TimeSeriesData)
                    TimeSeriesData[is.na(TimeSeriesData)] = 0
                    colnames(TimeSeriesData) = df[,2]$Code
                    
                    # Creating Month Data
                    months = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
                    month.num = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
                    MonthTable1 = data.frame(months, month.num)
                    
                    months = tolower(substr(row.names(TimeSeriesData), 1, 3))
                    years = tolower(substr(row.names(TimeSeriesData), 5, 8))
                    MonthTable2 = data.frame(months, years)
                    
                    MonthTable = merge(MonthTable1, MonthTable2, by="months")
                    MonthTable$date = paste(MonthTable$years, MonthTable$month.num, sep='/')
                    
                    MonthTable$date = as.Date(paste(MonthTable$date, '01', sep = '/'), '%Y/%m/%d')
                    MonthTable = arrange(MonthTable, desc(date))
                    
                    rownames(TimeSeriesData) = as.character(MonthTable$date)
                    
                    TimeSeriesData
          })
          
          
          loadData <- reactive({
                    mainData <- GetMainData()
                    TimeSeriesData <- GetTimeSeriesData()
                    
                    # input
                    LEADTIME = input$leadtme
                    CSL = input$servicelevel/100
                    
                    # regression table
                    products = mainData$Code
                    regressions = c()
                    stds = c()
                    for (product in products) {
                              demands = rev(TimeSeriesData[, product])
                              periods = seq(1, nrow(TimeSeriesData), 1)
                              regression = summary(lm(demands~periods))$coeff[2,1]
                              regressions = c(regressions, round(regression,3))
                              std = sd(demands, na.rm = F)
                              stds = c(stds, round(std,3))
                    }
                    regressionTable = data.frame(Code=products, Regression=regressions, SD = stds)
                    
                    # combin with regression table and main Table
                    MainTable = merge(mainData, regressionTable, by = "Code")
                    
                    # safe stock level calculation
                    MonthDemand = colSums(TimeSeriesData, na.rm = T)/colSums(!is.na(TimeSeriesData))
                    YearlyDemand = colSums(TimeSeriesData, na.rm = T)
                    QuarterlyDemand = colSums(tail(TimeSeriesData, 4), na.rm = T)
                    
                    ReviewTime = 12/(LEADTIME+1)
                    
                    DmdOverLeadtime = QuarterlyDemand*3/ReviewTime
                    StdOverLeadtime = MainTable$SD/sqrt(ReviewTime)
                        
                        SafetyStock = DmdOverLeadtime + StdOverLeadtime*qnorm(CSL)
                        
                    TopupStock = SafetyStock - mainData$currentQty
                    
                    OrderQty = TopupStock - MainTable$currentQty
                    OrderQty = ifelse(OrderQty > 0, round(OrderQty, 0), NA)
                    
                    MainTable$SafetyStock = round(SafetyStock)
                    MainTable$OrderQty = OrderQty
                    MainTable$YearlyDemand = YearlyDemand
                    MainTable$DmdOverLeadtime = DmdOverLeadtime
                    MainTable$ID = as.numeric(seq(1, nrow(MainTable)))
                    
                    MainTable = MainTable %>% select("ID", everything())
                    
                    PurchasePrice = as.data.frame(read.csv("./Supplier Cost Report.csv"))
                    PurchasePrice = select(PurchasePrice, Product.Code, Supplier.Cost)
                    PurchasePrice$Supplier.Cost = as.numeric(gsub("[\\$,]", "", PurchasePrice$Supplier.Cost))
                    colnames(PurchasePrice) = c("Code", "Cost")
                    PurchasePrice = PurchasePrice[!duplicated(PurchasePrice$Code), ]
                    MainTable = merge(MainTable, PurchasePrice, by = "Code")
                    
                    MainTable$EOQ = GetEOQ(MainTable$YearlyDemand, MainTable$Cost, input$MOQ, input$carryingcost/100, input$fixedcost)
                    
                    MainTable
          })
          
          ### title: GetModelTest
          ### content: it is a function that returns the series of data from the simulation from TimeSeriesData dataset in order to find the best fit on each alpah, beta and phi value.
          GetModelTest <- reactive({
                    TimeSeriesData <- GetTimeSeriesData()

                    product = colnames(TimeSeriesData)[input$index]
                    
                    ProductDemandData = TimeSeriesData[, product]
                    ProductDemandData = data.frame(Date=rownames(TimeSeriesData), Demand=as.numeric(ProductDemandData))
                    ProductDemandData = arrange(ProductDemandData, Date)
                    
                    # Simulation Input
                    alphaInput = seq(0.1, 0.3, 0.1)
                    betaInput = seq(0, 0.3, 0.1)
                    phiInput = seq(0.8,1, 0.1)
                    
                    SimulationData = data.frame()
                    for (alpha in alphaInput) {
                              for (beta in betaInput) {
                                        for (phi in phiInput) {
                                                  DemandData = GetExponentialModel(ProductDemandData, alpha, beta, phi)
                                                  mse = sum(DemandData$MSE)/(nrow(DemandData)-1)
                                                  error = abs(sum(DemandData$Error))
                                                  newData = data.frame(alpha=alpha, beta=beta, phi=phi, mse=mse, error=error)
                                                  SimulationData = rbind(SimulationData, newData)
                                        }
                              }
                    }
                    BestModel <- SimulationData[which(SimulationData$mse == min(SimulationData$mse)), ]
                    BestModel
          })
          
          ### title: GetStockData
          ### content:
          GetStockData <- reactive({
                    mainData <- GetMainData()
                    TimeSeriesData <- GetTimeSeriesData()
                    
                    time0 = mainData[input$index, 3]
                    demand = TimeSeriesData[ , input$index] 
                    
                    StockLevelData = time0 + cumsum(demand)
                    StockLevelData = c(time0, StockLevelData)
                    date0 = as.Date(names(StockLevelData)[2])
                    month(date0) = month(as.Date(names(StockLevelData)[2]))+1
                    names(StockLevelData)[1] = format(date0, "%Y-%m-%d")
                    StockLevelData = data.frame(Date=names(StockLevelData), Demand=StockLevelData)
                    
                    StockLevelData
          })
          
          ### title: GetPredictionData
          ### content: it is a function that returns  
          GetPredictionData <- reactive({
                    TimeSeriesData <- GetTimeSeriesData()
                    product = colnames(TimeSeriesData)[input$index]
                    
                    ProductDemandData = TimeSeriesData[, product]
                    ProductDemandData = data.frame(Date=rownames(TimeSeriesData), Demand=as.numeric(ProductDemandData))
                    ProductDemandData = arrange(ProductDemandData, Date)
                    
                    BestModel = GetModelTest()
                    alpha = BestModel[1,1]
                    beta = BestModel[1,2]
                    phi = BestModel[1,3]
                    
                    PredictionModel = GetExponentialModel(ProductDemandData, alpha, beta, phi)
                    PredictionModel[,-1] = round(PredictionModel[,-1], 2)
                    PredictionModel$Date = format(as.Date(PredictionModel$Date, "%Y-%m-%d"), "%b")
                    PredictionModel
          })
          
          ### title: table
          ### content: table is the main table shows the basic information from the excel spreadsheet.
          output$table <- renderDataTable({
                    if(is.null(input$file)){
                              return(NULL)
                    }
                    loadData()
          })
          
          ### title: table1
          ### content: table1 shows the prediction model of the demand over the next period, using the exponential smoothing model.
          output$table1 <- renderTable({
                    if(is.null(input$file)){
                              return(NULL)
                    }
                    GetPredictionData()
          })
          
          ### title: plot1
          ### content: plot1 shows monthly demand and the trend on the product.
          output$plot1 <- renderPlot({
                    if(is.null(input$file)){
                              return(NULL)
                    }
                    TimeSeriesData <- GetTimeSeriesData()
                    
                    # Input Control
                    product = colnames(TimeSeriesData)[input$index]
                    
                    PlotTable = TimeSeriesData[, product]
                    
                    PlotTable = data.frame(Date=rownames(TimeSeriesData), Demand=as.numeric(PlotTable))
                    
                    # product = "OPM29-01"
                    ggplot(data=PlotTable,
                           aes(x=as.Date(Date), y=Demand, group=1)) +
                              geom_point(size = 2) +
                              geom_text(aes(label=Demand), hjust=1, vjust=1) +
                              geom_line() +
                              geom_smooth(method = "glm", formula = y ~ x, se = T, na.rm = T, level=0.8) +
                              labs(x = "Month", title = paste(product, "Monthly Demand"))
          })
          
          ### title: plot2
          ### content: the plot2 shows the current stock level and highlights the safety stock level and current stock level. If the current stock level is lower than the safety stock level. The line is hightlighted so that the purchase officer can know the product is likely to be stock-out within the set leadtime.
          output$plot2 <- renderPlot({
                    if(is.null(input$file)){
                              return(NULL)
                    }
                    
                    StockLevelData <- GetStockData()
                    MainTable <- loadData()
                    product = MainTable$Code[input$index]
                    # product = MainTable$Code[34]
                    ProductInfo <- MainTable[which(MainTable$Code==product), ]
                    
                    ggplot(data=StockLevelData,
                           aes(x=as.Date(Date), y=Demand, group=1)) +
                              geom_point(size = 2) +
                              geom_text(aes(label=Demand), hjust=1, vjust=1) +
                              geom_hline(aes(yintercept = ProductInfo$currentQty), linetype="dashed", color = "red", alpha = ifelse(ProductInfo$Ordered > 0, 1, 1/10)) +
                              geom_hline(aes(yintercept = ProductInfo$SafetyStock), linetype="dashed", color = "blue", alpha = ifelse(ProductInfo$currentQty < ProductInfo$SafetyStock, 1, 1/10)) +
                              geom_line() +
                              labs(x = "Month", title = paste(product, "Monthly Stock Level"))
          })
          
          ### title: download
          ### content: download is a button that enable the user to download the csv. file. The file contains the data from the function, loadData().
          output$download <- downloadHandler(
                    # This function returns a string which tells the client browser what name to use when saving the file.
                    filename = paste(Sys.Date(), "csv", sep = "."),
                    
                    # This function should write data to a file given to it by the argument 'file'.
                    content = function(file) {
                              sep <- ","
                              
                              # Write to a file specified by the 'file' argument
                              write.csv(loadData(), file, sep = sep, row.names = FALSE)
                    }
          )
          
})


### Required variables for testing are recorded as following.

# setwd("/media/taepark/Tae Park External Hard Drive/Projects/Projects/Reorder_Report")
# df = read_excel("/home/taepark/R/2019Projects/Salop_2019.06.05.xlsx", sheet = 1, skip = 4, skipEmptyCols = F)
# LEADTIME = 8
# CSL = 0.95
# time0 = mainData[1, 3]
# demand = TimeSeriesData[ , 1] 
