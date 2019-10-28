WorkingDirectory = ".... Set Your Own Drirectory here!!"
#setting our working directory
setwd(WorkingDirectory)
#================================================================================      
# This Function only scrapes the Last Trading Day Price
ScrapPrice = function(Symbol){
  # condition for scrapping after 4:00 PM
  if( 16 <= as.numeric(substr(Sys.time(),12,13))){
    # scrapping
    library(rvest)
    library(XML)
    
    makeAddress=paste('https://finance.yahoo.com/quote/',Symbol,'/history?p=',Symbol,sep = '')
    web = read_html(makeAddress)
    employers1 <- web %>%
      html_nodes("table") %>%
      .[1] %>%
      html_table()
    
    employers <- employers1[[1]]
    TodayPrice=employers[1,]
    return(TodayPrice)
  }
}

#=================
# This Functin Add the updated price to your working directory as .CSV file
AddPriceToday = function(Symb,Price = getSymbols(Symbols = Symb, src = "yahoo", from =c("2005-01-01") ,to = Sys.Date(), auto.assign = F,periodicity = "daily"),condition=TRUE)
{
  library(quantmod)
  library(tseries)
  #if(condition==TRUE){
  
  # after scrapping the data we need to format it in our favor
  # here I removed the " , " and the date column from the scrapped stock price
  TodayPrice = as.numeric(gsub(',','',ScrapPrice(Symb)[-1]))
  
  CLOSE=TodayPrice[5]
  
  # Price is the historical price which is downloaded by getSymblos() function
  
  OPEN = TodayPrice[1]
  HIGH=TodayPrice[2]
  LOW=TodayPrice[3]
  
  VOL=TodayPrice[6]
  
  My_Date=gsub('-','',time(Price)) # removing the"-" in the DATE data
  
  # Adding a new date column into the time series data
  Price=cbind(My_Date,Price)
  
  Price=Price[,-7] # removing the ADJUSTED CLOSE
  
  
  #Todays Date
  Today=as.numeric(gsub('-','',Sys.Date()))
  
  CurrentPrice=cbind(Today,OPEN,HIGH,LOW,CLOSE,VOL)
  # rename the columns
  colnames(CurrentPrice) = c("<Date>","<OPEN>","<HIGH>","<LOW>","<CLOSE>","<VOL>")
  CurrentPrice=xts(CurrentPrice,Sys.Date())
  #putting the historical and current prices together
  Price = rbind(Price,CurrentPrice)
  # Save the file as .CSV in the working directory
  write.table(as.data.frame(Price),file = paste(Symb,".csv",sep=""),sep=",",col.names = T,row.names = F)
  
  # }
}
AddPriceToday(Symb = "IBM")