
setwd("C:/Users/0130719/Desktop")
sales <- read.csv("RFM Data dump from Sales Order-datadump.csv")

View(sales)
str(sales)
sales$Date <- as.Date(sales$Date, "%m/%d/%y")

#Function to calculate Recency, Frequency & Monetary values
get_rfm<- function(SalesOrder,sDate ,eDate ,tIDCol="CustCD",tDate="Date",tCustName="CustName",
                   tSegment="Segment",tCountry="Country",tRegion="Region",tVolume="SQM",
                   tAmount= "EUR")
{ 
  newSalesOrder = SalesOrder[!duplicated(SalesOrder[,tIDCol]),]
  SalesOrder = SalesOrder[SalesOrder[,tDate]>=sDate, ]
  SalesOrder = SalesOrder[SalesOrder[,tDate]<=eDate, ]

  if (nrow(SalesOrder)==0)
  {
    Recency  <- 0
    Frequency<- 0
    Monetary_Vol<- 0
    Monetary_Val<- 0
    Month<- month(end)
    Year<- year(end)
    newSalesOrder= cbind(newSalesOrder,Recency,Frequency,Monetary_Vol,Monetary_Val,Month,Year)
  }
  else
  {
  SalesOrder = SalesOrder[order(SalesOrder[,tDate],decreasing = TRUE),]
  newSalesOrder = SalesOrder[!duplicated(SalesOrder[,tIDCol]),]

  Recency <- as.numeric(difftime(eDate,newSalesOrder[,tDate],units="days"))
  if (Recency<0) {Recency=0}
  
  newSalesOrder = cbind(newSalesOrder,Recency)
  
  Frequency <- nrow(SalesOrder)
  newSalesOrder = cbind(newSalesOrder,Frequency)
  
  Monetary_Vol= sum(SalesOrder[,tVolume])/Frequency
  newSalesOrder = cbind(newSalesOrder,Monetary_Vol)
  
  Monetary_Val= sum(SalesOrder[,tAmount])/Frequency
  newSalesOrder = cbind(newSalesOrder,Monetary_Val)
  
  Month<- month(end)
  Year<- year(end)
  newSalesOrder= cbind(newSalesOrder,Month,Year)
  }
  
  return(newSalesOrder)
}

# Creating the final RFM Score based on the assigned weightages R=1, F=1, M1=2, M2=2
generate_final_score<- function(df)
{
  for (i in 1:nrow(df))
  {
    df$rfmm_score[i] = ((df$R[i] * 1) + (df$F[i] * 1) + (df$M1[i] * 2) + (df$M2[i] * 2))/6          
  }
  return(df)
}

'''
#function to generate the rfmm_matrix using numbers for Recency
generate_rfmm_matrix <- function(sordersper)
{
  #Converting the industry segments to upper case
  sordersper$Segment <- toupper(sordersper$Segment)
  #Converting the industry segments back to factors
  sordersper$Segment <- as.factor(sordersper$Segment)
  #Counting the number of industry segments
  num_ind_seg <- length(levels(sordersper$Segment))
  #Storing the industry segments into a vector
  ind_segs <- levels(sordersper$Segment)
  sorders_rfm <- data.frame()
  
  for (i in 1: num_ind_seg)
  {
    sub_sorders<- data.frame()
    sub_sorders <- subset(sordersper,Segment == ind_segs[i])
    #Calculating percentiles for recency calculation
    pr25 <- quantile(sub_sorders$Recency,0.25)[[1]]
    pr50 <- quantile(sub_sorders$Recency,0.50)[[1]]
    pr75 <- quantile(sub_sorders$Recency,0.70)[[1]]
    pr85 <- quantile(sub_sorders$Recency,0.85)[[1]]
    
    #Calculating percentiles for frequency calculation
#    pf80 <- quantile(sub_sorders$Frequency,0.80)[[1]]
#    pf60 <- quantile(sub_sorders$Frequency,0.60)[[1]]
#    pf40 <- quantile(sub_sorders$Frequency,0.40)[[1]]
#    pf20 <- quantile(sub_sorders$Frequency,0.20)[[1]]
    
    #Calculating percentiles for frequency calculation
    pf80 <- quantile(sordersper$Frequency,0.80)[[1]]
    pf60 <- quantile(sordersper$Frequency,0.60)[[1]]
    pf40 <- quantile(sordersper$Frequency,0.40)[[1]]
    pf20 <- quantile(sordersper$Frequency,0.20)[[1]]
    
    #Calculating percentiles for monetary_vol calculation
    pmvol80 <- quantile(sub_sorders$Monetary_Vol,0.80)[[1]]
    pmvol60 <- quantile(sub_sorders$Monetary_Vol,0.60)[[1]]
    pmvol40 <- quantile(sub_sorders$Monetary_Vol,0.40)[[1]]
    pmvol20 <- quantile(sub_sorders$Monetary_Vol,0.20)[[1]]
    
    #Calculating percentiles for monetary_val calculation
    pmval80 <- quantile(sub_sorders$Monetary_Val,0.80)[[1]]
    pmval60 <- quantile(sub_sorders$Monetary_Val,0.60)[[1]]
    pmval40 <- quantile(sub_sorders$Monetary_Val,0.40)[[1]]
    pmval20 <- quantile(sub_sorders$Monetary_Val,0.20)[[1]]
    
    # Loop to calculate the Recency Score
    for (i in 1: nrow(sub_sorders))
    {
      if (sub_sorders$Recency[i] <= 100)
      {
        if ((sub_sorders$Recency[i] == 0 & sub_sorders$Frequency[i] == 0))
        {  
          sub_sorders$R[i] = 0
        }
        else
        {
          sub_sorders$R[i] = 5 
        }
      }
      else if(sub_sorders$Recency[i] > 100 & sub_sorders$Recency[i] <= 150)
      {
        sub_sorders$R[i] = 4
      }
      else if(sub_sorders$Recency[i] > 150 & sub_sorders$Recency[i] <= 200)
      {
        sub_sorders$R[i] = 3
      }
      else if(sub_sorders$Recency[i] > 200 & sub_sorders$Recency[i] <= 250)
      {
        sub_sorders$R[i] = 2
      }
      else if(sub_sorders$Recency[i] > 250)
      {
        sub_sorders$R[i] = 1
      }
    }
    
    # Loop to calculate the Frequency Score 
    for (i in 1: nrow(sub_sorders))
    {
      if (sub_sorders$Frequency[i] > pf80)
      {
        sub_sorders$F[i] = 5
      }
      else if(sub_sorders$Frequency[i] > pf60 & sub_sorders$Frequency[i] <=pf80)
      {
        sub_sorders$F[i] = 4
      }
      else if(sub_sorders$Frequency[i] > pf40 & sub_sorders$Frequency[i] <= pf60)
      {
        sub_sorders$F[i] = 3
      }
      else if(sub_sorders$Frequency[i] > pf20 & sub_sorders$Frequency[i] <= pf40)
      {
        sub_sorders$F[i] = 2
      }
      else if(sub_sorders$Frequency[i] <=pf20)
      {
        if ((sub_sorders$Frequency[i] == 0))
        {  
          sub_sorders$F[i] = 0
        }
        else
        {
          sub_sorders$F[i] = 1
        }
      }
    }
    
    #Loop to create Monetary Volume Score
    for (i in 1: nrow(sub_sorders))
    {
      if (sub_sorders$Monetary_Vol[i] > pmvol80)
      {
        sub_sorders$M1[i] = 5
      }
      else if(sub_sorders$Monetary_Vol[i] > pmvol60 & sub_sorders$Monetary_Vol[i] <= pmvol80)
      {
        sub_sorders$M1[i] = 4
      }
      else if(sub_sorders$Monetary_Vol[i] > pmvol40 & sub_sorders$Monetary_Vol[i] <= pmvol60)
      {
        sub_sorders$M1[i] = 3
      }
      else if(sub_sorders$Monetary_Vol[i] > pmvol20 & sub_sorders$Monetary_Vol[i] <= pmvol40)
      {
        sub_sorders$M1[i] = 2
      }
      else if(sub_sorders$Monetary_Vol[i] <= pmvol20)
      {
        if ((sub_sorders$Monetary_Vol[i] == 0.00))
        {  
          sub_sorders$M1[i] = 0
        }
        else
        {
          sub_sorders$M1[i] = 1
        }
      }
    }
    
    #Loop to create Monetary Value Score
    for (i in 1: nrow(sub_sorders))
    {
      if (sub_sorders$Monetary_Val[i] > pmval80)
      {
        sub_sorders$M2[i] = 5
      }
      else if(sub_sorders$Monetary_Val[i] > pmval60 & sub_sorders$Monetary_Val[i] <= pmval80)
      {
        sub_sorders$M2[i] = 4
      }
      else if(sub_sorders$Monetary_Val[i] > pmval40 & sub_sorders$Monetary_Val[i] <= pmval60)
      {
        sub_sorders$M2[i] = 3
      }
      else if(sub_sorders$Monetary_Val[i] > pmval20 & sub_sorders$Monetary_Val[i] <= pmval40)
      {
        sub_sorders$M2[i] = 2
      }
      else if(sub_sorders$Monetary_Val[i] <= pmval20)
      {
        if ((sub_sorders$Monetary_Val[i] == 0.00))
        {  
          sub_sorders$M2[i] = 0
        }
        else
        {
          sub_sorders$M2[i] = 1
        }
      }
    }
    
    sorders_rfm <- rbind(sorders_rfm,sub_sorders)
  }
  return(sorders_rfm)
}
'''



#function to generate the rfmm_matrix using percentiles in Recency
generate_rfmm_matrix <- function(sordersper)
{
  #Converting the industry segments to upper case
  sordersper$Segment <- toupper(sordersper$Segment)
  #Converting the industry segments back to factors
  sordersper$Segment <- as.factor(sordersper$Segment)
  #Counting the number of industry segments
  num_ind_seg <- length(levels(sordersper$Segment))
  #Storing the industry segments into a vector
  ind_segs <- levels(sordersper$Segment)
  sorders_rfm <- data.frame()
  
  for (i in 1: num_ind_seg)
  {
    sub_sorders<- data.frame()
    sub_sorders <- subset(sordersper,Segment == ind_segs[i])
    #Calculating percentiles for recency calculation
    pr25 <- quantile(sub_sorders$Recency,0.25)[[1]]
    pr50 <- quantile(sub_sorders$Recency,0.50)[[1]]
    pr75 <- quantile(sub_sorders$Recency,0.70)[[1]]
    pr85 <- quantile(sub_sorders$Recency,0.85)[[1]]
    
    #Calculating percentiles for frequency calculation
    pf80 <- quantile(sub_sorders$Frequency,0.80)[[1]]
    pf60 <- quantile(sub_sorders$Frequency,0.60)[[1]]
    pf40 <- quantile(sub_sorders$Frequency,0.40)[[1]]
    pf20 <- quantile(sub_sorders$Frequency,0.20)[[1]]
    
    #Calculating percentiles for monetary_vol calculation
    pmvol80 <- quantile(sub_sorders$Monetary_Vol,0.80)[[1]]
    pmvol60 <- quantile(sub_sorders$Monetary_Vol,0.60)[[1]]
    pmvol40 <- quantile(sub_sorders$Monetary_Vol,0.40)[[1]]
    pmvol20 <- quantile(sub_sorders$Monetary_Vol,0.20)[[1]]
    
    #Calculating percentiles for monetary_val calculation
    pmval80 <- quantile(sub_sorders$Monetary_Val,0.80)[[1]]
    pmval60 <- quantile(sub_sorders$Monetary_Val,0.60)[[1]]
    pmval40 <- quantile(sub_sorders$Monetary_Val,0.40)[[1]]
    pmval20 <- quantile(sub_sorders$Monetary_Val,0.20)[[1]]
    
    # Loop to calculate the Recency Score
    for (i in 1: nrow(sub_sorders))
    {
      if (sub_sorders$Recency[i] <= pr25)
      {
        if ((sub_sorders$Recency[i] == 0 & sub_sorders$Frequency[i] == 0))
        {  
          sub_sorders$R[i] = 0
        }
        else
        {
          sub_sorders$R[i] = 5 
        }
      }
      else if(sub_sorders$Recency[i] > pr25 & sub_sorders$Recency[i] <= pr50)
      {
        sub_sorders$R[i] = 4
      }
      else if(sub_sorders$Recency[i] > pr50 & sub_sorders$Recency[i] <= pr75)
      {
        sub_sorders$R[i] = 3
      }
      else if(sub_sorders$Recency[i] > pr75 & sub_sorders$Recency[i] <= pr85)
      {
        sub_sorders$R[i] = 2
      }
      else if(sub_sorders$Recency[i] > pr85)
      {
        sub_sorders$R[i] = 1
      }
    }
    
    # Loop to calculate the Frequency Score 
    for (i in 1: nrow(sub_sorders))
    {
      if (sub_sorders$Frequency[i] > pf80)
      {
        sub_sorders$F[i] = 5
      }
      else if(sub_sorders$Frequency[i] > pf60 & sub_sorders$Frequency[i] <=pf80)
      {
        sub_sorders$F[i] = 4
      }
      else if(sub_sorders$Frequency[i] > pf40 & sub_sorders$Frequency[i] <= pf60)
      {
        sub_sorders$F[i] = 3
      }
      else if(sub_sorders$Frequency[i] > pf20 & sub_sorders$Frequency[i] <= pf40)
      {
        sub_sorders$F[i] = 2
      }
      else if(sub_sorders$Frequency[i] <=pf20)
      {
        if ((sub_sorders$Frequency[i] == 0))
        {  
          sub_sorders$F[i] = 0
        }
        else
        {
          sub_sorders$F[i] = 1
        }
      }
    }
    
    #Loop to create Monetary Volume Score
    for (i in 1: nrow(sub_sorders))
    {
      if (sub_sorders$Monetary_Vol[i] > pmvol80)
      {
        sub_sorders$M1[i] = 5
      }
      else if(sub_sorders$Monetary_Vol[i] > pmvol60 & sub_sorders$Monetary_Vol[i] <= pmvol80)
      {
        sub_sorders$M1[i] = 4
      }
      else if(sub_sorders$Monetary_Vol[i] > pmvol40 & sub_sorders$Monetary_Vol[i] <= pmvol60)
      {
        sub_sorders$M1[i] = 3
      }
      else if(sub_sorders$Monetary_Vol[i] > pmvol20 & sub_sorders$Monetary_Vol[i] <= pmvol40)
      {
        sub_sorders$M1[i] = 2
      }
      else if(sub_sorders$Monetary_Vol[i] <= pmvol20)
      {
        if ((sub_sorders$Monetary_Vol[i] == 0.00))
        {  
          sub_sorders$M1[i] = 0
        }
        else
        {
          sub_sorders$M1[i] = 1
        }
       }
    }
    
    #Loop to create Monetary Value Score
    for (i in 1: nrow(sub_sorders))
    {
      if (sub_sorders$Monetary_Val[i] > pmval80)
      {
        sub_sorders$M2[i] = 5
      }
      else if(sub_sorders$Monetary_Val[i] > pmval60 & sub_sorders$Monetary_Val[i] <= pmval80)
      {
        sub_sorders$M2[i] = 4
      }
      else if(sub_sorders$Monetary_Val[i] > pmval40 & sub_sorders$Monetary_Val[i] <= pmval60)
      {
        sub_sorders$M2[i] = 3
      }
      else if(sub_sorders$Monetary_Val[i] > pmval20 & sub_sorders$Monetary_Val[i] <= pmval40)
      {
        sub_sorders$M2[i] = 2
      }
      else if(sub_sorders$Monetary_Val[i] <= pmval20)
      {
        if ((sub_sorders$Monetary_Val[i] == 0.00))
        {  
          sub_sorders$M2[i] = 0
        }
        else
        {
          sub_sorders$M2[i] = 1
        }
      }
    }
    
    sorders_rfm <- rbind(sorders_rfm,sub_sorders)
  }
  return(sorders_rfm)
}


library(lubridate)
#Counting the number of Customer codes
num_cust <- length(levels(sales$CustCD))

#Storing the customer codes into a vector
cust_code <- levels(sales$CustCD)

rfm_sales<- data.frame()
for(i in 1:20)
{  
  start=as.Date("2016-01-01")
  end=as.Date("2016-01-31")
  sub_customer<- data.frame()
  sub_customer <- subset(sales,CustCD == cust_code[i])
  for (j in 1:30)
  {
    newsales <- get_rfm(sub_customer,start,end)
    rfm_sales<- rbind(rfm_sales,newsales)
    end<- as.Date(as.mondate(end) + 1)
  }
}

bef_final_scores<- generate_rfmm_matrix(rfm_sales)

final_scores<-generate_final_score(bef_final_scores)
View(final_scores)


customer_A <- subset(final_scores,CustCD=="'00000253")
customer_A_t <- as.data.frame(cbind(customer_A_custCD="'00000253",month=customer_A$Month,year=customer_A$Year,score=customer_A$rfmm_score))
write.csv(customer_A_t,"customer_data1a.csv")

customer_B <- subset(final_scores,CustCD=="'00000209")
customer_B_t <- as.data.frame(cbind(customer_B_custCD="'00000209",month=customer_B$Month,year=customer_B$Year,score=customer_B$rfmm_score))
write.csv(customer_B_t,"customer_data2a.csv")

customer_C <- subset(final_scores,CustCD=="'00000305")
customer_C_t <- as.data.frame(cbind(month=customer_C$Month,year=customer_C$Year,score=customer_C$rfmm_score))
write.csv(customer_C_t,"customer_data3.csv")

customer_D <- subset(final_scores,CustCD=="'00000307")
customer_D_t <- as.data.frame(cbind(month=customer_D$Month,year=customer_D$Year,score=customer_D$rfmm_score))
write.csv(customer_D_t,"customer_data4.csv")

### rmse function#####
#Root Mean Square Error (RMSE) is the standard deviation of the residuals (prediction errors). Residuals are a measure of how far from the regression line data points are; RMSE is a measure of how spread out these residuals are. 
#In other words, it tells you how concentrated the data is around the line of best fit. 

rmse<-function(error){
  sqrt(mean(error^2))
}
MAPE<-function(error,sales){
  mean(abs(error/sales))*100  
}

setwd("C:/Users/0130719/Desktop/customer data")
#-----test customer 1 #----------
customer = "'00000253"

customer_data<- read.csv("customer_data1.csv")
customer_ts<- ts(customer_data)
plot.ts(customer_ts[,2])

training<- as.data.frame(customer_ts[1:24,])
testing<- as.data.frame(customer_data[25:nrow(customer_ts),])
new<- data.frame(Month=testing$Month)
##linear trend model##

linear_customer <- lm(rfmm_score~Month,data=training)
linear_customer

Predict_test<-predict(linear_customer,newdata = new)
error<-testing$rfmm_score-Predict_test
rmse(error)
MAPE(error,testing$rfmm_score)

#-----------------------test customer 1 above---------#

#-----------------------test customer 2 below---------#
customer = "'00000209"
customer_data<- read.csv("customer_data2.csv")
customer_ts<- ts(customer_data)
plot.ts(customer_ts[,2])

training<- as.data.frame(customer_ts[1:24,])
testing<- as.data.frame(customer_data[25:nrow(customer_ts),])
new<- data.frame(Month=testing$Month)
##linear trend model##

linear_customer <- lm(rfmm_score~Month,data=training)
linear_customer

Predict_test<-predict(linear_customer,newdata = new)
error<-testing$rfmm_score-Predict_test
rmse(error)
MAPE(error,testing$rfmm_score)
View(Predict_test)
#-----------------------test customer 2 above---------#

#-----test customer 3 below #----------
customer = "'00000305"

customer_data<- read.csv("customer_data3.csv")
customer_ts<- ts(customer_data)
plot.ts(customer_ts[,2])

training<- as.data.frame(customer_ts[1:24,])
testing<- as.data.frame(customer_data[25:nrow(customer_ts),])
new<- data.frame(Month=testing$Month)
##linear trend model##

linear_customer <- lm(rfmm_score~Month,data=training)
linear_customer

Predict_test<-predict(linear_customer,newdata = new)
error<-testing$rfmm_score-Predict_test
rmse(error)
MAPE(error,testing$rfmm_score)

#-----------------------test customer 3 above---------#

#-----test customer 4 below #----------
customer = "'00000307"

customer_data<- read.csv("customer_data4.csv")
customer_ts<- ts(customer_data)
plot.ts(customer_ts[,2])

training<- as.data.frame(customer_ts[1:24,])
testing<- as.data.frame(customer_data[25:nrow(customer_ts),])
new<- data.frame(Month=testing$Month)
##linear trend model##

linear_customer <- lm(rfmm_score~Month,data=training)
linear_customer

Predict_test<-predict(linear_customer,newdata = new)
error<-testing$rfmm_score-Predict_test
rmse(error)
MAPE(error,testing$rfmm_score)

#-----------------------test customer 4 above---------#




#-----test customer 1 A #----------
customer = "'00000253"
customer_data<- read.csv("customer_data1a.csv")
customer_ts<- ts(customer_data)
plot.ts(customer_ts[,2])

training<- as.data.frame(customer_ts[1:24,])
testing<- as.data.frame(customer_data[25:nrow(customer_ts),])
new<- data.frame(month=testing$month)
##linear trend model##

linear_customer <- lm(score~month,data=training)
linear_customer

Predict_test<-predict(linear_customer,newdata = new)
error<-testing$score-Predict_test
rmse(error)
MAPE(error,testing$score)

#-----------------------test customer  A above---------#

#-----------------------test customer 2 A below #----------
customer = "'00000209"
customer_data<- read.csv("customer_data2a.csv")
customer_ts<- ts(customer_data)
plot.ts(customer_ts[,2])

training<- as.data.frame(customer_ts[1:24,])
testing<- as.data.frame(customer_data[25:nrow(customer_ts),])
new<- data.frame(testing[,-2])
##linear trend model with seasonality##

linear_customer <- lm(score~month,data=training)
linear_customer

Predict_test<-predict(linear_customer,newdata = new)
error<-testing$score-Predict_test
rmse(error)
MAPE(error,testing$score)

#-----------------------test customer 2 A above---------#

#-----------------Test Code Below-----------------#
#d <- as.Date('2004-01-01')
#d<- as.Date(as.mondate(d) + 1)
#d
#add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]

SalesOrder = SalesOrder[order(SalesOrder[,"Date"],decreasing = TRUE),]
newSalesOrder = SalesOrder[!duplicated(SalesOrder[,"CustCD"]),]
start="2016-01-01"
SalesOrder = SalesOrder[SalesOrder[,"Date"]>= start, ]
SalesOrder = SalesOrder[SalesOrder[,"Date"]<= end, ]

if (nrow(SalesOrder)==0)
{
  Recency  <- 0
  Frequency<- 0
  Monetary_Vol<- 0
  Monetary_Val<- 0
  new_SalesOder=cbind(newSalesOrder,Recency,Frequency,Monetary_Vol,Monetary_Val)
}

Recency <- as.numeric(difftime(end,newSalesOrder[,"Date"],units="days"))

newSalesOrder = cbind(newSalesOrder,Recency)

newSalesOrder <- newSalesOrder[order(newSalesOrder[,"CustCD"]),]

Frequency <- nrow(SalesOrder)
newSalesOrder = cbind(newSalesOrder,Frequency)

#m1<- as.data.frame(tapply(SalesOrder[,"SQM"], SalesOrder[,"CustCD"]=="'00000209", sum))
#Monetary_Vol = m1[,1]/Frequency
Monetary_Vol= sum(SalesOrder[,"SQM"])/Frequency
newSalesOrder = cbind(newSalesOrder,Monetary_Vol)

#m2<- as.data.frame(tapply(SalesOrder[,tAmount], SalesOrder[,tIDCol], sum))
#Monetary_Val = m2[,1]/Frequency
Monetary_Val= sum(SalesOrder[,"EUR"])/Frequency
newSalesOrder = cbind(newSalesOrder,Monetary_Val)

