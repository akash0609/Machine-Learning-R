#setwd("C:/Users/0130719/Desktop/RFM Progress")
#sorders <- read.csv("RFM Data dump from Sales Order-datadump.csv")

#sorders$Date <- as.Date(sorders$Date, "%m/%d/%y")

library(RJDBC)
library(MASS)
library(tseries)
library(forecast)

# one customer
#install.packages("sqldf")

library(sqldf)

vDriver <- JDBC(driverClass="com.vertica.jdbc.Driver", classPath="D:\\Software\\Vertica\\vertica-jdbc-9.1.0-0.jar")
vertica <- dbConnect(vDriver, "jdbc:vertica://APDVADC394:5433/VerticaBIDev", "dbadmin", "Avery@123")

sales = dbGetQuery(vertica, "select * from public.sales_orders")

sales$Date <- as.factor(sales$Date)
sales$CustName <- as.factor(sales$CustName)
sales$Segment <- as.factor(sales$Segment)
sales$Country <- as.factor(sales$Country)
sales$Region <- as.factor(sales$Region)

#Converting the industry segments to upper case
sales$Segment <- toupper(sales$Segment)
#Converting the industry segments back to factors
sales$Segment <- as.factor(sales$Segment)
sales$Seg_Country<- paste(sales[,"Segment"],sales[,"Country"])
sales$Seg_Country<- as.factor(sales$Seg_Country)

sales$Date <- as.Date(sales$Date, "%m/%d/%y")



str(sorders)
# to get RFM values

get_rfm<- function(SalesOrder,sDate ,eDate ,tIDCol="CustCD",tDate="Date",tCustName="CustName",
                   tSegment="Segment",tCountry="Country",tRegion="Region",tVolume="SQM",
                   tAmount= "EUR")
{
  SalesOrder = SalesOrder[order(SalesOrder[,tDate],decreasing = TRUE),]
  
  SalesOrder = SalesOrder[SalesOrder[,tDate]>=sDate, ]
  SalesOrder = SalesOrder[SalesOrder[,tDate]<=eDate, ]
  
  newSalesOrder = SalesOrder[!duplicated(SalesOrder[,tIDCol]),]
  
  Recency <- as.numeric(difftime(eDate,newSalesOrder[,tDate],units="days"))
  
  newSalesOrder = cbind(newSalesOrder,Recency)
  
  newSalesOrder <- newSalesOrder[order(newSalesOrder[,tIDCol]),]
  
  fre <- as.data.frame(table(SalesOrder[,tIDCol]))
  Frequency<- fre[,2]
  newSalesOrder = cbind(newSalesOrder,Frequency)
  
  m1<- as.data.frame(tapply(SalesOrder[,tVolume], SalesOrder[,tIDCol], sum))
  Monetary_Vol = m1[,1]/Frequency
  newSalesOrder = cbind(newSalesOrder,Monetary_Vol)
  
  m2<- as.data.frame(tapply(SalesOrder[,tAmount], SalesOrder[,tIDCol], sum))
  Monetary_Val = m2[,1]/Frequency
  newSalesOrder = cbind(newSalesOrder,Monetary_Val)
  
  return(newSalesOrder)
  
}

# Function to generate the RFM Matrix
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
        sub_sorders$F[i] = 1
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
        sub_sorders$M1[i] = 1
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
        sub_sorders$M2[i] = 1
      }
    }
    
    sorders_rfm <- rbind(sorders_rfm,sub_sorders)
  }
  return(sorders_rfm)
}

# Creating the final RFM Score based on the assigned weightages R=1, F=1, M1=2, M2=2
generate_final_score<- function(df)
{
  for (i in 1:nrow(df))
  {
    df$rfmm_score[i] = ((df$R[i] * 1) + (df$F[i] * 1) + (df$M1[i] * 1) + (df$M2[i] * 1))/4          
  }
  return(df)
}

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#--------------------------------------------------------------------#
# Call to function get_rfm to get the recency, frequency and monetary values
recency_freq_mon = get_rfm(sorders,"2016-01-01",Sys.Date())

#--------------------------------------------------------------------#
#call to generate the R,F & M matrix -
rfm_matrix_sorders<- generate_rfmm_matrix(recency_freq_mon)

#--------------------------------------------------------------------#
#call to generate the final R,F,M scores-
rfm_values_sorders<- generate_final_score(rfm_matrix_sorders)

rfm_values_sorders$scaled_Recency<- scale(rfm_values_sorders$Recency)
rfm_values_sorders$scaled_Frequency<- scale(rfm_values_sorders$Frequency)
rfm_values_sorders$scaled_Monetary_Vol<- scale(rfm_values_sorders$Monetary_Vol)
rfm_values_sorders$scaled_Monetary_Val<- scale(rfm_values_sorders$Monetary_Val)

View(rfm_values_sorders[,18:21])

set.seed(1111)
sales_fit <- kmeans(rfm_values_sorders[, 18:21], centers =6, nstart = 20)
sales_fit$centers
sales_fit$size


## Determine number of clusters
Cluster_Variability <- matrix(nrow=8, ncol=1)
for (i in 1:8) Cluster_Variability[i] <- kmeans(rfm_values_sorders[,18:21],centers=i, nstart=200)$tot.withinss
plot(1:8, Cluster_Variability, type="b", xlab="Number of clusters", ylab="Within groups sum of squares") ## Elbow curve or Scree plot

customer_cohorts<- cbind(rfm_values_sorders, sales_fit$cluster) # Append cluster id
#write.csv(customer_cohorts,"customer_cohorts.csv")

sorders_final_scores = customer_cohorts

for (i in 1:nrow(sorders_final_scores))
{
  if(sorders_final_scores$rfmm_score[i] >=3.8)
  {
    sorders_final_scores$newclass[i] <- "BEST"
  }
  else if((sorders_final_scores$rfmm_score[i] >=2.5) & (sorders_final_scores$rfmm_score[i] <3.8) 
          & sorders_final_scores$R[i] <=2)
  {
    sorders_final_scores$newclass[i] <- "INACTIVE BIG"
  }
  else if((sorders_final_scores$rfmm_score[i] >=2.5) & (sorders_final_scores$rfmm_score[i] <3.5)
          & sorders_final_scores$R[i] >=3)
  {
    sorders_final_scores$newclass[i] <- "ACTIVE"
  }
  else if((sorders_final_scores$rfmm_score[i] >=1.5) & (sorders_final_scores$rfmm_score[i] <2.5)
          & sorders_final_scores$R[i] <=3)
  {
    sorders_final_scores$newclass[i] <- "AT RISK"
  }
  else if((sorders_final_scores$rfmm_score[i] >=1.5) & (sorders_final_scores$rfmm_score[i] <2.5)
          & sorders_final_scores$R[i] >=4)
  {
    sorders_final_scores$newclass[i] <- "ACTIVE CHEAP"
  }
  else if((sorders_final_scores$rfmm_score[i] < 1.5)) 
  {
    sorders_final_scores$newclass[i] <- "LOST"
  }
}

cohort1 = subset(sorders_final_scores,sorders_final_scores$`sales_fit$cluster`==1)
barplot(table(cohort1$newclass))
sample_cohort1<- cohort1[sample(1:nrow(cohort1), 15),]

cohort2 = subset(sorders_final_scores,sorders_final_scores$`sales_fit$cluster`==2)
barplot(table(cohort2$newclass))
sample_cohort2<- cohort2[sample(1:nrow(cohort2), 15),]

cohort3 = subset(sorders_final_scores,sorders_final_scores$`sales_fit$cluster`==3)
barplot(table(cohort3$newclass))
sample_cohort3<- cohort3[sample(1:nrow(cohort3), 15),]

cohort4 = subset(sorders_final_scores,sorders_final_scores$`sales_fit$cluster`==4)
barplot(table(cohort4$newclass))

cohort5 = subset(sorders_final_scores,sorders_final_scores$`sales_fit$cluster`==5)
barplot(table(cohort5$newclass))

cohort6 = subset(sorders_final_scores,sorders_final_scores$`sales_fit$cluster`==6)
barplot(table(cohort6$newclass))

cohort7 = subset(sorders_final_scores,sorders_final_scores$`sales_fit$cluster`==7)
table(cohort7$newclass)

cohort8 = subset(sorders_final_scores,sorders_final_scores$`sales_fit$cluster`==8)
table(cohort8$newclass)

names(sorders_final_scores)[22]<-"cluster_id"

write.csv(sorders_final_scores,"customer_cohorts.csv")
dbWriteTable(vertica, name = "public.customer_scores_cohorts", value = sorders_final_scores, row.names = FALSE)


sample_cohort<- rbind(sample_cohort1,sample_cohort2,sample_cohort3)

test_recency<-as.data.frame(aggregate(Recency~newclass,sorders_final_scores,range))
write.csv(test_recency,"test_recency.csv")

test_freq<-as.data.frame(aggregate(Frequency~newclass,sorders_final_scores,range))
write.csv(test_freq,"test_freq.csv")

test_mon_vol<-as.data.frame(aggregate(Monetary_Vol~newclass,sorders_final_scores,range))
write.csv(test_mon_vol,"test_mon_vol.csv")

test_mon_val<-as.data.frame(aggregate(Monetary_Val~newclass,sorders_final_scores,range))
write.csv(test_mon_val,"test_mon_val.csv")

customer_count= as.data.frame(table(sorders_final_scores$newclass))
write.csv(customer_count,"customer_count.csv")



library(plotly)
library(scatterplot3d)

scatterplot3d(x = sorders_final_scores$Recency, # x axis
        y = sorders_final_scores$Frequency,  # y axis
        z = sorders_final_scores$Monetary_Val,  # z axis
        color= sorders_final_scores$`sales_fit$cluster`,
        pch= 1,
        main = "Customer RFM spread plot",
        xlab="Recency",
        ylab="Frequency",
        zlab="Monetary Value",
        label.tick.marks = T,
        box=T)        
 

scatterplot3d(x = sorders_final_scores$scaled_Recency, # x axis
              y = sorders_final_scores$scaled_Frequency,  # y axis
              z = sorders_final_scores$scaled_Monetary_Val,  # z axis
              color= sorders_final_scores$`sales_fit$cluster`,
              pch= 1,
              main = "Customer RFM spread plot",
              xlab="Scaled Recency",
              ylab="Scaled Frequency",
              zlab="scaled Monetary Value",
              label.tick.marks = T,
              box=T) 

