#install.packages("RJDBC")
library(RJDBC)

vDriver <- JDBC(driverClass="com.vertica.jdbc.Driver", classPath="D:\\Software\\Vertica\\vertica-jdbc-9.1.0-0.jar")

vertica <- dbConnect(vDriver, "jdbc:vertica://server_name:port/DB_Name", "user_name", "password")

myframe = dbGetQuery(vertica, "select * from v_monitor.active_events")

# Write the data frame to the database
dbWriteTable(vertica, name = "public.sales_orders", value = myframe, row.names = FALSE)

#write the sales order data into Vertica

sorders <- read.csv("RFM Data dump from Sales Order-datadump.csv")
dbWriteTable(vertica, name = "public.sales_orders", value = sorders, row.names = FALSE)

invoice<- read.csv("RFM Data dump from Sales Invoice-datadump.csv")
dbWriteTable(vertica, name = "public.invoice", value = sorders, row.names = FALSE)


sales<- dbGetQuery(vertica, "select * from public.sales_orders")
sales$CustCD <- as.factor(sales$CustCD)
sales$Date <- as.factor(sales$Date)
sales$CustName <- as.factor(sales$CustName)
sales$Segment <- as.factor(sales$Segment)
sales$Country <- as.factor(sales$Country)
sales$Region <- as.factor(sales$Region)

sales$Date <- as.Date(sales$Date, "%m/%d/%y")



sales_invoice<- dbGetQuery(vertica, "select * from public.invoice")

View(sales_invoice)

str(sales)
