setwd("~/Uni Dokumente/BA/Data")
library(reshape2)
library(plyr)
library(dummies)
library(mFilter)
library(plm)

# Load and modify the UN Annual population dataset

annual.pop <- read.csv("~/Uni Dokumente/BA/Data/R/WPP2015_POP_F15_1_ANNUAL_POPULATION_BY_AGE_BOTH_SEXES.csv", header=TRUE, sep=";", skip = 16, check.names = F)

annual.pop <- annual.pop[,-2]

names(annual.pop)[names(annual.pop) == "Major area, region, country or area *"] <- "Country"
names(annual.pop)[names(annual.pop) == "Reference date (as of 1 July)"] <- "Year"
names(annual.pop)[names(annual.pop) == "Country code"] <- "Country.code"

# subset countries

Pop_Data <- subset(annual.pop, Country.code %in% c("32", "56", "124", "756", "152", "36", "276", "203", "208", "724", "233", "246", "250", "826", "300", "372", "376",#
                                                   "352", "380", "410", "442", "428", "484", "528", "578", "554", "616", "620", "752", "705", "840", "392", "792",#
                                                   "348", "40", "703", "784", "76", "50", "100", "156", "170", "214", "218", "356", "404", "458", "504",#
                                                   "586", "604", "360", "608", "643", "634", "642", "648", "686", "710", "764", "788", "804", "858", "862", "191", "188", "818",#
                                                   "364", "566"))



#subset year observations, poor data consistency prior to 1960
Pop_Data <- Pop_Data[!(Pop_Data$Year < 1960), ]



Pop_Data[] <- lapply(Pop_Data, function(x) if(is.factor(x)) factor(x) else x)


# Load and subset the WDI dataset containing economic indicators



WDI <- read.csv2("~/Uni Dokumente/BA/Data/R/WDIlong.csv", header = TRUE, sep = ",", na.strings = "..")


# subsetting whole dataset to variables and countries of interest



WDI_total <- subset(WDI, Country.Code %in% c("BEL", "CAN", "CHE", "CHL", "AUS", "DEU", "CZE", "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "IRL",#
                                              "ISR", "ISL", "ITA", "KOR", "LUX", "LVA", "MEX", "NLD", "NOR", "NZL", "POL", "PRT", "SWE", "SVN", "USA",#
                                              "JPN", "TUR", "HUN", "AUT", "SVK", "ARG", "ARE", "BRA", "BGD", "BGR", "CHN", "COL", "DOM", "ECU", "IND", "IDN", "KEN", "MYS", "MAR",#
                                            "PAK", "PER", "PHL", "QAT", "ROM", "RUS", "SEN", "ZAF", "THA", "TUN", "UKR", "URY", "VEN", "HRV", "CRI",#
                                            "EGY", "IRN", "NGA"))


# relable features for easier use later on

names(WDI_total)[names(WDI_total) == "Country.Name"] <- "Country" 
names(WDI_total)[names(WDI_total) == "�..Time"] <- "Year"
names(WDI_total)[names(WDI_total) == "Broad.money.growth..annual.....FM.LBL.BMNY.ZG."] <- "M2Growth"
names(WDI_total)[names(WDI_total) == "Central.government.debt..total....of.GDP...GC.DOD.TOTL.GD.ZS."] <- "Gov.Debt.GDP"
names(WDI_total)[names(WDI_total) == "Inflation..consumer.prices..annual.....FP.CPI.TOTL.ZG."] <- "Inflation"
names(WDI_total)[names(WDI_total) == "GDP.growth..annual.....NY.GDP.MKTP.KD.ZG."] <- "GDP.Growth"
names(WDI_total)[names(WDI_total) == "GINI.index..World.Bank.estimate...SI.POV.GINI."] <- "Gini.index"
names(WDI_total)[names(WDI_total) == "Health.expenditure..total....of.GDP...SH.XPD.TOTL.ZS."] <- "Health.Exp"
names(WDI_total)[names(WDI_total) == "Current.account.balance....of.GDP...BN.CAB.XOKA.GD.ZS."] <- "Acc.Balance"
names(WDI_total)[names(WDI_total) == "Emigration.rate.of.tertiary.educated....of.total.tertiary.educated.population...SM.EMI.TERT.ZS."] <- "Em.Tert.Edu"
names(WDI_total)[names(WDI_total) == "Health.expenditure.per.capita..PPP..constant.2011.international.....SH.XPD.PCAP.PP.KD."] <- "Health.Exp.p.C"
names(WDI_total)[names(WDI_total) == "Life.expectancy.at.birth..total..years...SP.DYN.LE00.IN."] <- "Life.Expectancy"
names(WDI_total)[names(WDI_total) == "Net.migration..SM.POP.NETM."] <- "Net.Migration"
names(WDI_total)[names(WDI_total) == "Real.interest.rate......FR.INR.RINR."] <- "Real.Interest"
names(WDI_total)[names(WDI_total) == "Net.foreign.assets..current.LCU...FM.AST.NFRG.CN."] <- "Net.Fgn.Assets"
names(WDI_total)[names(WDI_total) == "Share.of.women.in.wage.employment.in.the.nonagricultural.sector....of.total.nonagricultural.employment...SL.EMP.INSV.FE.ZS."] <- "Share.Fem.Emp"
names(WDI_total)[names(WDI_total) == "Research.and.development.expenditure....of.GDP...GB.XPD.RSDV.GD.ZS."] <- "RnD.Exp"
names(WDI_total)[names(WDI_total) == "Urban.population....of.total...SP.URB.TOTL.IN.ZS."] <- "Urban.Pop"
names(WDI_total)[names(WDI_total) == "GDP.at.market.prices..constant.2010.US....NY.GDP.MKTP.KD."] <- "Real.GDP"


WDI_total[] <- lapply(WDI_total, function(x) if(is.factor(x)) factor(x) else x)

WDI_total[,1] <- as.numeric(levels(WDI_total$Year)[WDI_total$Year])




# load TED data set for labor productivity indicators

TED <- read.csv("~/Uni Dokumente/BA/Data/LaborOutputProductivityTEDadj.csv", header=TRUE, sep=";", skip = 4, check.names = F)

#extract variables of interest

TED.var <- subset(TED, INDICATOR %in% c("AVGHR", "LP_eksL", "LP_eksH", "LP_L_g", "LP_H_g", "EMP"))

# country selection

TED.data <- subset(TED.var, COUNTRY %in% c("Argentina", "Belgium", "Canada", "Switzerland", "Chile", "Australia", "Germany", "Czech Republic", "Denmark", "Spain", "Estonia",#
                                           "Finland", "France", "United Kingdom", "Greece", "Ireland", "Israel", "Iceland", "Italy", "South Korea", "Luxembourg",#
                                           "Latvia", "Mexico", "Netherlands", "Norway", "New Zealand", "Poland", "Portugal", "Sweden", "Slovenia", "United States",#
                                           "Japan", "Turkey", "Hungary", "Austria", "Slovak Republic", "United Arab Emirates", "Brazil", "Bangladesh", "Bulgaria", "China (Alternative)",#
                                           "Colombia", "Dominican Republic", "Ecuador", "India", "Indonesia", "Kenya", "Malaysia", "Morocco", "Pakistan", "Peru",#
                                           "Philippines", "Qatar", "Romania", "Russian Federation", "Senegal", "South Africa", "Thailand", "Tunisia", "Ukraine",#
                                           "Uruguay", "Venezuela", "Croatia", "Costa Rica", "Egypt", "Iran", "Nigeria"))

# reformat data to long

TED_long <- melt(TED.data, id.vars = c("REGION", "COUNTRY", "MEASURE", "INDICATOR"), variable.name = "Year")

TED.2 <- dcast(TED_long, REGION+COUNTRY+Year~MEASURE+INDICATOR)

# rename variables

names(TED.2)[names(TED.2) == "COUNTRY"] <- "Country"

names(TED.2)[names(TED.2) == "Annual hours worked per worker_AVGHR"] <- "HrsWorked"

names(TED.2)[names(TED.2) == "Growth of Labor Productivity per hour worked, percent change_LP_H_g"] <- "g.LP.Hrs"

names(TED.2)[names(TED.2) == "Growth of Labor Productivity per person employed, percent change_LP_L_g"] <- "g.LP.Emp"

names(TED.2)[names(TED.2) == "Labor productivity per hour worked in 2015 US$ (converted to 2015 price level with updated 2011 PPPs)_LP_eksH"] <- "LP.Hrs"

names(TED.2)[names(TED.2) == "Labor productivity per person employed in 2015 US$ (converted to 2015 price level with updated 2011 PPPs)_LP_eksL"] <- "LP.Emp"

names(TED.2)[names(TED.2) == "Persons employed (in thousands of persons)_EMP"] <- "Emp"


TED.2$REGION <- NULL



TED.2[sapply(TED.2, is.character)] <- lapply(TED.2[sapply(TED.2, is.character)], as.factor)


TED.2[,2] <- as.numeric(levels(TED.2$Year)[TED.2$Year])

TED.2 <- TED.2[!(TED.2$Year < 1960),]




TED.2[] <- lapply(TED.2, function(x) if(is.factor(x)) factor(x) else x)




# standardize the country labels among the datasets to use a primary merge key


TED.2$COUNTRY <- revalue(TED.2$COUNTRY, c("China (Alternative)" = "China", "Slovak Republic" = "Slovakia")) 

WDI_total$Country.Name <- revalue(WDI_total$Country.Name, c("Egypt, Arab Rep." = "Egypt", "Iran, Islamic Rep." = "Iran", "Korea, Rep." = "South Korea", "Slovak Republic" = "Slovakia", "Venezuela, RB" = "Venezuela"))

Pop_Data$Country <- revalue(Pop_Data$Country, c("Iran (Islamic Republic of)" = "Iran", "Republic of Korea" = "South Korea", "United States of America" = "United States", "Venezuela (Bolivarian Republic of)" = "Venezuela"))



# merge Data Sets

UN.WDI <- merge.data.frame(Pop_Data, WDI_total, by = c("Country", "Year"))

Total <- merge.data.frame(UN.WDI, TED.2, by = c("Country", "Year"))

Total$Notes <- NULL
Total$Index <- NULL
Total$Time.Code <- NULL



#############################################################
# save merged data for easier reuse
write.csv(Total, file = "Data", row.names = F)

#############################################################

Total <- read.csv("Data", check.names = F, stringsAsFactors = F)


factorvars <- dput(variable.names(Total))
factorvars <- factorvars[c(-1,-26)]

Total[] <- lapply(Total, function(x) if(is.factor(x)) factor(x) else x)


Total[] <- lapply(Total, function(x) gsub("[[:blank:]]", "", x))

Total[factorvars] <- lapply(Total[factorvars], function(x) gsub("E", "e", x))

Total[] <- lapply(Total, function(x) gsub(",", "\\.", x))

Total[factorvars] <- sapply(Total[factorvars], function(x) as.numeric(x))


# asses missing values to get understanding of the data quality
# important for assesment of regression results


propmiss <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      nmiss=sum(is.na(x)), 
      n=length(x), 
      propmiss=sum(is.na(x))/length(x)
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  return(d[order(d$propmiss), ])
}

Missing <- propmiss(Total)
Missing90 <- propmiss(Total[Total$Year>="1990",])
Missing80 <- propmiss(Total[Total$Year>="1980",])


# Build the category indicators for the regression

Total$Developed <- ifelse(Total$Country.code %in% c("32", "56", "124", "756", "152", "36", "276", "203", "208", "724", "233", "246", "250", "826", "300", "372", "376",#
                                                    "352", "380", "410", "442", "428", "484", "528", "578", "554", "616", "620", "752", "705", "840", "392", "792",#
                                                    "348", "40", "703"), 1, 0)


# Variable for 80+ post 1990

Total$`80+`[] <- ifelse(Total$Year >= "1990", Total$`80-84` + Total$`85-89`+ Total$`90-94` + Total$`95-99` + Total$`100+`, Total$`80+` )


# construct the standardized variables for demographics 

Total$Population <- rowSums(subset(Total, select = `0-4`:`80+`))

Total$Young <- (Total$`0-4` + Total$`5-9` + Total$`10-14`)/Total$Population

Total$Working <- rowSums(subset(Total, select = `15-19`:`60-64`))/Total$Population

Total$Old <- rowSums(subset(Total, select = `65-69`:`80+`))/Total$Population


Total$Dep.Ratio <- (Total$Young + Total$Old)/Total$Working

# Variable for growth of Net Foreign Assets 


Total$NFA.Growth <- with(Total, ave(Net.Fgn.Assets, Country, FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))




######### Detrend GDP variables with Hodrick Prescott Filter #####


Total$Country <- as.factor(Total$Country)

hodrick <- function(x){
  vals <- hpfilter(x, 100)
  hpcycle <- vals$cycle
  return(hpcycle)
}

Total$Output.Gap <- with(Total, ave(Real.GDP, Country, FUN=hodrick))

hodrick.trend <- function(x){
  vals <- hpfilter(x, 100)
  hptrend <- vals$trend
  return(hptrend)
}

Tot.panel$gdp.trend <- with(Tot.panel, ave(Real.GDP, Country, FUN=hodrick.trend))

Tot.panel$Y.gap <- (Tot.panel$Output.Gap/Tot.panel$gdp.trend)*100




##### tranform to plm compatible panel data, seperate by oecd and emerging ####

Tot.panel <- plm.data(Total, indexes = c("Country", "Year"))

                   




#####Remove outliers and rescale####




##### indicator for developed countries

Tot.panel$Developed <- ifelse(Tot.panel$Country.code %in% c("56", "124", "756", "36", "276", "203", "208", "724", "246", "250", "826", "300", "372", "376",#
                                                    "380", "410", "442", "528", "578", "554", "616", "620", "752", "705", "840", "392",#
                                                    "348", "40", "703"), 1, 0)


#####define age categories####

Tot.panel$Young <- (Tot.panel$X0.4 + Tot.panel$X5.9 + Tot.panel$X10.14 + Tot.panel$X15.19 + Tot.panel$X20.24)/Tot.panel$Population


Tot.panel$Working <- (Tot.panel$X25.29 + Tot.panel$X30.34 + Tot.panel$X35.39+ Tot.panel$X40.44 + Tot.panel$X45.49 + Tot.panel$X50.54 + Tot.panel$X55.59 + Tot.panel$X60.64)/Tot.panel$Population

Tot.panel$Old <- (Tot.panel$X65.69 + Tot.panel$X70.74 + Tot.panel$X80.)/Tot.panel$Population

Tot.panel$Dep.Ratio <- (Tot.panel$Young + Tot.panel$Old)/Tot.panel$Working

Tot.panel$Young <- Tot.panel$Young*100
Tot.panel$Working <- Tot.panel$Working*100
Tot.panel$Old <- Tot.panel$Old*100
Tot.panel$Dep.Ratio <- Tot.panel$Dep.Ratio*100
Tot.panel$NFA.Growth <- Tot.panel$NFA.Growth*100


Tot.panel$D74 <- ifelse(Tot.panel$Year==1974, 1, 0)
Tot.panel$D80 <- ifelse(Tot.panel$Year==1980, 1, 0)

Tot.panel$Pop.Growth <- with(Tot.panel, ave(Population, Country, FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))


###Total panel

# Out of convinience and due to rather small size of dataset, I decided bakc then to just save them seperately.
# When i look at it today its rather silly, but i was just getting started back then :)

Tot.panel <- read.csv("Totpanel", check.names = F, stringsAsFactors = F)


###OECD


oecd <- plm.data(Tot.panel[Tot.panel$Developed==1,], indexes = c("Country", "Year"))
write.csv(oecd, file = "Oecdpanel", row.names = F)

###Emerging
emerging <- plm.data(Tot.panel[Tot.panel$Developed==0,], indexes = c("Country", "Year"))
write.csv(emerging, file = "Emergingpanel", row.names = F)


####inflation

#####plots
####function for plotting the inflation developement with min max shades around the mean to gauge variance in the sample

plotLineWithRange <- function(x, yVal, yMin, yMax,
                              lineColor="Black", rangeColor="LightBlue",
                              main="", xlab="X", ylab="Y"){
  x <- as.numeric(levels(x))
  if(missing(x)){
    x <- 1:length(yVal)
  }
  stopifnot(length(yVal) == length(yMin) && length(yVal) == length(yMax))
  
  plot(x=c(min(x),max(x)),y=c(min(yMin),max(yMax)),type="n", main=main,xlab=xlab,ylab=ylab)
  polygon(x=c(x,rev(x)),y=c(yMax,rev(yVal)),col=rangeColor,border=NA)
  polygon(x=c(x,rev(x)),y=c(yMin,rev(yVal)),col=rangeColor,border=NA)
  lines(x=x,y=yVal,col=lineColor)
  legend('topright', legend = c("Mean", "Min. Max. Values in Sample"), lty=1, col=c('black', 'LightBlue'), bty='n', cex=.75)
}

###function to calculate mean min and max by year in samples

mean.min.max <- function(data1, data2, labelz){
  mean1 <- aggregate(data1, list(data2), mean, na.rm=TRUE, na.action=NULL)
  min1 <- aggregate(data1, list(data2), min, na.rm=TRUE, na.action=NULL)
  max1 <- aggregate(data1, list(data2), max, na.rm=TRUE, na.action=NULL)
  aggr1 <- merge.data.frame(mean1, min1, by = "Group.1")
  out <- merge.data.frame(aggr1, max1, by = "Group.1")
  colnames(out) <- labelz
  return(out)
}



##### generate old age variables with complete young cohorts#####

oecd.plot$Young <- (oecd.plot$X0.4 + oecd.plot$X5.9 + oecd.plot$X10.14 + oecd.plot$X15.19 + oecd.plot$X20.24)/oecd.plot$Population
emerging.plot$Young <- (emerging.plot$X0.4 + emerging.plot$X5.9 + emerging.plot$X10.14 + emerging.plot$X15.19 + emerging.plot$X20.24)/emerging.plot$Population
oecd.plot$Old.Dep.Ratio <- oecd.plot$Old/oecd.plot$Working
emerging.plot$Old.Dep.Ratio <- emerging.plot$Old/emerging.plot$Working
#### mean min max inflation

infl.oecd <- mean.min.max(oecd.plot$Inflation, oecd.plot$Year, labelz = c("Year", "Mean.Inflation", "Min", "Max"))
infl.emerging <- mean.min.max(emerging.plot$Inflation, emerging.plot$Year, labelz = c("Year", "Mean.Inflation", "Min", "Max"))

####mean min max dep ratio


depr.oecd <- mean.min.max(oecd.plot$Old.Dep.Ratio, oecd.plot$Year, labelz = c("Year", "Mean.Depr", "Min", "Max"))
depr.emerging <- mean.min.max(emerging.plot$Old.Dep.Ratio, emerging.plot$Year, labelz = c("Year", "Mean.Depr", "Min", "Max"))


####plot function#####

plotLineWithRange(infl.oecd$Year, infl.oecd$Mean.Inflation, infl.oecd$Min, infl.oecd$Max, main = "Inflation in Developed Countries", xlab = "Year", ylab = "Inflation")

plotLineWithRange(infl.emerging$Year, infl.emerging$Mean.Inflation, infl.emerging$Min, infl.emerging$Max, main = "Inflation in Emerging Countries", xlab = "Year", ylab = "Inflation")

plotLineWithRange(depr.oecd$Year, depr.oecd$Mean.Depr, depr.oecd$Min, depr.oecd$Max, main = "Old Age Dependency Ratio in Developed Countries", xlab = "Year", ylab = "Old Age Dependency Ratio")

plotLineWithRange(depr.emerging$Year, depr.emerging$Mean.Depr, depr.emerging$Min, depr.emerging$Max, main = "Old Age Dependency Ratio in Emerging Countries", xlab = "Year", ylab = "Old Age Dependency Ratio")


######### REGRESSION ANALYSIS ########

### Regressions for OECD###

library(plm)
library(stargazer)
library(sandwich)
library(lmtest)


oecd <- read.csv("Oecdpanel", check.names = F, stringsAsFactors = T)
oecd <- plm.data(oecd, indexes = c("Country", "Year"))



###basic country fixed

###just depratio


b1 <- plm(Inflation ~ Dep.Ratio, data = oecd, effect = "individual", model = "within")

# It was necessary to rule out biases in the standard errors of the estimates due to temporal and cross sectional dependence
# no adequat error estimators in the plm backage, therefor had to be computed seperately from the covariance matrix

cov1         <- vcovSCC(b1) 
se.b1    <- sqrt(diag(cov1)) 

b1t <- plm(Inflation ~ Dep.Ratio + Year, data = oecd, effect = "individual", model = "within")

cov1t         <- vcovSCC(b1t)
se.b1t    <- sqrt(diag(cov1t))



b2 <- plm(Inflation ~ Young + Working + Old - 1, data = oecd, effect = "individual", model = "within")

cov2         <- vcovSCC(b2)
se.b2    <- sqrt(diag(cov2))


b2t <- plm(Inflation ~ Young + Working + Old + Year - 1, data = oecd, effect = "individual", model = "within")

cov2t         <- vcovSCC(b2t)
se.b2t    <- sqrt(diag(cov2t))


b3 <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap + Real.Interest, data = oecd, effect = "individual", model = "within")

cov3         <- vcovSCC(b3)
se.b3    <- sqrt(diag(cov3))


b3t <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap + Real.Interest + Year, data = oecd, effect = "individual", model = "within")

cov3t         <- vcovSCC(b3t)
se.b3t    <- sqrt(diag(cov3t))



b4 <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap, data = oecd, effect = "individual", model = "within")

cov4         <- vcovSCC(b4)
se.b4    <- sqrt(diag(cov4))


b4t <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap + Year, data = oecd, effect = "individual", model = "within")

cov4t         <- vcovSCC(b4t)
se.b4t    <- sqrt(diag(cov4t))


b4o <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap + D74 + D80, data = oecd, effect = "individual", model = "within")

cov4o         <- vcovSCC(b4o)
se.b4o    <- sqrt(diag(cov4o))


#####latex output for nicely formatted tables

stargazer(b1, b1t, b2, b2t, b3, b3t, b4, b4t, type = "latex", se = list(se.b1, se.b1t, se.b2, se.b2t, se.b3, se.b3t, se.b4, se.b4t), title = "Basic Regression for Advanced Countries",#
          omit.stat = c("f", "rsq", "ser"), omit = c("Country", "Year"), add.lines = list(c("Country Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),#
                                                                                          c("Time Effects", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes")), out = "TeXSCCBasicReg.tex")


######### different time Periods#########

####create different samples

####1960-1990
oecd <- read.csv("Oecdpanel", check.names = F, stringsAsFactors = T)
oecd60.90 <- subset(oecd, Year<=1990)
write.csv(oecd60.90, file = "oecd6090panel", row.names = F)
oecd60.90 <- plm.data(oecd60.90, indexes = c("Country", "Year"))
miss90 <- propmiss(oecd60.90)

oecd60.90 <- read.csv("oecd6090panel", check.names = F, stringsAsFactors = T)


####1985-2015

oecd85.15 <- subset(oecd, Year>=1985)
write.csv(oecd85.15, file = "oecd8515panel", row.names = F)
oecd85.15 <- plm.data(oecd85.15, indexes = c("Country", "Year"))
miss15 <- propmiss(oecd85.15)

oecd85.15 <- read.csv("oecd8515panel", check.names = F, stringsAsFactors = T)


####1985 - 2006

oecd06 <- subset(oecd, Year>=1985 & Year<=2006)
write.csv(oecd06, file = "oecd06panel", row.names = F)
oecd06 <- plm.data(oecd06, indexes = c("Country", "Year"))
miss06 <- propmiss(oecd06)

oecd06 <- read.csv("oecd06panel", check.names = F, stringsAsFactors = T)


####load different periods again (Yes i liked loading data back then...)

oecd60.90 <- read.csv("oecd6090panel", check.names = F, stringsAsFactors = T)
oecd60.90 <- plm.data(oecd60.90, indexes = c("Country", "Year"))

oecd85.15 <- read.csv("oecd8515panel", check.names = F, stringsAsFactors = T)
oecd85.15 <- plm.data(oecd85.15, indexes = c("Country", "Year"))

oecd06 <- read.csv("oecd06panel", check.names = F, stringsAsFactors = T)
oecd06 <- plm.data(oecd06, indexes = c("Country", "Year"))

####basic model reg over diff periods with and without time effects for each period


p60 <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap, data = oecd60.90, effect = "individual", model = "within")
cov60p         <- vcovSCC(p60)
se.p60    <- sqrt(diag(cov60p))


p85 <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap, data = oecd85.15, effect = "individual", model = "within")

cov85p         <- vcovSCC(p85)
se.p85    <- sqrt(diag(cov85p))


p06 <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap, data = oecd06, effect = "individual", model = "within")

cov06p         <- vcovSCC(p06)
se.p06    <- sqrt(diag(cov06p))


####time fixed effects


p60t <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap + Year, data = oecd60.90, effect = "individual", model = "within")
cov60pt         <- vcovSCC(p60t)
se.p60t    <- sqrt(diag(cov60pt))


p85t <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap + Year, data = oecd85.15, effect = "individual", model = "within")

cov85pt         <- vcovSCC(p85t)
se.p85t    <- sqrt(diag(cov85pt))


p06t <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap + Year, data = oecd06, effect = "individual", model = "within")

cov06pt         <- vcovSCC(p06t)
se.p06t    <- sqrt(diag(cov06pt))



#####latex table

stargazer(p60, p60t, p85, p85t, p06, p06t, type = "latex", se = list(se.p60, se.p60t, se.p85, se.p85t, se.p06, se.p06t), title = "Different Time Periods for Advanced Countries",#
          omit.stat = c("f", "rsq", "ser"), omit = c("Country", "Year"), add.lines = list(c("Country Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),#
                                                                                          c("Time Effects", "No", "Yes", "No", "Yes", "No", "Yes"),#
                                                                                          c("Time Period", "1960-1990", "1960-1990", "1985-2015", "1985-2015", "1985-2006", "1985-2006")), out = "TeXSCCPeriodReg.tex")



#######different Controls######

r0 <- plm(Inflation ~ Young + Working + Old + Life.Expectancy - 1 + Y.gap, data = oecd, effect = "individual", model = "within")
cov0r         <- vcovSCC(r0)
se.r0    <- sqrt(diag(cov0r))


r1 <- plm(Inflation ~ Young + Working + Old + Pop.Growth - 1 + Y.gap, data = oecd, effect = "individual", model = "within")
cov1r         <- vcovSCC(r1)
se.r1    <- sqrt(diag(cov1r))


r2 <- plm(Inflation ~ Young + Working + Old + LP.Hrs - 1 + Y.gap, data = oecd, effect = "individual", model = "within")
cov2r         <- vcovSCC(r2)
se.r2    <- sqrt(diag(cov2r))

r3 <- plm(Inflation ~ Young + Working + Old + HrsWorked + LP.Hrs - 1 + Y.gap, data = oecd, effect = "individual", model = "within")
cov3r         <- vcovSCC(r3)
se.r3    <- sqrt(diag(cov3r))

r4 <- plm(Inflation ~ Young + Working + Old + Net.Fgn.Assets - 1 + Y.gap, data = oecd, effect = "individual", model = "within")
cov4r         <- vcovSCC(r4)
se.r4    <- sqrt(diag(cov4r))

r5 <- plm(Inflation ~ Young + Working + Old + M2Growth - 1 + Y.gap, data = oecd, effect = "individual", model = "within")
cov5r         <- vcovSCC(r5)
se.r5    <- sqrt(diag(cov5r))

r6 <- plm(Inflation ~ Young + Working + Old + Real.Interest + Acc.Balance + Year - 1 + Y.gap, data = oecd85.15, effect = "individual", model = "within")
cov6r         <- vcovSCC(r6)
se.r6    <- sqrt(diag(cov6r))


#####latex table

stargazer(r0, r1, r2, r3, r5, r6, type = "latex", se = list(se.r0, se.r1, se.r2, se.r3, se.r5, se.r6), title = "Different Controls for Advanced Countries",#
          omit.stat = c("f", "rsq", "ser"), omit = c("Country", "Year"), add.lines = list(c("Country Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),#
                                                                                          c("Time Effects", "No", "No", "No", "No", "No", "Yes"),#
                                                                                          c("Time Period", "1960-2015", "1960-2015", "1960-2015", "1960-2015", "1960-2015", "1985-2015")), out = "TeXSCCControlReg.tex")


#######Regressions for Emerging Country sample###########



emerging <- read.csv("Emergingpanel", check.names = F, stringsAsFactors = T)
emerging <- plm.data(emerging, indexes = c("Country", "Year"))
missemerge <- propmiss(emerging)

####1960-1990

emerging <- read.csv("emergingpanel", check.names = F, stringsAsFactors = T)
emerging60.90 <- subset(emerging, Year<=1990)
write.csv(emerging60.90, file = "emerging6090panel", row.names = F)
emerging60.90 <- plm.data(emerging60.90, indexes = c("Country", "Year"))
missemerging90 <- propmiss(emerging60.90)

emerging60.90 <- read.csv("emerging6090panel", check.names = F, stringsAsFactors = T)


####1985-2015

emerging85.15 <- subset(emerging, Year>=1985)
write.csv(emerging85.15, file = "emerging8515panel", row.names = F)
emerging85.15 <- plm.data(emerging85.15, indexes = c("Country", "Year"))
missemerging15 <- propmiss(emerging85.15)

emerging85.15 <- read.csv("emerging8515panel", check.names = F, stringsAsFactors = T)


####1985 - 2006

emerging06 <- subset(emerging, Year>=1985 & Year<=2006)
write.csv(emerging06, file = "emerging06panel", row.names = F)
emerging06 <- plm.data(emerging06, indexes = c("Country", "Year"))
missemerging06 <- propmiss(emerging06)

emerging06 <- read.csv("emerging06panel", check.names = F, stringsAsFactors = T)

#################Regression emerging######

#####basic####

b1 <- plm(Inflation ~ Dep.Ratio, data = emerging, effect = "individual", model = "within")

cov1         <- vcovSCC(b1)
se.b1    <- sqrt(diag(cov1))

b1t <- plm(Inflation ~ Dep.Ratio + Year, data = emerging, effect = "individual", model = "within")

cov1t         <- vcovSCC(b1t)
se.b1t    <- sqrt(diag(cov1t))



b2 <- plm(Inflation ~ Young + Working + Old - 1, data = emerging, effect = "individual", model = "within")

cov2         <- vcovSCC(b2)
se.b2    <- sqrt(diag(cov2))


b2t <- plm(Inflation ~ Young + Working + Old + Year - 1, data = emerging, effect = "individual", model = "within")

cov2t         <- vcovSCC(b2t)
se.b2t    <- sqrt(diag(cov2t))


b3 <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap + Real.Interest, data = emerging, effect = "individual", model = "within")

cov3         <- vcovSCC(b3)
se.b3    <- sqrt(diag(cov3))


b3t <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap + Real.Interest + Year, data = emerging, effect = "individual", model = "within")

cov3t         <- vcovSCC(b3t)
se.b3t    <- sqrt(diag(cov3t))



b4 <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap, data = emerging, effect = "individual", model = "within")

cov4         <- vcovSCC(b4)
se.b4    <- sqrt(diag(cov4))


b4t <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap + Year, data = emerging, effect = "individual", model = "within")

cov4t         <- vcovSCC(b4t)
se.b4t    <- sqrt(diag(cov4t))


b4o <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap + D74 + D80, data = emerging, effect = "individual", model = "within")

cov4o         <- vcovSCC(b4o)
se.b4o    <- sqrt(diag(cov4o))

####latex table

stargazer(b1, b1t, b2, b2t, b4, b4t, type = "latex", se = list(se.b1, se.b1t, se.b2, se.b2t, se.b4, se.b4t), title = "Basic Regression for Developing Countries",#
          omit.stat = c("f", "rsq", "ser"), omit = c("Country", "Year"), add.lines = list(c("Country Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),#
                                                                                          c("Time Effects", "No", "Yes", "No", "Yes", "No", "Yes")), out = "TeXSCCBasicRegemerging.tex")


###############Time Periods for emerging#####
p60 <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap, data = emerging60.90, effect = "individual", model = "within")
cov60p         <- vcovSCC(p60)
se.p60    <- sqrt(diag(cov60p))


p85 <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap, data = emerging85.15, effect = "individual", model = "within")

cov85p         <- vcovSCC(p85)
se.p85    <- sqrt(diag(cov85p))


p06 <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap, data = emerging06, effect = "individual", model = "within")

cov06p         <- vcovSCC(p06)
se.p06    <- sqrt(diag(cov06p))

####time effects



p60t <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap + Year, data = emerging60.90, effect = "individual", model = "within")
cov60pt         <- vcovSCC(p60t)
se.p60t    <- sqrt(diag(cov60pt))


p85t <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap + Year, data = emerging85.15, effect = "individual", model = "within")

cov85pt         <- vcovSCC(p85t)
se.p85t    <- sqrt(diag(cov85pt))


p06t <- plm(Inflation ~ Young + Working + Old - 1 + Y.gap + Year, data = emerging06, effect = "individual", model = "within")

cov06pt         <- vcovSCC(p06t)
se.p06t    <- sqrt(diag(cov06pt))



#####latex table
stargazer(p60, p60t, p85, p85t, p06, p06t, r0, r1, r6, type = "latex", se = list(se.p60, se.p60t, se.p85, se.p85t, se.p06, se.p06t, se.r0, se.r1, se.r6), title = "Different Time Periods and Controls for Developing Countries",#
          omit.stat = c("f", "rsq", "ser"), omit = c("Country", "Year"), add.lines = list(c("Country Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),#
                                                                                          c("Time Effects", "No", "Yes", "No", "Yes", "No", "Yes", "No", "No", "Yes"),#
                                                                                          c("Time Period", "1960-1990", "1960-1990", "1985-2015", "1985-2015", "1985-2006", "1985-2006", "1960-2015", "1960-2015", "1985-2015")),#
          column.labels = c("Time Periods", "Different Controls"), column.separate = c(6, 3) , out = "TeXSCCPeriodControlRegemerging.tex")




#######different Controls emerging######

###life expenctancy
r0 <- plm(Inflation ~ Young + Working + Old + Life.Expectancy - 1 + Y.gap, data = emerging, effect = "individual", model = "within")
cov0r         <- vcovSCC(r0)
se.r0    <- sqrt(diag(cov0r))


r1 <- plm(Inflation ~ Young + Working + Old + Pop.Growth - 1 + Y.gap, data = emerging, effect = "individual", model = "within")
cov1r         <- vcovSCC(r1)
se.r1    <- sqrt(diag(cov1r))


r6 <- plm(Inflation ~ Young + Working + Old + Real.Interest + Acc.Balance + Year - 1 + Y.gap, data = emerging85.15, effect = "individual", model = "within")
cov6r         <- vcovSCC(r6)
se.r6    <- sqrt(diag(cov6r))


#####latex table

stargazer(r0, r1, r6, type = "latex", se = list(se.r0, se.r1, se.r6), title = "Basic Regression for OECD Countries",#
          omit.stat = c("f", "rsq", "ser"), omit = c("Country", "Year"), add.lines = list(c("Country Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),#
                                                                                          c("Time Effects", "No", "No", "No", "No", "No", "Yes"),#
                                                                                          c("Time Period", "1960-2015", "1960-2015", "1960-2015", "1960-2015", "1960-2015", "1985-2015")), out = "TeXSCCControlReg.tex")


#######PLOT#####

# These plots aid the descriptive statistics of the sample

Tot.panel <- read.csv("Totpanel", check.names = F, stringsAsFactors = T)
Tot.panel <- plm.data(Tot.panel, indexes = c("Country", "Year"))

oecd <- read.csv("Oecdpanel", check.names = F, stringsAsFactors = T)
oecd.plot <- plm.data(oecd, indexes = c("Country", "Year"))

emerging <- read.csv("Emergingpanel", check.names = F, stringsAsFactors = T)
emerging.plot <- plm.data(emerging, indexes = c("Country", "Year"))






