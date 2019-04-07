library(dplyr)
library(ggplot2)
library(forecast)

#function determines if a country exists within a list of countries
countrymatch <- function(countryName, indicator){
  if(is.element(countryName, indicator[,3])){
    return (TRUE)
  } else {
    return (FALSE)
  }
}

#function finds the index of the country in a list of countries
countryindex <- function(countryname, countrylist){
  as.numeric(which(countryname==countrylist[,3]))
}

#function returns linear regression output for each indicator
countrytrend <- function(country){
  vpval <- c(rep("no data",41),"see 1.5.1", "see 1.5.2", "see 1.5.3", "see 1.5.4", 
      "see 8.4.1", "see 8.4.2","see 1.5.1", "see 1.5.3", "see 1.5.4", "see 15.a.1")
  vtrend <- c(rep("no data",41),"1.5.1", "1.5.2", "1.5.3", "1.5.4", "8.4.1", 
      "8.4.2","1.5.1", "1.5.3", "1.5.4", "15.a.1")
  vRsquared <- c(rep("no data",41),"see 1.5.1", "see 1.5.2", "see 1.5.3", "see 1.5.4", 
      "see 8.4.1", "see 8.4.2","see 1.5.1", "see 1.5.3", "see 1.5.4", "see 15.a.1")
  vcountry <- c(rep(country,51))
  vind <- c("1.4.2", "2.4.1", "4.7.1", "5.a.1", "6.3.1","6.4.1", "7.a.1", "7.b.1", "8.9.2", 
      "11.2.1", "11.3.1", "11.4.1", "11.7.1", "11.c.1", "12.3.1", "12.4.2", "12.5.1", "12.6.1", 
      "12.7.1", "12.8.1", "12.a.1", "12.b.1", "13.2.1", "13.3.1", "13.3.2", "13.a.1", "13.b.1", 
      "14.1.1", "14.2.1", "14.3.1", "14.6.1", "14.7.1", "14.c.1", "15.3.1", "15.7.1", "15.8.1", 
      "15.9.1", "15.c.1", "17.6.1", "17.7.1", "17.14.1", "11.5.1", "11.5.2", "11.b.1", "11.b.2", 
      "12.2.1", "12.2.2", "13.1.1", "13.1.2", "13.1.3", "15.b.1")
  for(i in 1:42){
    datname <- dir()[i]
    indicator <- read.csv(datname)
    if(countrymatch(country, indicator)==FALSE){
      pval <- "missing country"
      trend <- "missing country"
      rsquared <- "missing country"
    }
    else if(countrymatch(country, indicator)==TRUE){
      cindex <- countryindex(country, indicator)
      dat <- indicator[cindex,]
      values <- as.numeric(dat[,c(4:ncol(dat))])
      years <- names(dat)[4:length(names(dat))]
      years <- as.numeric(sub('.','',years))
      if(all(is.na(values))){
          pval <- NA
          trend <- NA
      }
      else if(!all(is.na(values))){
          lm <- lm(values~years)
          pval <- as.numeric(summary(lm)$coefficients[,4][2])
          trend <- as.numeric(lm$coefficients[2])
          rsquared <- as.numeric(summary(lm)$r.squared)
      }
    }
    vpval <- append(vpval, pval)
    vtrend <- append(vtrend, trend)
    vRsquared <- append(vRsquared, rsquared)
    vcountry <- append(vcountry, country)
    vind <- append(vind, as.character(indicator[1,1]))
  }
  dataout <- data.frame("country"=vcountry, "indicator"=vind,"trend"=vtrend, 
      "pvalue"=vpval,"rsquared"=vRsquared)
  dataout <- dataout[mixedorder(as.vector(dataout$indicator)),]
  return(dataout)
}

#function outputs one of five values depending on linear regression trend 
indicatorout <- function(dataframe){
  dataframe$trend <- as.vector(dataframe$trend)
  dataframe$pvalue <- as.vector(dataframe$pvalue)
  vindout <- rep(100,nrow(dataframe))
  trend <- as.vector(dataframe[,3])
  vindout[which(trend=="no data")] <- "0 no data"
  vindout[which(trend=="missing country")] <- "0 no data"
  vindout[which(is.na(trend))] <- "1 little data"
  trendnum <- as.numeric(trend)
  vindout[which(trendnum==0)] <- "2 no trend"
  vindout[which(trendnum>0)] <- "3 positive trend"
  vindout[which(trendnum<0)] <- "4 negative trend"
  repeatind <- which(is.element(trend, dataframe$indicator))
  for (i in repeatind){
      vindout[i] <- vindout[which(trend[i]==dataframe$indicator)]
  }
  dataframe <- cbind(dataframe, vindout) 
  return(dataframe) 
}

#function creates a ggplot barplot based on the indicator value 
indchart <- function(df){
  countryname <- as.character(df[1,1])
  titlename <- paste(countryname, "National Scorecard")
  df %>% mutate(indicator=fct_relevel(indicator,"1.4.2","1.5.1","1.5.2",
      "1.5.3","1.5.4","2.4.1","2.5.1","2.5.2","3.9.1","3.9.2","3.9.3","4.7.1",
      "5.a.1","6.1.1","6.3.1","6.3.2","6.4.1","6.4.2","6.5.1","6.5.2","6.6.1",
      "6.a.1","6.b.1","7.1.2","7.2.1","7.3.1","7.a.1","7.b.1","8.4.1","8.4.2",
      "8.9.2","9.4.1","11.2.1","11.3.1","11.4.1","11.5.1","11.5.2","11.6.1",
      "11.6.2","11.7.1","11.b.1","11.b.2","11.c.1","12.1.1","12.2.1","12.2.2",
      "12.3.1","12.4.1","12.4.2","12.5.1","12.6.1","12.7.1","12.8.1","12.a.1",
      "12.b.1","12.c.1","13.1.1","13.1.2","13.1.3","13.2.1","13.3.1","13.3.2",
      "13.a.1","13.b.1","14.1.1","14.2.1","14.3.1","14.4.1","14.5.1","14.6.1",
      "14.7.1","14.a.1","14.c.1","15.1.1","15.1.2a","15.1.2b","15.2.1","15.3.1",
      "15.4.1","15.4.2","15.5.1","15.6.1","15.7.1","15.8.1","15.9.1","15.a.1",
      "15.b.1","15.c.1","16.8.1","17.4.1","17.6.1","17.7.1","17.9.1","17.14.1"))%>%
  mutate(indicator=fct_rev(indicator))%>%
  ggplot(aes(x=indicator, y=vindout, fill=factor(vindout)))+
  geom_bar(stat="identity", color="black", size=0.2)+
  scale_fill_manual(values=c("darkgrey", "lightgrey", "khaki1", "lightskyblue", "coral1"),
      name="legend", drop=FALSE)+
  scale_y_discrete(labels=c("no data", "little data", "no trend", "positive trend", 
      "negative trend"))+
  ggtitle(titlename)+
  ylab("")+
  xlab("indicator")+
  labs(caption="Based on data available on UN Global SDG Database as of March 2019.")+
  theme(plot.caption = element_text(hjust = 0))+
  coord_flip()
  imgname <- paste(countryname, ".pdf", sep="")
  ggsave(imgname, height=13) 
}

#function brings together previous functions for quick scorecard
countryscorecard <- function(country){
  dat <- countrytrend(country)
  datout <- indicatorout(dat)
  return(indchart(datout))
}