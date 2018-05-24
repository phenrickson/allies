# Transforming data for alliance portfolios
library(tidyr)
library(dplyr)
library(foreign)
library(data.table)
library(foreach)
library(doParallel)
library(naniar)
library(xtable)


# setwd("F:/Dropbox/FSU/FSU Summer 2017/Mark_Project/")
setwd("/Users/phenrickson/Dropbox/FSU/Mark_Projects/")

ms_dat<-read.dta("ms_data_v2.dta")

# look at missingness
vis_miss(ms_dat, warn_large_data=F)

# look
head(ms_dat)
colSums(is.na(ms_dat))

# Omit NAs for nonexistent ccodes...
dat<-data.table(subset(ms_dat, !is.na(ccode1)))

# Clean ccodes: recall function made for this purpose
source("tidyCYear.R")

# Clean up the country codes; should take about 5-6 seconds to run the function
c1<-tidyCYear(dat$ccode1, dat$year)
c2<-tidyCYear(dat$ccode2, dat$year)
rm(changes.table)

# since we only need the country codes, just substring the first three didigts
ccode1<-substr(c1, 1, 3)
ccode2<-substr(c2, 1, 3)

# make dyadic ccodes
c1c2_year<-paste(ccode1, ccode2, dat$year, sep="_")
c2c1_year<-paste(ccode2, ccode1, dat$year, sep="_")

# make a data table of:
# ccode1
# ccode2
# ccode1ccode2year
# ccode2ccode1year
# features from dat

cleanDat<-data.table(ccode1, 
                      ccode2,
                      c1c2_year, 
                      c2c1_year, 
                      dplyr::select(dat, -ccode1, -ccode2, -cow_atopid)) %>% 
  `colnames<-`(c("ccode1", "ccode2", "c1c2_year", "c2c1_year", "year",
                 "s_un_glo", "atopdp", "milinst_atop",
                 "cowdp", "atopally", "cowalliance", "atopms2"))
cleanDat
save(cleanDat, file="cleanDat.Rdata")

rm(ms_dat, dat, c1c2_year, c2c1_year)

# output to Latex to show
xtable(head(dplyr::select(cleanDat, ccode1, ccode2, c1c2_year, c2c1_year, year, cowalliance, s_un_glo)))

# The end goal is to create a matrix with columns for each country code
# Then make each row be a country year observation
# with alliances with each of those countries in the cells

allccodes<-sort(unique(c(unique(cleanDat$ccode1), unique(cleanDat$ccode2))))
allyears<-sort(unique(cleanDat$year))


# check for duplicates
which(duplicated(cleanDat$c1c2_year)==T)
which(duplicated(cleanDat$c2c1_year)==T)


# Test: cow alliance profile for UK in 1950
ccode<-"020"
yr<-1950

# filter the dataset to find the UK's alliances in 1950
foo<-cleanDat%>%
        filter((ccode1==ccode | ccode2==ccode) & year==yr)
foo

temp<-filter(cleanDat, year==yr)
ccodes<-unique(c(temp$ccode1, temp$ccode2))

# create holder for each year
hold<-data.frame(matrix(NA, 1, length(ccodes)))
colnames(hold)<-ccodes

# the problem is that this dataset will sometimes have the country in the first or the second ccode slot
# need to do a quick re code so that the country whose portfolio we want always shows up first

# rearrange
foo$ccode1[which(foo$ccode2 != ccode)] <- foo$ccode2[which(foo$ccode2 != ccode)]
foo$ccode2<-ccode

# flip to make this the row for the matrix
bar<-t(foo$cowalliance)
colnames(bar)<-foo$ccode1
rownames(bar)<-paste(ccode, yr, sep="_")
# output to latex
xtable(head(bar))
       
# now, ideally, we'd be able to just proceed like this and stack all rows together to get our final matrix
# but, there's an issue. not all countries exist at all times. 
# so, I only need to include the countries which are present in ms_dat at this time

# need
test<-hold

test[,which(colnames(test) %in% colnames(bar)==T)]<-bar
rownames(test)<-paste(ccode, yr, sep="_")

# then, make it an ally with itself
test[,which(colnames(test)==ccode)]<-max(na.omit(cleanDat$cowalliance))
test

# output to latex
xtable(head(test))


# run over all years for all countries
# using cowalliance
portfolio_cowalliance<-
        foreach(i=1:length(allyears), .packages=c('foreach', 'dplyr')) %do% {
                yr<-allyears[i]
                temp<-filter(cleanDat, year==yr)
                ccodes<-unique(c(temp$ccode1, temp$ccode2))
                
                # create holder for each year
                hold<-data.frame(matrix(NA, 1, length(ccodes)))
                colnames(hold)<-ccodes
                
                portfolio_year<-foreach(j=1:length(ccodes), .combine=rbind.data.frame, .packages=c('dplyr', 'foreach')) %do% {
                        
                        ccode<-ccodes[j]
                        
                        # filter the dataset 
                        foo<-temp%>%
                                filter((ccode1==ccode | ccode2==ccode))
                        
                        foo$ccode1[which(foo$ccode2 != ccode)] <- foo$ccode2[which(foo$ccode2 != ccode)]
                        foo$ccode2<-ccode
                        
                        # flip to make this the row for the matrix
                        bar<-t(foo$cowalliance)
                        colnames(bar)<-foo$ccode1
                        rownames(bar)<-paste(ccode, yr, sep="_")
                        
                        # now bring in missing ccodes for this year
                        test<-hold
                        test[,which(colnames(test) %in% colnames(bar)==T)]<-bar
                        rownames(test)<-paste(ccode, yr, sep="_")
                        
                        # then, make it an ally with itself
                        test[,which(colnames(test)==ccode)]<-max(na.omit(cleanDat$cowalliance))
                        out<-cbind(ccode, yr, test)
                        year<-yr
                        out<-cbind.data.frame(ccode, year, test)

                        out
                        }
                
                print(yr)
 
                portfolio_year
        
        }

names(portfolio_cowalliance)<-do.call(c, lapply(allyears, toString))
save(portfolio_cowalliance, file="portfolio_cowalliance.RDATA")


# using atopms2
portfolio_atopms2<-
        foreach(i=1:length(allyears), .packages=c('foreach', 'dplyr')) %do% {
                yr<-allyears[i]
                temp<-filter(cleanDat, year==yr)
                ccodes<-unique(c(temp$ccode1, temp$ccode2))
                
                # create holder for each year
                hold<-data.frame(matrix(NA, 1, length(ccodes)))
                colnames(hold)<-ccodes
                
                portfolio_year<-foreach(j=1:length(ccodes), .combine=rbind.data.frame, .packages=c('dplyr', 'foreach')) %do% {
                        
                        ccode<-ccodes[j]
                        
                        # filter the dataset 
                        foo<-temp%>%
                                filter((ccode1==ccode | ccode2==ccode))
                        
                        foo$ccode1[which(foo$ccode2 != ccode)] <- foo$ccode2[which(foo$ccode2 != ccode)]
                        foo$ccode2<-ccode
                        
                        # flip to make this the row for the matrix
                        bar<-t(foo$atopms2)
                        colnames(bar)<-foo$ccode1
                        rownames(bar)<-paste(ccode, yr, sep="_")
                        
                        # now bring in missing ccodes for this year
                        test<-hold
                        test[,which(colnames(test) %in% colnames(bar)==T)]<-bar
                        rownames(test)<-paste(ccode, yr, sep="_")
                        
                        # then, make it an ally with itself
                        test[,which(colnames(test)==ccode)]<-max(na.omit(cleanDat$atopms2))
                        out<-test
                        year<-yr
                        out<-cbind.data.frame(ccode, year, test)
                        
                        out
                }
                
                print(yr)
                
                portfolio_year
                
        }

names(portfolio_atopms2)<-do.call(c, lapply(allyears, toString))
save(portfolio_atopms2, file="portfolio_atopms2.RDATA")


# using milinst_atop
portfolio_milinst_atop<-
        foreach(i=1:length(allyears), .packages=c('foreach', 'dplyr')) %do% {
                yr<-allyears[i]
                temp<-filter(cleanDat, year==yr)
                ccodes<-unique(c(temp$ccode1, temp$ccode2))
                
                # create holder for each year
                hold<-data.frame(matrix(NA, 1, length(ccodes)))
                colnames(hold)<-ccodes
                
                portfolio_year<-foreach(j=1:length(ccodes), .combine=rbind.data.frame, .packages=c('dplyr', 'foreach')) %do% {
                        
                        ccode<-ccodes[j]
                        
                        # filter the dataset 
                        foo<-temp%>%
                                filter((ccode1==ccode | ccode2==ccode))
                        
                        foo$ccode1[which(foo$ccode2 != ccode)] <- foo$ccode2[which(foo$ccode2 != ccode)]
                        foo$ccode2<-ccode
                        
                        # flip to make this the row for the matrix
                        bar<-t(foo$milinst_atop)
                        colnames(bar)<-foo$ccode1
                        rownames(bar)<-paste(ccode, yr, sep="_")
                        
                        # now bring in missing ccodes for this year
                        test<-hold
                        test[,which(colnames(test) %in% colnames(bar)==T)]<-bar
                        rownames(test)<-paste(ccode, yr, sep="_")
                        
                        # then, make it an ally with itself
                        test[,which(colnames(test)==ccode)]<-max(na.omit(cleanDat$milinst_atop))
                        year<-yr
                        out<-cbind.data.frame(ccode, year, test)
                        
                        out
                }
                
                print(yr)
                
                portfolio_year
                
        }

names(portfolio_milinst_atop)<-do.call(c, lapply(allyears, toString))
save(portfolio_milinst_atop, file="portfolio_milinst_atop.RDATA")

# done