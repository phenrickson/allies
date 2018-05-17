# Create alliance portfolios
library(tidyr)
library(dplyr)
library(foreign)
library(data.table)
library(foreach)
library(doParallel)

#setwd("F:/Dropbox/FSU/FSU Summer 2017/Mark_Project/")
setwd("/Users/phenrickson/Dropbox/FSU/Mark_Projects/")

# load in ms data
ms_dat<-as.data.table(read.dta("ms_data_v2.dta"))

# load in portfolio data
load("portfolio_cowalliance.Rdata")
load("portfolio_atopms2.Rdata")
load("portfolio_milinst_atop.Rdata")

# load in S function
source("S.R")
source("cleanCCode.R")

# let's try computing US and Britain for 1970
foo<-portfolio_cowalliance$`1970` %>%
        filter(ccode=="002" | ccode== "020") %>%
        select(-ccode, -year)

a<-as.vector(as.matrix(foo[1,]))
b<-as.vector(as.matrix(foo[2,]))
w<-rep(1 / length(a), length(a))
m<-3

s_new<-S(a, b, w, m)
s_new
# works

# what was the S score for this pairing in the original?
ms_dat %>%
        filter(ccode1==002 & ccode2==020 & year==1970) %>%
        select(s_un_glo)
# alright we're pretty close

### loop over US Britain for all years
allyears<-sort(unique(ms_dat$year))

test<-foreach(i=1:length(portfolio_cowalliance), .combine=rbind.data.frame) %do% {
        
        foo<-portfolio_cowalliance[[i]] %>%
                filter(ccode=="002" | ccode== "020") %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        w<-rep(1 / length(a), length(a))
        m<-3
        
        s_new<-S(a, b, w, m)
        
        out<-data.frame(year=allyears[i],
                        S_new=s_new)
        out
}

new<-na.omit(test)

# now, compare to original
c1<-002
c2<-020

old<-ms_dat %>% 
        filter((ccode1==c1 | ccode2==c1)) %>%
        filter((ccode1==c2 | ccode2==c2)) %>%
        select(year, s_un_glo) %>%
        na.omit()

# merge
df<-left_join(new, old, by="year")
meltdf<-melt(df, id.vars="year")

library(ggplot2)
library(RColorBrewer)
g1<-ggplot(meltdf, aes(x=year, y=value, color=variable))+
        geom_line(size=1.5)+
        ggtitle("US-UK")+
        xlab("Year")+
        coord_cartesian(y=c(-1,1))+
        scale_colour_brewer(palette = "Paired")+
        theme_bw()
g1
# looks good


### US Russia
c1<-"002"
c2<-"365"

test<-foreach(i=1:length(portfolio_cowalliance), .combine=rbind.data.frame) %do% {
        
        foo<-portfolio_cowalliance[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        w<-rep(1 / length(a), length(a))
        m<-3
        
        s_new<-S(a, b, w, m)
        
        out<-data.frame(year=allyears[i],
                        S_new=s_new)
        out
}

new<-na.omit(test)

# now, compare to original
old<-ms_dat %>% 
        filter((ccode1==as.numeric(c1) | ccode2==as.numeric(c1))) %>%
        filter((ccode1==as.numeric(c2) | ccode2==as.numeric(c2))) %>%
        select(year, s_un_glo) %>%
        na.omit()

# merge
df<-left_join(new, old, by="year")
meltdf<-melt(df, id.vars="year")

# Plot
g2<-ggplot(meltdf, aes(x=year, y=value, color=variable))+
        geom_line(size=1.5)+
        ggtitle("US-Russia")+
        xlab("Year")+
        coord_cartesian(y=c(-1,1))+
        scale_colour_brewer(palette = "Paired")+
        theme_bw()
g2
# looks good



### UK Germany
### loop over US Russia for all years

c1<-"020"
c2<-"255"

test<-foreach(i=1:length(portfolio_cowalliance), .combine=rbind.data.frame) %do% {
        
        foo<-portfolio_cowalliance[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        w<-rep(1 / length(a), length(a))
        m<-3
        
        s_new<-S(a, b, w, m)
        
        out<-data.frame(year=allyears[i],
                        S_new=s_new)
        out
}

new<-na.omit(test)

# now, compare to original
old<-ms_dat %>% 
        filter((ccode1==as.numeric(c1) | ccode2==as.numeric(c1))) %>%
        filter((ccode1==as.numeric(c2) | ccode2==as.numeric(c2))) %>%
        select(year, s_un_glo) %>%
        na.omit()

# merge
df<-left_join(new, old, by="year")
meltdf<-melt(df, id.vars="year")

# Plot
g3<-ggplot(meltdf, aes(x=year, y=value, color=variable))+
        geom_line(size=1.5)+
        ggtitle("UK-Germany")+
        xlab("Year")+
        coord_cartesian(y=c(-1,1))+
        scale_colour_brewer(palette = "Paired")+
        theme_bw()
g3
# looks good


### China and Japan
### loop over US Russia for all years

c1<-"740"
c2<-"710"

test<-foreach(i=1:length(portfolio_cowalliance), .combine=rbind.data.frame) %do% {
        
        foo<-portfolio_cowalliance[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        w<-rep(1 / length(a), length(a))
        m<-3
        
        s_new<-S(a, b, w, m)
        
        out<-data.frame(year=allyears[i],
                        S_new=s_new)
        out
}

new<-na.omit(test)

# now, compare to original
old<-ms_dat %>% 
        filter((ccode1==as.numeric(c1) | ccode2==as.numeric(c1))) %>%
        filter((ccode1==as.numeric(c2) | ccode2==as.numeric(c2))) %>%
        select(year, s_un_glo) %>%
        na.omit()

# merge
df<-left_join(new, old, by="year")
meltdf<-melt(df, id.vars="year")

# Plot
g4<-ggplot(meltdf, aes(x=year, y=value, color=variable))+
        geom_line(size=1.5)+
        ggtitle("China-Japan")+
        xlab("Year")+
        coord_cartesian(y=c(-1,1))+
        scale_colour_brewer(palette = "Paired")+
        theme_bw()
g4
# looks good


library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2)


### Compute S codes for all countries
ccodes<-cleanCCode(sort(unique(c(ms_dat$ccode1, ms_dat$ccode2))))

# loop over years
S_new<-foreach(i=1:length(portfolio_cowalliance), .combine=rbind.data.frame) %do% {
        
                # grab one state
                c1<-ccodes[i]
                others<-ccodes[-which(ccodes<=c1)]
                
                # then pair with all others
                hold_ccodes<-foreach(j=1:length(others), .combine=rbind.data.frame) %do% {
                        
                        c2<-others[j]
                        
                        # then run across all years
                        hold_years<-foreach(k=1:length(allyears), .combine=rbind.data.frame) %do% {
                                
                                foo<-portfolio_cowalliance[[k]] %>%
                                        filter(ccode==c1 | ccode== c2) %>%
                                        select(-ccode, -year)
                                
                                a<-as.vector(as.matrix(foo[1,]))
                                b<-as.vector(as.matrix(foo[2,]))
                                w<-rep(1 / length(a), length(a))
                                m<-3
                                
                                s_new<-S(a, b, w, m)
                                
                                out<-data.frame(ccode1=c1,
                                                ccode2=c2,
                                                year=allyears[k],
                                                S_new=s_new)
                                out
                                
                        }
                        print(c(c1, c2))
                        hold_years
                        
                }
                print(c1)
                hold_ccodes
}

                
        
        

S_new<-foreach(i=1:length(portfolio_cowalliance), .combine=rbind.data.frame) %do% {
        
        yr<-allyears[i]
        temp<-filter(cleanDat, year==yr)
        ccodes<-unique(c(temp$ccode1, temp$ccode2))
        
        foreach(j=1:)
        
        foo<-portfolio_cowalliance[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        w<-rep(1 / length(a), length(a))
        m<-3
        
        s_new<-S(a, b, w, m)
        
        out<-data.frame(year=allyears[i],
                        S_new=s_new)
        out
}



# how to deal with NAs?
# recode NAs as 0s?
# have to recode NAs in order to use the function
foreach(i=1:length(po))
cow_dat<-portfolio_cowalliance %>% 
        mutate_all(funs(replace(., is.na(.), 0)))

atopms2_dat<-portfolio_atopms2 %>% 
        mutate_all(funs(replace(., is.na(.), 0)))

milinst_atop_dat<- portfolio_milinst_atop%>%
        mutate_all(funs(replace(., is.na(.), 0)))

# bind in the ccode_year to all of these
cow<-data.table(year, ccode, ccode_year, cow_dat)
rownames(cow)<-rownames(portfolio_cowalliance)

atopms2<-data.table(year, ccode, atopms2_dat)
milinst_atop<-data.table(year, milinst_atop_dat)


# compute S scores using cowalliance data
# inputs: two countries
# restrict columns to relevant countries
# then, restrict further based on which countries existed during this time period
# score max distance

# using cow, compute s scores for a specific country pairing
yr<-1970

# grab countries
a<-as.data.table(cow) %>% 
        filter(year==yr, ccode=="002") %>%
        dplyr::select(-year, -ccode, -ccode_year)

b<-as.data.table(cow) %>% 
        filter(year==yr, ccode=="020") %>%
        dplyr::select(-year, -ccode, -ccode_year)

a<-as.vector(as.matrix(a))
b<-as.vector(as.matrix(b))
w<-rep(1 / length(a), length(a))
m<-3



# output to latex
foo<-rbind(a,b)
colnames(foo)<-colnames(cow)[-c(1,2)]
rownames(foo)<-c("002_1970", "020_1970")
xtable(foo)
rm(foo)

# Hmm, okay so there's one issue here: I'm setting some countries to zero when these countries didn't exist
# The portfolios need to be restricted to the countries which existed in that particular year

# test 1816
foo<-ms_dat %>%
        filter(year==1816) %>%
        select(ccode1, ccode2)

ccodes<-unique(c(unique(foo$ccode1), unique(foo$ccodde2)))
length(ccodes)
# so for instance there should be only 22 countries appearing in the portfolios for 1816

# restrict the portfolios based on this
bar<-cow %>%
        filter(year==1816)

bar<-bar[,which(as.numeric(colnames(bar)) %in% ccodes)]


# okay, with this in mind, redo the britain US example
# using cow, compute s scores for a specific country pairing
yr<-1970

# restrict portfolios
foo<-ms_dat %>%
        filter(year==yr) %>%
        select(ccode1, ccode2)

ccodes<-unique(c(unique(foo$ccode1), unique(foo$ccodde2)))
ccodes
# should only be 133 in here
bar<-cow %>%
        filter(year==yr)

bar<-bar[,which(as.numeric(colnames(bar)) %in% ccodes)]


# grab countries
a<-as.data.table(bar) %>% 
        filter(year==yr, ccode=="002") %>%
        dplyr::select(-year, -ccode)

b<-as.data.table(bar) %>% 
        filter(year==yr, ccode=="020") %>%
        dplyr::select(-year, -ccode)

a<-as.vector(as.matrix(a))
b<-as.vector(as.matrix(b))
w<-rep(1 / length(a), length(a))
m<-3

s_new<-S(a, b, w, m)
s_new
# works

# what was the S score for this pairing in the original?
ms_dat %>%
        filter(ccode1==002 & ccode2==020 & year==1970) %>%
        select(s_un_glo)
# ugh



# Compute unrestricted S scores for all countries for all years
uniq.ccodes<-unique(cow$ccode)
yrs<-sort(unique(cow$year))

# loop over years
s_unrestricted<-foreach(h=1:length(yrs), .combine=rbind.data.frame) %do% {
        
        yr<-yrs[h]
        
        # loop over countries
        hold<- foreach(i=1:length(uniq.ccodes), .combine=rbind.data.frame) %do% {
                # start with one ccode
                country<-uniq.ccodes[i]
                # all other ccodes
                others<-uniq.ccodes[-which(country %in% uniq.ccodes)]
                
                # grab portfolio for country
                a<-cow %>% 
                        filter(year==yr, ccode==country) %>%
                        dplyr::select(-year, -ccode)
                
                temp<-foreach(j=1:length(others), .combine=rbind.data.frame) %do% {
                        
                        # rotate in portfolio for each other country
                        b<-cow %>% 
                                filter(year==yr, ccode==others[j]) %>%
                                dplyr::select(-year, -ccode)
                        
                        # compute S scores; m<-3 because of the variable
                        a<-as.vector(as.matrix(a))
                        b<-as.vector(as.matrix(b))
                        w<-rep(1 / length(a), length(a))
                        m<-3
                        
                        # compute
                        s_new<-S(a, b, w, m)
                        
                        # store in data frame
                        dat<-data.frame("ccode1"=country,
                                        "ccode2"=others[j],
                                        "year"=yr,
                                        "s_new"=s_new)
                        dat
                        
                }
                temp
        }
        hold
}

save(s_unrestricted, file="s_unrestricted.RDATA")


# But, this is problematic; the portfolios end up being very similar
# this is because we are including 'irrelevant' dyads in the calculation
# therefore, we need to do something to filter for the relevant dyads
# compute using the major powers from COW




# compare to original
# grab s_un_glo
# for a specific country
c1<-"002"
c2<-"020"
yr<-1970

test<-s_dat %>% 
        filter(ccode1==c1, ccode2==c2, year==yr)
test

s_old<-test$s_un_glo

# Compare over time

foreach(i=1:length(dat$year)) %do% {
        
}


### Compute using different portfolios



