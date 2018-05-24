# Create S-ATOP Using ATOP portfolios
library(tidyr)
library(dplyr)
library(foreign)
library(data.table)
library(foreach)
library(doParallel)

#setwd("F:/Dropbox/FSU/FSU Summer 2017/Mark_Project/")
#setwd("/Users/phenrickson/Dropbox/FSU/Mark_Projects/")
setwd("C:/Users/PoliSci Lab_2/Downloads/Mark_Projects/")


# load in ms data
ms_dat<-as.data.table(read.dta("ms_data_v2.dta"))
load("cleanDat.Rdata")

# load in portfolio data
load("portfolio_cowalliance.Rdata")
load("portfolio_atopms2.Rdata")
load("portfolio_milinst_atop.Rdata")

# load in S function
source("S.R")
source("cleanCCode.R")


### Let's look at the ATOP data
foo<-portfolio_atopms2$`1816`

# ATOP seems to have NAs take the place of 0s
foo1<-portfolio_cowalliance$`1816`
rm(foo, foo1)

# the portfolios were constructed using the same countries as cowalliance
# we should be safe to recode NAs as 0
atopms2<-suppressWarnings(foreach(i=1:length(portfolio_atopms2)) %do% {
        portfolio_atopms2[[i]] %>% 
                as.data.frame() %>%
                mutate_all(funs(replace(., is.na(.), 0)))
})
allyears<-sort(unique(cleanDat$year))
names(atopms2)<-do.call(c, lapply(allyears, toString))


# now look at US and UK in 1970
foo<-atopms2$`1970` %>%
        filter(ccode=="002" | ccode== "200") %>%
        select(-ccode, -year)

a<-as.vector(as.matrix(foo[1,]))
b<-as.vector(as.matrix(foo[2,]))
w<-rep(1 / length(a), length(a))
m<-3

s_atop<-S(a, b, w, m)
s_atop

# works

# what was the S score for this pairing in the original?
ms_dat %>%
        filter(ccode1==002 & ccode2==200 & year==1970) %>%
        select(s_un_glo)
# okay pretty similar to S

### loop over US Britain for all years
allyears<-sort(unique(ms_dat$year))

test<-foreach(i=1:length(atopms2), .combine=rbind.data.frame) %do% {
        
        foo<-atopms2[[i]] %>%
                filter(ccode=="002" | ccode== "200") %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        w<-rep(1 / length(a), length(a))
        m<-3
        
        s_atop<-S(a, b, w, m)
        
        out<-data.frame(year=allyears[i],
                        S_atop=s_atop)
        out
}

new<-na.omit(test)

# now, compare to original
c1<-002
c2<-200

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
# pretty similar to S


### US Russia
c1<-"002"
c2<-"365"

test<-foreach(i=1:length(atopms2), .combine=rbind.data.frame) %do% {
        
        foo<-atopms2[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        w<-rep(1 / length(a), length(a))
        m<-3
        
        s_atop<-S(a, b, w, m)
        
        out<-data.frame(year=allyears[i],
                        S_atop=s_atop)
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

c1<-"200"
c2<-"255"

test<-foreach(i=1:length(atopms2), .combine=rbind.data.frame) %do% {
        
        foo<-atopms2[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        w<-rep(1 / length(a), length(a))
        m<-3
        
        s_atop<-S(a, b, w, m)
        
        out<-data.frame(year=allyears[i],
                        S_atop=s_atop)
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

test<-foreach(i=1:length(atopms2), .combine=rbind.data.frame) %do% {
        
        foo<-atopms2[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        w<-rep(1 / length(a), length(a))
        m<-3
        
        s_atop<-S(a, b, w, m)
        
        out<-data.frame(year=allyears[i],
                        S_atop=s_atop)
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
rm(a, b, c1, c2, foo, df, meltdf, new, old, out, test, s_atop, w, portfolio_atopms2, portfolio_cowalliance, portfolio_milinst_atop)


### Compute S_atop codes for all countries
ccodes<-cleanCCode(sort(unique(c(ms_dat$ccode1, ms_dat$ccode2))))

# loop over years
S_atop<-foreach(i=1:length(atopms2), .combine=rbind.data.frame) %do% {
        
        # grab one state
        c1<-ccodes[i]
        others<-ccodes[-which(ccodes<=c1)]
        
        # then pair with all others
        hold_ccodes<-foreach(j=1:length(others), .combine=rbind.data.frame) %do% {
                
                c2<-others[j]
                
                # then run across all years
                hold_years<-foreach(k=1:length(allyears), .combine=rbind.data.frame) %do% {
                        
                        foo<-atopms2[[k]] %>%
                                filter(ccode==c1 | ccode== c2) %>%
                                select(-ccode, -year)
                        
                        a<-as.vector(as.matrix(foo[1,]))
                        b<-as.vector(as.matrix(foo[2,]))
                        w<-rep(1 / length(a), length(a))
                        m<-3
                        
                        s_atop<-S(a, b, w, m)
                        
                        out<-data.frame(ccode1=c1,
                                        ccode2=c2,
                                        year=allyears[k],
                                        S_atop=s_atop)
                        out
                        
                }
                print(c(c1, c2))
                hold_years
                
        }
        print(c1)
        hold_ccodes
}



# save
save(S_atop, file="S_atop.Rdata")
load("S_atop.Rdata")

# load
load("cleanDat.Rdata")

# compare
check<-as.tbl(left_join(cleanDat, S_atop, by=c("ccode1", "ccode2", "year")))

# correlation
corr<-cor(check$s_un_glo, check$S_atop, use="pairwise")

# plot
g<- check %>%
        dplyr::select(ccode1, ccode2, year, s_un_glo, S_atop)

p<-ggplot(g, aes(x=S_atop, y=s_un_glo))+
        geom_point(alpha=0.2)+
        geom_abline(slope=1)+
        ylab("S_Old")+
        xlab("S_atop")+
        annotate("text", x=0.25, y=0.75, label= paste("corr=", paste(round(corr,3)), sep="")) + 
        theme_bw()
p

# oh god don't do this
#library(plotly)

#p2<-plot_ly(
# g, x=~S_atop, y=~s_un_glo,
#  text=~paste(ccode1, ccode2, year)
#)

#p2

### stop
