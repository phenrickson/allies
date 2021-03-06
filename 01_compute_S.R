# Create alliance portfolios
library(tidyr)
library(dplyr)
library(foreign)
library(data.table)
library(foreach)
library(doParallel)

#setwd("F:/Dropbox/FSU/FSU Summer 2017/Mark_Project/")
setwd("/Users/phenrickson/Dropbox/FSU/Mark_Projects/")
#setwd("C:/Users/PoliSci Lab_2/Downloads/Mark_Projects/Mark_Projects/")

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
        filter(ccode=="002" | ccode== "200") %>%
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
        filter(ccode1==002 & ccode2==200 & year==1970) %>%
        select(s_un_glo)
# alright we're pretty close

### loop over US Britain for all years
allyears<-sort(unique(ms_dat$year))

test<-foreach(i=1:length(portfolio_cowalliance), .combine=rbind.data.frame) %do% {
        
        foo<-portfolio_cowalliance[[i]] %>%
                filter(ccode=="002" | ccode== "200") %>%
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

c1<-"200"
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

# save
save(S_new, file="S_new.Rdata")
load("S_new.Rdata")

# load
load("cleanDat.Rdata")

# compare
check<-as.tbl(left_join(cleanDat, S_new, by=c("ccode1", "ccode2", "year")))

# correlation
corr<-cor(check$s_un_glo, check$S_new, use="pairwise")

# plot
g<- check %>%
  dplyr::select(ccode1, ccode2, year, s_un_glo, S_new)

p<-ggplot(g, aes(x=S_new, y=s_un_glo))+
  geom_point(alpha=0.2)+
  geom_abline(slope=1)+
  ylab("S_Old")+
  xlab("S_New")+
  annotate("text", x=0.25, y=0.75, label= paste("corr=", paste(round(corr,3)), sep="")) + 
  theme_bw()
p

# oh god don't do this
#library(plotly)

#p2<-plot_ly(
 # g, x=~S_new, y=~s_un_glo,
#  text=~paste(ccode1, ccode2, year)
#)

#p2

### stop

