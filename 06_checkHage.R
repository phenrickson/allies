# Compare the new measures

library(tidyr)
library(dplyr)
library(foreign)
library(data.table)
library(foreach)
library(doParallel)
library(ggplot2)
library(ggpubr)


#setwd("F:/Dropbox/FSU/FSU Summer 2017/Mark_Project/")
setwd("/Users/phenrickson/Dropbox/FSU/Mark_Projects/")
#setwd("C:/Users/PoliSci Lab_2/Downloads/Mark_Projects/Mark_Projects/")

# load in ms data
ms_dat<-as.data.table(read.dta("ms_data_v2.dta"))
load("cleanDat.Rdata")

# load in portfolio data
load("portfolio_cowalliance.Rdata")
load("portfolio_atopms2.Rdata")
load("portfolio_milinst_atop.Rdata")

# load in functions
source("S.R")
source("S_pi.R")
source("S_kappa.R")
source("cleanCCode.R")

# load in scores
load("S_new.Rdata")
load("S_atop.Rdata")
load("S_milinst_atop.Rdata")

# merge
check<-left_join(cleanDat, S_new, by=c("ccode1", "ccode2", "year")) %>%
        left_join(., S_atop, by=c("ccode1", "ccode2", "year")) %>%
        left_join(., S_milinst_atop, by=c("ccode1", "ccode2", "year")) %>%
        dplyr::select(-atopdp, -milinst_atop, -cowdp, -atopally, -cowalliance, -atopms2) %>%
        as.tbl()
colnames(check)[6:9]<-c("S_old", "S_new", "S_atop", "S_milinst_atop")
check$c1c2<-substr(check$c1c2_year, 1,7)
head(check)


# recreate figure 1 on page 288
c1<-"200"
c2<-c("220", "002", "365", "710")

foo<-check %>%
        filter(ccode1==c1 & ccode2 %in% c2 | ccode2==c1 & ccode1 %in% c2) %>%
        select(c1c2, year, S_old, S_new, S_atop, S_milinst_atop)

meltS<-melt(foo, id.vars=c("year", "S_old", "S_new", "S_atop", "S_milinst_atop"))

p11<-ggplot(meltS, aes(x=year, y=S_new, color=value))+
        geom_point(size=1)+
        geom_line(size=0.75)+
        ggtitle("S_new")+
        xlab("Year")+
        coord_cartesian(y=c(-0.5,1))+
        scale_colour_brewer(palette = "Paired")+
        theme_bw()

p21<-ggplot(meltS, aes(x=year, y=S_atop, color=value))+
        geom_point(size=1)+
        geom_line(size=0.75)+
        ggtitle("S_atop")+
        xlab("Year")+
        coord_cartesian(y=c(-0.5,1))+
        scale_colour_brewer(palette = "Paired")+
        theme_bw()

p31<-ggplot(meltS, aes(x=year, y=S_milinst_atop, color=value))+
        geom_point(size=1)+
        geom_line(size=0.75)+
        ggtitle("S_milinst_atop")+
        xlab("Year")+
        coord_cartesian(y=c(-0.5,1))+
        scale_colour_brewer(palette = "Paired")+
        theme_bw()

# Compute S_pi and S_kappa for these countries
allyears<-sort(unique(ms_dat$year))

c1="002"
c2="200"

test_cowalliance<-foreach(i=1:length(portfolio_cowalliance), .combine=rbind.data.frame) %do% {

        foo<-portfolio_cowalliance[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        
        s_pi<-S_pi(a, b)
        s_kappa<-S_kappa(a,b)
        
        out<-data.frame(year=allyears[i],
                        S_pi=s_pi,
                        S_kappa=s_kappa,
                        c1c2=paste(c1, c2, sep="_"))
        out
}

store1<-test_cowalliance


c1="200"
c2="220"

test_cowalliance<-foreach(i=1:length(portfolio_cowalliance), .combine=rbind.data.frame) %do% {
        
        foo<-portfolio_cowalliance[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        
        s_pi<-S_pi(a, b)
        s_kappa<-S_kappa(a,b)
        
        out<-data.frame(year=allyears[i],
                        S_pi=s_pi,
                        S_kappa=s_kappa,
                        c1c2=paste(c1, c2, sep="_"))
        out
}

store2<-test_cowalliance


c1="200"
c2="365"

test_cowalliance<-foreach(i=1:length(portfolio_cowalliance), .combine=rbind.data.frame) %do% {
        
        foo<-portfolio_cowalliance[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        
        s_pi<-S_pi(a, b)
        s_kappa<-S_kappa(a,b)
        
        out<-data.frame(year=allyears[i],
                        S_pi=s_pi,
                        S_kappa=s_kappa,
                        c1c2=paste(c1, c2, sep="_"))
        out
}

store3<-test_cowalliance


c1="200"
c2="710"

test_cowalliance<-foreach(i=1:length(portfolio_cowalliance), .combine=rbind.data.frame) %do% {
        
        foo<-portfolio_cowalliance[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        
        s_pi<-S_pi(a, b)
        s_kappa<-S_kappa(a,b)
        
        out<-data.frame(year=allyears[i],
                        S_pi=s_pi,
                        S_kappa=s_kappa,
                        c1c2=paste(c1, c2, sep="_"))
        out
}

store4<-test_cowalliance

hold<-rbind(store1, store2, store3, store4)

p12<-ggplot(hold, aes(x=year, y=S_pi, color=c1c2))+
        geom_point(size=1)+
        geom_line(size=0.75)+
        ggtitle("S_pi")+
        xlab("Year")+
        coord_cartesian(y=c(-0.5,1))+
        scale_colour_brewer(palette = "Paired")+
        theme_bw()
p12

p13<-ggplot(hold, aes(x=year, y=S_kappa, color=c1c2))+
        geom_point(size=1)+
        geom_line(size=0.75)+
        ggtitle("S_kappa")+
        xlab("Year")+
        coord_cartesian(y=c(-0.5,1))+
        scale_colour_brewer(palette = "Paired")+
        theme_bw()
p13

ggarrange(p11, p12, p13, ncol=3, nrow=1, common.legend = TRUE, legend="top")


# now, re run using the different portfolios
# atop

# adjust atop portfolios
# we should be safe to recode NAs as 0
atopms2<-suppressWarnings(foreach(i=1:length(portfolio_atopms2)) %do% {
        portfolio_atopms2[[i]] %>% 
                as.data.frame() %>%
                mutate_all(funs(replace(., is.na(.), 0)))
})
allyears<-sort(unique(cleanDat$year))
names(atopms2)<-do.call(c, lapply(allyears, toString))



# Compute S_pi and S_kappa for these countries
allyears<-sort(unique(ms_dat$year))

c1="002"
c2="200"

test_cowalliance<-foreach(i=1:length(atopms2), .combine=rbind.data.frame) %do% {
        
        foo<-atopms2[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        
        s_pi<-S_pi(a, b)
        s_kappa<-S_kappa(a,b)
        
        out<-data.frame(year=allyears[i],
                        S_pi=s_pi,
                        S_kappa=s_kappa,
                        c1c2=paste(c1, c2, sep="_"))
        out
}

store1<-test_cowalliance


c1="200"
c2="220"

test_cowalliance<-foreach(i=1:length(atopms2), .combine=rbind.data.frame) %do% {
        
        foo<-atopms2[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        
        s_pi<-S_pi(a, b)
        s_kappa<-S_kappa(a,b)
        
        out<-data.frame(year=allyears[i],
                        S_pi=s_pi,
                        S_kappa=s_kappa,
                        c1c2=paste(c1, c2, sep="_"))
        out
}

store2<-test_cowalliance


c1="200"
c2="365"

test_cowalliance<-foreach(i=1:length(atopms2), .combine=rbind.data.frame) %do% {
        
        foo<-atopms2[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        
        s_pi<-S_pi(a, b)
        s_kappa<-S_kappa(a,b)
        
        out<-data.frame(year=allyears[i],
                        S_pi=s_pi,
                        S_kappa=s_kappa,
                        c1c2=paste(c1, c2, sep="_"))
        out
}

store3<-test_cowalliance


c1="200"
c2="710"

test_cowalliance<-foreach(i=1:length(atopms2), .combine=rbind.data.frame) %do% {
        
        foo<-atopms2[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        
        s_pi<-S_pi(a, b)
        s_kappa<-S_kappa(a,b)
        
        out<-data.frame(year=allyears[i],
                        S_pi=s_pi,
                        S_kappa=s_kappa,
                        c1c2=paste(c1, c2, sep="_"))
        out
}

store4<-test_cowalliance

hold<-rbind(store1, store2, store3, store4)

p22<-ggplot(hold, aes(x=year, y=S_pi, color=c1c2))+
        geom_point(size=1)+
        geom_line(size=0.75)+
        ggtitle("S_pi")+
        xlab("Year")+
        coord_cartesian(y=c(-0.5,1))+
        scale_colour_brewer(palette = "Paired")+
        theme_bw()
p22

p23<-ggplot(hold, aes(x=year, y=S_kappa, color=c1c2))+
        geom_point(size=1)+
        geom_line(size=0.75)+
        ggtitle("S_kappa")+
        xlab("Year")+
        coord_cartesian(y=c(-0.5,1))+
        scale_colour_brewer(palette = "Paired")+
        theme_bw()
p23

ggarrange(p21, p22, p23, ncol=3, nrow=1, common.legend = TRUE, legend="top")



# now, re run using the different portfolios
# milinst_atop

# adjust milinst_atop portfolios
# we should be safe to recode NAs as 0
milinst_atop<-suppressWarnings(foreach(i=1:length(portfolio_milinst_atop)) %do% {
        portfolio_milinst_atop[[i]] %>% 
                as.data.frame() %>%
                mutate_all(funs(replace(., is.na(.), 0)))
})
allyears<-sort(unique(cleanDat$year))
names(milinst_atop)<-do.call(c, lapply(allyears, toString))


# Compute S_pi and S_kappa for these countries
allyears<-sort(unique(ms_dat$year))

c1="002"
c2="200"

test_cowalliance<-foreach(i=1:length(milinst_atop), .combine=rbind.data.frame) %do% {
        
        foo<-milinst_atop[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        
        s_pi<-S_pi(a, b)
        s_kappa<-S_kappa(a,b)
        
        out<-data.frame(year=allyears[i],
                        S_pi=s_pi,
                        S_kappa=s_kappa,
                        c1c2=paste(c1, c2, sep="_"))
        out
}

store1<-test_cowalliance


c1="200"
c2="220"

test_cowalliance<-foreach(i=1:length(milinst_atop), .combine=rbind.data.frame) %do% {
        
        foo<-milinst_atop[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        
        s_pi<-S_pi(a, b)
        s_kappa<-S_kappa(a,b)
        
        out<-data.frame(year=allyears[i],
                        S_pi=s_pi,
                        S_kappa=s_kappa,
                        c1c2=paste(c1, c2, sep="_"))
        out
}

store2<-test_cowalliance


c1="200"
c2="365"

test_cowalliance<-foreach(i=1:length(milinst_atop), .combine=rbind.data.frame) %do% {
        
        foo<-milinst_atop[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        
        s_pi<-S_pi(a, b)
        s_kappa<-S_kappa(a,b)
        
        out<-data.frame(year=allyears[i],
                        S_pi=s_pi,
                        S_kappa=s_kappa,
                        c1c2=paste(c1, c2, sep="_"))
        out
}

store3<-test_cowalliance


c1="200"
c2="710"

test_cowalliance<-foreach(i=1:length(milinst_atop), .combine=rbind.data.frame) %do% {
        
        foo<-milinst_atop[[i]] %>%
                filter(ccode==c1 | ccode== c2) %>%
                select(-ccode, -year)
        
        a<-as.vector(as.matrix(foo[1,]))
        b<-as.vector(as.matrix(foo[2,]))
        
        s_pi<-S_pi(a, b)
        s_kappa<-S_kappa(a,b)
        
        out<-data.frame(year=allyears[i],
                        S_pi=s_pi,
                        S_kappa=s_kappa,
                        c1c2=paste(c1, c2, sep="_"))
        out
}

store4<-test_cowalliance

hold<-rbind(store1, store2, store3, store4)

p32<-ggplot(hold, aes(x=year, y=S_pi, color=c1c2))+
        geom_point(size=1)+
        geom_line(size=0.75)+
        ggtitle("S_pi")+
        xlab("Year")+
        coord_cartesian(y=c(-0.5,1))+
        scale_colour_brewer(palette = "Paired")+
        theme_bw()

p33<-ggplot(hold, aes(x=year, y=S_kappa, color=c1c2))+
        geom_point(size=1)+
        geom_line(size=0.75)+
        ggtitle("S_kappa")+
        xlab("Year")+
        coord_cartesian(y=c(-0.5,1))+
        scale_colour_brewer(palette = "Paired")+
        theme_bw()

ggarrange(p31, p32, p33, ncol=3, nrow=1, common.legend = TRUE, legend="top")

ggarrange(p11, p12, p13, p21, p22, p23, p31, p32, p33, ncol=3, nrow=3, common.legend=T, legend="top")

# done
