# Compare the new measures

library(tidyr)
library(dplyr)
library(foreign)
library(data.table)
library(foreach)
library(doParallel)
library(ggplot2)

#setwd("F:/Dropbox/FSU/FSU Summer 2017/Mark_Project/")
setwd("/Users/phenrickson/Dropbox/FSU/Mark_Projects/")
#setwd("C:/Users/PoliSci Lab_2/Downloads/Mark_Projects/")

# load in ms data
ms_dat<-as.data.table(read.dta("ms_data_v2.dta"))
load("cleanDat.Rdata")

# load in new measures
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

head(check)

#### Compare
vars<-dplyr::select(check, S_old, S_new, S_atop, S_milinst_atop)


# quick plot of that
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        # correlation coefficient
        r <- cor(x, y, use="pairwise")
        txt <- format(c(r, 0.123456789), digits = digits)[1]
        txt <- paste("r= ", txt, sep = "")
        text(0.5, 0.6, txt)
}

# plot
pairs(vars, upper.panel = panel.cor, cex=0.5)

# plot over time
# make dyad var
check$c1c2<-substr(check$c1c2_year, 1,7)

meltdf<-melt(dplyr::select(check, year, c1c2, S_old, S_new, S_atop, S_milinst_atop), id.vars=c("S_old", "S_new", "S_atop", "S_milinst_atop", "year"))
head(meltdf)

library(ggplot2)

# S_old
plot_time1<-ggplot(meltdf, aes(x=year, y=S_old, group=value, color=S_old)) + 
        geom_point(alpha=0.1)+
        coord_cartesian(ylim=c(-0.5, 1))+
        labs(x="Year", y="value")+
        ggtitle("S_old")+
        scale_color_gradient(low="red", high="blue") +
        theme_minimal()
plot_time1

# S_new
plot_time2<-ggplot(meltdf, aes(x=year, y=S_new, group=value, color=S_new)) + 
        geom_point(alpha=0.1)+
        coord_cartesian(ylim=c(-0.5, 1))+
        labs(x="Year", y="value")+
        ggtitle("S_new")+
        scale_color_gradient(low="red", high="blue") +
        theme_minimal()
plot_time2

# S_atop
plot_time3<-ggplot(meltdf, aes(x=year, y=S_atop, group=value, color=S_atop)) + 
        geom_point(alpha=0.1)+
        coord_cartesian(ylim=c(-0.5, 1))+
        labs(x="Year", y="value")+
        ggtitle("S_atop")+
        scale_color_gradient(low="red", high="blue") +
        theme_minimal()
plot_time3

# S_milinst_atop
plot_time4<-ggplot(meltdf, aes(x=year, y=S_milinst_atop, group=value, color=S_milinst_atop)) + 
        geom_point(alpha=0.1)+
        coord_cartesian(ylim=c(-0.5, 1))+
        labs(x="Year", y="value")+
        ggtitle("S_milinst_atop")+
        scale_color_gradient(low="red", high="blue") +
        theme_minimal()
plot_time4


### look at specific countries
meltdf<-melt(dplyr::select(check, -S_new, -c1c2, -c1c2_year, -c2c1_year), id.vars=c("year", "ccode1", "ccode2"))

# function
plot_countries<-function(c1, c2) {
        
        foo<-meltdf %>%
                filter(ccode1==c1 | ccode1== c2) %>%
                filter(ccode2==c1 | ccode2== c2)
        
        ggplot(foo, aes(x=year, y=value, color=variable))+
                geom_point(size=1)+
                geom_line(size=0.75)+
                ggtitle(paste(c1, c2, sep="-"))+
                xlab("Year")+
                coord_cartesian(y=c(-0.5,1))+
                scale_colour_brewer(palette = "Paired")+
                theme_bw()
}

# using the function

### US
# US-UK
p1<-plot_countries("002", "200")

# US-France
p2<-plot_countries("002", "220")

# US-Germany
p3<-plot_countries("002", "255")
p3

# US-Russia
p4<-plot_countries("002", "365")
p4

# combine
library(ggpubr)
ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = TRUE, legend="top")

### China
# China-US
p1<-plot_countries("710", "002")

# China-Russia
p2<-plot_countries("710", "365")

# China-Japan
p3<-plot_countries("710", "740")

# China-India
p4<-plot_countries("710", "750")

ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = TRUE, legend="top")

# interesting pairs
# Israel-Iran
p1<-plot_countries("666", "630")

# UK-India
p2<-plot_countries("200", "750")

# Argentina-Brazil
p3<-plot_countries("160", "140")

# South Africa Zimbabwe
p4<-plot_countries("560", "552")

ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = TRUE, legend="top")


# UK-US
p1<-plot_countries("002", "200")
p1

# UK-China
p2<-plot_countries("200", "710")
p2

ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="top")


