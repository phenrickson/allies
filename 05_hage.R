# S score with tie corrections from Hage 2003

#setwd("F:/Dropbox/FSU/FSU Summer 2017/Mark_Project/")
setwd("/Users/phenrickson/Dropbox/FSU/Mark_Projects/")

# Recreate Signorino and Ritter Gauss Code for S scores

# X and Y are portfolios

# X is a Nx1 or NxM matrix 
# Y is a Nx1 or NxM matrix

# W is a Nx1 vector of weights, where each row represents the weight of the corresponding
# row element for the portfolios
# if W is a scalar, all weights are set to that scalar

# M is a length N vector of max distances, where each column is the maximum distance of the corresponding
# row element for the portfolios
# Note that this differs slightly from the original Gauss code because of how R treats division with matrices
# I'm choosing to input M as a vector to make the division cleaner

S<-function(x, y, w, m) {
        x<-as.matrix(x)
        y<-as.matrix(y)
        w<-as.matrix(w)
        
        r <- nrow(x)
        
        # if w is a scalar
        if (nrow(w)==1) {w<-matrix(1, r, 1)%*%w}
        
        wm<-t(t(w) / m)
        xy<-abs(x-y)
        
        d<-colSums(wm*xy)
        
        dmax<-colSums(w)
        
        sim<-1-2*(d/dmax)
        
        return(sim)
        
}

# rewrite using simplified no weights version, equation 1 from Hage page 290

S_h <- function(x, y, m) {
        x<-as.matrix(x)
        y<-as.matrix(y)
        
        r<-nrow(x)
        
        Dmax<-m*r
        D0<-sum(abs(x-y))
        Pd<-D0/Dmax
        
        S<-1-2*Pd
        
        return(S)
}


# this should be the same when all countries are weighted equally


# use binary examples from Hage page 291
x<-c(1,0,0,1,0,0,1,0,0,0)
y<-c(0,1,1,0,0,0,1,1,0,0)
# equal weights
w<-rep(1/length(x), length(x))
m<-1

S(x,y,w,m)
S_h(x,y,m)
# alright this reformulation works

# using the contingency table
abs<-table(x,y)

abs


# compute using contingency table
S_table<-function(abs) {
        D0<-abs[3]+abs[2]
        Dmax<-nrow(x)*m
        Pd<-D0/Dmax
        S<-1-2*Pd
        return(S)
}

S_table(abs)

# page 296
tab<-matrix(NA, 2,2)
tab[1,1]<-0.3
tab[1,2]<-0.2
tab[2,1]<-0.2
tab[2,2]<-0.3

# S
Do<-tab[3]+tab[2]
De<-0.5^2+0.5^2
S<-1-Do/De
S

# pi
Do<-tab[3]+tab[2]
De<-((sum(tab[1,])+sum(tab[2,]))/2)^2+((sum(tab[1,])+sum(tab[2,]))/2)^2
pi<-1-Do/De
pi

# kappa
Do<-tab[3]+tab[2]
De<-sum(tab[1,])^2+sum(tab[2,])^2
kappa<-1-Do/De
kappa


# page 297
tab<-matrix(NA, 2,2)
tab[1,1]<-0.6
tab[1,2]<-0.2
tab[2,1]<-0.2
tab[2,2]<-0.0

# S
Do<-tab[3]+tab[2]
De<-0.5^2+0.5^2
S<-1-Do/De
S

# pi
D0<-tab[3]+tab[2]
tab_chance<-matrix(NA, 2,2)
tab_chance[1,1]<-sum(tab[1,])*sum(tab[,1])
tab_chance[1,2]<-sum(tab[1,])*sum(tab[,2])
tab_chance[2,1]<-sum(tab[2,])*sum(tab[,1])
tab_chance[2,2]<-sum(tab[2,])*sum(tab[,2])

De<-((sum(tab[1,])+sum(tab[,2]))/2)^2
pi<-1-D0/De
pi

# kappa
D0<-tab[3]+tab[2]
De<-sum(tab[1,])*sum(tab[,2])+sum(tab[2,])*sum(tab[,1])
kappa<-1-D0/De
kappa


# second on page 297
# hage has this formula incorrect
tab<-matrix(NA, 2,2)
tab[1,1]<-0.3
tab[1,2]<-0.4
tab[2,1]<-0.0
tab[2,2]<-0.3

D0<-tab[3]+tab[2]
De<-sum(tab[1,])*sum(tab[,2])+sum(tab[2,])*sum(tab[,1])
kappa<-1-D0/De
kappa


# Example 1 from Signorino and Ritter
x<-c(3, 3, 2, 2)
y<-c(2, 2, 3, 3)
w<-c(.5, .3, .1, .1)
m<-3

# should be 0.33
S(x,y,w, m)
S_h(x, y, m)

# Hypothetical UN from Signorino and Ritter
x<-c(3, 3, 1, 2)
y<-c(3, 3, 1, 3)
w<-c(0.25, 0.25, 0.25, 0.25)
m<-2

S(x,y,w,m)
S_h(x,y, m)
# should be 0.75
# good

# to get the similarity of the combined policy portfolios,
# take the weighted average of the individual similarity scores
# should be 0.475
(1*.2+1*.75)/2
# good

# is the function flexible enough to handle multiple portfolios?
# ie, just recreating these two previous examples in one shot
X_1<-c(3, 2, 3, 0)
X_2<-c(3, 3, 1, 2)
x<-cbind(X_1, X_2)

Y_1<-c(2, 3, 0, 3)
Y_2<-c(3, 3, 1, 3)
y<-cbind(Y_1, Y_2)

W_1<-c(0.5, 0.4, 0.05, 0.05)
W_2<-c(0.25, 0.25, 0.25, 0.25)
w<-cbind(W_1, W_2)

m<-c(3,2)

S(x, y, w, m)
# works, good


# page 133 from Signorino and Ritter examples, assuming unweighted 
# (a)
x<-c(0, 1, 2, 3)
y<-c(3, 2, 1, 0)
w<-c(0.25, 0.25, 0.25, 0.25)
m<-3
# should be -0.33
S(x, y, w, m)
S_h(x,y, m)
# good

# (b)
x<-c(1, 2, 2, 3)
y<-c(3, 2, 2, 1)
w<-c(0.25, 0.25, 0.25, 0.25)
m<-3
# should be 0.33
S(x, y, w, m)
S_h(x,y,m)
# good

# (c)
x<-c(2, 2, 3, 3)
y<-c(3, 3, 2, 2)
w<-c(0.25, 0.25, 0.25, 0.25)
m<-3
# should be 0.33
S(x, y, w, m)
S_h(x,y,m)

# (d)
x<-c(3, 3, 3, 3)
y<-c(3, 3, 3, 3)
w<-c(0.25, 0.25, 0.25, 0.25)
m<-3
# should be 1
S(x, y, w, m)
S_h(x,y,m)
# good


### Now, let's test an updated version taking into account ties

S_pi<-function(x, y) {
        
        x<-as.matrix(x)
        y<-as.matrix(y)
        
        num<-sum((x-y)^2)
        denoma<-sum((x-(mean(x)+mean(y))/2)^2)
        denomb<-sum((y-(mean(x)+mean(y))/2)^2)
        
        denom<-denoma+denomb
        
        sim<-1-num/denom
        
        return(sim)
        
}

S_kappa<-function(x, y) {
        
        x<-as.matrix(x)
        y<-as.matrix(y)
        
        num<-sum((x-y)^2)
        denom<-sum((x-mean(x))^2)+sum((y-mean(y))^2)+sum((mean(x)-mean(y))^2)
        
        sim<-1-num/denom
        return(sim)
                
}

dump("S", file="S.R")
dump("S_pi", file="S_pi.R")
dump("S_kappa", file="S_kappa.R")



