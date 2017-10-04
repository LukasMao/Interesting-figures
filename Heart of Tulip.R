library(ggplot2)
library(dplyr)
library(cowplot)


dat<- data.frame(t=seq(0, 2*3.1415926535897932384626, by=0.26) )
xhrt <- function(t) 16*sin(t)^3
yhrt <- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
dat$y=yhrt(dat$t)
dat$x=xhrt(dat$t)
with(dat, plot(x,y, type="l",lwd = 3,col = "red"))
distance <- c(1:dim(dat)[1])
coordinate <- c(1:dim(dat)[1])
hori <- c(1:dim(dat)[1])
vert <- c(1:dim(dat)[1])
for (i in c(2:dim(dat)[1])){
        hori[i]=abs(dat$x[i]-dat$x[i-1])
        vert[i]=abs(dat$y[i]-dat$y[i-1])
        distance[i]=sqrt(hori[i]^2+vert[i]^2)
        coordinate[i]=paste("(",as.character(round(hori[i],1)),seq=';',as.character(round(vert[i],1)),")")
        }
dat <- mutate(dat,dis=as.character(round(distance,1)))
dat <- mutate(dat,coordinate=coordinate)
dat$dis[1] <- NA
dat$coordinate[1] <- NA
dat <- dat[,c('t','x','y','coordinate','dis')]


ggplot(dat[,2:4],aes(x=x,y=y,label=coordinate))+geom_point(aes(color="red",size=1))+
        geom_path(aes(color="red",size=0.5))+
        geom_text(hjust = 0.5,nudge_y = 1)+
        ggtitle("Tulip Heart Garden for Celebrating Cute Crey's Birthday")+
        scale_x_continuous(breaks=c(-18:16),
                         labels=c("-18","-17","-16","-15","-14","-13","-12","-11","-10",
                                  "-9","-8","-7","-6","-5","-4","-3","-2","-1","0",
                                  "1","2","3","4","5","6","7","8","9","10",
                                  "11","12","13","14","15","16"))+
        scale_y_continuous(breaks=c(-18:13),
                           labels=c("-18","-17","-16","-15","-14","-13","-12","-11","-10",
                                    "-9","-8","-7","-6","-5","-4","-3","-2","-1","0",
                                    "1","2","3","4","5","6","7","8","9","10",
                                    "11","12","13"))+
        theme(plot.title = element_text(hjust = 0.5),
                axis.title.x = element_blank(),
              axis.title.y = element_blank(),
                axis.text.x = element_text(size=13),
              axis.text.y = element_text(size=13),
                legend.position = "none")+
        draw_label("Happy Birthday, Crey" , angle = 35, size = 40, alpha = .1)


dim(dat)

300/(max(dat$y)-min(dat$y))

grid.newpage()
dat<- data.frame(t=seq(0, 2*3.1415926535897932384626, by=0.1) )
rhrt <- function(t) sin(t)*sqrt(abs(cos(t)))/(sin(t)+7/5)-2*sin(t)+2
dat$x=rhrt(dat$t)*cos(dat$t)  ## x=r*cos(t)
dat$y=rhrt(dat$t)*sin(dat$t)  ## y=r*sin(t)
