#New Keynesian Model with the list of defined parameters
#Preamble
setwd("D:/R working dir")
y=function(){dev.new();x=par(no.readonly=T);dev.off();x}
par(y());options(scipen=0);dev.off();rm(list=ls())
Sys.setenv(LANG = "en")

library("tidyverse")
library("fabletools") 
library("feasts") 
library("ggplot2")
library("dplyr")
library("tsibble")

#Define variables
Beta <- 0.99
Sig <- 3
Omega <- 0.95
Rho <- 2
FiPi <- 1.5
n <- 2
kcap <- ((1 - Omega) * (1 - Omega * Beta)) / Omega
k <- (n + Sig) * kcap
Fpi <-  (k*Sig)/(k*(FiPi-1)+Sig*(Rho-1)*(Beta-1))
Fy <- (Sig*(Beta-1))/(-k*(FiPi-1)-Sig*(Rho-1)*(Beta-1))

#Create a vector of u_t
i <- c(1:20)
to_j <- function(x) {
  u <- x^i
  return(u)
}
u_t <- to_j(0.8)

#Create a data frame 
P_t <- Fpi * u_t 
Y_t <- Fy * u_t
i_t <- Rho + (FiPi*P_t)
qtr <- yearquarter("2000 Q1")
date <- seq(qtr, length.out = 20, by = 1)

df <- data.frame(date, P_t, Y_t, i_t)
names(df) <- c("date", "Inflation", "Output Gap", "Interest Rate")

df <- as_tsibble(df, index = date)

#Create 3 separate plots
df %>%
  autoplot(vars("Inflation", "Output Gap", "Interest Rate")) + 
  geom_line(col = "#0c2ac1") +
  xlab("Years") + 
  ggtitle("New Keynesian Model of Economy") +
  theme(plot.title = element_text(hjust = 0.5))

#Create one plot
df %>%
  ggplot() +
  geom_line(aes(date, P_t, color = "Inflation"), size = 1) +
  geom_line(aes(date, Y_t, color = "Output Gap"), size = 1) +
  geom_line(aes(date, i_t, color = "Interest Rate"), size = 1) +
  xlab("Years") + ylab("%") +
  ggtitle("New Keynesian Model of Economy") +
  scale_colour_manual("Indicators:", 
                      values = c("#00FF00", "#FF0000", "#0000FF")) +
  theme(legend.position = c(.95, .90),
        legend.justification = c("right", "top"),
        plot.title = element_text(hjust = 0.5))
