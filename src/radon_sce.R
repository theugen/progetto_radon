library(ggplot2)
library(dplyr)

setwd('~/Documents/Radon')
rn <- read.csv(file = 'misure.csv', sep=',', header = T, stringsAsFactors = F)
#filt <- filter(rn, CID == 'LNO')

rnp <- ggplot(rn, aes(x=-DEPTH, y=CONC, color = CID)) + geom_point()
#rnp <- rnp + facet_grid(CID ~ .)
rnp
