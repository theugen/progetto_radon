library(ggplot2)
library(dplyr)

#Setting working directory
setwd('~/tmp/progetto_radon/')
#Importing data file
rn <- read.csv(file = 'data/misure.csv', sep=',', header = T, stringsAsFactors = F)
#Computing absolute error and mean temperature
rn <- mutate(rn, ABS_ERR = (ERR * CONC)/100)
rn <- mutate(rn, TMEAN = (T1+T2)/2)

#Computing correlations
cor_conc_dep <- cor(rn$CONC, rn$DEPTH)
cor_conc_temp <- cor(rn$CONC, rn$TMEAN)
cor_temp_dep <- cor(rn$TMEAN, rn$DEPTH)


#Want to check if it does exist a directory for the graphs: if not, we'll create it
if(dir.exists('graphs') == F){
  dir.create('graphs')
}


#Plotting all concentrations with depth
rnp <- ggplot(rn, aes(x=-DEPTH, y=CONC, color = CID)) + geom_point() + 
       labs(title='Concentration with depth', x='Depth (m)', y='Concentration (Bq/m^3)') 

ggsave(rnp, file='graphs/conc.png', width = 8, height = 6)     

#Now the same as above, but faceting based on cave, and adding errors as given from the lab
rnp_fc <- ggplot(rn, aes(x=-DEPTH, y=CONC)) + geom_point() + 
          labs(title='Concentration with depth', x='Depth (m)', y='Concentration (Bq/m^3)')  + 
          facet_grid(. ~ CID) + geom_errorbar(aes(x =-DEPTH, ymin = (CONC - ABS_ERR), ymax = (CONC + ABS_ERR)))
ggsave(rnp_fc, file='graphs/conc_fc_err.png', width = 12, height = 6)   



#Now the same as above, but checking for presence of water
rnp_fc_water <- ggplot(rn, aes(x=-DEPTH, y=CONC, color=H2O)) + geom_point() + 
  labs(title='Concentration with depth', x='Depth (m)', y='Concentration (Bq/m^3)')  + 
  facet_grid(. ~ CID) + geom_errorbar(aes(x =-DEPTH, ymin = (CONC - ABS_ERR), ymax = (CONC + ABS_ERR)))
ggsave(rnp_fc_water, file='graphs/conc_fc_err.png', width = 12, height = 6)   


#Now the same as above, but with linear smoothing
rnp_fc_water_lm <- ggplot(rn, aes(x=-DEPTH, y=CONC, color=H2O)) + geom_point() +
  labs(title='Concentration with depth', x='Depth (m)', y='Concentration (Bq/m^3)')  +
  geom_smooth(method='lm') +
  facet_grid(. ~ CID) + geom_errorbar(aes(x =-DEPTH, ymin = (CONC - ABS_ERR), ymax = (CONC + ABS_ERR)))
ggsave(rnp_fc_water_lm, file='graphs/conc_fc_err.png', width = 12, height = 6)

# #Now the same as above, but with loess
# rnp_fc_water_loess <- ggplot(rn, aes(x=-DEPTH, y=CONC, color=H2O)) + geom_point() + 
#   labs(title='Concentration with depth', x='Depth (m)', y='Concentration (Bq/m^3)')  + 
#   geom_smooth(method='loess') +
#   facet_grid(. ~ CID) + geom_errorbar(aes(x =-DEPTH, ymin = (CONC - ABS_ERR), ymax = (CONC + ABS_ERR)))
# ggsave(rnp_fc_water_loess, file='graphs/conc_fc_err.png', width = 12, height = 6) 
