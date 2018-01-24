### Inlämningsuppgift multi-level modelling

library(lme4)
library(tidyr)

data <- read.table("C:\\Users\\Arian\\Documents\\R\\psych-stats\\mlm_ses.txt", header = TRUE)
names(data)

# Centrera variabler efter grand means
int_cent <- data[,"intelligens"] - mean(data[,"intelligens"])
utb_cent <- data[,"utbildning"] - mean(data[,"utbildning"])
seb_cent <- data[,"SEB"] - mean(data[,"SEB"])

# Formatera om data så att varje person får en rad för varje mätning på USEP
long_data <- gather(data, tid25, resultat, USEP25:USEP40, factor_key = TRUE)
# Omvandla till "år sedan 25 års ålder"
long_data$tid25 <- as.character(long_data$tid25)
long_data$tid25[long_data$tid25 == "USEP25"] <- "0"
long_data$tid25[long_data$tid25 == "USEP30"] <- "5"
long_data$tid25[long_data$tid25 == "USEP35"] <- "10"
long_data$tid25[long_data$tid25 == "USEP40"] <- "15"
long_data$tid25 <- as.factor(long_data$tid25)
