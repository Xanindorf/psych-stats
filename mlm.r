### Inlämningsuppgift multi-level modelling

library(lme4)
library(tidyr)
options(scipen = 999)

data <- read.table("C:\\Users\\Arian\\Documents\\R\\psych-stats\\mlm_ses.txt", header = TRUE)
names(data)

# Centrera variabler efter grand means
data$int_cent <- data[,"intelligens"] - mean(data[,"intelligens"])
data$utb_cent <- data[,"utbildning"] - mean(data[,"utbildning"])
data$seb_cent <- data[,"SEB"] - mean(data[,"SEB"])

# Formatera om data så att varje person får en rad för varje mätning på USEP
long_data <- gather(data, tid25, USEP, USEP25:USEP40, factor_key = TRUE)

# Omvandla mättillfälle till "år sedan 25 års ålder"
long_data$tid25 <- as.character(long_data$tid25)
long_data$tid25[long_data$tid25 == "USEP25"] <- "0"
long_data$tid25[long_data$tid25 == "USEP30"] <- "5"
long_data$tid25[long_data$tid25 == "USEP35"] <- "10"
long_data$tid25[long_data$tid25 == "USEP40"] <- "15"
long_data$tid25 <- as.numeric(long_data$tid25)

# Första: fixed intercept, inga prediktorer, resultat som utfall
#   Går inte att göra med lmer(), hoppar över. 
# model1 <- lmer(USEP ~ idnum + tid25, data = long_data)

# Andra modellen: Lägger till random intercept över individer
model2 <- lmer(USEP ~ (1 | idnum), data = long_data, REML = FALSE)
summary(model2)

# Tredje modellen: Lägg till fixed effekt av tid
model3 <- lmer(USEP ~ (1 | idnum) + tid25, data = long_data, REML = FALSE)
summary(model3)

# Jämför modellerna 2 & 3 och se ifall det skett någon förbättring.
anova(model2, model3)

# Fjärde modellen: Lägg till random effect av tid
model4 <- lmer(USEP ~ (1 | idnum) + (0 + tid25 | idnum) + tid25, data = long_data, REML = FALSE)
summary(model4)
anova(model3, model4)

# Femte modellen: Lägg till fixed effect av intelligens 
# samt interaktion mellan intelligens och tid
model5 <- lmer(USEP ~ (1 | idnum) + (0 + tid25 | idnum) + tid25 + tid25 * int_cent, data = long_data, REML = FALSE)
summary(model5)
anova(model4, model5)

# Sjätte modellen: byter ut intelligens mot utbildning
model6 <- lmer(USEP ~ (1 | idnum) + (0 + tid25 | idnum) + tid25 + tid25 * utb_cent, data = long_data, REML = FALSE)
summary(model6)
anova(model4, model6)

# Sjunde modellen: byter ut utbildning mot socioekonomisk bakgrund
model7 <- lmer(USEP ~ (1 | idnum) + (0 + tid25 | idnum) + tid25 + tid25 * seb_cent, data = long_data, REML = FALSE)
summary(model7)
anova(model4, model7)
