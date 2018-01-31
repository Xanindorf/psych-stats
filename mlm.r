### Inlämningsuppgift multi-level modelling

library(lme4)
library(tidyr)
options(scipen = 999)

data <- read.table("C:\\Users\\Arian\\Documents\\R\\psych-stats\\mlm_ses.txt", header = TRUE)
names(data)

# Centrera variabler efter grand means
int_cent <- data[,"intelligens"] - mean(data[,"intelligens"])
utb_cent <- data[,"utbildning"] - mean(data[,"utbildning"])
seb_cent <- data[,"SEB"] - mean(data[,"SEB"])

# Formatera om data så att varje person får en rad för varje mätning på USEP
long_data <- gather(data, tid25, USEP, USEP25:USEP40, factor_key = TRUE)

# Omvandla mättillfälle till "år sedan 25 års ålder"
long_data$tid25 <- as.character(long_data$tid25)
long_data$tid25[long_data$tid25 == "USEP25"] <- "0"
long_data$tid25[long_data$tid25 == "USEP30"] <- "5"
long_data$tid25[long_data$tid25 == "USEP35"] <- "10"
long_data$tid25[long_data$tid25 == "USEP40"] <- "15"
long_data$tid25 <- as.numeric(long_data$tid25)

# Nollmodellen: fixed intercept, inga prediktorer, resultat som utfall
#   Går inte att göra med lmer(), hoppar över. 
# model0 <- lmer(USEP ~ idnum + tid25, data = long_data)

# Första modellen: Lägger till random intercept över individer
model1 <- lmer(USEP ~ (1 | idnum), data = long_data, REML = FALSE)
summary(model1)

# Andra modellen: Lägg till fixed effekt av tid
model2 <- lmer(USEP ~ (1 | idnum) + tid25, data = long_data, REML = FALSE)
summary(model2)

# Jämför modellerna 1 & 2 och se ifall det skett någon förbättring.
anova(model1, model2)

# Tredje modellen: Lägg till random effect av tid
model3 <- lmer(USEP ~ (1 | idnum) + (0 + tid25 | idnum) + tid25, data = long_data, REML = FALSE)

# Jämför modellerna 2 & 3
anova(model2, model3)

