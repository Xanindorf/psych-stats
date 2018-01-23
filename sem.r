### Inlämningsuppgift Strukturekvationsmodeller
library(lavaan)
library(sem)

data_path <- ("C:\\Users\\Arian\\Documents\\R\\psych-stats\\SEM_Attachment_Depression.txt")
data <- read.table(data_path, header = TRUE)

names(data)

model <- '
  # mätmodellen
  attachment_anxiety =~ Anxiety_1 + Anxiety_2 + Anxiety_3
  attachment_avoidance =~ Avoid_1 + Avoid_2 + Avoid_3
  dependence =~ DEQ_D + PSI_D
  self_criticism =~ DEQ_SC + PSI_SC
  depressive_symptoms =~ SDS + CES_D + DASS_D
  
  # regressionseffekter
  dependence ~ attachment_anxiety + attachment_avoidance
  self_criticism ~ attachment_anxiety + attachment_avoidance
  depressive_symptoms ~ attachment_anxiety + attachment_avoidance + dependence + self_criticism

  # korrelationer
  attachment_anxiety ~~ attachment_avoidance
'

model_fit <- sem(model, data = data)
summary(model_fit)
