### Inlämningsuppgift Strukturekvationsmodeller
library(lavaan)
library(semPlot)

data_path <- ("C:\\Users\\Arian\\Documents\\R\\psych-stats\\SEM_Attachment_Depression.txt")
data <- read.table(data_path, header = TRUE)

names(data)

model <- '
  # mätmodellen
  attach_anxiety =~ Anxiety_1 + Anxiety_2 + Anxiety_3
  attach_avoid =~ Avoid_1 + Avoid_2 + Avoid_3
  dependence =~ DEQ_D + PSI_D
  self_criticism =~ DEQ_SC + PSI_SC
  depression =~ SDS + CES_D + DASS_D
  
  # regressionseffekter
  dependence ~ attach_anxiety + attach_avoid
  self_criticism ~ attach_anxiety + attach_avoid
  depression ~ attach_avoid + dependence + self_criticism
  
  # korrelationer
  attach_anxiety ~~ attach_avoid
'

# Testa modellen
model_fit <- sem(model, data = data)
parameterEstimates(model_fit, standardized = TRUE)
summary(model_fit, fit.measures = TRUE, standardized = TRUE)
inspect(model_fit, "rsquare")

# Nu kikar vi på modification indices
summary(model_fit, fit.measures = TRUE, standardized = TRUE, modindices = TRUE)

# Rita upp en enkel graf
# För vidare graftutorial, kolla https://rstudio-pubs-static.s3.amazonaws.com/78926_5aa94ae32fae49f3a384ce885744ef4a.html
semPaths(model_fit, intercept = FALSE, whatLabel = "std", residuals = FALSE)

# Lägg till en direkt effekt av attach_anxiety på depression
adjusted_model <- '
  # mätmodellen
  attach_anxiety =~ Anxiety_1 + Anxiety_2 + Anxiety_3
  attach_avoid =~ Avoid_1 + Avoid_2 + Avoid_3
  dependence =~ DEQ_D + PSI_D
  self_criticism =~ DEQ_SC + PSI_SC
  depression =~ SDS + CES_D + DASS_D
  
  # regressionseffekter
  dependence ~ attach_anxiety + attach_avoid
  self_criticism ~ attach_anxiety + attach_avoid
  depression ~ attach_avoid + attach_anxiety + dependence + self_criticism
  
  # korrelationer
  attach_anxiety ~~ attach_avoid
'

# Testar den nya modellen
# OBS -- modellen konvergerar inte. Varför??
adjusted_model_fit <- sem(adjusted_model, data = data)
parameterEstimates(adjusted_model_fit, standardized = TRUE)
summary(adjusted_model_fit, fit.measures = TRUE, standardized = TRUE)
inspect(adjusted_model_fit, "rsquare")
