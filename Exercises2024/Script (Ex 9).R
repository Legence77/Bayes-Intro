

# Im Folgenden analysieren wir einen Mietspiegeldatensatz aus dem Jahre 1999
# (\texttt{rent99.raw}). Er enthält insbesondere die Nettoquadratmetermiete, 
# gehobene Ausstattung mit Bad und Küche, die Lage (normal, gut, beste) sowie 
# Fläche und Baujahr.

# Die Datei \texttt{rent.R} enthält Code, um die Daten mit BayesX bzw. INLA zu 
# analysieren.
# 
# Installieren Sie die notwendigen Pakete, um den Code auszuführen.


# Installation der notwendigen Pakete
install.packages("BayesXsrc")
install.packages("INLA", repos="https://inla.r-inla-download.org/R/stable",
                 dependencies = TRUE)



library(BayesXsrc)
library(INLA)

rent_data<-read.csv("./Daten/rent99.raw", header = TRUE, sep=" ")



# Die ersten drei Teile analysieren nur den Einfluß von Lage, Küche und Bad, 
# mit empirischem Bayes-Ansatz (REML), MCMC und INLA. Vergleichen Sie die Ergebnisse. 
# Gibt es Unterschiede? Warum?\\
# * Keine Unterschiede, bis auf numerische. Punktschätzer, Posteriori-Varianz/
# Intervallschätzer betrachten.
# * Grund: Flache Priori für die Dummy-Variablen, keine REML-Schätzung, da kein
# Hyperparameter. Sehr einfaches Normalverteilungsmodell. 


names(rent_data)

reml_result <- glm(rent ~ location + kitchen + bath, data=rent_data)
mcmc_result <- MCMCpack::MCMCregress(rent ~ location + kitchen + bath, 
                                     data=rent_data)
inla_result <- inla(rent ~ location + kitchen + bath, data=rent_data, 
                    family="gaussian",
                    control.predictor = list(compute = TRUE),
                    control.compute = list(dic = TRUE, 
                                           return.marginals.predictor = TRUE))

summary(reml_result)
# Call:
#   glm(formula = rent ~ location + kitchen + bath, data = rent_data)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  356.075      9.443  37.707   <2e-16 ***
#   location      59.012      6.093   9.685   <2e-16 ***
#   kitchen      148.976     16.498   9.030   <2e-16 ***
#   bath         191.417     13.811  13.860   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 33986.81)
# 
# Null deviance: 117945363  on 3081  degrees of freedom
# Residual deviance: 104611398  on 3078  degrees of freedom
# AIC: 40909
# 
# Number of Fisher Scoring iterations: 2
# summary(mcmc_result)
# Iterations = 1001:11000
# Thinning interval = 1 
# Number of chains = 1 
# Sample size per chain = 10000 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#   
#   Mean      SD Naive SE Time-series SE
# (Intercept)   355.99   9.435  0.09435        0.09435
# location       59.03   6.123  0.06123        0.06123
# kitchen       148.94  16.379  0.16379        0.16022
# bath          191.63  13.811  0.13811        0.13811
# sigma2      34001.23 864.985  8.64985        8.83819
# 
# 2. Quantiles for each variable:
#   
#   2.5%      25%      50%      75%    97.5%
# (Intercept)   337.77   349.62   356.07   362.24   375.03
# location       46.83    54.96    59.03    63.17    70.87
# kitchen       116.38   137.87   149.12   160.14   180.93
# bath          164.51   182.23   191.60   200.88   219.44
# sigma2      32349.89 33407.33 33984.93 34575.06 35757.40

summary(inla_result)

# Time used:
#   Pre = 0.651, Running = 0.932, Post = 0.388, Total = 1.97 
# Fixed effects:
#   mean     sd 0.025quant 0.5quant 0.975quant    mode kld
# (Intercept) 361.790  9.286    343.583  361.788    380.006 361.788   0
# location     57.182  5.982     45.449   57.182     68.911  57.182   0
# kitchen     118.977 14.635     90.260  118.984    147.657 118.984   0
# bath        162.411 12.666    137.550  162.418    187.229 162.418   0
# 
# Model hyperparameters:
#   mean   sd 0.025quant 0.5quant 0.975quant
# Precision for the Gaussian observations 0.00 0.00       0.00     0.00       0.00
# mode
# Precision for the Gaussian observations 0.00
# 
# Deviance Information Criterion (DIC) ...............: 40916.85
# Deviance Information Criterion (DIC, saturated) ....: -10405.96
# Effective number of parameters .....................: 4.69
# 
# Marginal log-Likelihood:  -20498.65 
# is computed 
# Posterior summaries for the linear predictor and the fitted values are computed
# (Posterior marginals needs also 'control.compute=list(return.marginals.predictor=TRUE)')


# Mit \texttt{tic()} bzw. \texttt{toc()} lässt sich einfach die Zeit messen, die 
# ein (oder mehrere) Befehl(e) braucht. Woraus resultieren die unterschiedlichen 
# Laufzeiten?\\

library(tictoc)


tic("REML")
reml_result <- glm(rent ~ location + kitchen + bath, data=rent_data)
toc()


tic("MCMC")
mcmc_result <- MCMCpack::MCMCregress(rent ~ location + kitchen + bath, 
                                     data=rent_data)
toc()


tic("INLA")
inla_result <- inla(rent ~ location + kitchen + bath, data=rent_data, 
                    family="gaussian",
                    control.predictor = list(compute = TRUE),
                    control.compute = list(dic = TRUE, 
                                           return.marginals.predictor = TRUE))
toc()


# * Die unterschiedlichen Laufzeiten resultieren aus den verschiedenen Methoden.
# REML ist oft schneller, da es eine deterministische Optimierung durchführt.
# * MCMC ist langsamer, da es viele Iterationen benötigt, um Konvergenz zu erreichen. 
# INLA ist ebenfalls relativ schnell, da es approximative Inferenzmethoden verwendet.




# Die letzten beiden Teile benutzen ein Modell, in dem Baujahr und Wohnfläche 
# nichtlinear eingehen. Dafür werden REML und INLA benutzt. Interpretieren Sie 
# die Ergebnisse.\\
# Dummyvariablen interpretieren wie oben. Nichtlineare Effekte mit punktweisen 
# (!) Intervallen. Schätzer der Hyperparameter bei REML, Posteriori der 
# Hyperparameter bei INLA. 



reml_nonlinear <- mgcv::gam(rent ~ location + kitchen + bath+s(yearc), data=rent_data)
par(mfrow=c(1,1))
plot(reml_nonlinear)

inla_nonlinear <- inla(rent ~ location + kitchen + bath+ 
                         f(yearc, model="rw2"), 
                       data=rent_data, family="gaussian",
                       control.predictor = list(compute = TRUE),
                       control.compute = list(dic = TRUE, return.marginals.predictor = TRUE))

plot(inla_nonlinear)

# Verändern Sie den Code, so dass MCMC benutzt wird. Vergleichen die Ergebnisse 
# von REML und MCMC. Gibt es Unterschiede? Warum?\\


library(splines)
mcmc_nonlinear <- MCMCpack::MCMCregress(rent ~ location + kitchen + bath + 
                                          ns(yearc, df=4) + ns(rentsqm, df=4), 
                                        data=rent_data)

plot(mcmc_nonlinear)

# * Schätzer der Hyperparameter bei REML, Posteriori der Hyperparameter bei MCMC. 
# 
# * Nicht lineare Effekte bei REML glatter, da ein fester Glättungsparameter, 
#   bei MCMC durch Model averaging mehr Flexibilität, führt zu weniger Glattheit.



# Zusatzfrage: Bei INLA wird für die nichtlinearen Effekte als Priori ein Random 
# Walk zweiter Ordnung (RW2, ähnlich dem RW1-Zeittrend im Kapitel "Hierarchische 
# Modelle") angenommen. BayesX benutzt dagegen P-Splines mit RW2-Priori auf den 
# Splines-Koeffizienten. Machen Sie sich die Unterschiede klar. Wie wirkt sich das 
# hier auf die Ergebnisse aus?\\

# Prinzip der Splines, RW2 als Priori auf benachbarte Koeffizienten. Bei INLA
# werden die Variablen in Intervallen gepoolt, darauf dann RW2 (also nicht RW2 
# auf den eigentlichen Kovariablen). Im Ergebnis dadurch kaum Unterschiede zu 
# sehen, da hier relativ viele Daten.


# Interpretieren Sie die Konfidenzbänder der nichtlinearen Effekte. Warum werden 
# diese zu Anfang und gegen Ende größer?\\
# Interpretation punktweise. Am Anfang/Ende weniger Information durch RW-Priori, 
# da keine Punkte davor/danach.
# 

#  Führen Sie für eines der Modelle eine Sensitivitätsanalyse durch.\\




sensitivity_result <- inla(rent ~ location + kitchen + bath + 
                             f(yearc, model="rw2") +
                             f(rentsqm, model="iid"), 
                           data=rent_data, family="gaussian",
                           control.predictor = list(compute = TRUE),
                           control.compute = list(dic = TRUE, 
                                      return.marginals.predictor = TRUE))



# Interpretieren Sie für INLA den DIC bzw. die Anzahl der effektiven Parameter. 
#Versuchen Sie ein optimales Modell zu finden.\\


dic_result <- summary(inla_result)
effective_parameters <- inla_result$dic$p.eff

