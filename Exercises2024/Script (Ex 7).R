# lungenkrebs_modell.R

# ---- Setup ----
# Load required packages
library(rjags)
library(coda)

path<-"C:/Users/ra98jiq/Documents/GitHub/Bayes_Uebung_2425"

# ---- Question: Load data ----
# Der Datensatz 'lungenkrebs-frauen.RData' enthält die Anzahl der 
# Lungenkrebssterbefälle in Deutschland über 55 Jahre.
# Dabei enthält y die Anzahl der Fälle pro Jahr und E eine standardisierte 
# Bevölkerungszahl pro Jahr.
load(paste0(path, "/Daten/lungenkrebs-frauen.RData"))  # Pfad ggf. anpassen

# ---- Question: Modellbeschreibung ----
# Modellannahmen:
# y_t ~ Poisson(λ_t * E_t)
# log(λ_t) = φ_t + ε_t
# φ_t ~ N(φ_{t-1}, τ^2), φ_1 ~ konst.
# ε_t ~ N(0, ν^2)
# τ^2 ~ Gamma(a,b), ν^2 ~ Gamma(c,d)






# ---- Teil (a) ----
# Erläutern Sie die Bedeutung des Parameters φ:
# → φ beschreibt einen glatten zeitlichen Trend (Random Walk), erlaubt also 
# Korrelation zwischen benachbarten Jahren.





# ---- Teil (b) ----
# Erläutern Sie die Bedeutung der Parameter τ² und ν²:
# → τ²: Steuerung der Glattheit des Trends φ.
# → ν²: Erfasst zusätzliche (Über-)Dispersion im Modell. 
# Beide regulieren die Varianz der zufälligen Effekte.





# ---- Teil (c) ----
# Wählen Sie im Folgenden a=b=0.1, c=d=1. Erläutern Sie, welche Priori-
# Information dadurch angenommen wird.
# → a=b=0.1: schwach informative Prior, erlaubt größere Werte für τ² 
# (weniger Glättung)
# → c=d=1: moderat informative Prior für ν² (Gamma mit Erwartungswert 1)

# ---- Teil (d): JAGS-Modell definieren und schätzen ----
model_string <- "
model {
  for (t in 1:T) {
    y[t] ~ dpois(E[t] * lambda[t])
    lambda[t] <- exp(gamma[t] + epsilon[t])
    epsilon[t] ~ dnorm(0, nu_sq)
  }
  for (t in 2:T) {
    gamma[t] ~ dnorm(gamma[t-1], tau_sq)
  }
  gamma[1] ~ dnorm(0, 1e-4)

  # Priors
  tau_sq ~ dgamma(tau_a, tau_b)
  nu_sq ~ dgamma(nu_c, nu_d)
}
"

# Daten und Priorparameter an JAGS übergeben
daten <- list(
  y = y,
  E = E,
  T = length(y),
  tau_a = 0.1,
  tau_b = 0.1,
  nu_c = 1,
  nu_d = 1
)

# Modell kompilieren
model <- jags.model(
  file = textConnection(model_string),
  data = daten,
  n.chains = 3,
  quiet = TRUE
)

# Burn-in Phase
update(model, 15000, progress.bar = "none")

# Ziehen der Posterior-Samples
samples <- coda.samples(
  model,
  variable.names = c("gamma", "tau_sq", "nu_sq"),
  n.iter = 20000,
  progress.bar = "none"
)

# ---- Teil (e): Graphische Darstellung der Parameterschätzungen ----
# Visualisieren Sie die Posteriorverteilungen:
# → plot(samples): Trace- und Dichteplots
# → traceplot(samples): Konvergenzkontrolle
# → densityplot(samples): Posteriorverteilungen
plot(samples)
traceplot(samples)
densityplot(samples)

# → Gelman-Rubin: Konvergenzdiagnostik
gelman.diag(samples)


# lungenkrebs_modell_tabak.R

# ---- Frage: Erweiterung des Modells um Tabakkonsum ----
# Der Datensatz enthält zusätzlich den (geschätzten) Tabakkonsum X von Frauen 
# im selben Zeitraum.

# Teil (a)
# Erstellen Sie ein Modell, das den Tabakkonsum berücksichtigt:
#
# Modellgleichungen:
# y_t ~ Poisson(λ_t * E_t)
# log(λ_t) = φ_t + β * X_t + ε_t
# φ_1 ∝ konst.
# φ_t ~ N(φ_{t-1}, τ²)   für t = 2, ..., T
# ε_t ~ N(0, ν²)
# τ² ~ Gamma(a, b)
# ν² ~ Gamma(c, d)
# β ~ N(0, e)


# ---- Modellcode definieren (mit Tabakkonsum) ----
model_X <- "
model {
  for (t in 1:T) {
    y[t] ~ dpois(E[t] * lambda[t])
    lambda[t] <- exp(gamma[t] + beta * X[t] + epsilon[t])
    epsilon[t] ~ dnorm(0, nu_sq)
  }
  for (t in 2:T) {
    gamma[t] ~ dnorm(gamma[t-1], tau_sq)
  }
  gamma[1] ~ dnorm(0, 1e-4)

  # Priors
  tau_sq ~ dgamma(tau_a, tau_b)
  nu_sq ~ dgamma(nu_c, nu_d)
  beta ~ dnorm(0, e)
}
"

# ---- Teil (b): Modell schätzen ----
# JAGS-Datenliste definieren
daten <- list(
  y = y,
  X = X,
  E = E,
  T = length(y),
  tau_a = 0.001,
  tau_b = 0.001,
  nu_c = 0.001,
  nu_d = 0.001,
  e = 1e4
)

# Modell kompilieren und anpassen
model_X <- jags.model(
  file = textConnection(model_X),
  data = daten,
  n.chains = 3,
  quiet = TRUE
)

# Burn-in
update(model_X, 9000, progress.bar = "none")

# Ziehen der Posterior-Samples
samples_X <- coda.samples(
  model_X,
  variable.names = c("gamma", "tau_sq", "nu_sq", "beta"),
  n.iter = 10000,
  progress.bar = "none"
)

# ---- Teil (c): Modellvergleich ----
# Vergleichen Sie die Modelle mit und ohne Tabakkonsum. Sollte der Tabakkonsum 
# hier berücksichtigt werden?

# → Posteriorzusammenfassung von beta
print(summary(samples_X)$quantiles["beta",])

# → DIC beider Modelle vergleichen:
# Vorheriges Modell ohne Tabakkonsum (angenommen, es heißt 'model' und wurde 
# bereits geschätzt)
# Falls nicht vorhanden, bitte erneut mit dem ursprünglichen Code ausführen.

dic_ohne_tabak <- dic.samples(model, 10000)
dic_mit_tabak  <- dic.samples(model_X, 10000)

print(dic_ohne_tabak)
print(dic_mit_tabak)

# Interpretation:
# Beide Modelle scheinen sehr ähnlich.
# Das 95%-Kredibilitätsintervall für beta enthält 0 → schwache Evidenz für 
# einen Effekt.
# → Daher könnte auf den Tabakkonsum als Kovariate verzichtet werden.