# ---- Load or install required packages ----
required_packages <- c("rjags", "coda")
installed <- required_packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(required_packages[!installed])
lapply(required_packages, library, character.only = TRUE)

# ---- Load data ----
# Change the path if needed
data_path <- "../Daten/kaiserschnitt.raw.txt"
kaiserschnitt <- read.table(data_path, header = TRUE)

# ---- Prepare data list for JAGS ----
daten <- list(
  n = kaiserschnitt$n,
  risk = kaiserschnitt$risk,
  nplan = kaiserschnitt$nplan,
  N = length(kaiserschnitt$n),
  a = 0.001,
  b = 0.001
)

# ---- Define the JAGS model ----
modell <- "
model {
  for(i in 1:N) {
    n[i] ~ dpois(lambda[i])
    log(lambda[i]) <- alpha + beta * nplan[i] + gamma * risk[i] + eta[i]
    eta[i] ~ dnorm(0, sqrt(1 / tau))
  }
  alpha ~ dunif(-100, 100)
  beta ~ dnorm(-1, 10)
  gamma ~ dnorm(0, 0.0001)
  tau ~ dgamma(a, b)
}
"

# ---- Compile and burn-in the model ----
model <- jags.model(file = textConnection(modell), data = daten, n.chains = 3, quiet = TRUE)
update(model, 9000, progress.bar = "none")

# ---- Sample posterior for beta and gamma (task b) ----
samples_bg <- coda.samples(model, c("beta", "gamma"), 10000, progress.bar = "none")
plot(samples_bg)

# ---- Sample full model output for summary (task c) ----
samples_all <- coda.samples(model, c("alpha", "beta", "gamma", "tau"), 10000, progress.bar = "none")
print(summary(samples_all))

# ---- Sample and plot eta effects (task d) ----
samples_eta <- jags.samples(model, "eta", 10000, progress.bar = "none")
eta_quant <- apply(samples_eta$eta, 1, quantile, probs = c(0.1, 0.5, 0.9))

plot(1:24, eta_quant[2, ], ylim = range(eta_quant), ylab = "eta", xlab = "Krankenhaus", pch = 16)
for (i in 1:24) lines(rep(i, 3), eta_quant[, i], lwd = 2)




# Im Folgenden ist ein Datensatz über Infektionen bei Kaiserschnitt-Geburten an 24 Kliniken gegeben. Zielvariable ist die Anzahl an Infektionen bei 10.000 Geburten (n). Als weitere Variablen sind gegeben: Risiko-Faktor ja oder nein (risk), geplanter Kaiserschnitt ja oder nein (nplan).
# 
# Es wird folgendes JAGS-Modell verwendet:
  
modell = "
model {
    for(i in 1:N)
    {
    n[i] ~ dpois(lambda[i])
    log(lambda[i]) = alpha + beta * nplan[i] + gamma * risk[i] + eta[i]
    eta[i] ~ dnorm(0, sqrt(1/tau))
    }
    alpha ~ dunif(-100,100)
    beta ~ dnorm(-1, 10)
    gamma ~ dnorm(0, 0.0001)
    tau ~ dgamma(a,b)
}"

# a) Benennen Sie die einzelnen Teile des Modelles. Beschreiben Sie insbesondere, welche Arten von Prioris benutzt werden.

# Poisson-Modell mit Überdispersion ($\eta$). Kovariablen wirken linear auf log-Rate $\lambda$.
# Priori auf Intercept ($\alpha$): Gleichverteilung auf [-100,100], wenig informativ.
# Priori auf Kovariableneffekt von nplan ($\beta$): Normalverteilung mit niederiger Varianz/großer Präzision und Erwartungswert 1, informative Priori.
# Priori auf Kovariableneffekt von risk ($\gamma$): Normalverteilung mit hoher Varianz/niederiger Präzision, wenig informativ.
# Priori auf Überdispersion ($\eta_i$): Normalverteilung mit Erwartungswert Null und unbekannter Präzision, Regularisierungspriori
# (Hyper-)Priori auf Präzision $\tau$: Gamma-Verteilung, Priori-Information hängt von $a$ und $b$ ab.


# b) Interpretieren Sie folgende Graphiken:

kaiserschnitt <- read.table("../Daten/kaiserschnitt.raw.txt", header=TRUE)
daten<-list("n"=kaiserschnitt$n,
            "risk"=kaiserschnitt$risk,
            "nplan"=kaiserschnitt$nplan,
            "N"=length(kaiserschnitt$n),
            "a" = 0.001,
            "b" = 0.001)

model <- jags.model(file = textConnection(modell), data = daten, quiet=TRUE)
update(model, 9000, progress.bar="none")
samples = coda.samples(model, c("beta","gamma"), 10000, progress.bar="none")
plot(samples)



# Trace plot und geschätzte Dichte von beta und gamma, den Kovariableneffekten. 
# Der Trace plot gibt uns eine gute Idee, ob der Algorithmus konvergiert ist.
# Wir sehen, dass der Traceplot von $\beta$, nach einem Burnin, sehr zufällig um den Schätzwert oszilliert, ohne sehbare Korrelation zwischen den Draws aus der Posterior.
# Die $\gamma$ Draws sehen aber eher korreliert aus, da der Traceplot um einen Wert oszilliert und dann schrittweise auf den nächsten Wert läuft, daher müsste hier nochmal Thinning und mehr Durchläufe gemacht werden. 


# c) Geben Sie aus folgendem Output für den Einfluß der Kovariablen jeweils einen Punktschätzer und ein 95%-Kredibilitätsintervall an. Kann man aus dem Output schliessen, dass die Variablen "risk" und "nplan" wichtig für das Vorkommen von Infektionen sind?
  
 
model <- jags.model(file = textConnection(modell), data = daten, quiet=TRUE)
update(model, 9000, progress.bar="none")
samples = coda.samples(model, c("alpha","beta","gamma","tau"), 10000, progress.bar="none")
summary(samples)

# "risk" $\gamma \in (exp(0.88), exp(5.34))$: Wenn "risk"$=1$, dann erhöht sich die Anzahl der Infektionen multiplikativ zu 95\% W'keit um zwischen $exp(0.88)$ und  $exp(5.34)$.
# "nplan" $\beta \in (exp(-1.5), exp(-0.30))$: Wenn "nplan"$=1$, dann "erhöht" sich die Anzahl der Infektionen multiplikativ zu $95\%$ W'keit um zwischen $exp(-1.5)$ und  $exp(-0.30)$. 


# d) Folgende Graphik enthält Posteriori-Median (Punkt) und 80%-Kredibilitätsintervall (senkrechter Strich) des Parameters eta. Interpretieren Sie die Graphik.


model <- jags.model(file = textConnection(modell), data = daten, quiet=TRUE)
update(model, 9000, progress.bar="none")
samples = jags.samples(model, "eta", 10000, progress.bar="none")
eta<-apply(samples$eta,1,quantile,c(.1,.5,.9))
plot(1:24, eta[2,],ylim=range(eta), ylab="eta", xlab="Krankenhaus")
for (i in 1:24)lines(rep(i,3),eta[,i],lwd=2)


# Wir sehen, dass die $80\%$ Kredibilitätsintervalle für einige Krankenhäuser sehr weit auseinander liegen. 












