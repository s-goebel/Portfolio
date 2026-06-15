#Seminararbeit 

#Setzt euer Working Directory!

load("/home/s/Dokumente/Studium/Seminare/Mediation und Moderation/job.rda")       #Datensatz laden
library(lavaan)       #lavaan-Paket laden
library(interactions) #interactions-Paket laden


#Der Datensatz "job" stammt aus einer fiktiven Evaluationsstudie ueber potentielle Effekte des "assertiveness-trainings"
#(Training der Durchsetzungsfaehigkeit) auf die Zufriedenheit im Beruf (alle Studienteilnehmer waren im Beruf vergleichbar unzufrieden).
#Die Idee ist, dass eine gewisse Durchsetzungsfaehigkeit fuer Zufriedenheit im Beruf noetig ist, da man sonst beispielsweise
#wohlverdiente Gehaltserhoehungen nicht durchbekommt.
#Die kategoriale Variable "training" gibt an, ob eine Versuchsperson das Training
#durchlaufen hat (training = 1) oder nicht (sich also in der Kontrollgrupee befand; training = 0).
#Die z-standardisierte Variable "open" gibt die Offenheit fuer Erfahrungen der Person wieder (hoeherer Wert spricht fuer hoehere Offenheit).
#Die z-standardisierte Variable "assert" gibt die Durchsetzungsfaehigkeit (assertiveness) einer Person (nach dem Training) wieder. 
#Wir gehen davon aus, dass vor dem Training alle Personen aehnlich geringe Werte auf dieser Skala hatten.
#Die z-standardisierte Variable "satisfy" gibt die Zufriedenheit einer Person im Beruf wieder.
#Wir gehen davon aus, dass vor dem Training alle Personen aehnlich geringe Werte auf dieser Skala hatten.
#Die Variable "train_open" berechnet sich aus "training" multipliziert mit "open".

#Wir gehen davon aus, dass alle Voraussetzungen für kausale Schlussfolgerungen erfüllt waren.

#Ueberblick:
table(job$training)
#50 VP haben das Training durchlaufen, 50 weitere nicht.
summary(job$open)
sd(job$open)
boxplot(job$open)
summary(job$assert)
sd(job$assert)
boxplot(job$assert)
summary(job$satisfy)
sd(job$satisfy)
boxplot(job$satisfy)
cor(job)
#Die Variablen sind insgesamt guenstig fuer Analysen verteilt.


#Allgemeine Hinweise:
#Zeige bei allen Aufgaben deinen Rechenweg in R an; bzw. begruende deine Antworten. 
#Dabei darfst du dich auch auf Berechnungen/Ausfuehrungen, die du in vorherigen Aufgaben gemacht hast, beziehen. 
#Mache bei Interpretationen von Parametern auch immer Inferenz-statistische Aussagen. Alpha sei immer .05!
#Wenn es mehrere Lösungswege gibt, genügt es für die volle Punktzahl, nur einen zu präsentieren.

#Es koennen maximal 50 Punkte vergeben werden.

###



#Aufgabe 1:
#Haben trainierte Personen tendenziell eine hoehere Durchsetzungsfaehigkeit? Interpretiere den Effekt.
# (2 Punkte)
#Antwort:
fit1 <- lm(assert ~ 1 + training, data = job)
summary(fit1)
# Estimate (training) = 0.5243 => Trainierte zeigten im Mittel eine um 0.52 Standardabweichungen erhöhte Durchsetzungsfähigkeit.
# p-value: 0.008088 => dieser Effekt ist signifikant von Null verschieden.
# Interpretation: Ja, trainierte Personen zeigten signifikant hoehere Durchsetzungsfaehigkeit.




#Aufgabe 2:
#Folgende Hypothese wird aufgestellt: 
#Durch das Training erhoeht sich die Durchsetzungsfaehigkeit. Jedoch profitieren "offenere" Personen mehr von dem Training,
#da diese empfaenglicher fuer die fuer sie neuen Ideen des Trainings sind.
#Mit welcher Art von Modell kann diese Hypothese ueberprueft werden?
#Berechne dieses Modell und interpretiere den Modellkoeffizienten, der diese Hypothese testet.
# (4 Punkte)
#Antwort:

# Moderationsmodell, um zu prüfen, ob Offenheit den Effekt des Trainings beeinflusst:
fit2 <- lm(assert ~ 1 + training * open, data = job)
summary(fit2)
# Intercept:
# Personen ohne Training mit durchschnittlicher Offenheit
# zeigten im Mittel eine Durchsetzungsfähigkeit von -0.26 Standardabweichungen.
# p-value: 0.04592 => Dieser Wert ist signifikant von Null verschieden.

# Haupteffekt von Training:
# Bei Personen mit durchschnittlicher Offenheit führt das Training zu einer um 0.52 SD höheren Durchsetzungsfähigkeit.
# p-value: 0.00564 => Dieser Effekt ist signifikant von Null verschieden.

# Haupteffekt von Offenheit:
# Bei Personen ohne Training erhöhte sich die Durchsetzungsfähigkeit um 0.04 SD pro SD Anstieg in Offenheit.
# p-value: 0.77616 => Dieser Effekt ist NICHT signifikant von Null verschieden.
# => Offenheit ohne Training wirkte sich nicht signifikant auf die Durchsetzungsfähigkeit aus.

# Interaktionseffekt training:open:
# bei der Verschiebung von Offenheit um 1 SD nach oben, verstärkt sich der Trainingseffekt um 0.41 Standardabweichungen.
# p-value: 0.03052 => Dieser Interaktionseffekt ist signifikant von Null verschieden.
# => Der Effekt des Trainings auf die Durchsetzungsfähigkeit wird durch Offenheit moderiert.

# Antwort: Ja, offenere Personen profitieren signifikant stärker vom Training als weniger offene Personen.




#Aufgabe 3: 
#Plotte die Auspraegung der Offenheit (x-Achse) gegen den Effekt des Trainings
#auf die Durchsetzungsfaehigkeit (y-Achse). 
#Wie hoch muss die Offenheit einer Person auf Grundlage der aktuellen Schaetzung mindestens sein,
#damit sie von dem Training fuer ein signifikant erhoehtes Durchsetzungsvermoegen profitieren kann?
#Begruende deine Antwort.
# (8 Punkte)
#Antwort:
probe_interaction(model = fit2,    
                  pred = training,     
                  modx = open,
                  johnson_neyman = T, 
                  jnplot = T,         
                  interval = T,         
                  jnalpha = .05      
)
# When open is OUTSIDE the interval [-13.49, -0.33], the slope of training is p < .05.
# => Offenheit darf maximal .33 SD unter dem Mittelwert liegen
# Antwort: Der Trainingseffekt wird ab etwa open = -0.33 statistisch signifikant positiv.


#Aufgabe 4: 
#Gab es in der aktuellen Stichprobe Personen, fuer die auf Grundlage der aktuellen Analyse gar eine statistisch
#ueberzufaellige VerSCHLECHTERUNG der Durchsetzungsfaehigkeit durch das Training erwartet wird? Begruende deine Antwort.
# (3 Punkte)
#Antwort:

# Johnson-Neyman-Interval von Aufgabe 3 ergab:
# When open is OUTSIDE the interval [-13.49, -0.33], the slope of training is p < .05.
# => Im beobachteten Wertebereich ist der Trainingseffekt nur fuer open > -0.33 signifikant von 0 verschieden.
range(job$open) # gibt aus: -2.943856  3.573154
# Da in der Stichprobe open bis -2.94 beobachtet wird, gibt es Personen, fuer die eine Verschlechterung erwartet wird.
# Allerdings liegt dieser Bereich innerhalb der Zone, in der der Trainingseffekt NICHT signifikant ist (p >= .05; naemlich open <= -0.33).
# => Es gibt in der Stichprobe keine Personen, fuer die eine statistisch signifikante Verschlechterung durch das Training erwartet wird.



#Aufgabe 5:
#Verbessert das Training die Zufriedenheit, wenn man fuer die 
#Durchsetzungsfaehigkeit kontrolliert?
#Wie groß ist der Effekt? Interpretiere ihn.
#Hinweis: Fuer diese Aufgabe soll keine Moderation modelliert werden.
# (4 Punkte)
#Antwort:

# Effekt von training auf satisfy, unter Kontrolle der Durchsetzungsfähigkeit:
fit5 <- lm(satisfy ~ 1 + training + assert, data = job)
summary(fit5)

# Training unter Kontrolle der Durchsetzungsfähigkeit: SD: +0.34, p-value: .04
# => Es wird vorhergesagt, dass die Zufriedenheit bei Trainierten, unter Kontrolle der Durchsetzungsfähigkeit .34 SD höher liegt, als bei Untrainierten.
# Dieser Effekt ist statistisch signifikant.
# => Unabhängig vom Effekt auf die Durchsetzungsfähigkeit wird eine Steigerung der Zufriedenheit durch das Training bewirkt.





#Aufgabe 6:
#Gibt es einen signifikanten indirekten Effekt des Trainings auf die Zufriedenheit
#ueber die Durchsetzungsfaehigkeit?
#Interpretiere diesen Effekt. Gibt es einen signifikanten totalen Effekt? 
#Interpretiere auch diesen. Verwende fuer die Inferenz-Statistik z-Tests.
#Muesste eine Person, die Aufgabe 5 bereits richtig beantwortet hat, die Groeße des direkten Effekts schon
#vor der Rechnung fuer Aufgabe 6 kennen? Begruende deine Antwort.
#Hinweis: Fuer diese Aufgabe soll keine Moderation modelliert werden.
# (10 Punkte)
#Antwort:

# Mediations-Modell: training -> assert -> satisfy

mod6 <- '
# a-Pfad: Effekt des Trainings auf die Durchsetzungsfähigkeit
assert ~ 1 + a*training

# b-Pfad: Effekt der Durchsetzungsfähigkeit auf die Zufriedenheit
# c_l-Pfad: Direkter Effekt des Trainings auf die Zufriedenheit
satisfy ~ 1 + b*assert + c_l*training

# Indirekter Effekt:
ind := a*b

# Totaler Effekt:
total := ind + c_l
'

fit6 <- sem(mod6, data = job)
summary(fit6, rsquare = T)

# Interpretation indirekter Effekt (ind):
# Trainierte Personen zeigen im Mittel eine um 0.291 Standardabweichungen 
# höhere Zufriedenheit bedingt durch eine tendenziell höhere Durchsetzungsfähigkeit. 
# Dieser Effekt ist signifikant von Null verschieden (z = 2.537, p = .011).

# Interpretation totaler Effekt (total):
# Trainierte Personen erzielen im Mittel eine um 0.632 Standardabweichungen höhere 
# Zufriedenheit als Personen der Kontrollgruppe. Dieser Effekt ist signifikant 
# von Null verschieden (z = 3.352, p = .001).

# Zur Frage des direkten Effekts:
# Ja, eine Person, die Aufgabe 5 bereits richtig beantwortet hat, müsste die Größe 
# des direkten Effekts (c_l) bereits kennen. Er entspricht exakt dem 
# Regressionskoeffizienten von "training" aus Aufgabe 5 (b = 0.341). In beiden 
# Fällen wird der Effekt des Trainings auf die Zufriedenheit unter statistischer 
# Kontrolle der Durchsetzungsfähigkeit geschätzt.



#Aufgabe 7:
#Fuege deine Erkenntnisse ueber die aus den bisherigen Aufgaben gewonnenen Moderations- und Mediationsablaeufe zu einem
#moderierten Mediationsmodell zusammen. Berechne den Index der Moderation der Mediation. Ist er signifikant?
#Welche Schlussfolgerungen koennen aus ihm gewonnen werden? 
#Ist der bedingte indirekte Effekt fuer folgende Personengruppen signifikant?:
#Personen mit durchschnittlicher Auspraegung auf dem Moderator,
#Personen die eine Standardabweichung ueber dem Mittelwert der Moderator-Variablen liegen,
#Personen die eine Standardabweichungen unter dem Mittelwert der Moderator-Variablen liegen.
#Interpretiere diese bedingten indirekten Effekte!
#Verwende fuer die Inferenz-Statistik z-Tests.
# (19 Punkte)
#Antwort:


# Produktvariable für die Interaktion erstellen (wie in rumination-Beispiel)
job$train_open <- job$training * job$open

#Moderiertes Mediationsmodell spezifizieren
mod7 <- '
# Moderierte Regression: Training (UV) auf Durchsetzungsfähigkeit (M) 
# mit Moderator Offenheit
assert ~ 1 + a1*training + a2*open + a3*train_open

# Mediator (assert) auf Zufriedenheit mit direktem Trainingseffekt
satisfy ~ 1 + b*assert + c_l*training

# Index der Moderation der Mediation (a3 * b)
index := a3*b

# Bedingte indirekte Effekte (bei verschiedenen Offenheitswerten):
# Durchschnittliche Offenheit (open = 0)

bie0 := a1*b + a3*b*0

# 1 SD unter dem Mittelwert (open = -1)
bie1SDlow := a1*b + a3*b*-1

# 1 SD über dem Mittelwert (open = +1)  
bie1SDhigh := a1*b + a3*b*+1
'

fit7 <- sem(mod7, data = job)
summary(fit7, rsquare = T)

# Produktvariable für die Interaktion erstellen
job$train_open <- job$training * job$open

#Moderiertes Mediationsmodell spezifizieren
mod7 <- '
# Moderierte Regression: Training (UV) auf Durchsetzungsfähigkeit (M) 
# mit Moderator Offenheit
assert ~ 1 + a1*training + a2*open + a3*train_open

# Mediator (assert) auf Zufriedenheit mit direktem Trainingseffekt
satisfy ~ 1 + b*assert + c_l*training

# Index der Moderation der Mediation (a3 * b)
index := a3*b

# Bedingte indirekte Effekte (bei verschiedenen Offenheitswerten):
# Durchschnittliche Offenheit (open = 0)
bie0 := a1*b + a3*b*0

# 1 SD unter dem Mittelwert (open = -1)
bie1SDlow := a1*b + a3*b*-1

# 1 SD über dem Mittelwert (open = +1)  
bie1SDhigh := a1*b + a3*b*+1
'

fit7 <- sem(mod7, data = job)
summary(fit7, rsquare = T)

# Index der Moderation der Mediation:
# Wenn Offenheit um 1 Standardabweichung steigt, verändert sich der indirekte 
# Effekt des Trainings über die Durchsetzungsfähigkeit um 0.228 Standardabweichungen.
# Dieser Index ist signifikant von Null verschieden (z = 2.130, p = .033).

# Bedingte indirekte Effekte:
# Bei durchschnittlicher Offenheit: 0.288, signifikant von Null verschieden (z = 2.663, p = .008)
# Bei Offenheit 1 SD unter Mittelwert: 0.060, nicht signifikant von Null verschieden (z = 0.423, p = .672)
# Bei Offenheit 1 SD über Mittelwert: 0.516, signifikant von Null verschieden (z = 3.201, p = .001)

# Schlussfolgerungen:
# Der indirekte Effekt des Trainings über die Durchsetzungsfähigkeit wird durch 
# Offenheit moderiert. Bei durchschnittlicher und hoher Offenheit gibt es einen 
# signifikanten positiven indirekten Effekt. Bei niedriger Offenheit (1 SD unter 
# Mittelwert) ist der indirekte Effekt nicht signifikant.

