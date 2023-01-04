
library(VIM)
library(caret)
library(ggplot2)


# Carreguem l'arxiu

heart <- read.csv("heart.csv")


# 2. Integracció i selecció
# seleccionem les columnes que volem (falta triar quines no es volen):

heart_sel<- heart[c("age", "sex", "trtbps", "chol", "fbs", "restecg", "exng", "caa", "output")]

summary(heart_sel)

# seleccionem dades en funció dels filtres que volguem (male/female)

heart_male <- subset(heart_sel, sex == 1)
heart_female <- subset(heart_sel, sex == 0)

summary(heart_male)
summary(heart_female)


# 3.1 0 o valors buits? no hi han valors bits o igual a 0 que no toquin

summary(heart_sel$age)
summary(heart_sel$trtbps)
summary(heart_sel$chol)
table(heart_sel$sex)
table(heart_sel$fbs)
table(heart_sel$exng)
table(heart_sel$caa)
table(heart_sel$output)

# en el cas de la variable restecg la convertim en dicotòmica convertint els velors 2 en 0s

table(heart_sel$restecg)
heart_sel$restecg[heart_sel$restecg == 2] <- 0
table(heart_sel$restecg)

# 3.2 valors extrems

boxplot(heart_sel$trtbps, main="Resting blood pressure")
rbp_outliers <- boxplot.stats(heart_sel$trtbps)$out
sort(rbp_outliers)

boxplot(heart_sel$chol, main="Colesterol level")
col_outliers <- boxplot.stats(heart_sel$chol)$out
sort(col_outliers)

# No sé si aquests valors son realment atípics o no però el que fariem és aquests valors
# els transformariem a NAs (com a exemple amb trtbps):

heart_sel$trtbps[heart_sel$trtbps >= min(rbp_outliers)] <- NA
summary(heart_sel)

colSums(is.na(heart_sel))

# busquem l'index dels NAs

idx <- which(is.na(heart_sel$trtbps))
# aqui tambe es podrien posar condicions per exemple si nomes volem dones
# which(is.na(heart_sel$trtbps) & heart_sel$sex == 0)
idx

# imputem valors nous

heart_in <- kNN(heart_sel, variable= "trtbps", k = 11)
summary(heart_in$trtbps)
colSums(is.na(heart_in))

# 4

# regressió logistica

mod1 <- glm(output ~ ., family = binomial, data = heart_sel)
summary(mod1)

# Càlcul de les OR (Odds-Ràtio)
exp(coefficients(mod1))

# Relació entre dolor anginós en repós (exng) i presentar elevació del segment ST(restecg) com a marcador d'isquèmia miocàrdica. 

confusionMatrix(as.factor(heart_sel$exng), as.factor(heart_sel$restecg))

# Quin és el nivell de relació entre edat(age) i colesterol(chol) i edat i pressió arterial en repós(trtbps). 

# mirem normalitat de les dades:

nrow(heart_sel) # com hi ha un valor significatiu de dades (>30) podem aplicar el teorema del limit central i assumim normalitat en les dades

shapiro.test(heart_sel$age) # no ho tinc clar ja que en teoria per el teorema del limit central 
# podem considerar normalitat de les dades, però això diu que no ho són????
shapiro.test(heart_sel$chol)
shapiro.test(heart_sel$trtbps)

qqnorm(heart_sel$age)
qqline(heart_sel$age)
qqnorm(heart_sel$chol)
qqline(heart_sel$chol)
qqnorm(heart_sel$trtbps)
qqline(heart_sel$trtbps)

plot(heart_sel$age, heart_sel$chol, main="Age vs Colesterol",
     xlab="Age", ylab="Colesterol in blood", pch=19)
abline(lm(chol ~ age, data = heart_sel), col = "blue")

cor.test(heart_sel$age, heart_sel$chol) # crec que aquest as default fa Pearson però no sé si és correcte si no són normals


plot(heart_sel$age, heart_sel$trtbps, main="Age vs Colesterol",
     xlab="Age", ylab="Resting blood pressure", pch=19)
abline(lm(trtbps ~ age, data = heart_sel), col = "blue")

cor.test(heart_sel$age, heart_sel$trtbps) # crec que aquest as default fa Pearson però no sé si és correcte si no són normals


# 3.- Compar si homes i dones tenen un nivell de colesterio i pressió arterial igual o diferent. 
# Previament verificar si l'edat d'homes i dones d'aquesta cohort es comparable. 

nrow(heart_female) # assumim normalitat per el TLC?
nrow(heart_male)

var.test(heart_female$age, heart_male$age) # El resultat del test no mostra diferències significatives entre variàncies. Per tant, aplicarem un test de dues
# mostres independents sobre la mitjana amb variàncies desconegudes iguals.

var.test(heart_female$age, heart_male$chol) # El resultat del test mostra diferències significatives entre variàncies. Per tant, aplicarem un test de dues
# mostres independents sobre la mitjana amb variàncies desconegudes diferents.
var.test(heart_female$age, heart_male$trtbps) # El resultat del test mostra diferències significatives entre variàncies. Per tant, aplicarem un test de dues
# mostres independents sobre la mitjana amb variàncies desconegudes diferents.

# És un test de dues mostres sobre la mitjana amb variàncies desconegudes. Pel teorema del límit central,
# podem assumir normalitat. Comprovem igualtat de variàncies:


boxplot(heart_sel$age ~ heart_sel$sex, main="Age vs gender",
        xlab="Gender: 0= Female, 1 = Male", ylab="Age")

t.test( heart_female$age, heart_male$age, alternative="two.sided", conf.level=0.95, var.equal=TRUE)


boxplot(heart_sel$chol ~ heart_sel$sex, main="Colesterol vs gender",
        xlab="Gender: 0= Female, 1 = Male", ylab="Colesterol level in blood")

t.test( heart_female$chol, heart_male$chol, alternative="two.sided", conf.level=0.95, var.equal=FALSE)

boxplot(heart_sel$trtbps ~ heart_sel$sex, main="Blood resting pressure vs gender",
        xlab="Gender: 0= Female, 1 = Male", ylab="Blood resting pressure")

t.test( heart_female$trtbps, heart_male$trtbps, alternative="two.sided", conf.level=0.95, var.equal=FALSE)
