
install.packages("Hmisc")
install.packages("RColorBrewer")
library("RColorBrewer")
library("Hmisc")
f = file.choose()
cereal = read.csv(f)
cereal 
attach(cereal)

hist(calories)
mean(calories)
median(calories)

barplot(table(mfr), col=brewer.pal(n = 7, name = "Dark2")) #qualitative, no mean/median
barplot(table(type), col=brewer.pal(n = 7, name = "Dark2"))
hist(protein, col=brewer.pal(n = 11, name = "Spectral"))
hist(fat, col=brewer.pal(n = 11, name = "Spectral"))
hist(sodium, col=brewer.pal(n = 11, name = "Spectral"))
hist(fiber, col=brewer.pal(n = 11, name = "Spectral"))
hist(sugars, col=brewer.pal(n = 11, name = "Spectral"))
hist(potass, col=brewer.pal(n = 11, name = "Spectral"))
hist(vitamins, col=brewer.pal(n = 11, name = "Spectral"))
hist(shelf, col=brewer.pal(n = 11, name = "Spectral"))
hist(weight, col=brewer.pal(n = 11, name = "Spectral"))
hist(cups, col=brewer.pal(n = 11, name = "Spectral"))
hist(rating, col=brewer.pal(n = 11, name = "Spectral"))
barplot(table(vitamins), col=brewer.pal(n = 7, name = "Dark2"))
barplot(table(shelf), col=brewer.pal(n = 7, name = "Dark2"))

manafac = aov(calories~mfr)
summary(manafac)

typ = aov(calories~type)
summary(typ)

vit = aov(calories~vitamins)
summary(vit)

shel = aov(calories~shelf)
summary(shel)

TukeyHSD(manafac)
TukeyHSD(vit)

mfrtyp = aov(rating~mfr+type+mfr*type)
summary(mfrtyp)
boxplot(rating~mfr+type)

mfrvit = aov(rating~mfr+vitamins+mfr*vitamins)
summary(mfrvit)
boxplot(rating~mfr+vitamins)

mfrshelf = aov(rating~mfr+shelf+mfr*shelf)
summary(mfrshelf)
boxplot(rating~mfr+shelf)

typvit = aov(rating~type+vitamins+type*vitamins)
summary(typvit)
boxplot(rating~type+vitamins)

typshel = aov(rating~type+shelf+type*shelf)
summary(typshel)
boxplot(rating~type+shelf)

vitshel = aov(rating~vitamins+shelf+vitamins*shelf)
summary(vitshel)
boxplot(rating~vitamins+shelf)

pairs(cereal[,4:11])
cor(cereal[,4:11], use='pairwise.complete.obs')