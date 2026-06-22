library(readxl)

#Vamos fazer a organização e sistematização dos dados
consolidado <- as.data.frame(ibge_recor_microclima_ponte_corujao_26maio2026)
saveRDS(consolidado, file="consolidado1.rds")
save(consolidado, file="consolidado2.RData")

consolidado$habitat = as.factor(consolidado$habitat)
consolidado$ponto = as.factor(consolidado$ponto)
consolidado$altura = as.factor(consolidado$altura)

boxplot(temp ~ habitat, data=consolidado, xlab="habitat", ylab="temperatura", outline=FALSE)
boxplot(umidrel ~ habitat, data=consolidado, xlab="habitat", ylab="Umidade Relativa %", outline=FALSE)
ggplot(consolidado, aes(x=habitat,y=temp, fill = altura))+ geom_boxplot()
ggplot(consolidado, aes(x=habitat,y=umidrel, fill = altura))+ geom_boxplot()
shapiro.test(consolidado$temp)
shapiro.test(consolidado$umidrel)
library(car)
leveneTest(consolidado$temp, group=consolidado$habitat)
leveneTest(consolidado$umidrel, group=consolidado$habitat)

# Compute the analysis of variance
res.aov <- aov(umidrel ~ habitat, data = consolidado)
# Summary of the analysis
summary(res.aov)
shapiro.test(res.aov$residuals)
TukeyHSD(res.aov)
boxplot(umidrel ~ habitat, data=consolidado, xlab="habitat", ylab="Umidade Relativa %", outline=FALSE)
