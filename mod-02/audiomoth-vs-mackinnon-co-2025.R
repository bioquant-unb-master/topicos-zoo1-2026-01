
# Testar primeiro com os dados exemplo do pacote inext
library(iNEXT)
data("spider")
summary(spider)
spider
str(spider)
iNEXT(spider, q=0, datatype="abundance")
#
# import csv data exportados da planilha reanalise 2021

library(readr)
#co= as.data.frame(co_2025_all)
co= as.data.frame(co_2025_all_with_zeros)
#
#calcular a curva de rarefacao mackinnon
#
dadografmack = iNEXT(co$mack, q=0, datatype = "abundance")
ggiNEXT(dadografmack, type=1, se=TRUE, facet.var="None", color.var="Both", grey=FALSE)  
dadografmack500 <- iNEXT(co$mack, q=0, datatype = "abundance", endpoint = 500)
dadografmack1000 <- iNEXT(co$mack, q=0, datatype = "abundance", endpoint = 1000)
ggiNEXT(dadografmack500, type=1, se=TRUE, facet.var="None", color.var="Both", grey=FALSE)  
ggiNEXT(dadografmack1000, type=1, se=TRUE, facet.var="None", color.var="Both", grey=FALSE)  
dadografmack1000
#
#calcular a curva de rarefacao cerradao
#
dadografcerradao = iNEXT(co$cerradao, q=0, datatype = "abundance")
ggiNEXT(dadografcerradao, type=1, se=TRUE, facet.var="None", color.var="Both", grey=FALSE)  
dadografcerradao500 <- iNEXT(co$cerradao, q=0, datatype = "abundance", endpoint = 500)
dadografcerradao1000 <- iNEXT(co$cerradao, q=0, datatype = "abundance", endpoint = 1000)
ggiNEXT(dadografcerradao500, type=1, se=TRUE, facet.var="None", color.var="Both", grey=FALSE)  
ggiNEXT(dadografcerradao1000, type=1, se=TRUE, facet.var="None", color.var="Both", grey=FALSE)  
dadografcerradao1000


#calcular a curva de rarefacao mata
#
dadografmata = iNEXT(co$mata, q=0, datatype = "abundance")
ggiNEXT(dadografmata, type=1, se=TRUE, facet.var="None", color.var="Both", grey=FALSE)  
dadografmata500 <- iNEXT(co$mata, q=0, datatype = "abundance", endpoint = 500)
dadografmata1000 <- iNEXT(co$mata, q=0, datatype = "abundance", endpoint = 1000)
ggiNEXT(dadografmata500, type=1, se=TRUE, facet.var="None", color.var="Both", grey=FALSE)  
ggiNEXT(dadografmata1000, type=1, se=TRUE, facet.var="None", color.var="Both", grey=FALSE)  
dadografmata1000

#
#calcular riqueza estimada e diversidade de especies entre os habitats
#

iNEXT(co, q=0, datatype="abundance")
dadografco = iNEXT(co, q=0, datatype="abundance")
ggiNEXT(dadografco, type=1, se=TRUE, facet.var="None", color.var="Both", grey=FALSE)
#
