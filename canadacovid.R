#install.packages("canadacovid")
library(canadacovid)


# Apartado 3 --------------------------------------------------------------

#A través de la librería canadacovid genera una query que permita conocer quien 
#ha sido la provincia con mayor tasa de contagios y con mayor tasa de vacunación
#diaria en los últimos seis meses
hoy<-Sys.Date()
hoy

lalala<-get_reports("province",
                    after = 2022-01-01,
                    before = Sys.Date())

poblacion<-get_provinces()%>%
  select(province=code, population)

union<-full_join(lalala, poblacion)

lelele<-union%>%
  group_by(province)%>%
  summarise(tasaCasos = ((mean(change_cases))/(mean(population)))*100)%>%
  arrange(desc(tasaCasos))%>%
  head(1)

lilili<-union%>%
  group_by(province)%>%
  summarise(tasaVac = ((mean(change_vaccinations))/(mean(population)))*100)%>%
  arrange(desc(tasaVac))%>%
  head(1)

