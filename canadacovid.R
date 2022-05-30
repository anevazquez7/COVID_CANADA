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

# Apartado 6 --------------------------------------------------------------

#Automatiza la consulta generada en el paso 3 para obtener una función a la 
#que informes de una fecha y te devuelva la provincia con más casos de covid y 
#la provincia con mayor tasa de vacunación diaria en los 3 meses anteriores a 
#dicha fecha

if (month(fecha)-3>=1){
  fecha_3antes<-paste(year(fecha),"-",month(fecha)-3, "-", day(fecha))
  fecha_3antes<-gsub(" ", "", fecha_3antes)
  fecha_3antes<-as.Date(fecha_3antes)
}else{
  fecha_3antes<-paste(year(fecha)-1,"-",month(fecha)-3+12, "-", day(fecha))
  fecha_3antes<-gsub(" ", "", fecha_3antes)
  fecha_3antes<-as.Date(fecha_3antes)
}


superLOLO<-function(fecha){
  fecha<-as.Date(fecha)
  if (month(fecha)-3>=1){
    fecha_3antes<-paste(year(fecha),"-",month(fecha)-3, "-", day(fecha))
    fecha_3antes<-gsub(" ", "", fecha_3antes)
    fecha_3antes<-as.Date(fecha_3antes)
  }else{
    fecha_3antes<-paste(year(fecha)-1,"-",month(fecha)-3+12, "-", day(fecha))
    fecha_3antes<-gsub(" ", "", fecha_3antes)
    fecha_3antes<-as.Date(fecha_3antes)
  }
  
  lalala<-get_reports("province",
                      after = fecha_3antes,
                      before = fecha)
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
  print(paste("la provincia con mas casos en los ultimos tres meses es",
              lelele$province,"con", round(lelele$tasaCasos,2),
              "y con mas tasa de vacunacion",
              lilili$province, "con", round(lilili$tasaVac,2)))
  
}
superLOLO("2021-03-30") #ejemplo

