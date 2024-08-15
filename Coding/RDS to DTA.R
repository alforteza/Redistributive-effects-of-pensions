library(foreign)

ifelse(Sys.getenv("USERDOMAIN")=="DIEGO-TUZMAN0",muestra_HL <- readRDS("C:/Users/Usuario/OneDrive/Documentos/Doctorado/Pensions/Data/Inputs/Bases BPS recibidas 2022/Muestra HL/muestra_HL.RDS"),muestra_HL <- readRDS("/Users/alvaroforteza/Documents/Pensions/Data/Inputs/Bases BPS recibidas 2022/Muestra HL/muestra_HL.RDS"))

muestra_HL<- muestra_HL[,-c(36,38,40,41)]

ifelse(Sys.getenv("USERDOMAIN")=="DIEGO-TUZMAN0",write.dta(muestra_HL,file="C:/Users/Usuario/OneDrive/Documentos/Doctorado/Pensions/Data/Intermediate/BPS data received 2022/muestra_HL.dta"),write.dta(muestra_HL,file="/Users/alvaroforteza/Documents/Pensions/Data/Intermediate/BPS data received 2022/muestra_HL.dta"))

