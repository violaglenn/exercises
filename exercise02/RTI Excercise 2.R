library(XML)

#Load main XML file and convert to data.frame
aviation.xml <- xmlParse("C:\\Users\\Viola\\Documents\\GitHub\\exercises\\exercise02\\data\\AviationData.xml")
aviation <- xmlToList(aviation.xml)
aviation <- as.data.frame(t(aviation))

#Basic import QA checks

char.vars <- c("EventId","InvestigationType","AccidentNumber", "Location", 
               "Country", "AirportCode", "AirportName", "InjurySeverity",
               "AircraftDamage", "AircraftCategory", "RegistrationNumber", "Make",
               "Model", "AmateurBuilt", "EngineType", "FARDescription","Schedule",              
               "PurposeofFlight", "AirCarrier", "WeatherCondition",
               "BroadPhaseofFlight", "ReportStatus")