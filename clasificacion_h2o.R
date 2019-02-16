## Libreria
library(tidyverse)
library(MicroDatosEs)
library(h2o)


##  cargar datos

# fpath_dropbox

# enlace_dropbox <- "https://www.dropbox.com/s/h8am8g2yk3dq1y2/md_EPA_2018T4.txt?dl=0"


fpath <- "~/Dropbox/Public/datos_4t18/md_EPA_2018T4.txt"

epa <- epa2005(fpath)
names(epa) <- tolower(names(epa))
epa <- subset(epa, select = c(prov, edad, nforma, aoi))

recodificacion <- function(dat) {
  dat$aoi[grepl("^Inactivos", dat$aoi)] <- "i"
  dat$aoi[grepl("[O-o]cupados", dat$aoi)] <- "o"
  dat$aoi[grepl("^Parados", dat$aoi)] <- "p"

  dat$aoi <- factor(dat$aoi)
  dat$nforma3 <- dat$nforma
  dat$nforma3[dat$nforma == "Analfabetos" |
    dat$nforma == "Educación primaria" |
    dat$nforma == "Educación primaria incompleta"] <-
    "Est primarios o menos"
  dat$nforma3[dat$nforma == "Educación superior"] <-
    "Est. Universitarios"
  dat$nforma3[dat$nforma == "Primera etapa de educación secundaria" |
    dat$nforma == "Segunda etapa de educación secundaria, orientación general" |
    dat$nforma == "Segunda etapa de educación secundaria, orientación profesional"] <-
    "Est. Secundarios"

  dat$nforma3 <- factor(dat$nforma3)

  dat$gedad <- dat$edad
  dat$gedad[dat$edad == "de 0 A 4 años" |
    dat$edad == "de 5 A 9 años" |
    dat$edad == "de 10 A 15 años"] <- "15 años o menos "
  dat$gedad[dat$edad == "de 16 A 19 años" |
    dat$edad == "de 20 A 24 años" |
    dat$edad == "de 25 A 29 años" |
    dat$edad == "de 30 A 34 años"] <- "De 16 a 34"

  dat$gedad[dat$edad == "de 35 A 39 años" |
    dat$edad == "de 40 A 44 años" |
    dat$edad == "de 45 A 49 años" |
    dat$edad == "de 50 A 54 años"] <- "De 35 a 54"

  dat$gedad[dat$edad == "de 55 A 59 años" |
    dat$edad == "de 60 A 64 años" |
    dat$edad == "65 o más años"] <- "55 o más"


  dat$gedad <-
    factor(dat$gedad,
      levels = c("15 años o menos ", "De 16 a 34", "De 35 a 54", "55 o más")
    )

  dat
}

epa <- recodificacion(epa)

# eliminar menores de 16 años  
epa <- epa[epa$gedad != "15 años o menos ", ]

# eliminar inactivos
epa <- epa[epa$aoi != "i", ]

epa$parado <- as.factor(ifelse(epa$aoi == "p", 1, 0))
epa <- epa[, c("parado","gedad","nforma3","prov")]
head(epa)


## Iniciamos h2o

h2o.init(nthreads = 4, max_mem_size = "7g")

epa_hex <-  as.h2o(epa, destination_frame = "epa_hframe")
epa_hex[,"parado"] <- h2o.asfactor(epa_hex[,"parado"])
epa_hex[,"prov"] <- h2o.asfactor(epa_hex[,"prov"])

partition <- h2o.splitFrame(epa_hex, ratios = c(0.6), seed = 13)
write_csv(as.data.frame(partition[[2]]), path = "epa_test.csv")


# ajustamos un tan injustamente denostado glm
x = c("gedad","nforma3","prov")
y = "parado"

mod_glm <- h2o.glm(
  model_id = "epa_glm",
  family = "binomial",
  nfolds = 5,
  x = x,
  y = y,
  training_frame = partition[[1]])

mod_gbm <- h2o.gbm(
  model_id = "epa_gbm",
  nfolds = 5,
  ntrees = 50,
  max_depth = 4,
  # family = "binomial",
  x = x,
  y = y,
  training_frame = partition[[1]])


summary(mod_glm)

mod_glm %>% h2o.performance(newdata = partition[[2]])
mod_glm %>% h2o.performance(newdata = partition[[2]]) %>% h2o.auc()
mod_gbm %>% h2o.performance(newdata = partition[[2]]) %>% h2o.auc()

mod_glm %>% predict(newdata= partition[[2]])


h2o.download_mojo(mod_glm, get_genmodel_jar = TRUE,
                  genmodel_name = "mojo_jar.jar")

h2o.shutdown(prompt = FALSE)
