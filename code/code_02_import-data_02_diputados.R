source(here::here("code", "code_01_libraries.R"))

legislaturas  <- read_rds(here("data", "data_03_legislaturas.rds"))

# CAMARA DE DIPUTADOS -----------------------------------------------------

#    Diputados Vigentes ------------------------------------------------------

diputados_url <- "http://opendata.congreso.cl/wscamaradiputados.asmx/getDiputados_Vigentes"

diputados <- diputados_url %>% read_xml
diputados <- diputados %>% as_list
diputados <- diputados %>% as_tibble
diputados <- diputados %>% rename(diputados = Diputados)
diputados <- diputados %>% mutate(diputados = diputados %>% map(discard, is_empty))
diputados <- diputados %>% mutate(diputados = diputados %>% map(as_tibble))
diputados <- diputados %>% unnest(diputados)
diputados <- diputados %>% unnest(everything())
diputados <- diputados %>% set_colnames(diputados %>% colnames %>% make_clean_names)

diputados %>% write_rds(here("data", "data_05_camara_01_diputados.rds"))

#    Sesiones de sala --------------------------------------------------------

sesiones_url <- "http://opendata.congreso.cl/wscamaradiputados.asmx/getSesiones?prmLegislaturaID="

camara_sesiones <- legislaturas %>% select(id_legis = id)
camara_sesiones <- camara_sesiones %>% mutate(sesiones = sesiones_url %>% paste0(id_legis))
camara_sesiones <- camara_sesiones %>% mutate(sesiones = sesiones %>% map(read_xml))
camara_sesiones <- camara_sesiones %>% mutate(sesiones = sesiones %>% map(as_list))
camara_sesiones <- camara_sesiones %>% mutate(sesiones = sesiones %>% map(as_tibble))
camara_sesiones <- camara_sesiones %>% unnest(sesiones)
camara_sesiones <- camara_sesiones %>% rename(sesiones = Sesiones)
camara_sesiones <- camara_sesiones %>% mutate(sesiones = sesiones %>% map(as_tibble))
camara_sesiones <- camara_sesiones %>% mutate(sesiones = sesiones %>% map(~.x %>% set_colnames(.x %>% colnames %>% make_clean_names())))
camara_sesiones <- camara_sesiones %>% mutate(sesiones = sesiones %>% map(~.x %>% set_colnames(.x %>% colnames %>% paste0("_sesion"))))
camara_sesiones <- camara_sesiones %>% unnest(sesiones)
camara_sesiones <- camara_sesiones %>% unnest(everything())
camara_sesiones <- camara_sesiones %>% rename(fecha_inicio_sesion = fecha_sesion)

camara_sesiones %>% write_rds(here("data", "data_05_camara_02_sesiones.rds"))

#    Comisiones --------------------------------------------------------------

# No operativo

