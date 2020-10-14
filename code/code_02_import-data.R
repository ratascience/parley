source(here::here("code", "code_01_libraries.R"))

# TRÁMITE LEGISLATIVO -----------------------------------------------------

# Periodos Legislativos ---------------------------------------------------

periodos_legis_url <- "http://opendata.congreso.cl/wscamaradiputados.asmx/getPeriodosLegislativos"

periodos_legis <- periodos_legis_url %>% read_xml
periodos_legis <- periodos_legis %>% as_list
periodos_legis <- periodos_legis %>% as_tibble
periodos_legis <- periodos_legis %>% set_colnames(periodos_legis %>% colnames %>% make_clean_names())
periodos_legis <- periodos_legis %>% mutate(periodos_legislativo = periodos_legislativo %>% map(as_tibble))
periodos_legis <- periodos_legis %>% unnest(periodos_legislativo)
periodos_legis <- periodos_legis %>% unnest(everything())
periodos_legis <- periodos_legis %>% set_colnames(periodos_legis %>% colnames %>% make_clean_names())
periodos_legis <- periodos_legis %>% mutate(across(starts_with("fecha"), as.Date))


# Legislatura Actual ------------------------------------------------------

legislatura_actual_url <- "http://opendata.congreso.cl/wscamaradiputados.asmx/getLegislaturaActual"

legislatura_actual <- legislatura_actual_url %>% read_xml
legislatura_actual <- legislatura_actual %>% as_list
legislatura_actual <- legislatura_actual %>% transpose
legislatura_actual <- legislatura_actual %>% as_tibble
legislatura_actual <- legislatura_actual %>% set_colnames(legislatura_actual %>% colnames %>% make_clean_names)
legislatura_actual <- legislatura_actual %>% unnest(everything())
legislatura_actual <- legislatura_actual %>% unnest(everything())
legislatura_actual <- legislatura_actual %>% mutate(across(starts_with("fecha"), as.Date))


# Legislaturas ------------------------------------------------------------

legislaturas_url <- "http://opendata.congreso.cl/wscamaradiputados.asmx/getLegislaturas"

legislaturas <- legislaturas_url %>% read_xml
legislaturas <- legislaturas %>% as_list
legislaturas <- legislaturas %>% as_tibble
legislaturas <- legislaturas %>% set_colnames(legislaturas %>% colnames %>% make_clean_names())
legislaturas <- legislaturas %>% mutate(legislaturas = legislaturas %>% map(as_tibble))
legislaturas <- legislaturas %>% unnest(legislaturas)
legislaturas <- legislaturas %>% unnest(everything())
legislaturas <- legislaturas %>% set_colnames(legislaturas %>% colnames %>% make_clean_names())
legislaturas <- legislaturas %>% mutate(across(starts_with("fecha"), as.Date))

# SENADO ------------------------------------------------------------------

# Senadores Vigentes ------------------------------------------------------

senadores_url <- "https://www.senado.cl/wspublico/senadores_vigentes.php"

senadores <- senadores_url %>% read_xml
senadores <- senadores %>% as_list
senadores <- senadores %>% as_tibble
senadores <- senadores %>% mutate(senadores = senadores %>% map(as_tibble))
senadores <- senadores %>% unnest(senadores)
senadores <- senadores %>% unnest(everything())
senadores <- senadores %>% set_colnames(senadores %>% colnames %>% make_clean_names)

# Sesiones de sala --------------------------------------------------------

sesiones_url <- "https://www.senado.cl/wspublico/sesiones.php?legislatura="

senado_sesiones <- legislaturas %>% select(numero)
senado_sesiones <- senado_sesiones %>% mutate(sesiones = sesiones_url %>% paste0(numero))
senado_sesiones <- senado_sesiones %>% mutate(sesiones = sesiones %>% map(read_xml))
senado_sesiones <- senado_sesiones %>% mutate(sesiones = sesiones %>% map(as_list))
senado_sesiones <- senado_sesiones %>% mutate(sesiones = sesiones %>% map(as_tibble))
senado_sesiones <- senado_sesiones %>% unnest(sesiones)
senado_sesiones <- senado_sesiones %>% mutate(sesiones = sesiones %>% map(as_tibble))
senado_sesiones <- senado_sesiones %>% mutate(sesiones = sesiones %>% map(~.x %>% set_colnames(.x %>% colnames %>% make_clean_names())))
senado_sesiones <- senado_sesiones %>% mutate(sesiones = sesiones %>% map(~.x %>% set_colnames(.x %>% colnames %>% paste0("_sesion"))))
senado_sesiones <- senado_sesiones %>% unnest(sesiones)
senado_sesiones <- senado_sesiones %>% unnest(everything())
senado_sesiones <- senado_sesiones %>% rename(fecha_inicio_sesion = fechainicio_sesion)
senado_sesiones <- senado_sesiones %>% rename(fecha_termino_sesion = fechatermino_sesion)
senado_sesiones <- senado_sesiones %>% rename(id_diario_sesion = iddiario_sesion)
senado_sesiones <- senado_sesiones %>% rename(id_sesion = sesiid_sesion)
senado_sesiones <- senado_sesiones %>% select(-numero)

# Comisiones --------------------------------------------------------------

comisiones_url <- "https://www.senado.cl/wspublico/comisiones.php"

senado_comisiones <- comisiones_url %>% read_xml
senado_comisiones <- senado_comisiones %>% as_list
senado_comisiones <- senado_comisiones %>% as_tibble
senado_comisiones <- senado_comisiones %>% mutate(comisiones = comisiones %>% map(discard, is_empty))
senado_comisiones <- senado_comisiones %>% mutate(comisiones = comisiones %>% map(as_tibble))
senado_comisiones <- senado_comisiones %>% unnest(comisiones)
senado_comisiones <- senado_comisiones %>% mutate(integrantes = integrantes %>% map(discard, is_empty))
senado_comisiones <- senado_comisiones %>% mutate(integrantes = integrantes %>% map(as_tibble))
senado_comisiones <- senado_comisiones %>% mutate(integrantes = integrantes %>% map(~.x %>% set_colnames(.x %>% colnames %>% make_clean_names)))
senado_comisiones <- senado_comisiones %>% mutate(integrantes = integrantes %>% map(~.x %>% set_colnames(.x %>% colnames %>% paste0("_parlamentario"))))
senado_comisiones <- senado_comisiones %>% unnest(everything())
senado_comisiones <- senado_comisiones %>% unnest(ends_with("parlamentario"))


# Diario de sesion --------------------------------------------------------

diarios_url <- "https://www.senado.cl/wspublico/diariosesion.php?idsesion="

senado_diarios <- tibble(id_sesion = 2643)
senado_diarios <- senado_diarios %>% mutate(diario = diarios_url %>% paste0(id_sesion))
senado_diarios <- senado_diarios %>% mutate(diario = diario %>% map(read_xml))
senado_diarios <- senado_diarios %>% mutate(diario = diario %>% map(as_list))
senado_diarios <- senado_diarios %>% mutate(diario = diario %>% map(as_tibble))
senado_diarios <- senado_diarios %>% unnest(diario)
senado_diarios


# CAMARA DE DIPUTADOS -----------------------------------------------------

# Diputados Vigentes ------------------------------------------------------

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