source(here::here("code", "code_01_libraries.R"))

legislaturas  <- read_rds(here("data", "data_03_legislaturas.rds"))

# SENADO ------------------------------------------------------------------

#    Senadores Vigentes ------------------------------------------------------

senadores_url <- "https://www.senado.cl/wspublico/senadores_vigentes.php"

senadores <- senadores_url %>% read_xml
senadores <- senadores %>% as_list
senadores <- senadores %>% as_tibble
senadores <- senadores %>% mutate(senadores = senadores %>% map(as_tibble))
senadores <- senadores %>% unnest(senadores)
senadores <- senadores %>% unnest(everything())
senadores <- senadores %>% set_colnames(senadores %>% colnames %>% make_clean_names)

senadores %>% write_rds(here("data", "data_04_senadores.rds"))

#    Sesiones de sala --------------------------------------------------------

sesiones_url <- "https://www.senado.cl/wspublico/sesiones.php?legislatura="

senado_sesiones <- legislaturas %>% select(numero_legis = numero)
senado_sesiones <- senado_sesiones %>% mutate(sesiones = sesiones_url %>% paste0(numero_legis))
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

senado_sesiones %>% write_rds(here("data", "data_05_senado_sesiones.rds"))

#    Comisiones --------------------------------------------------------------

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

senado_comisiones %>% write_rds(here("data", "data_06_senado_comisiones.rds"))

#    Votaciones por Boletin de Sesión ---------------------------------------

votaciones_boletin_url <- "https://www.senado.cl/wspublico/votaciones.php?boletin="

senado_votaciones <- senado_sesiones
senado_votaciones <- senado_votaciones %>% select(id_sesion)
senado_votaciones <- senado_votaciones %>% mutate(votaciones = votaciones_boletin_url %>% paste0(id_sesion))
senado_votaciones <- senado_votaciones %>% mutate(votaciones = votaciones %>% map(read_xml))
senado_votaciones <- senado_votaciones %>% mutate(votaciones = votaciones %>% map(as_list))
senado_votaciones <- senado_votaciones %>% mutate(votaciones = votaciones %>% map(as_tibble))
senado_votaciones <- senado_votaciones %>% unnest(votaciones)
senado_votaciones <- senado_votaciones %>% mutate(votaciones = votaciones %>% map(as_tibble))
senado_votaciones <- senado_votaciones %>% mutate(votaciones = votaciones %>% map(~.x %>% set_colnames(.x %>% colnames %>% make_clean_names)))
senado_votaciones <- senado_votaciones %>% unnest(votaciones)
senado_votaciones <- senado_votaciones %>% mutate(detalle_votacion = detalle_votacion %>% map(as_tibble))
senado_votaciones <- senado_votaciones %>% mutate(detalle_votacion = detalle_votacion %>% map(~.x %>% set_colnames(.x %>% colnames %>% make_clean_names)))
senado_votaciones <- senado_votaciones %>% unnest(detalle_votacion)
senado_votaciones <- senado_votaciones %>% unnest(everything())
senado_votaciones <- senado_votaciones %>% rename(tipo_votacion = tipovotacion)

senado_votaciones %>% write_rds(here("data", "data_07_senado_votaciones.rds"))


# Diario de Sesión --------------------------------------------------------

diario_sesion_url <- "https://www.senado.cl/wspublico/diariosesion.php?idsesion="

senado_diarios <- senado_sesiones
senado_diarios <- senado_diarios %>% select(id_sesion)
senado_diarios <- senado_diarios %>% mutate(diarios = diario_sesion_url %>% paste0(id_sesion))
senado_diarios <- senado_diarios %>% mutate(diarios = diarios %>% map(~try(read_xml(.x))))
senado_diarios <- senado_diarios %>% mutate(clases  = diarios %>% map(class))
senado_diarios <- senado_diarios %>% unnest(clases)
senado_diarios <- senado_diarios %>% filter(clases %>% str_detect("try-error") %>% not)
senado_diarios <- senado_diarios %>% group_by(id_sesion, diarios)
senado_diarios <- senado_diarios %>% nest
senado_diarios <- senado_diarios %>% select(-data)
senado_diarios <- senado_diarios %>% mutate(diarios = diarios %>% map(~as_list(.x)))
senado_diarios <- senado_diarios %>% mutate(diarios = diarios %>% map(~as_tibble(.x)))
senado_diarios <- senado_diarios %>% mutate(diarios = diarios %>% map(~mutate(.x, nombres = Diario %>% names)))
senado_diarios <- senado_diarios %>% unnest(diarios)
senado_diarios <- senado_diarios %>% rename(diarios = Diario)

senado_diarios %>% write_rds(here("data", "data_08_senado_diarios.rds"))

senado_cuentas <- senado_diarios %>% filter(nombres %>% equals("Cuenta"))
senado_cuentas <- senado_cuentas %>% unnest(diarios)
senado_cuentas <- senado_cuentas %>% mutate(diarios = diarios %>% map(as.list))
senado_cuentas <- senado_cuentas %>% unnest(diarios)
senado_cuentas <- senado_cuentas %>% mutate(diarios = diarios %>% map(as.list))
senado_cuentas <- senado_cuentas %>% unnest(diarios)
senado_cuentas <- senado_cuentas %>% unnest(diarios)

senado_cuentas %>% write_rds(here("data", "data_09_senado_cuentas.rds"))
senado_ordenes <- senado_diarios %>% filter(nombres %>% equals("OrdenDeldia"))
