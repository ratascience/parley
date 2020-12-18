source(here::here("code", "code_01_libraries.R"))

# TRÁMITE LEGISLATIVO -----------------------------------------------------

#    Periodos Legislativos ---------------------------------------------------

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

periodos_legis %>% write_rds(here("data", "data_01_periodos-legis.rds"))

#    Legislatura Actual ------------------------------------------------------

legislatura_actual_url <- "http://opendata.congreso.cl/wscamaradiputados.asmx/getLegislaturaActual"

legislatura_actual <- legislatura_actual_url %>% read_xml
legislatura_actual <- legislatura_actual %>% as_list
legislatura_actual <- legislatura_actual %>% transpose
legislatura_actual <- legislatura_actual %>% as_tibble
legislatura_actual <- legislatura_actual %>% set_colnames(legislatura_actual %>% colnames %>% make_clean_names)
legislatura_actual <- legislatura_actual %>% unnest(everything())
legislatura_actual <- legislatura_actual %>% unnest(everything())
legislatura_actual <- legislatura_actual %>% mutate(across(starts_with("fecha"), as.Date))

legislatura_actual %>% write_rds(here("data", "data_02_legislatura_actual.rds"))

#    Legislaturas ------------------------------------------------------------

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

legislaturas %>% write_rds(here("data", "data_03_legislaturas.rds"))

