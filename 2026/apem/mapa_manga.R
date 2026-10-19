# Mapa de la producción de mango en Brasil (2025) + tabla en español
# Fronteras: IBGE (paquete geobr). Puntos: centroides oficiales de los municipios.
# Salida: imgs/mapa_manga_es.png (fondo transparente)
#
# Ejecutar desde la carpeta apem:  Rscript mapa_manga.R

suppressPackageStartupMessages({
  library(geobr)
  library(sf)
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
  library(dplyr)
})

sf_use_s2(FALSE)

# ---------------------------------------------------------------------------
# 1. Fronteras oficiales de los estados (IBGE)
# ---------------------------------------------------------------------------
estados <- read_state(year = 2020, showProgress = FALSE)
stopifnot(nrow(estados) == 27)

top4 <- c("BA", "PE", "SP", "MG")
estados$destaque <- estados$abbrev_state %in% top4

# ---------------------------------------------------------------------------
# 2. Puntos: centroides oficiales de los municipios
#    Vale del São Francisco = punto medio entre Petrolina (PE) y Juazeiro (BA)
# ---------------------------------------------------------------------------
munis <- bind_rows(
  read_municipality(code_muni = "PE", year = 2020, showProgress = FALSE),
  read_municipality(code_muni = "BA", year = 2020, showProgress = FALSE),
  read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE),
  read_municipality(code_muni = "SP", year = 2020, showProgress = FALSE)
)

centro <- function(nome, uf) {
  m <- munis[munis$name_muni == nome & munis$abbrev_state == uf, ]
  stopifnot(nrow(m) == 1)
  st_coordinates(st_centroid(st_geometry(m)))[1, ]
}

pet <- centro("Petrolina", "PE")
jua <- centro("Juazeiro", "BA")
liv <- centro("Livramento de Nossa Senhora", "BA")
jai <- centro("Jaíba", "MG")
mon <- centro("Monte Alto", "SP")

pontos <- data.frame(
  id = 1:4,
  nombre = c("Valle del São Francisco (Petrolina/Juazeiro)",
             "Livramento de Nossa Senhora (centro-sur de Bahía)",
             "Jaíba (norte de Minas Gerais)",
             "Monte Alto (São Paulo)"),
  x = c(mean(c(pet[1], jua[1])), liv[1], jai[1], mon[1]),
  y = c(mean(c(pet[2], jua[2])), liv[2], jai[2], mon[2])
)
print(pontos[, c("id", "nombre", "x", "y")])

# Siglas de los 4 estados destacados, en posiciones fijas (lejos de los puntos)
rot_df <- data.frame(
  sigla = c("PE", "BA", "MG", "SP"),
  X = c(-37.6, -43.2, -45.2, -49.6),
  Y = c(-8.25, -11.3, -18.6, -23.4)
)

# ---------------------------------------------------------------------------
# 3. Mapa
# ---------------------------------------------------------------------------
COR_DESTAQUE <- "#FFA500"
COR_PONTO    <- "#b22200"

p_mapa <- ggplot() +
  geom_sf(data = estados, aes(fill = destaque), colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = c(`TRUE` = COR_DESTAQUE, `FALSE` = "#cfcfcf"), guide = "none") +
  geom_text(data = rot_df, aes(X, Y, label = sigla),
            fontface = "bold", size = 7, colour = "#1c1c1c") +
  geom_point(data = pontos, aes(x, y), shape = 21, size = 9,
             fill = COR_PONTO, colour = "white", stroke = 1.4) +
  geom_text(data = pontos, aes(x, y, label = id),
            colour = "white", fontface = "bold", size = 6) +
  coord_sf(xlim = c(-74, -33), ylim = c(-34, 5.5), expand = FALSE) +
  theme_void() +
  theme(plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))

# ---------------------------------------------------------------------------
# 4. Tabla (en español). Datos tal como se proporcionaron (PAM/IBGE 2025).
# ---------------------------------------------------------------------------
tab <- data.frame(
  estado = c("Bahía", "Pernambuco", "São Paulo", "Minas Gerais", "Brasil (total)"),
  area   = c("37,5", "23,3", "9,5", "6,7", "88,7"),
  # espacio fino (U+202F) como separador de miles: no se confunde con un decimal
  prod   = c("867 170", "609 940", "176 880", "111 721", "1,91 millones"),
  stringsAsFactors = FALSE
)
n <- nrow(tab)
tab$y <- seq(n, 1, by = -1) * 1.0

x_estado <- 0.0; x_area <- 5.2; x_prod <- 8.6   # columnas (alineadas a la derecha en area y prod)
y_head <- n + 1.5

p_tab <- ggplot() +
  # encabezado
  annotate("text", x = x_estado, y = y_head, label = "Estado", hjust = 0,
           fontface = "bold", size = 8) +
  annotate("text", x = x_area, y = y_head, label = "Superficie\n(miles de ha)", hjust = 1, lineheight = 0.95,
           fontface = "bold", size = 8) +
  annotate("text", x = x_prod, y = y_head, label = "Producción\n(toneladas)", hjust = 1, lineheight = 0.95,
           fontface = "bold", size = 8) +
  annotate("segment", x = -0.1, xend = x_prod + 0.1, y = y_head - 0.85, yend = y_head - 0.85,
           linewidth = 1.2, colour = "#1c1c1c") +
  # filas
  geom_text(data = tab, aes(x = x_estado, y = y, label = estado), hjust = 0, size = 8,
            fontface = ifelse(tab$estado == "Brasil (total)", "bold", "plain")) +
  geom_text(data = tab, aes(x = x_area, y = y, label = area), hjust = 1, size = 8,
            fontface = ifelse(tab$estado == "Brasil (total)", "bold", "plain")) +
  geom_text(data = tab, aes(x = x_prod, y = y, label = prod), hjust = 1, size = 8,
            fontface = ifelse(tab$estado == "Brasil (total)", "bold", "plain")) +
  annotate("segment", x = -0.1, xend = x_prod + 0.1, y = 1.5, yend = 1.5,
           linewidth = 0.6, colour = "#555555") +
  # notas regionales
  annotate("text", x = x_estado, y = 0.1, hjust = 0, vjust = 1, size = 7, lineheight = 1.1,
           label = paste0("Nordeste: 70,6 mil ha (79,6% del total)\n",
                          "Sudeste: 17,1 mil ha (19,2% del total)\n",
                          "Otros estados: Ceará (4,1 mil ha) y\nRio Grande do Norte (2,4 mil ha)")) +
  # leyenda de los puntos del mapa
  annotate("text", x = x_estado, y = -2.85, hjust = 0, vjust = 1, size = 7, lineheight = 1.15,
           fontface = "bold", colour = COR_PONTO, label = "Zonas productoras (ubicación en el mapa):") +
  annotate("text", x = x_estado, y = -3.5, hjust = 0, vjust = 1, size = 7, lineheight = 1.15,
           label = paste0("1  ", pontos$nombre[1], "\n",
                          "2  ", pontos$nombre[2], "\n",
                          "3  ", pontos$nombre[3], "\n",
                          "4  ", pontos$nombre[4])) +
  coord_cartesian(xlim = c(-0.2, x_prod + 0.3), ylim = c(-7.2, y_head + 0.6), expand = FALSE) +
  theme_void() +
  theme(plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))

# ---------------------------------------------------------------------------
# 5. Composición y guardado (fondo transparente)
# ---------------------------------------------------------------------------
fig <- (p_mapa | p_tab) +
  plot_layout(widths = c(1.15, 1)) +
  plot_annotation(
    caption = "Fuente: PAM/IBGE (2026), reprocesado por el Observatorio de Mercado de Mango de Embrapa. Fronteras: IBGE.",
    theme = theme(
      plot.caption = element_text(size = 16, hjust = 0),
      plot.background = element_rect(fill = "transparent", colour = NA)
    )
  ) &
  theme(plot.background = element_rect(fill = "transparent", colour = NA))

dir.create("imgs", showWarnings = FALSE)
ggsave("imgs/mapa_manga_es.png", fig, width = 16, height = 7.6, dpi = 200, bg = "transparent")
cat("Listo: imgs/mapa_manga_es.png\n")
