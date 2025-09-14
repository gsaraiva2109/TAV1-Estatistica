library(readxl)
library(dplyr)
library(scales)
library(tidyverse)

dados <- read_excel("C:/Users/heito/Desktop/CVLI_2009-2024.xlsx")
dados <- dados %>%
  mutate(`Idade da Vítima` = parse_number(`Idade da Vítima`))
frequencia_simples <- function(coluna) {
  resultado <- dados %>%
    count({{coluna}}, name = "freq_simples", sort = TRUE) %>%
    return(resultado)
}

frequencia_relativa <- function(coluna) {
  frequencia_relativa_bruta({{coluna}}) %>%
    mutate(freq_relativa = scales::percent(freq_relativa_bruta, accuracy = 0.1)) %>%
    select({{coluna}}, freq_relativa)
}

frequencia_relativa_bruta <- function(coluna) {
  tabela_simples <- frequencia_simples({{coluna}})
  resultado <- tabela_simples %>%
    mutate(freq_relativa_bruta = freq_simples / sum(freq_simples)) %>%
    select({{coluna}}, freq_relativa_bruta)
}

combinar_tabelas <- function(tabela_simples, tabela_relativa) {
  left_join(tabela_simples, tabela_relativa)
}

tabela_cruzada <- function(coluna_linha, coluna_coluna) {
  
  tabela_processada <- dados %>%
    filter(!is.na({{ coluna_linha }}), !is.na({{ coluna_coluna }})) %>%
    count({{ coluna_linha }}, {{ coluna_coluna }}, name = "n") %>%
    group_by({{ coluna_linha }}) %>%
    mutate(
      porcentagem = n / sum(n),
      valor_final = paste0(n, " (", scales::percent(porcentagem, accuracy = 0.1), ")")
    ) %>%
    ungroup()
  
  tabela_processada %>%
    select({{ coluna_linha }}, {{ coluna_coluna }}, valor_final) %>%
    pivot_wider(
      names_from = {{ coluna_coluna }},
      values_from = valor_final,
      values_fill = "0 (0.0%)"
    )
}

preparar_dados_grafico <- function(coluna) {
  dados %>%
    filter(!is.na({{ coluna }})) %>%
    count({{ coluna }}, name = "contagem") %>%
    mutate(
      porcentagem = contagem / sum(contagem),
      `:=`({{ coluna }}, fct_reorder({{ coluna }}, contagem, .desc = TRUE)),
      posicao_rotulo = cumsum(porcentagem) - 0.5 * porcentagem
    )
}

gerar_grafico_setor_ggplot <- function(coluna) {
  dados_grafico <- preparar_dados_grafico({{ coluna }})
  
  nome_coluna <- deparse(substitute(coluna))
  
  ggplot(dados_grafico, aes(x = "", y = porcentagem, fill = {{ coluna }})) +
    geom_col(width = 1) +
    
    coord_polar(theta = "y") +
    
    geom_text(
      aes(label = if_else(
        porcentagem >= 0.0005, 
        percent(porcentagem, accuracy = 0.1), 
        NA_character_
      ), y = posicao_rotulo),
      color = "white", size = 5, fontface = "bold"
    ) +
    
    theme_void() +
    
    labs(
      title = paste("Distribuição Percentual por", nome_coluna),
      fill = nome_coluna
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.title = element_text(size = 12, face = "bold")
    )
}

gerar_grafico_barra_ggplot <- function(coluna) {
  nome_coluna <- deparse(substitute(coluna))
  
  dados %>%
    filter(!is.na({{coluna}})) %>%
    count({{coluna}}, name = "n") %>%
    mutate({{coluna}} := fct_reorder({{coluna}}, n)) %>%
    ggplot(aes(x = {{coluna}}, y = n, fill = {{coluna}})) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(
      title = paste("Distribuição por", nome_coluna),
      x = nome_coluna,
      y = "Frequência"
    ) +
    theme_minimal()
}

gerar_grafico_linha_tempo <- function(unidade = c("mes","ano","hora")) {
  unidade <- match.arg(unidade)
  
  dados %>%
    filter(!is.na(`Data`)) %>%
    mutate(
      data = as.Date(`Data`),
      ano = format(data, "%Y"),
      mes = format(data, "%Y-%m"),
      hora = format(as.POSIXct(`Data`), "%H")
    ) %>%
    count(.data[[unidade]], name = "n") %>%
    ggplot(aes(x = .data[[unidade]], y = n, group = 1)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "steelblue", size = 2) +
    labs(
      title = paste("Ocorrências por", unidade),
      x = unidade,
      y = "Quantidade"
    ) +
    theme_minimal()
}

medidas_posicao_idade <- function() {
  dados %>%
    filter(!is.na(`Idade da Vítima`)) %>%
    summarise(
      media = mean(`Idade da Vítima`),
      mediana = median(`Idade da Vítima`),
      q1 = quantile(`Idade da Vítima`, 0.25),
      q3 = quantile(`Idade da Vítima`, 0.75)
    )
}

medidas_dispersao_idade <- function() {
  dados %>%
    filter(!is.na(`Idade da Vítima`)) %>%
    summarise(
      variancia = var(`Idade da Vítima`),
      desvio_padrao = sd(`Idade da Vítima`),
      amplitude = max(`Idade da Vítima`) - min(`Idade da Vítima`)
    )
}

gerar_boxplot_idade <- function() {
  ggplot(dados, aes(y = `Idade da Vítima`)) +
    geom_boxplot(fill = "steelblue", color = "black") +
    labs(
      title = "Boxplot da Idade da Vítima",
      y = "Idade"
    ) +
    theme_minimal()
}

print(gerar_grafico_setor_ggplot(`Meio Empregado`))
print(combinar_tabelas(frequencia_simples(Natureza), frequencia_relativa(Natureza)))
print(tabela_cruzada(`Escolaridade da Vítima`, `Raça da Vítima`))
print(gerar_grafico_barra_ggplot(`Dia da Semana`))
print(gerar_grafico_linha_tempo("mes"))
print(gerar_grafico_linha_tempo("ano"))
print(gerar_grafico_linha_tempo("hora"))


print(medidas_posicao_idade())
print(medidas_dispersao_idade())
print(gerar_boxplot_idade())

colnames(dados)