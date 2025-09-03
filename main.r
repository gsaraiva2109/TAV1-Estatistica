library(readxl)
library(dplyr)
library(scales)
library(tidyverse)

dados <- read_excel("~/.r/TAV1-Estatistica/CVLI_2009-2024.xlsx")

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

print(gerar_grafico_setor_ggplot(`Meio Empregado`))
print(combinar_tabelas(frequencia_simples(Natureza), frequencia_relativa(Natureza)))
print(tabela_cruzada(`Escolaridade da Vítima`, `Raça da Vítima`))