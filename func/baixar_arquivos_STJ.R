baixar.arquivo.STJ <- function() {

  # Definir os URLs base e os caminhos locais para salvar os arquivos
  base.url <- "https://dadosabertos.web.stj.jus.br/dataset/4238da2f-c07b-4c1a-b345-4402accacdcf/resource/"
  
  recursos <- c(
    'd5e50514-6dba-4f1e-8557-94f135eae03b/download/',
    'df29da13-7d6b-41ba-ad96-cd1a5bbd191c/download/',
    '162e58f0-01c1-4d91-94a4-664b4de81e79/download/',
    '7ed21202-0049-4fcb-aa7c-48d810d3c499/download/'
  )
  
  arquivos <- c(
    'dicionario-temas.csv',
    'temas.csv',
    'dicionario-processos.csv',
    'processos.csv'
  )
  
  lista.df <- list()
  
  # Loop para baixar os arquivos
  for (i in seq_along(arquivos)) {
    arquivo <- arquivos[i]
    url.completa <- paste0(base.url, recursos[i], arquivo)
    destino <- paste0('dados/', arquivo)
    
    # Baixar o arquivo
    download.file(url.completa, destfile = destino, mode = "wb")
    
    # Ler o arquivo CSV e armazenar no dataframe
    df <- readr::read_csv2(destino)
    
    # Adicionar o dataframe à lista
    lista.df[[arquivo]] <- df
  }
  
  
  # Gravar em disco o resultado da consulta
  saveRDS(lista.df, file = 'dados/dados-completos.rds')
  
}