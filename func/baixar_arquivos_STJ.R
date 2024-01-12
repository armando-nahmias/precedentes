
baixar.info.STJ <- function() {
  
  # Definir os URLs base e os caminhos locais para salvar os arquivos
  base.url <- 'https://dadosabertos.web.stj.jus.br/dataset/4238da2f-c07b-4c1a-b345-4402accacdcf/resource/'
  
  recursos <- c(
    'd5e50514-6dba-4f1e-8557-94f135eae03b/download/',
    'df29da13-7d6b-41ba-ad96-cd1a5bbd191c/download/',
    '162e58f0-01c1-4d91-94a4-664b4de81e79/download/',
    '7ed21202-0049-4fcb-aa7c-48d810d3c499/download/'
  )
  
  informacoes <- c(
    'dicionario-temas.csv',
    'temas.csv',
    'dicionario-processos.csv',
    'processos.csv'
  )
  
  lista.df <- list()
  
  # Loop para baixar as informacoes
  for (i in seq_along(informacoes)) {
    informacao <- informacoes[i]
    url.completa <- paste0(base.url, recursos[i], informacao)
    arquivo <- paste0('dados/', informacao)
    
    if (file.exists(arquivo)) {
      consultar <- FALSE
      info.arquivo <- file.info(arquivo)
      modificacao.arquivo <- format(info.arquivo$mtime, '%Y-%m-%d')
      if (Sys.Date() > modificacao.arquivo) {
        consultar <- TRUE
        cat(epoxy::epoxy('Precisa atualizar o arquivo {informacao}.\n\n\n'))
      } else {
        cat(epoxy::epoxy('Não precisa atualizar o arquivo {informacao}.\n\n\n'))
      }
    } else {
      consultar <- TRUE
      cat(epoxy::epoxy('Precisa atualizar o arquivo {informacao}.\n\n\n'))
    }
    
    if (consultar) {
      
      arquivo.novo <- paste0(arquivo, '.novo')
      
      try({
        download.file(url.completa, destfile = arquivo.novo, mode = 'wb')
        # Ler o arquivo CSV e armazenar no dataframe
        df.novo <- readr::read_csv2(arquivo.novo)
        df.anterior <- readr::read_csv2(arquivo)
        if (identical(df.novo, df.anterior)) {
          atualizar <<- FALSE
          cat(epoxy::epoxy('Arquivo {informacao} baixado é igual ao anterior.\n\n\n'))
          df <- df.anterior
        } else {
          atualizar <<- TRUE
          diffdf::diffdf(df.novo, df.anterior)
          file.rename(arquivo, paste0(arquivo, '.antigo'))
          file.rename(arquivo.novo, arquivo)
          cat(epoxy::epoxy('Arquivo {informacao} anterior substituído pelo baixado.\n\n\n'))
          df <- df.novo
        }
      })
      
    } else {
      cat(epoxy::epoxy('Arquivo {informacao} atualizado.\n\n\n'))
      # Ler o arquivo CSV e armazenar no dataframe
      df <- readr::read_csv2(arquivo)
    }
    
    
    # Adicionar o dataframe à lista
    lista.df[[informacao]] <- df
  }
  
  
  # Gravar em disco o resultado da consulta
  saveRDS(lista.df, file = 'dados/dados-completos.rds')
  
}
