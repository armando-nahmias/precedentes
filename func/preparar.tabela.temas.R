preparar.tabela.temas <- function() {
  df <- read.csv2('dados/temas.csv', sep = ';', header = T, colClasses = 'character', fileEncoding = 'Windows-1252')
  
  df <- tibble::tibble(df)
  
  # Lista de colunas que precisam ser convertidas para data
  colunas.data <- c("dataPrimeiraAfetacao", "dataJulgamento", "dataPublicacaoAcordao", "dataAudienciaPublica")
  
  # Converter as colunas para o tipo de data
  df[colunas.data] <- lapply(df[colunas.data], function(x) {
    data.formatada <- format(as.Date(x, format = '%Y-%m-%d'), '%d/%m/%Y')
    return(data.formatada)
  })
  
  df.filtrado <- dplyr::filter(df, tipoPrecedente == 'Tema')
  
  tabela.temas <-
    df.filtrado[, c(
      'numeroPrecedente',
      'dataPrimeiraAfetacao',
      'dataJulgamento',
      'dataPublicacaoAcordao',
      'situacao',
      'questaoSubmetidaAJulgamento',
      'teseFirmada'
    )]
  

  
  return(tabela.temas)
}

