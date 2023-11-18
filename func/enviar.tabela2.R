enviar.tabela.html_dt <- function(df) {
  # Verifique se o data frame é vazio
  if (nrow(df) == 0) {
    cat("Nenhum registro disponível para envio.\n")
    return()
  } else {
    cat(paste0(nrow(df), " registros disponíveis para envio.\n"))
  }
  
  # Mapeamento de nomes de colunas
  mapeamento.colunas <- c(
    "numeroPrecedente" = "Número",
    "questaoSubmetidaAJulgamento" = "Questão",
    "situacao" = "Situação",
    "dataPublicacaoAcordao" = "Publicação",
    "teseFirmada" = "Tese"
  )
  
  # Renomeie as colunas no DataFrame usando o mapeamento
  colnames(df) <- mapeamento.colunas[match(colnames(df), names(mapeamento.colunas))]
  
  # Nome do arquivo HTML com carimbo de data e hora
  timestamp <- format(Sys.time(), format = "%Y%m%d-%H%M%S")
  arquivo.html <- paste0("saida/tabela-", timestamp, ".html")
  
  df$Publicação <- format(df$Publicação, format = "%d/%m/%Y")
  
  # Crie a tabela HTML usando DT
  tabela <- DT::datatable(df, rownames = F, class = 'cell-border stripe', filter = 'top')
  
  DT::saveWidget(tabela, file = arquivo.html, selfcontained = T)
  
  assunto <- sprintf(
    "Relação de contratos atualizada até %s.",
    format(Sys.time(), format = "%d/%m/%Y %H:%m")
  )
  
  corpo <- sprintf(
    "Há %s registros no total.",
    nrow(df)
  )
  
  configuracao.email <- list(
    host.name = "smtp.gmail.com",
    port = 465,
    user.name = "informartizar@gmail.com",
    passwd = "hlzxyfpoymmtpvho",
    ssl = TRUE
  )
  
  # Configurações de envio de email
  email <- mailR::send.mail(
    from = "informartizar@gmail.com",
    to = "armando.nahmias@tjrr.jus.br",
    subject = assunto,
    body = corpo,
    smtp = configuracao.email,
    authenticate = TRUE,
    send = TRUE,
    attach.files = arquivo.html
  )
  
  # Verifique se o email foi enviado com sucesso
  if (exists('email')) {
    cat("Email enviado com sucesso.\n")
  } else {
    cat("Falha ao enviar o email.\n")
  }
}
