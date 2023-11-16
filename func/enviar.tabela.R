enviar.tabela.html <- function(df) {
  # Verifique se o data frame é vazio
  if (nrow(df) == 0) {
    cat("Nenhum registro disponível para envio.\n")
    return()
  } else {
    cat(paste0(nrow(df), " registros disponíveis para envio.\n"))
  }
  
  # Mapeamento de nomes de colunas
  mapeamento_colunas <- c(
    "numeroPrecedente" = "Número",
    "questaoSubmetidaAJulgamento" = "Questão",
    "situacao" = "Situação",
    "dataPublicacaoAcordao" = "Publicação",
    "teseFirmada" = "Tese"
    # Adicione mais colunas conforme necessário
  )
  
  # Renomeie as colunas no DataFrame usando o mapeamento
  colnames(df) <- mapeamento_colunas[match(colnames(df), names(mapeamento_colunas))]
  
  # Crie a tabela HTML
  tabela_html <- tableHTML::tableHTML(df)
  
  # Nome do arquivo HTML com carimbo de data e hora
  timestamp <- format(Sys.time(), format = "%Y%m%d_%H%M%S")
  arquivo_html <- paste0("saida/tabela_", timestamp, ".html")
  
  # Salve a tabela HTML em um arquivo
  tableHTML::write_tableHTML(tabela_html, file = arquivo_html)
  
  # Construa o corpo do email
  corpo_email <- sprintf(
    "Há %s contratos registrados no total.",
    nrow(df)
  )
  
  # Configurações de envio de email
  email <- mailR::send.mail(
    from = "informartizar@gmail.com",
    to = "armando.nahmias@tjrr.jus.br",
    subject = paste0(
      "Relação de contratos atualizada até ",
      format(Sys.Date(), format = "%d/%m/%Y")
    ),
    body = corpo_email,
    smtp = list(
      host.name = "smtp.gmail.com",
      port = 465,
      user.name = "informartizar@gmail.com",
      passwd = "hlzxyfpoymmtpvho",
      ssl = TRUE
    ),
    authenticate = TRUE,
    send = TRUE,
    attach.files = arquivo_html
  )
  
  # Verifique se o email foi enviado com sucesso
  if (exists('email')) {
    cat("Email enviado com sucesso.\n")
  } else {
    cat("Falha ao enviar o email.\n")
  }
}
