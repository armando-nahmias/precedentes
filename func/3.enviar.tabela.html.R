enviar.tabela.html <- function(df) {
  epoxy::epoxy_transform_set(
    .dia = function(x) {
      format(x, '%d/%m/%Y')
    },
    .numero = function(x) {
      format(x, big.mark = '.', decimal.mark = ',')
    })
  
  
  # Verifique se o data frame é vazio
  if (nrow(df) == 0) {
    cat("Nenhum registro disponível para envio.\n")
    return()
  } else {
    cat(epoxy::epoxy('Há {.numero nrow(df)} registros disponíveis para envio.\n'))
  }
  
  # Mapeamento de nomes de colunas
  mapeamento.colunas <- c(
    "numeroPrecedente" = "Número",
    "dataPrimeiraAfetacao" = "Afetação",
    "dataJulgamento" = "Julgamento",
    "dataPublicacaoAcordao" = "Publicação",
    "situacao" = "Situação",
    "questaoSubmetidaAJulgamento" = "Questão",
    "teseFirmada" = "Tese"
  )
  
  # Renomeie as colunas no DataFrame usando o mapeamento
  colnames(df) <- mapeamento.colunas[match(colnames(df), names(mapeamento.colunas))]
  
  # Nome do arquivo HTML com carimbo de data e hora
  horario <- format(Sys.time(), format = "%Y%m%d-%H%M%S")
  arquivo.html <- epoxy::epoxy('saida/tabela-{horario}.html')
  
  # Crie a tabela HTML usando DT
  tabela.html <- DT::datatable(df, rownames = F, class = 'cell-border stripe', filter = 'top', options = list(pageLength = 50))
  DT::saveWidget(tabela.html, file = arquivo.html, selfcontained = T)
  
  # Construa o corpo do email
  corpo.email <- paste0(epoxy::epoxy('Há {.numero nrow(df)} precedentes qualificados do STJ registrados no total.'))
  assunto.email <- paste0(epoxy::epoxy('Relação de precedentes qualificados do STJ atualizada até {.dia Sys.Date()}.'))
  
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
    # to = c("armando.nahmias@tjrr.jus.br", "debora.silva@tjrr.jus.br", "rayandria.santiago@tjrr.jus.br"),
    subject = assunto.email,
    body = corpo.email,
    smtp = configuracao.email,
    authenticate = TRUE,
    send = TRUE,
    attach.files = arquivo.html
  )
  
  # Verifique se o email foi enviado com sucesso
  if (exists('email')) {
    cat("Email enviado com sucesso para .\n")
  } else {
    cat("Falha ao enviar o email para .\n")
  }
}
