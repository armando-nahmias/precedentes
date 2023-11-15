enviar_tabela_html <- function(contract_data) {
  # Check if the data frame is empty
  if (nrow(contract_data) == 0) {
    cat("No contracts available for sending.\n")
    return()
  }
  
  # Create HTML table
  html_table <- tableHTML::tableHTML(contract_data)
  
  # Generate HTML file with timestamp
  timestamp <- format(Sys.time(), format = "%Y%m%d_%H%M%S")
  html_file <- paste0("saida/tabela_", timestamp, ".html")
  
  # Save HTML table to a file
  tableHTML::write_tableHTML(html_table, file = html_file)
  
  # Build email body
  email_body <- sprintf(
    "There are %s contracts registered in total.",
    nrow(contract_data)
  )
  
  # Email settings
  smtp_settings <- list(
    host.name = "smtp.gmail.com",
    port = 465,
    user.name = "informartizar@gmail.com",
    passwd = "hlzxyfpoymmtpvho",
    ssl = TRUE
  )
  
  # Send email with attachment
  email <- mailR::send.mail(
    from = "informartizar@gmail.com",
    to = "armando.nahmias@tjrr.jus.br",
    subject = paste0(
      "Updated contracts list as of ",
      format(Sys.Date(), format = "%d/%m/%Y")
    ),
    body = email_body,
    smtp = smtp_settings,
    authenticate = TRUE,
    send = TRUE,
    attach.files = html_file
  )
  
  # Check if the email was sent successfully
  if (exists('email')) {
    cat("Email sent successfully.\n")
  } else {
    cat("Failed to send the email.\n")
  }
}
