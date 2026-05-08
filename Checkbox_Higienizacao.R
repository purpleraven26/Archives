library(tcltk)

checkbox_dialog <- function(titulo = 'titulo',
                            mensagem = 'subtitulo:',
                            escolhas = c('opcao 1', 'opcao 2', 'opcao 3')) {
  
  #Cria Janela principal  
  tt <- tktoplevel()
  tkwm.title(tt, titulo)
  tkwm.resizable(tt, FALSE, FALSE)
  
  #Centraliza na tela
  tkwm.geometry(tt, "+500+300")
  
  #Variáveis reativas (uma por checkbox)
  vars <- lapply(escolhas, function(x) tclVar("0"))
  
  #Label de Instrução
  tkpack(tklabel(tt, text = mensagem, justify = "left"),
         padx = 20, pady = c(15,5), anchor = "w")
  
  #Frame dos checkboxes
  frameCheckbox <- tkframe(tt)
  tkpack(frameCheckbox, padx = 20, pady = 5, anchor = "w")
  
  for (i in seq_along(escolhas)) {
    cb <- tkcheckbutton(frameCheckbox,
                        text = escolhas[i],
                        variable = vars[[i]])
    tkpack(cb, anchor = "w", pady = 2)
  }
  
  #Resultado (NULL = cancelou ou fechou, lista de boleanos = confirmou)
  resultado <- NULL
  
  #Botões OK e Cancelar
  frameBotao <- tkframe(tt)
  tkpack(frameBotao, padx = 20, pady = 15, side = "bottom", anchor = "e")
  
  botaoCancelar <- tkbutton(frameBotao, text = "Cancelar", command = function() {
    resultado <- NULL
    tkdestroy(tt)
  })
  
  botaoOk <- tkbutton(frameBotao, text = "OK", command = function() {
    resultado <<- setNames(
      lapply(vars, function(v) as.integer(tclvalue(v)) == 1L),
      escolhas
    )
    tkdestroy(tt)
  })
  
  tkpack(botaoCancelar, side = "left", padx = c(0, 8))
  tkpack(botaoOk, side = "left")
  
  #Bloqueia execução até a janela fechar
  tkwait.window(tt)
  
  return(resultado)
}

selecoes <- checkbox_dialog(titulo = "Selecionar filtros", 
                            mensagem = "Marque as opções de filtro desejadas:", 
                            escolhas = c("Verifica Obito", "Verifica Conta Ativa", "Verifica Possui CL"))


if(is.null(selecoes)) stop("Operação cancelada pelo usuário")


paste0("\nEXECUTANDO RESTO DO CÓDIGO")
print(selecoes)


# RODA VERIFICACAO DE OBITO, SE SELECIONADO
tryCatch(
  if(selecoes[["Verifica Obito"]]) {
    cat("\nDeu Certo! Verificando na base de dados...")
  }, error = function(e) {
    cat("\n Erro ao verificar Obito:", conditionMessage(e), "\n")
  }
  
)


