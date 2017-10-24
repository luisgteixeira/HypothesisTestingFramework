library(tcltk)

# Seleciona nomes dos arquivos e cria lista de tabelas
nomes_arquivos <- tk_choose.files(multi = TRUE)
  
vetor_tabelas <- vector(mode = "list", length = length(nomes_arquivos))
nomes_algoritmos <- vector(mode = "list", length = length(nomes_arquivos))

# Abre cada arquivo em uma posicao da lista
i <- 1
for (nome in nomes_arquivos) {
  vetor_tabelas[[i]] <- read.csv(nome, sep = ';', header = T)
  nome_arq <- tail(strsplit(nomes_arquivos[[i]], '/')[[1]], 1)
  nomes_algoritmos[[i]] <- strsplit(nome_arq, '[.]')[[1]][1]
  i <- i + 1
}

tclRequire("BWidget")
tt <- tktoplevel()
tkgrid(tklabel(tt,text="Qual taxa voce deseja calcular?"))

# Retorna cabecalho da primeira tabela
nome_taxas <- names(vetor_tabelas[[1]])
comboBox <- tkwidget(tt,"ComboBox",editable=TRUE,values=nome_taxas,textvariable=tclVar(nome_taxas[1]))
tkgrid(comboBox)

OnOK <- function()
{
  nome_taxa_esc <- nome_taxas[as.numeric(tclvalue(tcl(comboBox,"getvalue")))+1]
  tkdestroy(tt)
  
  i <- 1
  eh_normal = logical(length = length(vetor_tabelas))
  
  vetor_taxas <- vector(mode = "list", length = length(vetor_tabelas))
  
  for (tabela in vetor_tabelas) {
    taxa_escolhida<-tabela[[nome_taxa_esc]]
    taxa_escolhida<-as.numeric(sub(",", ".", taxa_escolhida))
    if (ks.test(taxa_escolhida, 'pnorm')[2] < 0.05) {
      eh_normal[[i]] = TRUE
      vetor_taxas[[i]] <- taxa_escolhida
    } else {
      eh_normal[[i]] = FALSE
    }
    i <- i + 1
  }
  
  unique_ = unique(eh_normal)
  if (length(unique_) == 1) {
    if (unique_ == TRUE) {
      print("Todos os conjunto sao normais!")
      
      i <- 1
      dados = data.frame(
        Taxa = vetor_taxas[[i]],
        Algoritmo = factor(rep(c(nomes_algoritmos[[i]]),
                          length(vetor_taxas[[i]]))),
        Imagem = factor(rep(1:length(vetor_taxas[[i]]),
                        rep(1, length(vetor_taxas[[i]]))))
      )

      # Caso seja mais de uma tabela
      if (length(vetor_tabelas) > 1) {

        for (i in 2 : length(vetor_tabelas)) {
          dados_aux = data.frame(
            Taxa = vetor_taxas[[i]],
            Algoritmo = factor(rep(c(nomes_algoritmos[[i]]),
                                   length(vetor_taxas[[i]]))),
            Imagem = factor(rep(1:length(vetor_taxas[[i]]),
                                rep(1, length(vetor_taxas[[i]]))))
          )
          dados <- rbind(dados, dados_aux)
        }

      }
      
      ajuste = lm(data = dados, Taxa ~ .)
      print(summary(ajuste))
      print(anova(ajuste))
      
      a1 <- aov(data = dados, Taxa ~ Algoritmo + Imagem)
      posthoc <- TukeyHSD(x=a1, 'Algoritmo', conf.level=0.95)
      print(posthoc)
      
    } else {
      print("Nem todos os conjunto sao normais!")
    }
  }
  
}

OK.but <- tkbutton(tt,text="   OK   ",command=OnOK)
tkgrid(OK.but)
tkfocus(tt)
