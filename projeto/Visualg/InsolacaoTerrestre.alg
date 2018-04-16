algoritmo "InsolacaoTerrestre"
// Função : Calcula Insolação Terrestre
// Autor : Sérgio da Silva Pereira
// Data : 18/09/2016

// Seção de Declarações 
var
  exit: logico

   // Função Ajustar UTC
   funcao utcToStr (nUTC: real): caracter
   var
      str: caracter
   inicio

      se (nUTC > 0) OU (nUTC = 0) entao
         str <- numpcarac(nUTC)
         se (Compr(str) = 1) entao
            str <- "+0" + str
         senao
            str <- "+" + str
         fimse
      senao
         str <-  numpcarac(Abs(nUTC))
         se (Compr(str) = 1) entao
            str <- "-0" + str
         senao
            str <- "-" + str
         fimse
      fimse

      retorne(str)
   fimfuncao

   // Função verifica se o ano é bissesto
   funcao bissesto (a: inteiro): logico
   var
      bi: logico
      ac: inteiro

   inicio
      se a < 1 entao
        ac <- a-1
      senao
        ac <- a
      fimse

      se (((ac MOD 4)=0) e ((ac MOD 100)<>0)) OU ((ac MOD 400)=0) entao
         bi <- VERDADEIRO
      senao
        bi <- FALSO
      fimse

      retorne(bi)
   fimfuncao


   //Função converte o número referente ao mês em texto
   funcao mesToStr (m: inteiro): caracter
   var
      str: caracter

   inicio
      escolha (m)
         caso 1
              str <- "janeiro"
         caso 2
              str <- "fevereiro"
         caso 3
              str <- "março"
         caso 4
              str <- "abril"
         caso 5
              str <- "maio"
         caso 6
              str <- "junho"
         caso 7
              str <- "julho"
         caso 8
              str <- "agosto"
         caso 9
              str <- "setembro"
         caso 10
              str <- "outubro"
         caso 11
              str <- "novembro"
         caso 12
              str <- "dezembro"
      fimescolha

      retorne(str)
   fimfuncao

   // Função que calcula o numero de dias corridos no ano
   funcao diaDoAno(d, m: inteiro; bi: logico): inteiro
   var
      dias: inteiro
      diasAno: vetor [1..12] de inteiro

   inicio
      diasAno[1]  <- 0
      diasAno[2]  <- 31
      diasAno[3]  <- 59
      diasAno[4]  <- 90
      diasAno[5]  <- 120
      diasAno[6]  <- 151
      diasAno[7]  <- 181
      diasAno[8]  <- 212
      diasAno[9]  <- 243
      diasAno[10] <- 273
      diasAno[10] <- 304
      diasAno[12] <- 334

      se ((bi = VERDADEIRO) E (m > 2))  entao
         dias <- diasAno[m] + d + 1
      senao
         dias <- diasAno[m] + d
      fimse

      retorne(dias)
   fimfuncao

   // Função que informa quantos dias tem um determinado mês
   funcao diasMes(ms: inteiro; bi: logico): inteiro
   var
      dias: inteiro
      diasDoMes: vetor [1..12] de inteiro

   inicio

      diasDoMes[1]  <- 31
      diasDoMes[2]  <- 28
      diasDoMes[3]  <- 31
      diasDoMes[4]  <- 30
      diasDoMes[5]  <- 31
      diasDoMes[6]  <- 30
      diasDoMes[7]  <- 31
      diasDoMes[8]  <- 31
      diasDoMes[9]  <- 30
      diasDoMes[10] <- 31
      diasDoMes[11] <- 30
      diasDoMes[12] <- 31

      se ((bi = VERDADEIRO) E (ms = 2)) entao
         dias <- diasDoMes[ms] + 1
      senao
           dias <- diasDoMes[ms]
      fimse

      retorne(dias)
   fimfuncao

   // Função que converte grau, minuto e segundo em decimal
   funcao gmsToDec(g, m, s: real): real
   var
     decimal: real

   inicio
      retorne(g+m/60+s/60/60)
   fimfuncao

   // Funçao que converte a hora parrasda em decimal para hora, minuto e segundo
   funcao timeToStr (time: real): caracter
   var
      strHora, strMinuto, strSegundo: caracter

   inicio
      strHora <- numpcarac(int(time))+"h "
      strMinuto <- numpcarac(int((time-int(time))*60))+"min "
      strSegundo <- Copia(numpcarac(((time-int(time))*60-int((time-int(time))*60))*60),0,5)+"s")

      retorne(strHora + strMinuto + strSegundo)
   fimfuncao

   // Procedimento que plota o resultado para o usuário
   procedimento saida(d: inteiro; m: caracter; a: inteiro; hi, ha, hp: real; sUTC: caracter)
   inicio
      escreval(" ")
      escreval(" ")
      escreval("RESULTADO - INSOLAÇÃO TERRESTRE EM "+numpcarac(d)+" DE "+Maiusc(m)+" DE "+numpcarac(a)+":")
      escreval("===========================================================")
      escreval("Duração prevista de "+timeToStr(hi)+" "+sUTC+" UTC;")
      escreval("Aurora prevista às "+timeToStr(ha)+" "+sUTC+" UTC;")
      escreval("Poente previsto às "+timeToStr(hp)+" "+sUTC+" UTC;")
      escreva ("===========================================================")
      escreval(" ")
   fimprocedimento

   // Função de controle lógico do aplicativo
   funcao execInsolacoTerrestre (): logico
   var
      // Variáveis de Valores de Entrada
      dia, mes, ano: inteiro
      latitudeGraus, latitudeMinutos, latitudeSegundos, UTC: real

      // Variáveis de valores internos
      grausEquadorSolar, minutosEquadorSolar, segundosEquadorSolar: real
      equinocio: real
      mesStr, utcStr: caracter

      // Variáveis de Valores Calculados Auxiliares
      fBissesto: logico
      diasAnoCivil, diasX: real
      equadorSolar, latitude: real

      // Variáveis de Valores Calculados de Saída
      horasDeInsolacao, HoraAurora, HoraPoente: real

      // variável de opção do usuário
      encerrar: caracter
      sair: logico
      str: caracter

   inicio
      // Seção de Comandos


      // Parâmetros Iniciais
      grausEquadorSolar <- 23;
      minutosEquadorSolar <- 27;
      segundosEquadorSolar <- 0;
      equinocio <- 284.5;

      dia <- 0
      mes <- 0
      ano <- 0

      latitudeGraus <- 0
      latitudeMinutos <- 0
      latitudeSegundos <- 0
      UTC <- 0

      mesStr <- ""
      utcStr <- ""

      fBissesto <- FALSO
      diasAnoCivil <- 0
      diasX <- 0

      equadorSolar <- 0
      latitude <- 0

      horasDeInsolacao <- 0
      HoraAurora <- 0
      HoraPoente <- 0

      encerrar <- ""
      sair <- FALSO

      // Dados de Entrada do Usuário
      limpatela
      escreval("===========================================================")
      escreval("                  INSOLAÇÂO TERRESTRE")
      escreval("===========================================================")

      // Seguem filtros para tratamento das entradas

      repita
         escreva("Digite o Ano [-2418 a 5582]: ")
         leia(ano) // 2016
      ate (ano > -2419) E (ano < 5583)

      escreval(" ")

      fBissesto <- bissesto(ano)

      repita
         escreva("Digite o Mês [1 a 12]: ")
         leia(mes) // 9
      ate ((mes > 0) E (mes < 13))

      escreval(" ")

      str <- "Digite o Dia [1 a "+numpcarac(diasMes(mes, fBissesto))+"]: "

      repita
         escreva(str)
         leia(dia) // 20
      ate ((dia >= 1) E (dia <= diasMes(mes, fBissesto)))

      escreval(" ")

      repita
         escreva("Digite o Grau da Latitude: ")
         leia(latitudeGraus)    // -23
      ate (latitudeGraus > -90) E (latitudeGraus < 90)

      escreval(" ")

      repita
         escreva("Digite o Minuto da Latitude: ")
         leia(latitudeMinutos)  // 0
      ate (latitudeMinutos >= 0) E (latitudeMinutos < 60)


      escreval(" ")

      repita
         repita
            escreva("Digite o Segundo da Latitude: ")
            leia(latitudeSegundos) // 0
         ate (latitudeSegundos >= 0) E (latitudeSegundos < 61)
      ate ((latitudeGraus <> 0) OU (latitudeMinutos <> 0) OU (latitudeSegundos <> 0))

      escreval(" ")

      repita
         escreva("Digite o UTC do Fuso: ")
         leia(UTC) // -3
      ate ((UTC >= -12) E (UTC <= 12))


      // Indicação de hora local em relação a hora GMT, DST não está previsto
      utcStr <- utcToStr(UTC)

      // Ano Civil
      se fBissesto entao
         diasAnoCivil <- 366
      senao
           diasAnoCivil <- 365
      fimse

      // Ajustar o Mês por Extenso
      mesStr <- mesToStr(mes)

      // Conta dias no ano
      diasX <- diaDoAno(dia, mes, fBissesto)

      // Concatenar e Ajustar Valores
      equadorSolar <- gmsToDec(grausEquadorSolar, minutosEquadorSolar, segundosEquadorSolar)
      latitude <- gmsToDec(latitudeGraus, latitudeMinutos, latitudeSegundos)

      // Ajuste da latitude para evitar error de divisão por zero na função
      se (latitude = 0) entao
         latitude <- 0.000000001) // Valor suficiente para ser insignificante na ordem de grandeza nas dimenções para latitude terrestre
      fimse

      // Cálculos de Insolação, Auróra e Poente
      horasDeInsolacao <- ((2/15)*ArcCos(-Tan(Pi/180*latitude)*Tan(Pi/180*equadorSolar*Sen(Pi/180*(360/diasAnoCivil*(equinocio+diasX)))))*180/Pi)
      HoraAurora <- 12-((2/15)*ArcCos(-Tan(Pi/180*latitude)*Tan(Pi/180*equadorSolar*Sen(Pi/180*(360/diasAnoCivil*(equinocio+diasX)))))*180/Pi)/2
      HoraPoente <- 12+((2/15)*ArcCos(-Tan(Pi/180*latitude)*Tan(Pi/180*equadorSolar*Sen(Pi/180*(360/diasAnoCivil*(equinocio+diasX)))))*180/Pi)/2

      // Saida
      saida(dia, mesStr, ano, horasDeInsolacao, HoraAurora, HoraPoente, utcStr)

      // Tratamento para fim do app ou para repetir a consulta
      escreval("Digite 'S' para Sair,")
      escreva ("ou 'qualquer outra tecla' para reiniciar: ")
      leia(encerrar)
      limpatela

      se (encerrar = "S") ou (encerrar = "s") entao
         sair <- VERDADEIRO
      senao
           sair <- FALSO
      fimse

      retorne(sair)
   fimfuncao

// Escopo principal
inicio

   // loop que permite refazer a consulta
   repita
      exit <- execInsolacoTerrestre ()
   ate (exit = VERDADEIRO)
   
fimalgoritmo
