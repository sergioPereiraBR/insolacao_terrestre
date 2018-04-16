import java.util.Scanner;

public class InsolacaoTerrestre {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		
		execInsolacoTerrestre ();
	}

	// M�todo Ajustar UTC
    private static String utcToStr (double timeUTC) {

	   String utcStr;
      
       // Ajustar timeUTC
       if (Double.compare(timeUTC, 0.0)  >= 0) {
           utcStr = String.valueOf(timeUTC);

           if (utcStr.length() == 1) {
               utcStr = "+0" + utcStr;
           } else {
               utcStr = "+" + utcStr;
           }
       } else {
           utcStr = String.valueOf(Math.abs(timeUTC));

           if (utcStr.length() == 1) {
               utcStr = "-0" + utcStr;
           } else {
               utcStr = "-" + utcStr;
           }
       }

       return utcStr;
    }

    // M�todo verifica se o ano � bissesto
    private static boolean bissesto (int a) {
	   
       boolean bissesto; //bi: logico
       int ac; //: inteiro

       if (a < 1){
          ac = a-1;
       }else{
          ac = a;
       }
      
       if ((((ac % 4) == 0) && ((ac % 100) != 0)) || ((ac % 400) == 0)) {
          bissesto = true;
       } else {
          bissesto = false;
       }
      
       return bissesto;
    }


    //M�todo converte o n�mero referente ao m�s em texto
    private static String mesToStr (int m) {

       String str = "";

       switch (m){
	      case 1:
	          str = "janeiro";
	  		  break;
	      case 2:
		      str = "fevereiro";
		      break;
	      case 3:
		      str = "mar�o";
		      break;
	      case 4:
		      str = "abril";
		      break;
	      case 5:
	          str = "maio";
		      break;
		  case 6:
	          str = "junho";
		      break;
		  case 7:
	          str = "julho";
		      break;
		  case 8:
	          str = "agosto";
		      break;
		  case 9:
		      str = "setembro";
		      break;
	      case 10:
		      str = "outubro";
		      break;
	      case 11:
		      str = "novembro";
		      break;
	      case 12:
		      str = "dezembro";
		      break;
       }

       return str;
      
    }

    // M�todo que calcula o numero de dias corridos no ano
    private static int diaDoAno(int d, int m, boolean bi){
   
       int dias;
       int diasAno[] = new int[12];

       diasAno[0]  = 0;
       diasAno[1]  = 31;
       diasAno[2]  = 59;
       diasAno[3]  = 90;
       diasAno[4]  = 120;
       diasAno[5]  = 151;
       diasAno[6]  = 181;
       diasAno[7]  = 212;
       diasAno[8]  = 243;
       diasAno[9]  = 273;
       diasAno[10] = 304;
       diasAno[11] = 334;

       if ((bi = true) && (m > 2)) {
          dias = diasAno[m-1] + d + 1;
	   } else {
	     dias = diasAno[m-1] + d;
	   }
      
       return dias;
    }

    // M�todo que informa quantos dias tem um determinado m�s
    private static int diasMes(int ms, boolean bi) {
	   
       int dias;
       int diasDoMes[] = new int[12];

       diasDoMes[0]  = 31;
       diasDoMes[1]  = 28;
       diasDoMes[2]  = 31;
       diasDoMes[3]  = 30;
       diasDoMes[4]  = 31;
       diasDoMes[5]  = 30;
       diasDoMes[6]  = 31;
       diasDoMes[7]  = 31;
       diasDoMes[8]  = 30;
       diasDoMes[9]  = 31;
       diasDoMes[10] = 30;
       diasDoMes[11] = 31;

       if ((bi = true) && (ms > 1)) {
          dias = diasDoMes[ms-1] + 1;
       } else {
          dias = diasDoMes[ms-1];
       }

       return dias;
    }

    // M�todo que converte grau, minuto e segundo em decimal
    private static double gmsToDec(double g, double m, double s) {
  
       //double decimal;

       return g+m/60+(s/60)/60;
      
    }

    // M�todo que converte a hora parrasda em decimal para hora, minuto e segundo
    private static String timeToStr (double time) {

       String strHora, strMinuto, strSegundo;

       strHora = String.valueOf((int) time) + "h ";
       strMinuto = String.valueOf((int)((time-((int)time))*60)) + "min ";
       strSegundo = String.format("%.2f",((time-(int)time)*60-((int)((time-(int)time)*60)))*60)+"s";

       return strHora + strMinuto + strSegundo;
      
    }
    
    // M�todo que plota o t�tulo do app na interface
    private static void head(){
    	
    	System.out.println("===========================================================");
        System.out.println("                  INSOLA��O TERRESTRE");
        System.out.println("===========================================================");
    }

    // M�todo que plota o resultado para o usu�rio
    private static void saida(int d, String m, int a, double hi, double ha, double hp, String sUTC) {
	   
	    //System.out.println(" ");
	    //System.out.println(" ");
	    System.out.println("RESULTADO - INSOLA��O TERRESTRE EM "+String.valueOf(d)+" DE "+m.toUpperCase()+" DE "+String.valueOf(a)+":");
	    System.out.println("===========================================================");
	    System.out.println("Dura��o prevista de "+timeToStr(hi)+";");
	    System.out.println("Aurora prevista �s "+timeToStr(ha)+" "+sUTC+" UTC;");
	    System.out.println("Poente previsto �s "+timeToStr(hp)+" "+sUTC+" UTC;");
	    System.out.println("===========================================================");
	    //System.out.println(" ");
    }

    private static int anoInput (Scanner entrada_) {

        int ano_;

	    do {
	    	  System.out.println("Digite o Ano [-2418 a 5582]: ");
	       ano_ = Integer.parseInt(entrada_.nextLine()); // 2016
	    } while ((ano_ < -2418) || (ano_ > 5582));
	    
	    return ano_;
	    
    }
    
    private static int mesInput (Scanner entrada_) {

	    int mes_;
	    
	    do {
	 	  System.out.println("Digite o M�s [1 a 12]: ");
	       mes_ = Integer.parseInt(entrada_.nextLine()); // 9
	    } while ((mes_ < 1) || (mes_ > 12));
	    
	    return mes_;
	    
    }
    
    private static int diaInput(Scanner entrada_, int mes_, boolean fBissesto_){

	    String str = "Digite o Dia [1 a "+Integer.toString(diasMes(mes_, fBissesto_))+"]: ";
	    int dia_;
	    
	    do {
	 	  System.out.println(str);
	       dia_ = Integer.parseInt(entrada_.nextLine()); // 20
	    } while ((dia_ < 1) || (dia_ > diasMes(mes_, fBissesto_)));
	    
	    return dia_;
	    
    }
    
    private static double latitudeGrausInput (Scanner entrada_) { 

	    double latitudeGraus_;
	    do {
	 	  System.out.println("Digite o Grau da Latitude: ");
	       latitudeGraus_ = Double.parseDouble(entrada_.nextLine());
	    } while ((latitudeGraus_ < -90) || (latitudeGraus_ > 90));
	    
	    return latitudeGraus_;
	    
    }

    private static double latitudeMinutosInput (Scanner entrada_) {

	    double latitudeMinutos_;
	    
	    do {
	 	  System.out.println("Digite o Minuto da Latitude: ");
	       latitudeMinutos_ = Double.parseDouble(entrada_.nextLine());  // 0
	    } while ((latitudeMinutos_ < 0) || (latitudeMinutos_ > 59));
	    
	    return latitudeMinutos_;
    }
	
    private static double latitudeSegundosInput(Scanner entrada_, double latitudeGraus_, double latitudeMinutos_) {

	    double latitudeSegundos_;
	    
	    do {
	       do {
	          System.out.println("Digite o Segundo da Latitude: ");
	          latitudeSegundos_ = Double.parseDouble(entrada_.nextLine()); // 0
	       } while ((latitudeSegundos_ < 0) || (latitudeSegundos_ > 59));
	    } while ((latitudeGraus_ == 0) && (latitudeMinutos_ == 0) && (latitudeSegundos_ == 0));
	
	    return latitudeSegundos_;
	    
    }
	
	private static int UTCinput (Scanner entrada_) {

		int UTC_;
		
		do {
	 	  System.out.println("Digite o UTC do Fuso: ");
	   	  UTC_ = Integer.parseInt(entrada_.nextLine()); // -3
	    } while ((UTC_ < -12) && (UTC_ > 12));
	
	    return UTC_;
    }    
    
    // M�todo de controle l�gico do aplicativo
    private static void execInsolacoTerrestre () {
       // Vari�veis de Valores de Entrada
       int dia, mes, ano;
       double latitudeGraus, latitudeMinutos, latitudeSegundos, UTC;

       // Vari�veis de valores internos
       double grausEquadorSolar, minutosEquadorSolar, segundosEquadorSolar;
       double equinocio;
       String mesStr, utcStr;

       // Vari�veis de Valores Calculados Auxiliares
       boolean fBissesto;
       int diasAnoCivil, diasX;
       double equadorSolar, latitude;

       // Vari�veis de Valores Calculados de Sa�da
       double horasDeInsolacao, HoraAurora, HoraPoente;

       // vari�vel de op��o do usu�rio
       String str;
      
       // Comandos
      

       // Par�metros Iniciais
       grausEquadorSolar = 23;
       minutosEquadorSolar = 27;
       segundosEquadorSolar = 0;
       equinocio = 284.5;

       dia = 0;
       mes = 0;
       ano = 0;

       latitudeGraus = 0;
       latitudeMinutos = 0;
       latitudeSegundos = 0;
       UTC = 0;

       mesStr = "";
       utcStr = "";

       fBissesto = false;
       diasAnoCivil = 0;
       diasX = 0;

       equadorSolar = 0;
       latitude = 0;

       horasDeInsolacao = 0;
       HoraAurora = 0;
       HoraPoente = 0;

       head();
       
       Scanner entrada = new Scanner(System.in);
       
       ano = anoInput(entrada);
       fBissesto = bissesto(ano);
       mes = mesInput(entrada);
       dia = diaInput(entrada, mes, fBissesto);
       latitudeGraus = latitudeGrausInput(entrada);
       latitudeMinutos = latitudeMinutosInput(entrada);
       latitudeSegundos = latitudeSegundosInput(entrada, latitudeGraus, latitudeMinutos);
       UTC = UTCinput(entrada);
       
       entrada.close();
      
       // Indica��o de hora local em rela��o a hora GMT, DST n�o est� previsto
       utcStr = utcToStr(UTC);

       // Ano Civil
       if (fBissesto = true){
          diasAnoCivil = 366;
	   } else {
		  diasAnoCivil = 365;
	   }

       // Ajustar o M�s por Extenso
       mesStr = mesToStr(mes);

       // Conta dias no ano
       diasX = diaDoAno(dia, mes, fBissesto);
       
       // Concatenar e Ajustar Valores
       equadorSolar = gmsToDec(grausEquadorSolar, minutosEquadorSolar, segundosEquadorSolar);
       latitude = gmsToDec(latitudeGraus, latitudeMinutos, latitudeSegundos);

       // Ajuste da latitude para evitar error de divis�o por zero na fun��o
       if (latitude == 0) { 
          latitude = 0.000000001; // Valor suficiente para ser insignificante na ordem de grandeza nas dimen��es para latitude terrestre
       }
       
 
       // C�lculos de Insola��o, Aur�ra e Poente     
       horasDeInsolacao =  ((2.0 / 15.0) * Math.acos(-Math.tan(Math.PI / 180.0 * latitude) *
               Math.tan(Math.PI / 180.0 * equadorSolar * Math.sin(Math.PI / 180.0 * (360.0 /
               diasAnoCivil * (equinocio + diasX))))) * 180.0 / Math.PI);

       HoraAurora = 12.0 - ((2.0 / 15.0) * Math.acos(-Math.tan(Math.PI / 180.0 * latitude) *
               Math.tan(Math.PI / 180.0 * equadorSolar * Math.sin(Math.PI / 180.0 * (360.0 /
               diasAnoCivil * (equinocio + diasX))))) * 180.0 / Math.PI) / 2.0;

       HoraPoente = 12.0 + ((2.0 / 15.0) * Math.acos(-Math.tan(Math.PI / 180.0 * latitude) *
               Math.tan(Math.PI / 180.0 * equadorSolar * Math.sin(Math.PI / 180.0 * (360.0 /
               diasAnoCivil * (equinocio + diasX))))) * 180.0 / Math.PI) / 2.0;
       
       // Saida
       saida(dia, mesStr, ano, horasDeInsolacao, HoraAurora, HoraPoente, utcStr);

   }
}
