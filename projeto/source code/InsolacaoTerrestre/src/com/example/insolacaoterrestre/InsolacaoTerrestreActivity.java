package com.example.insolacaoterrestre;

import android.R.string;
import android.os.Bundle;
import android.view.View;
import android.app.Activity;
import android.widget.Button;
import android.widget.TextView;
import android.widget.EditText;
import java.text.DecimalFormat;
import android.view.View.OnClickListener;

public class InsolacaoTerrestreActivity extends Activity {
	boolean fCalcula;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_insolacao_terrestre);				
		
		Button button1 = (Button) findViewById(R.id.button1);
		Button button2 = (Button) findViewById(R.id.button2);
	   
		button1.setOnClickListener(new OnClickListener () {
			
			public void onClick (View arg0) {
	
			    try {
			    	execInsolacoTerrestre ();
				} catch (Exception e) {
					// TODO: handle exception
					Button btn = (Button) findViewById(R.id.button1);					
					btn.setError("Por favor, preencha os campos corretamente e obrigado");
				}  
			}
		}
		);
		
		button2.setOnClickListener(new OnClickListener () {
			
			public void onClick (View arg0) {
	
				Limpa ();
			}
		}
		);
	}
	
	private void Limpa () {	
		EditText editText1 = (EditText) findViewById(R.id.editText1);
		EditText editText3 = (EditText) findViewById(R.id.editText3);
		EditText editText4 = (EditText) findViewById(R.id.editText4);
		EditText editText5 = (EditText) findViewById(R.id.editText5);
		EditText editText6 = (EditText) findViewById(R.id.editText6);
		EditText editText7 = (EditText) findViewById(R.id.editText7);
		TextView editText13 = (TextView) findViewById(R.id.textView13);
		TextView editText14 = (TextView) findViewById(R.id.textView14);
		TextView editText15 = (TextView) findViewById(R.id.textView15);
		TextView editText12 = (TextView) findViewById(R.id.textView12);
		
		editText1.setText("");
		editText3.setText("");
		editText4.setText("");
		editText5.setText("");
		editText6.setText("");
		editText7.setText("");
		editText13.setText("");
		editText14.setText("");
		editText15.setText("");
		editText12.setText("");
		LimpaFimMes ();
		editText1.setFocusable(true);
		editText1.requestFocus();
	}
	
	private void LimpaFimMes () {	
		EditText editText2 = (EditText) findViewById(R.id.editText2);
		TextView textView2 = (TextView) findViewById(R.id.textView2);
		
		editText2.setText("");
		textView2.setText("Dia [1 a 31*]:");
	}
	
	// M�todo Ajustar UTC
    private String utcToStr (double timeUTC) {
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
    private boolean bissesto (int a) {   
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
    private String mesToStr (int m) {
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
    private int diaDoAno(int d, int m, boolean bi){  
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

       if ((bi == true) && (m >= 2)) {
          dias = diasAno[m-1] + d + 1;
	   } else {
	      dias = diasAno[m-1] + d;
	   }
      
       return dias;
    }

    // M�todo que informa quantos dias tem um determinado m�s
    private int diasMes(int ms, boolean bi) {	   
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

       if ((bi == true) && (ms == 2)) {
          dias = diasDoMes[ms-1] + 1;
       } else {
          dias = diasDoMes[ms-1];
       }

       return dias;
    }

    // M�todo que converte grau, minuto e segundo em decimal
    private double gmsToDec(double g, double m, double s) {
       return g+m/60+(s/60)/60; 
    }

    // M�todo que converte a hora parrasda em decimal para hora, minuto e segundo
    private String timeToStr (double time) {
       String strHora, strMinuto, strSegundo;

       strHora = String.valueOf((int) time) + "h ";
       strMinuto = String.valueOf((int)((time-((int)time))*60)) + "min ";
       strSegundo = String.format("%.4f",((time-(int)time)*60-((int)((time-(int)time)*60)))*60)+"s";
       
       return strHora + strMinuto + strSegundo;      
    }
    
    // M�todo que plota o t�tulo do app na interface
    private void head(){ 	
    	//System.out.println("===========================================================");
        //System.out.println("                  INSOLA��O TERRESTRE");
        //System.out.println("===========================================================");
    }

    // M�todo que plota o resultado para o usu�rio
    private void saida(int d, String m, int a, double hi, double ha, double hp, String sUTC) {	
		final TextView editText13 = (TextView) findViewById(R.id.textView13);
		final TextView editText14 = (TextView) findViewById(R.id.textView14);
		final TextView editText15 = (TextView) findViewById(R.id.textView15);
		final TextView editText12 = (TextView) findViewById(R.id.textView12);
		
	    editText13.setText(String.valueOf("INSOLA��O TERRESTRE EM " + String.valueOf(d)+ " DE " + m.toUpperCase()+" DE "+String.valueOf(a)+":"));
	    editText14.setText(String.valueOf("Dura��o prevista de " + timeToStr(hi) + " " + sUTC + " UTC;"));
	    editText15.setText(String.valueOf("Aurora prevista �s " + timeToStr(ha) + " " + sUTC + " UTC;"));
	    editText12.setText(String.valueOf("Poente previsto �s " + timeToStr(hp) + " " + sUTC + " UTC;"));
    }

    private int anoInput () {
    	int ano_;
        EditText editText1 = (EditText) findViewById(R.id.editText1);
        
	    try{
	        ano_ = Integer.parseInt(editText1.getText().toString()); // 2016
		    
		    if ((ano_ < -2418) || (ano_ > 5582)) {
		    	editText1.setError("O ano est� fora da faixa!");
		    	fCalcula = false;
		    }
	    }catch (Exception e){
	    	editText1.setText("");
	    	editText1.setError("Preencha o ano, obrigado!");
	    	LimpaFimMes();
	    	fCalcula = false;
	    	ano_ = 0;
	    }
	    
	    return ano_;	    
    }
    
    private int mesInput (boolean fBissesto) {
    	int mes_;    	
    	EditText editText2 = (EditText) findViewById(R.id.editText2);
    	
    	try{
		    mes_ = Integer.parseInt(editText2.getText().toString()); // 2016
		    
		    if ((mes_ < 1) || (mes_ > 12)) {
		    	editText2.setError("O m�s inserido n�o existe!");
		    	fCalcula = false;
		    }else{
		    	// TODO: faixaDia("Dia [1 a "+Integer.toString(diasMes(mes_, fBissesto_))+"]: ");
		    	AlteraFimMes(diasMes(mes_, fBissesto));
		    }
    	}catch (Exception e){
    		mes_=0;
    		LimpaFimMes();
    		editText2.setError("Insira um m�s, obrigado!");
	    	fCalcula = false;
    	}
    	
	    return mes_;	    
    }
    
    private void AlteraFimMes(int dia) {
    	final TextView editText20 = (TextView) findViewById(R.id.textView2);
    	
    	switch (dia) {
		case 28:
			editText20.setText("Dia [1 a 28*]:");
			break;
		case 29:
			editText20.setText("Dia [1 a 29*]:");
			break;
		case 30:
			editText20.setText("Dia [1 a 30*]:");
			break;
		case 31:
			editText20.setText("Dia [1 a 31*]:");
			break;
		default:
			editText20.setText("Dia [1 a 31*]:");
			break;
		} 	
    }
    
    private int diaInput(int mes_, boolean fBissesto_){  	
    	int dia_;
    	EditText editText3 = (EditText) findViewById(R.id.editText3);
    	
    	try{		    
		    dia_ = Integer.parseInt(editText3.getText().toString()); // 2016
		    
		    if ((dia_ < 1) || (dia_ > diasMes(mes_, fBissesto_))) {
		    	editText3.setError("Corrija o dia por favor!");
		    	fCalcula = false;
		    }
    	}catch (Exception e){
    		dia_= 0;
    		editText3.setText("");
    		editText3.setError("Digite o dia por favor!");
	    	fCalcula = false;
    	}
    	
	    return dia_;	    
    }
    
    private double latitudeGrausInput () {     
    	double latitudeGraus_;
	    EditText editText4 = (EditText) findViewById(R.id.editText4);
	    
	    try{
		    latitudeGraus_ = Double.parseDouble(editText4.getText().toString());
		    
		    if ((latitudeGraus_ < -90) || (latitudeGraus_ > 90)) {
		    	editText4.setError("Corrija o grau da latitude por favor!");
		    	fCalcula = false;
		    }
	    }catch (Exception e){
	    	latitudeGraus_=0;
	    	editText4.setText("");
	    	editText4.setError("Digite o grau da latitude por favor!");
	    	fCalcula = false;
	    }
	    
	    return latitudeGraus_;	    
    }

    private double latitudeMinutosInput () {    
    	double latitudeMinutos_;
	    EditText editText5 = (EditText) findViewById(R.id.editText5);
    	
    	try{
		    latitudeMinutos_ = Double.parseDouble(editText5.getText().toString());
		    
		    if ((latitudeMinutos_ < 0) || (latitudeMinutos_ >= 60)) {
		    	editText5.setError("Corrija o minuto da latitude por favor!");
		    	fCalcula = false;
		    }
    	}catch (Exception e){
    		latitudeMinutos_=0;
    		editText5.setText("");
    		editText5.setError("Digite o minuto da latitude por favor!");
    		fCalcula = false;
    	}
	    
	    return latitudeMinutos_;
    }
	
    private double latitudeSegundosInput(double latitudeGraus_, double latitudeMinutos_) {   
    	double latitudeSegundos_;
    	EditText editText6 = (EditText) findViewById(R.id.editText6);
    	
    	try{
		    latitudeSegundos_ = Double.parseDouble(editText6.getText().toString());
		    
		    if (((latitudeSegundos_ < 0) || (latitudeSegundos_ >= 60)) || ((latitudeGraus_ == 0) &&
		    (latitudeMinutos_ == 0) && (latitudeSegundos_ == 0))) {
		    	editText6.setError("Corrija o segundo da latitude por favor!");
		    	fCalcula = false;
		    }
    	}catch (Exception e){
    		latitudeSegundos_=0;
    		editText6.setText("");
    		editText6.setError("Digite o segundo da latitude por favor!");
    		fCalcula = false;
    	}
    	
	    return latitudeSegundos_;    
    }
	
	private int UTCinput () {
		int UTC_;
		EditText editText7 = (EditText) findViewById(R.id.editText7);
		
		try{
		    UTC_ = Integer.parseInt(editText7.getText().toString()); // 2016
		    
		    if ((UTC_ < -12) || (UTC_ > 12)) {
		    	editText7.setError("Corrija o valor UTC do Fuso Hor�rio por favor!");
		    	fCalcula = false;
		    }
		}catch (Exception e){
			UTC_=0;
			editText7.setText("");
			editText7.setError("Insira um valor UTC para o Fuso Hor�rio por favor!");
			fCalcula = false;
		}
		
	    return UTC_;
    }    
    
    // M�todo de controle l�gico do aplicativo
    private void execInsolacoTerrestre () {
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

       // Dados de Entrada do Usu�rio
       //head();
       fCalcula = true;
       
       ano = anoInput();
       fBissesto = bissesto(ano);
       mes = mesInput(fBissesto);
       dia = diaInput(mes, fBissesto);
       latitudeGraus = latitudeGrausInput();
       latitudeMinutos = latitudeMinutosInput();
       latitudeSegundos = latitudeSegundosInput(latitudeGraus, latitudeMinutos);
       UTC = UTCinput();
      
       // Indica��o de hora local em rela��o a hora GMT, DST n�o est� previsto
       utcStr = utcToStr(UTC);

       if (fCalcula == true){
    	   
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
}
