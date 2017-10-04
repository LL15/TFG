/* Generated By:JavaCC: Do not edit this line. LecturaDatos.java */
package lecturaDatos;

import java.io.*;
import java.util.*;

public class LecturaDatos implements LecturaDatosConstants {
  //Fichero del que se extraen los datos
  public final static String path = "C:\u005c\u005cbisdev_jira_com.html";

  public static void main(String args []) throws ParseException
  {
    System.out.println("Acaba de empezar el main");
    //Se abre el fichero
    try {
      BufferedReader buf = new BufferedReader(new FileReader (path));

      //Se crea el parser
      LecturaDatos parser = new LecturaDatos(buf);

          //Se empieza a leer el fichero
          parser.leer_head();
          //Obtencion del n�mero de incidencias que hay
          int incidencias = parser.leer_body();
          //Obtencion de las cabeceras de la tabla
          String [] cab = parser.leerCabecera();
          //Se empiezan a leer los atributos de las incidencias
          parser.leerContenido();


          //Se cierra el fichero
          System.out.println("Se cierra el fichero");
          buf.close();
    }
    catch(Exception e) {
                System.out.println("Exception " + e.getMessage());
        }
  }

  static final public void saltar() throws ParseException {
    System.out.println("He entrado en saltar");
    label_1:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        jj_consume_token(CARACTERES);
        break;
      case ETIQUETA:
        jj_consume_token(ETIQUETA);
        break;
      case ETIQUETA_FIN:
        jj_consume_token(ETIQUETA_FIN);
        break;
      case NUMEROS:
        jj_consume_token(NUMEROS);
        break;
      case IMG:
        jj_consume_token(IMG);
        break;
      case A_HREF:
        jj_consume_token(A_HREF);
        break;
      case STRONG:
        jj_consume_token(STRONG);
        break;
      case STRONG_FIN:
        jj_consume_token(STRONG_FIN);
        break;
      case TD_FIN:
        jj_consume_token(TD_FIN);
        break;
      default:
        jj_la1[0] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case NUMEROS:
      case ETIQUETA:
      case ETIQUETA_FIN:
      case CARACTERES:
      case TD_FIN:
      case A_HREF:
      case IMG:
      case STRONG:
      case STRONG_FIN:
        ;
        break;
      default:
        jj_la1[1] = jj_gen;
        break label_1;
      }
    }
  }

  static final public void leer_head() throws ParseException {
   System.out.println("Principio de leer_head");
    jj_consume_token(HTML);
    jj_consume_token(HEAD);
    jj_consume_token(TITLE);
    jj_consume_token(CARACTERES);
    jj_consume_token(TITLE_FIN);
    jj_consume_token(STYLE);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    saltar();
    jj_consume_token(STYLE_FIN);
    jj_consume_token(META);
    saltar();
    jj_consume_token(STYLE_FIN);
    saltar();
    jj_consume_token(HEAD_FIN);
   System.out.println("Fin de leer_head");
  }

  static final public int leer_body() throws ParseException {
                   int n;
   System.out.println("Principio de leer_body");
    jj_consume_token(BODY);
    jj_consume_token(TABLA_INICIO);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    jj_consume_token(TR_BG);
    jj_consume_token(TD_COL);
    jj_consume_token(IMG);
    saltar();
    jj_consume_token(TR_FIN);
    jj_consume_token(TR);
    jj_consume_token(TD_COL);
    jj_consume_token(A_HREF);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    jj_consume_token(CARACTERES);
    jj_consume_token(A_FIN);
    jj_consume_token(TD_FIN);
    jj_consume_token(TR_FIN);
    jj_consume_token(TR);
    jj_consume_token(TD_COL);
    jj_consume_token(CARACTERES);
    jj_consume_token(STRONG);
    n = numero();
    jj_consume_token(STRONG_FIN);
    saltar();
    jj_consume_token(TR_FIN);
    jj_consume_token(TABLA_FIN);
   System.out.println("El numero de incidencias es:");System.out.println(n);
  {if (true) return n;}
    throw new Error("Missing return statement in function");
  }

  static final public String[] leerCabecera() throws ParseException {
  String [] cabecera= new String [58];
  int i=0;
  String nombre="", n1;
   System.out.println("Empieza la cabecera de la tabla");
    jj_consume_token(TABLA_INICIO);
    saltar();
    jj_consume_token(THEAD);
    jj_consume_token(TR_CLASS);
    label_2:
    while (true) {
      jj_consume_token(TH_CLASS);
      jj_consume_token(CARACTERES);
      jj_consume_token(CARACTERES);
      jj_consume_token(ETIQUETA_FIN);
      nombre = cadena();
      label_3:
      while (true) {
        switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
        case CARACTERES:
          ;
          break;
        default:
          jj_la1[2] = jj_gen;
          break label_3;
        }
        n1 = cadena();
                                  nombre +=" " + n1;
      }
   cabecera[i]=nombre; i++;
      jj_consume_token(TH_FIN);
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case TH_CLASS:
        ;
        break;
      default:
        jj_la1[3] = jj_gen;
        break label_2;
      }
    }
    jj_consume_token(TR_FIN);
    jj_consume_token(THEAD_FIN);
    jj_consume_token(TBODY);
   System.out.println("Se ha terminado de leer la cabecera");
  {if (true) return cabecera;}
    throw new Error("Missing return statement in function");
  }

  static final public void leerContenido() throws ParseException {
                       String n="", n1, cadena=""; int num;
   System.out.println("Se ha entrado en leerContenido");
    jj_consume_token(TR_BODY);
    saltar();
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_4:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[4] = jj_gen;
        break label_4;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    jj_consume_token(A_CLASS);
    label_5:
    while (true) {
      jj_consume_token(CARACTERES);
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[5] = jj_gen;
        break label_5;
      }
    }
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_6:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[6] = jj_gen;
        break label_6;
      }
      n1 = cadena();
                                        n +=" " + n1;
    }
    jj_consume_token(A_FIN);
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    jj_consume_token(P);
    n = cadena();
    label_7:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[7] = jj_gen;
        break label_7;
      }
      n1 = cadena();
                                                                 n +=" " + n1;
    }
    jj_consume_token(P_FIN);
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_8:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[8] = jj_gen;
        break label_8;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    jj_consume_token(SPAN);
    label_9:
    while (true) {
      jj_consume_token(CARACTERES);
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[9] = jj_gen;
        break label_9;
      }
    }
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_10:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[10] = jj_gen;
        break label_10;
      }
      n1 = cadena();
                          n +=" " + n1;
    }
    jj_consume_token(SPAN_FIN);
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_11:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[11] = jj_gen;
        break label_11;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_12:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[12] = jj_gen;
        break label_12;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_13:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[13] = jj_gen;
        break label_13;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_14:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[14] = jj_gen;
        break label_14;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_15:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[15] = jj_gen;
        break label_15;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_16:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[16] = jj_gen;
        break label_16;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_17:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[17] = jj_gen;
        break label_17;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_18:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[18] = jj_gen;
        break label_18;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_19:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[19] = jj_gen;
        break label_19;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_20:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[20] = jj_gen;
        break label_20;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_21:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[21] = jj_gen;
        break label_21;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_22:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[22] = jj_gen;
        break label_22;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_23:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[23] = jj_gen;
        break label_23;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    num = numero();
    jj_consume_token(TD_FIN);
   cadena+= num; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    num = numero();
    jj_consume_token(TD_FIN);
   cadena+= num; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_24:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[24] = jj_gen;
        break label_24;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_FIN);
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    num = numero();
    jj_consume_token(TD_FIN);
   cadena+= num; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    num = numero();
    jj_consume_token(TD_FIN);
   cadena+= num; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    num = numero();
    jj_consume_token(TD_FIN);
   cadena+= num; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_25:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[25] = jj_gen;
        break label_25;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_26:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[26] = jj_gen;
        break label_26;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_27:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[27] = jj_gen;
        break label_27;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_28:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[28] = jj_gen;
        break label_28;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_29:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[29] = jj_gen;
        break label_29;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
   cadena+= n; cadena+=", ";
    jj_consume_token(TD_FIN);
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_30:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[30] = jj_gen;
        break label_30;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_31:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[31] = jj_gen;
        break label_31;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_32:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[32] = jj_gen;
        break label_32;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    num = numero();
    jj_consume_token(TD_FIN);
   cadena+= num; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    num = numero();
    jj_consume_token(TD_FIN);
   cadena+= num; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    num = numero();
    jj_consume_token(TD_FIN);
   cadena+= num; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_33:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[33] = jj_gen;
        break label_33;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_34:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[34] = jj_gen;
        break label_34;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_35:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[35] = jj_gen;
        break label_35;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_36:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[36] = jj_gen;
        break label_36;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_37:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[37] = jj_gen;
        break label_37;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_38:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[38] = jj_gen;
        break label_38;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_39:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[39] = jj_gen;
        break label_39;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_40:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[40] = jj_gen;
        break label_40;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_41:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[41] = jj_gen;
        break label_41;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_42:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[42] = jj_gen;
        break label_42;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_43:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[43] = jj_gen;
        break label_43;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_44:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[44] = jj_gen;
        break label_44;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_45:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[45] = jj_gen;
        break label_45;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_46:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[46] = jj_gen;
        break label_46;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_47:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[47] = jj_gen;
        break label_47;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_48:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[48] = jj_gen;
        break label_48;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    jj_consume_token(DIV);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    jj_consume_token(DIV);
         System.out.println("Se va a hacer un saltar()");
    saltar();
    jj_consume_token(A_CLASS);
    saltar();
    jj_consume_token(A_FIN);
    jj_consume_token(DIV_FIN);
    jj_consume_token(DIV_FIN);
    jj_consume_token(TD_FIN);
                                            cadena+= "null, ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_49:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[49] = jj_gen;
        break label_49;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_50:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[50] = jj_gen;
        break label_50;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_51:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[51] = jj_gen;
        break label_51;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_52:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[52] = jj_gen;
        break label_52;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_53:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[53] = jj_gen;
        break label_53;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+=", ";
    jj_consume_token(TD_CLASS);
    jj_consume_token(CARACTERES);
    jj_consume_token(ETIQUETA_FIN);
    n = cadena();
    label_54:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case CARACTERES:
        ;
        break;
      default:
        jj_la1[54] = jj_gen;
        break label_54;
      }
      n1 = cadena();
                                                              n +=" " + n1;
    }
    jj_consume_token(TD_FIN);
           cadena+= n; cadena+="; ";
   System.out.println(cadena);
    jj_consume_token(TR_FIN);
    jj_consume_token(BODY_FIN);
  }

  static final public String cadena() throws ParseException {
  Token s ;
    s = jj_consume_token(CARACTERES);
    {if (true) return s.image;}
    throw new Error("Missing return statement in function");
  }

  static final public int numero() throws ParseException {
  Token s ;
    s = jj_consume_token(NUMEROS);
    {if (true) return Integer.parseInt(s.image);}
    throw new Error("Missing return statement in function");
  }

  static private boolean jj_initialized_once = false;
  /** Generated Token Manager. */
  static public LecturaDatosTokenManager token_source;
  static SimpleCharStream jj_input_stream;
  /** Current token. */
  static public Token token;
  /** Next token. */
  static public Token jj_nt;
  static private int jj_ntk;
  static private int jj_gen;
  static final private int[] jj_la1 = new int[55];
  static private int[] jj_la1_0;
  static private int[] jj_la1_1;
  static {
      jj_la1_init_0();
      jj_la1_init_1();
   }
   private static void jj_la1_init_0() {
      jj_la1_0 = new int[] {0x7800,0x7800,0x4000,0x8000000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,0x4000,};
   }
   private static void jj_la1_init_1() {
      jj_la1_1 = new int[] {0xc841,0xc841,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,};
   }

  /** Constructor with InputStream. */
  public LecturaDatos(java.io.InputStream stream) {
     this(stream, null);
  }
  /** Constructor with InputStream and supplied encoding */
  public LecturaDatos(java.io.InputStream stream, String encoding) {
    if (jj_initialized_once) {
      System.out.println("ERROR: Second call to constructor of static parser.  ");
      System.out.println("       You must either use ReInit() or set the JavaCC option STATIC to false");
      System.out.println("       during parser generation.");
      throw new Error();
    }
    jj_initialized_once = true;
    try { jj_input_stream = new SimpleCharStream(stream, encoding, 1, 1); } catch(java.io.UnsupportedEncodingException e) { throw new RuntimeException(e); }
    token_source = new LecturaDatosTokenManager(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 55; i++) jj_la1[i] = -1;
  }

  /** Reinitialise. */
  static public void ReInit(java.io.InputStream stream) {
     ReInit(stream, null);
  }
  /** Reinitialise. */
  static public void ReInit(java.io.InputStream stream, String encoding) {
    try { jj_input_stream.ReInit(stream, encoding, 1, 1); } catch(java.io.UnsupportedEncodingException e) { throw new RuntimeException(e); }
    token_source.ReInit(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 55; i++) jj_la1[i] = -1;
  }

  /** Constructor. */
  public LecturaDatos(java.io.Reader stream) {
    if (jj_initialized_once) {
      System.out.println("ERROR: Second call to constructor of static parser. ");
      System.out.println("       You must either use ReInit() or set the JavaCC option STATIC to false");
      System.out.println("       during parser generation.");
      throw new Error();
    }
    jj_initialized_once = true;
    jj_input_stream = new SimpleCharStream(stream, 1, 1);
    token_source = new LecturaDatosTokenManager(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 55; i++) jj_la1[i] = -1;
  }

  /** Reinitialise. */
  static public void ReInit(java.io.Reader stream) {
    jj_input_stream.ReInit(stream, 1, 1);
    token_source.ReInit(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 55; i++) jj_la1[i] = -1;
  }

  /** Constructor with generated Token Manager. */
  public LecturaDatos(LecturaDatosTokenManager tm) {
    if (jj_initialized_once) {
      System.out.println("ERROR: Second call to constructor of static parser. ");
      System.out.println("       You must either use ReInit() or set the JavaCC option STATIC to false");
      System.out.println("       during parser generation.");
      throw new Error();
    }
    jj_initialized_once = true;
    token_source = tm;
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 55; i++) jj_la1[i] = -1;
  }

  /** Reinitialise. */
  public void ReInit(LecturaDatosTokenManager tm) {
    token_source = tm;
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 55; i++) jj_la1[i] = -1;
  }

  static private Token jj_consume_token(int kind) throws ParseException {
    Token oldToken;
    if ((oldToken = token).next != null) token = token.next;
    else token = token.next = token_source.getNextToken();
    jj_ntk = -1;
    if (token.kind == kind) {
      jj_gen++;
      return token;
    }
    token = oldToken;
    jj_kind = kind;
    throw generateParseException();
  }


/** Get the next Token. */
  static final public Token getNextToken() {
    if (token.next != null) token = token.next;
    else token = token.next = token_source.getNextToken();
    jj_ntk = -1;
    jj_gen++;
    return token;
  }

/** Get the specific Token. */
  static final public Token getToken(int index) {
    Token t = token;
    for (int i = 0; i < index; i++) {
      if (t.next != null) t = t.next;
      else t = t.next = token_source.getNextToken();
    }
    return t;
  }

  static private int jj_ntk() {
    if ((jj_nt=token.next) == null)
      return (jj_ntk = (token.next=token_source.getNextToken()).kind);
    else
      return (jj_ntk = jj_nt.kind);
  }

  static private java.util.List<int[]> jj_expentries = new java.util.ArrayList<int[]>();
  static private int[] jj_expentry;
  static private int jj_kind = -1;

  /** Generate ParseException. */
  static public ParseException generateParseException() {
    jj_expentries.clear();
    boolean[] la1tokens = new boolean[52];
    if (jj_kind >= 0) {
      la1tokens[jj_kind] = true;
      jj_kind = -1;
    }
    for (int i = 0; i < 55; i++) {
      if (jj_la1[i] == jj_gen) {
        for (int j = 0; j < 32; j++) {
          if ((jj_la1_0[i] & (1<<j)) != 0) {
            la1tokens[j] = true;
          }
          if ((jj_la1_1[i] & (1<<j)) != 0) {
            la1tokens[32+j] = true;
          }
        }
      }
    }
    for (int i = 0; i < 52; i++) {
      if (la1tokens[i]) {
        jj_expentry = new int[1];
        jj_expentry[0] = i;
        jj_expentries.add(jj_expentry);
      }
    }
    int[][] exptokseq = new int[jj_expentries.size()][];
    for (int i = 0; i < jj_expentries.size(); i++) {
      exptokseq[i] = jj_expentries.get(i);
    }
    return new ParseException(token, exptokseq, tokenImage);
  }

  /** Enable tracing. */
  static final public void enable_tracing() {
  }

  /** Disable tracing. */
  static final public void disable_tracing() {
  }

}
