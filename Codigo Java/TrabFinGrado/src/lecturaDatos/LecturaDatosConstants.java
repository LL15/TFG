/* Generated By:JavaCC: Do not edit this line. LecturaDatosConstants.java */
package lecturaDatos;


/**
 * Token literal values and constants.
 * Generated by org.javacc.parser.OtherFilesGen#start()
 */
public interface LecturaDatosConstants {

  /** End of File. */
  int EOF = 0;
  /** RegularExpression Id. */
  int HTML = 5;
  /** RegularExpression Id. */
  int HTML_FIN = 6;
  /** RegularExpression Id. */
  int HEAD = 7;
  /** RegularExpression Id. */
  int HEAD_FIN = 8;
  /** RegularExpression Id. */
  int BODY = 9;
  /** RegularExpression Id. */
  int BODY_FIN = 10;
  /** RegularExpression Id. */
  int NUMEROS = 11;
  /** RegularExpression Id. */
  int ETIQUETA = 12;
  /** RegularExpression Id. */
  int ETIQUETA_FIN = 13;
  /** RegularExpression Id. */
  int CARACTERES = 14;
  /** RegularExpression Id. */
  int TABLA_INICIO = 15;
  /** RegularExpression Id. */
  int TABLA_FIN = 16;
  /** RegularExpression Id. */
  int THEAD = 17;
  /** RegularExpression Id. */
  int THEAD_FIN = 18;
  /** RegularExpression Id. */
  int TBODY = 19;
  /** RegularExpression Id. */
  int TBODY_FIN = 20;
  /** RegularExpression Id. */
  int TR = 21;
  /** RegularExpression Id. */
  int TR_BG = 22;
  /** RegularExpression Id. */
  int TR_ID = 23;
  /** RegularExpression Id. */
  int TR_CLASS = 24;
  /** RegularExpression Id. */
  int TR_BODY = 25;
  /** RegularExpression Id. */
  int TR_FIN = 26;
  /** RegularExpression Id. */
  int TH_CLASS = 27;
  /** RegularExpression Id. */
  int TH_FIN = 28;
  /** RegularExpression Id. */
  int TD_COL = 29;
  /** RegularExpression Id. */
  int TD_CLASS = 30;
  /** RegularExpression Id. */
  int TD_BG = 31;
  /** RegularExpression Id. */
  int TD_FIN = 32;
  /** RegularExpression Id. */
  int TITLE = 33;
  /** RegularExpression Id. */
  int TITLE_FIN = 34;
  /** RegularExpression Id. */
  int STYLE = 35;
  /** RegularExpression Id. */
  int STYLE_FIN = 36;
  /** RegularExpression Id. */
  int META = 37;
  /** RegularExpression Id. */
  int A_HREF = 38;
  /** RegularExpression Id. */
  int A_CLASS = 39;
  /** RegularExpression Id. */
  int A_FIN = 40;
  /** RegularExpression Id. */
  int DIV = 41;
  /** RegularExpression Id. */
  int DIV_FIN = 42;
  /** RegularExpression Id. */
  int IMG = 43;
  /** RegularExpression Id. */
  int SPAN = 44;
  /** RegularExpression Id. */
  int SPAN_FIN = 45;
  /** RegularExpression Id. */
  int STRONG = 46;
  /** RegularExpression Id. */
  int STRONG_FIN = 47;
  /** RegularExpression Id. */
  int EM = 48;
  /** RegularExpression Id. */
  int EM_FIN = 49;
  /** RegularExpression Id. */
  int FONT = 50;
  /** RegularExpression Id. */
  int FONT_FIN = 51;
  /** RegularExpression Id. */
  int P = 52;
  /** RegularExpression Id. */
  int P_FIN = 53;
  /** RegularExpression Id. */
  int TIME = 54;
  /** RegularExpression Id. */
  int TIME_FIN = 55;

  /** Lexical state. */
  int DEFAULT = 0;

  /** Literal token values. */
  String[] tokenImage = {
    "<EOF>",
    "\" \"",
    "\"\\r\"",
    "\"\\t\"",
    "\"\\n\"",
    "\"<html xmlns=\\\"http://www.w3.org/TR/REC-html40\\\">\"",
    "\"</html>\"",
    "\"<head>\"",
    "\"</head>\"",
    "\"<body>\"",
    "\"</body>\"",
    "<NUMEROS>",
    "\"<\"",
    "\">\"",
    "<CARACTERES>",
    "\"<table \"",
    "\"</table>\"",
    "\"<thead>\"",
    "\"</thead>\"",
    "\"<tbody>\"",
    "\"</tbody>\"",
    "\"<tr>\"",
    "\"<tr bgcolor=\\\"#003466\\\" height=\\\"30\\\">\"",
    "\"<tr id=\\\"\"",
    "\"<tr class=\\\"rowHeader\\\">\"",
    "\"<tr id=\\\"issuerow\"",
    "\"</tr>\"",
    "\"<th class=\\\"colHeaderLink headerrow-\"",
    "\"</th>\"",
    "\"<td colspan=\\\"58\\\">\"",
    "\"<td class=\\\"\"",
    "\"<td bgcolor=\\\"#dddddd\\\" colspan=\\\"58\\\">\"",
    "\"</td>\"",
    "\"<title>\"",
    "\"</title>\"",
    "\"<style \"",
    "\"</style>\"",
    "\"<META HTTP-EQUIV=\\\"Content-Type\\\" Content=\\\"application/vnd.ms-excel; charset=UTF-8\\\">\"",
    "\"<a href=\\\"\"",
    "\"<a class=\\\"\"",
    "\"</a>\"",
    "\"<div \"",
    "\"</div>\"",
    "\"<img src=\\\"\"",
    "\"<span\"",
    "\"</span>\"",
    "\"<strong>\"",
    "\"</strong>\"",
    "\"<em>\"",
    "\"</em>\"",
    "\"<font\"",
    "\"</font>\"",
    "\"<p>\"",
    "\"</p>\"",
    "\"<time \"",
    "\"</time>\"",
  };

}