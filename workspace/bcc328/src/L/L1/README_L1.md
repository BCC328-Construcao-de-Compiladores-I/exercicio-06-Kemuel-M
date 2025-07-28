# A Linguagem L1

A linguagem L1 permite a definição de programas simples sem qualquer tipo de desvio de controle. Programas são apenas uma sequência de comandos. Existem apenas três tipos de comandos em L1: atribuições, leitura de valores {read} e impressão {print}.

# Sintaxe da linguagem L1

A sintaxe da linguagem L1 é definida pela seguinte gramática livre de contexto:

P & \to  & S\, P\:|\:\lambda\\
S & \to  & v := E ; \\
  & \mid & read(E,v);\\
  & \mid & print(E); \\
E & \to  & n \\
  & \mid & v \\
  & \mid & s \\
  & \mid & E + E \\
  & \mid & E - E \\
  & \mid & E * E \\
  & \mid & E \ E \\

A gramática é formada por três variáveis: \(P,\,S\) e \(E\); e pelos seguintes tokens (símbolos do alfabeto):

\item \(v\): representam identificadores. O token de identificador segue as regras usuais presentes em linguagens de programação: um identitificador começa com uma letra seguida de uma sequência de zero ou mais dígitos ou letras.

\item \(n\): representam constantes numéricas. No momento, vamos suportar apenas números inteiros (tanto positivos, quanto negativos).

\item \(s\): representam literais de strings. A linguagem L1 utiliza aspas duplas para delimitar literais de string.

# Programa de exemplo

A seguir, apresentamos um programa escrito na linguagem L1:

x := 0;
read("Digite o valor de x:", x);
print("O valor de x ao quadrado é:" + (x * x));
