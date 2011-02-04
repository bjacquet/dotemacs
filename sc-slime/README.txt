/*
;;;-----------------------------------------------------------------------------
;;;
;;;           Copyright (C) 2003, SISCOG - Sistemas Cognitivos Lda.
;;;                           All rights reserved
;;;
;;;-----------------------------------------------------------------------------
;;;
;;;                         RESTRICTED RIGHTS LEGEND
;;;
;;;-----------------------------------------------------------------------------
;;;
;;;     Use, duplication or disclosure is subject to authorization by
;;;
;;;                 SISCOG - Sistemas Cognitivos Lda.
;;;                      Campo Grande 378, 3�
;;;                        1700-097 LISBOA
;;;                           PORTUGAL
;;;
;;;-----------------------------------------------------------------------------
;;; Description
;;;	
;;; History
;;;	Date		Author		Description
;;;	03/07/28	A. Frazao	Created
;;;-----------------------------------------------------------------------------
*/

Este pacote contem uma s�rie de defini��es para uso do Emacs ou XEmacs com o
Lucid ou Allegro Common Lisp. Este pacote fica na Siscog.
Qualquer altera��o dever� ser feita usando o mecanismo de altera��es.
Agradece-se qualquer sugest�o ou altera��o que possibilitem um melhor
ambiente de trabalho para todos os colaboradores.

Para evitar problemas na refer�ncia de ficheiros, deve-se usar sempre o
formato Unix, isto � com barras '/' entre os nomes das directorias (pastas),
na defini��o das vari�veis de ambiente (e.g. CREWS_DIR, HOME, etc...)

Para se usar este pacote, deve existir uma vari�vel de ambiente de nome
SISCOG_EMACS_DIR que referencia a directoria onde est� este pacote.

No Linux, Windows NT e Solaris (XEmacs) deve-se criar o ficheiro .emacs
na directoria referenciada pela vari�vel de ambiente HOME. Esse ficheiro
deve conter o seguinte

(load (format "%s/init.el" (getenv "SISCOG_EMACS_DIR")))


No Solaris (Emacs), o emacs n�o l� o ficheiro .emacs. Para isso, pode-se
criar um alias (no ficheiro .cshrc):

alias emacs emacs -i -l $HOME/.emacs

Se algu�m quiser acrescentar alguma funcionalidade pr�pria para sua
utiliza��o, deve faz�-lo num ficheiro pr�prio a ser carregado pelo .emacs
apos a instrucao de carregamento descrita atras.


Ficheiros que compoem o pacote
init.el
	Define quais os ficheiros a carregar conforme o sistema
	operativo e a vers�o Emacs
cl-shell/*
	Ficheiros para a liga��o do Emacs ou XEmacs do Solaris com o Lucid
	Common Lisp
eli-x/*
	Ficheiros para a liga��o do XEmacs do Solaris com o Allegro Common Lisp
eli-win/*
	Ficheiros para a liga��o do Emacs (linux ou windows) com o Allegro
	Common Lisp
siscog/*
	Esta directoria cont�m uma s�rie de ficheiros desenvolvidos pela Siscog.
	Os seus ficheiros n�o se encontram compilados e sugere-se que n�o os
	compilem. A compatibilidade entre os v�rios Emacs � mais garantida.
	Por outro lado, qualquer altera��o que se fa�a ficar� imediatamente
	dispon�vel em qualquer ambiente na pr�xima vez que for carregado, n�o
	havendo a necessidade de os recompilar conforme o ambiente.
	Os ficheiros desta directoria s�o:
siscog/sc-emacs.el
	Defini��es espec�ficas do GNU Emacs.
siscog/sc-xemacs.el
	Definicoes espec�ficas do XEmacs
siscog/sc-allegro.el
	Defini��es relacionadas com o Allegro Common Lisp.
siscog/sc-lucid.el
	Defini��es para arrancar o Lucid Common Lisp.
siscog/sc-util.el
	Utilidades de funcionamento, como, por exemplo, menu de edi��o de
	ficheiros
siscog/sc-win.el
	Utilidades de funcionamento espec�ficas para o windows, incluindo
	a impressora e redefini��o do menu de buffers para n�o ordenar
siscog/sc-bb.el
	Big Brother (registo do que se faz nas altera��es)
siscog/sc-models.el
	Defini��o de modelos
siscog/sc-crews-global-param.el
	Defini��es de aplica��es CREWS e sistemas, bem como vari�veis de
	mapeamento entre ficheiros crews e sub-sistemas e respons�veis.
siscog/sc-run-crews-images.el
	Execu��o de imagens do CREWS e op��es que facilitam o arranque de
	imagens CREWS-X, mudan�a de vers�o do e de outras vari�veis.
siscog/sc-install-crews-images.el
	Instala��o de imagens CREWS. Mecanismo que permite criar imagens
	para diferentes vers�es do ACL, bem como copiar o c�digo da �rea
	da Siscog.
siscog/sc-crews-utilities.el
	Menu de utilidades de funcionamento do CREWS
siscog/sc-update-crews-code.el
	Utilidade para actualizar c�digo que que est� na �rea da SISCOG.
siscog/sc-mod.el
	Gestor de altera��es
siscog/sc-main-utilities-menu.el
	Funcionalidade que permite mudar o menu de utilities para o da
	documenta��o interna.
siscog/sc-user-param-example.el
	Exemplos das vari�veis que t�m que ser definidas pelos utilizadores,
	bem como algumas especializa��es. Este ficheiro n�o � carregado por
	defeito. Devem-se colocar num ficheiro espec�fico do utilizador e
	cujo carregamento seja feito no fim do ficheiro .emacs. Torna-se mais
	f�cil assim, actualizarem este pacote quando houverem altera��es.

Vari�veis �teis para especializa��o de funcionalidades.
S� uma � que tem valor T e reflecte o Emacs currente.
	*solaris-emacs*
	*solaris-xemacs*
	*linux-emacs*
	*windows-emacs*




Algumas informa��es pr�ticas:

---------------
1. Configura��o
---------------

Existem algumas regras que podem ajud�-los a configurar correctamente o vosso
ambiente para o gestor de altera��es:
(i)   No Windows as refer�ncias de ficheiros/directorias devem usar SEMPRE o 
      separador do Unix '/' e N�O o separador normal do windows '\'.
      Isto aplica-se tamb�m para as vari�veis de ambiente (e.g. CREWS_DIR).
(ii)  No windows, a directoria da siscog na Lisboa, deve ser mapeada numa drive
      fixa (ver exemplo de *DEFAULT-ORG-DIR*)
(iii) Para quem trabalhe no NT e no LINUX (e eventualmente no Solaris),
      aconselha-se que a directoria new-emacs (SISCOG_EMACS_DIR) seja colocada
      na �rea do Solaris. Devem assim mapear a vossa directoria do Solaris
      tamb�m numa drive fixa e atribuir o valor da vari�vel SISCOG_EMACS_DIR
      para o local escolhido. Assim, t�m as vossas funcionalidades do Emacs
      centralizadas, sendo que este pacote est� preparado para correr em
      qualquer ambiente.
(iv)  Da mesma maneira, podem centralizar a directoria de altera��es
      *DEFAULT-MOD-DIR* na �rea do Solaris. Assim, as altera��es s�o comuns em
      qualquer ambiente (n�o os ficheiros fonte em *DEFAULT-SRC-DIR*). 
(v)   A directoria referente � vari�vel *CREWS-MAIL-DIR* deve estar sempre na
      vossa �rea do Solaris, pois � da� que � executada a mailtool que permite o
      envio de altera��es.

------------------------------
2. Funcionalidades de Teclas
------------------------------

Existem algumas funcionalidades associadas a teclas F que podem ajudar
(est�o definidas no ficheiro sc-emacs.el)
F3 Insere um TAB no ponto onde est� o cursor
F4 Insere 4 TABs no ponto onde est� o cursor. Isto ajuda a colocar o cursor no
   s�tio correcto quando se est� a prencher cabe�alhos de modifica��es.
F5 Invoca o find-definition do Allegro IDE
F6 Invoca o find-string-in-file do Allegro IDE

M-p No Lisp, insere a forma avaliada anterior
M-n No Lisp, insere a forma avaliada seguinte

-----------------------
3. Gestor de altera��es
-----------------------

O ficheiro sc-mod.el cont�m, no seu cabe�alho a explica��o actualizada das
op��es do gestor de altera��es.

-------------
4. Par�metros
-------------

O ficheiro sc-user-param-example.el cont�m exemplos dos par�metros usados no Emacs e
que s�o espec�ficos para cada utilizador.

Uma vez que este par�metros s�o diferentes para cada um, sugere-se que coloquem
estas defini��es no ficheiro pr�prio do utilizador em que se redefinem os
valores das vari�veis. Por exemplo, criem um ficheiro na directoria new-emacs de
nome custom.el e inroduzam l� as vossas especifica��es. Segue-se um exemplo em
que se indicam os valores que, normalmente s�o diferentes para cada utilizador

No ficheiro .emacs introduzir a seguinte instru��o ap�s o carregamento do
ficheiro init.el. Desta maneira, as vossas altera��es s�o carregadas ap�s a
atribui��o dos valores de defeito.
(load (format "%s/custom.el" (getenv "SISCOG_EMACS_DIR")))

------------------------
5. Instala��o de imagens
------------------------

No ficheiro siscog/sc-crews.el � definido um comando que permite j� a cria��o de
imagens completas de desenvolvimento do CREWS para Windows e Linux. Para
processar aplica��es CREWS (compila��o ou cria��o de imagens) existe o seguinte
comando:
  M-x install-crews-image

Ap�s a invoca��o deste comando o Emacs pede para o utilizador introduzir uma
sequ�ncia de op��es (n�o usar aspas). As op��es est�o descritas na fun��o
install-crews-image. Este descri��o pode ser obtida atrav�s da op��o
"Describe Function..." do menu "Help" do Emacs

Este comando pode ser invocado como fun��o. Assim, pode-se especializar o
ambiente de trabalho quando, na maior parte dos casos, o utilizador esteja
constantemente a fazer certas imagens.

----------------------
6. Execu��o de imagens
----------------------

Existem duas alternativas
(i) M-x run
Esta op��o pede ao utilizador para introduzir a path da imagem CREWS para ser
executada seguida dos argumentos necess�rios.
NOTA: No Allegro n�o devem introduzir a extens�o .dxl.
Ex:
M-x run
Command: z:/siscog/bin/wagn-data-manager-win -design

(ii) M-left-button
Aparece um menu com quatro sub-menus (um para cada companhia). Seleccionando uma
companhia, aparece um menu com as aplica��es. Ao escolherem uma, o Emacs tenta
execut�-la.
NOTA: O Emacs procura as imagens na directoria CREWS_BIN o nome da imagem deve
ser o standard do CREWS. Corresponde ao nome usado quando as imagens s�o criadas
pelo instalador referido acima (por exemplo, wagn-data-manager-win.dxl).

-------------
7. Utilidades
-------------
Usando M-C-mouse-left aparece um menu com v�rias utilidades.
Segue-se um resumo das v�rias op��es. Para mais detalhes ver o cabe�alho do ficheiro sc-crews.lisp

Set package		Permite mudar o package do buffer atrav�s de menu. Funciona tamb�m no Lisp
Find in dictionaries	Procura de chaves de dicion�rios
Insert in dictionaries	Inserir chaves em dicion�rios
Insert all keywords	Inserir todas chaves em dicion�rios
Reset ignored keywords	Reset de processamento de chaves de dicion�rios
Insert comment tabs	Insere ponto-e-v�rgulas e tabs necess�rios durante o preenchimento de cabe�alhos
Insert in system	Insere string no ficheiro de system
Find In Other Window	Procura de uma string de um buffer num segundo buffer
Kill Line With Pattern	Appaga linhas que t�m determinado padr�o
PC -> UNIX		Remove o caracter 13
PC -> UNIX (Reopen)	Remove o caracter 13 mas reabrindo o ficheiro em modo bin�rio (�til para dados)
Set all file as new	Marca ficheiro como novo, inserindo cabe�alhos (n�o devia estar aqui)
Install Crews Image	Apresenta uma sequ�ncia de menus que permite construir imagens.
