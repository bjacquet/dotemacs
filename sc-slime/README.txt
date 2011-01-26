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
;;;                      Campo Grande 378, 3º
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

Este pacote contem uma série de definições para uso do Emacs ou XEmacs com o
Lucid ou Allegro Common Lisp. Este pacote fica na Siscog.
Qualquer alteração deverá ser feita usando o mecanismo de alterações.
Agradece-se qualquer sugestão ou alteração que possibilitem um melhor
ambiente de trabalho para todos os colaboradores.

Para evitar problemas na referência de ficheiros, deve-se usar sempre o
formato Unix, isto é com barras '/' entre os nomes das directorias (pastas),
na definição das variáveis de ambiente (e.g. CREWS_DIR, HOME, etc...)

Para se usar este pacote, deve existir uma variável de ambiente de nome
SISCOG_EMACS_DIR que referencia a directoria onde está este pacote.

No Linux, Windows NT e Solaris (XEmacs) deve-se criar o ficheiro .emacs
na directoria referenciada pela variável de ambiente HOME. Esse ficheiro
deve conter o seguinte

(load (format "%s/init.el" (getenv "SISCOG_EMACS_DIR")))


No Solaris (Emacs), o emacs não lê o ficheiro .emacs. Para isso, pode-se
criar um alias (no ficheiro .cshrc):

alias emacs emacs -i -l $HOME/.emacs

Se alguém quiser acrescentar alguma funcionalidade própria para sua
utilização, deve fazê-lo num ficheiro próprio a ser carregado pelo .emacs
apos a instrucao de carregamento descrita atras.


Ficheiros que compoem o pacote
init.el
	Define quais os ficheiros a carregar conforme o sistema
	operativo e a versão Emacs
cl-shell/*
	Ficheiros para a ligação do Emacs ou XEmacs do Solaris com o Lucid
	Common Lisp
eli-x/*
	Ficheiros para a ligação do XEmacs do Solaris com o Allegro Common Lisp
eli-win/*
	Ficheiros para a ligação do Emacs (linux ou windows) com o Allegro
	Common Lisp
siscog/*
	Esta directoria contém uma série de ficheiros desenvolvidos pela Siscog.
	Os seus ficheiros não se encontram compilados e sugere-se que não os
	compilem. A compatibilidade entre os vários Emacs é mais garantida.
	Por outro lado, qualquer alteração que se faça ficará imediatamente
	disponível em qualquer ambiente na próxima vez que for carregado, não
	havendo a necessidade de os recompilar conforme o ambiente.
	Os ficheiros desta directoria são:
siscog/sc-emacs.el
	Definições específicas do GNU Emacs.
siscog/sc-xemacs.el
	Definicoes específicas do XEmacs
siscog/sc-allegro.el
	Definições relacionadas com o Allegro Common Lisp.
siscog/sc-lucid.el
	Definições para arrancar o Lucid Common Lisp.
siscog/sc-util.el
	Utilidades de funcionamento, como, por exemplo, menu de edição de
	ficheiros
siscog/sc-win.el
	Utilidades de funcionamento específicas para o windows, incluindo
	a impressora e redefinição do menu de buffers para não ordenar
siscog/sc-bb.el
	Big Brother (registo do que se faz nas alterações)
siscog/sc-models.el
	Definição de modelos
siscog/sc-crews-global-param.el
	Definições de aplicações CREWS e sistemas, bem como variáveis de
	mapeamento entre ficheiros crews e sub-sistemas e responsáveis.
siscog/sc-run-crews-images.el
	Execução de imagens do CREWS e opções que facilitam o arranque de
	imagens CREWS-X, mudança de versão do e de outras variáveis.
siscog/sc-install-crews-images.el
	Instalação de imagens CREWS. Mecanismo que permite criar imagens
	para diferentes versões do ACL, bem como copiar o código da área
	da Siscog.
siscog/sc-crews-utilities.el
	Menu de utilidades de funcionamento do CREWS
siscog/sc-update-crews-code.el
	Utilidade para actualizar código que que está na área da SISCOG.
siscog/sc-mod.el
	Gestor de alterações
siscog/sc-main-utilities-menu.el
	Funcionalidade que permite mudar o menu de utilities para o da
	documentação interna.
siscog/sc-user-param-example.el
	Exemplos das variáveis que têm que ser definidas pelos utilizadores,
	bem como algumas especializações. Este ficheiro não é carregado por
	defeito. Devem-se colocar num ficheiro específico do utilizador e
	cujo carregamento seja feito no fim do ficheiro .emacs. Torna-se mais
	fácil assim, actualizarem este pacote quando houverem alterações.

Variáveis úteis para especialização de funcionalidades.
Só uma é que tem valor T e reflecte o Emacs currente.
	*solaris-emacs*
	*solaris-xemacs*
	*linux-emacs*
	*windows-emacs*




Algumas informações práticas:

---------------
1. Configuração
---------------

Existem algumas regras que podem ajudá-los a configurar correctamente o vosso
ambiente para o gestor de alterações:
(i)   No Windows as referências de ficheiros/directorias devem usar SEMPRE o 
      separador do Unix '/' e NÃO o separador normal do windows '\'.
      Isto aplica-se também para as variáveis de ambiente (e.g. CREWS_DIR).
(ii)  No windows, a directoria da siscog na Lisboa, deve ser mapeada numa drive
      fixa (ver exemplo de *DEFAULT-ORG-DIR*)
(iii) Para quem trabalhe no NT e no LINUX (e eventualmente no Solaris),
      aconselha-se que a directoria new-emacs (SISCOG_EMACS_DIR) seja colocada
      na área do Solaris. Devem assim mapear a vossa directoria do Solaris
      também numa drive fixa e atribuir o valor da variável SISCOG_EMACS_DIR
      para o local escolhido. Assim, têm as vossas funcionalidades do Emacs
      centralizadas, sendo que este pacote está preparado para correr em
      qualquer ambiente.
(iv)  Da mesma maneira, podem centralizar a directoria de alterações
      *DEFAULT-MOD-DIR* na área do Solaris. Assim, as alterações são comuns em
      qualquer ambiente (não os ficheiros fonte em *DEFAULT-SRC-DIR*). 
(v)   A directoria referente à variável *CREWS-MAIL-DIR* deve estar sempre na
      vossa área do Solaris, pois é daí que é executada a mailtool que permite o
      envio de alterações.

------------------------------
2. Funcionalidades de Teclas
------------------------------

Existem algumas funcionalidades associadas a teclas F que podem ajudar
(estão definidas no ficheiro sc-emacs.el)
F3 Insere um TAB no ponto onde está o cursor
F4 Insere 4 TABs no ponto onde está o cursor. Isto ajuda a colocar o cursor no
   sítio correcto quando se está a prencher cabeçalhos de modificações.
F5 Invoca o find-definition do Allegro IDE
F6 Invoca o find-string-in-file do Allegro IDE

M-p No Lisp, insere a forma avaliada anterior
M-n No Lisp, insere a forma avaliada seguinte

-----------------------
3. Gestor de alterações
-----------------------

O ficheiro sc-mod.el contêm, no seu cabeçalho a explicação actualizada das
opções do gestor de alterações.

-------------
4. Parâmetros
-------------

O ficheiro sc-user-param-example.el contém exemplos dos parâmetros usados no Emacs e
que são específicos para cada utilizador.

Uma vez que este parâmetros são diferentes para cada um, sugere-se que coloquem
estas definições no ficheiro próprio do utilizador em que se redefinem os
valores das variáveis. Por exemplo, criem um ficheiro na directoria new-emacs de
nome custom.el e inroduzam lá as vossas especificações. Segue-se um exemplo em
que se indicam os valores que, normalmente são diferentes para cada utilizador

No ficheiro .emacs introduzir a seguinte instrução após o carregamento do
ficheiro init.el. Desta maneira, as vossas alterações são carregadas após a
atribuição dos valores de defeito.
(load (format "%s/custom.el" (getenv "SISCOG_EMACS_DIR")))

------------------------
5. Instalação de imagens
------------------------

No ficheiro siscog/sc-crews.el é definido um comando que permite já a criação de
imagens completas de desenvolvimento do CREWS para Windows e Linux. Para
processar aplicações CREWS (compilação ou criação de imagens) existe o seguinte
comando:
  M-x install-crews-image

Após a invocação deste comando o Emacs pede para o utilizador introduzir uma
sequência de opções (não usar aspas). As opções estão descritas na função
install-crews-image. Este descrição pode ser obtida através da opção
"Describe Function..." do menu "Help" do Emacs

Este comando pode ser invocado como função. Assim, pode-se especializar o
ambiente de trabalho quando, na maior parte dos casos, o utilizador esteja
constantemente a fazer certas imagens.

----------------------
6. Execução de imagens
----------------------

Existem duas alternativas
(i) M-x run
Esta opção pede ao utilizador para introduzir a path da imagem CREWS para ser
executada seguida dos argumentos necessários.
NOTA: No Allegro não devem introduzir a extensão .dxl.
Ex:
M-x run
Command: z:/siscog/bin/wagn-data-manager-win -design

(ii) M-left-button
Aparece um menu com quatro sub-menus (um para cada companhia). Seleccionando uma
companhia, aparece um menu com as aplicações. Ao escolherem uma, o Emacs tenta
executá-la.
NOTA: O Emacs procura as imagens na directoria CREWS_BIN o nome da imagem deve
ser o standard do CREWS. Corresponde ao nome usado quando as imagens são criadas
pelo instalador referido acima (por exemplo, wagn-data-manager-win.dxl).

-------------
7. Utilidades
-------------
Usando M-C-mouse-left aparece um menu com várias utilidades.
Segue-se um resumo das várias opções. Para mais detalhes ver o cabeçalho do ficheiro sc-crews.lisp

Set package		Permite mudar o package do buffer através de menu. Funciona também no Lisp
Find in dictionaries	Procura de chaves de dicionários
Insert in dictionaries	Inserir chaves em dicionários
Insert all keywords	Inserir todas chaves em dicionários
Reset ignored keywords	Reset de processamento de chaves de dicionários
Insert comment tabs	Insere ponto-e-vírgulas e tabs necessários durante o preenchimento de cabeçalhos
Insert in system	Insere string no ficheiro de system
Find In Other Window	Procura de uma string de um buffer num segundo buffer
Kill Line With Pattern	Appaga linhas que têm determinado padrão
PC -> UNIX		Remove o caracter 13
PC -> UNIX (Reopen)	Remove o caracter 13 mas reabrindo o ficheiro em modo binário (útil para dados)
Set all file as new	Marca ficheiro como novo, inserindo cabeçalhos (não devia estar aqui)
Install Crews Image	Apresenta uma sequência de menus que permite construir imagens.
