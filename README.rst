======
.emacs
======

Copyright (C) 2011 Bruno Jacquet <bruno.jacquet@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Introduction
------------

This is the Emacs setup I use both at home and at work. Feel free to use it,
modify it and share alike.

Most of its setup is towards Lisp and Orgmode files. A random colour theme is
used in each run, mostly dark themes (non-white backgrounds).

I'm running this setup in both Linux, OSX and Windows environments without any
obscure tweaks. Should work in other environments too.

Additional Modes
----------------

- Auto Complete
- Color Theme
- Color Theme Random
- Flyspell
- Magit
- Pabbrev
- Pager
- Undo Tree
- Windpoint
- YaSnippet

Installation
------------

Download the code through Git::

  $ git clone git://github.com/bjacquet/dotemacs ~/.emacs.d

Create a symlink [1]_ for the **.emacs** file::

  $ ln -s ~/.emacs.d/dotemacs/home.el ~/.emacs

Launch emacs::

  $ emacs

Load additional modes with the following command (example with ``flyspell``)::

  M-x load-configuration flyspell

.. [1] Windows handles symlinks different. One must create a **.emacs** file
   with the following contents::

    (load-file (expand-file-name "~/.emacs.d/dotemacs/home.el"))
