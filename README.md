emacs_dotfile
=============

Basic emacs_dotfile.

* To use this as the default emacs init file:
git clone https://github.com/prairie-guy/emacs_dotfile.git .emacs.d

* To use as a secondary emacs init file:
git clone https://github.com/prairie-guy/emacs_dotfile.git .emacs_dotfile
Update .bashrc to include: alias em='/usr/local/bin/emacs -q -l ~/.emacs_dotfile/init.el'

* Run setup.sh to configure emacs with Ess, which is not part of the package libraries. It adds functionality for both R and Julia.
