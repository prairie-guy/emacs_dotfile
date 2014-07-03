#!/bin/bash

# setup.sh should be run the first time after using 'git clone https://github.com/prairie-guy/emacs_dotfile.git .emacs.d'


# ADD ESS for R and JULIA

if [ ! -d "ess.d" ]; then
    git clone https://github.com/emacs-ess/ESS.git ess.d
    cd ess.d
    make
else
    echo ess.d has been previously installed;
 fi

# To use these lines have been added to 'init.el'
# (add-to-list 'load-path  (concat my-init-directory "ess.d/lisp"))')
# (load "ess-site")


  
