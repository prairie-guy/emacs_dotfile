# emacs_dotfile

My personal emacs configuration, tuned for my needs. One of which is to be able to use multiple emacs configurations. On such alternative is 'emacs-live', which I want to be able to isolate from my main emacs configuration.

### To use this as the default emacs init file:
* git clone https://github.com/prairie-guy/emacs_dotfile.git .emacs.d

### To use as a secondary emacs init file:
 * git clone git@github.com:prairie-guy/emacs_dotfile.git .emacs_dotfile
 * Update .bashrc to include: alias em='/usr/local/bin/emacs -q -l ~/.emacs_dotfile/init.el'
 * Alternatively:
  * cp emacs_basic.sh ~/bin/.
  * Update .bashrc to include alias em='/Users/cdaniels/bin/emacs_basic.sh'

### Run setup.sh to configure emacs with Ess, which is not part of the package libraries. It adds functionality for both R and Julia.

### My Current Emacs Configuration on Sodium:
* .emacs_dotfile/
 * This emacs configuration: https://github.com/prairie-guy/emacs_dotfile
 * usage:  em [file ...]
 * update: git pull

* .emacs.d/
 * emacs-live distribution: http://overtone.github.com/emacs-live/
 * usage:  emacs [file ...]
 * update: git pull

### Example usage of git work-flows:
* git clone https://github.com/prairie-guy/emacs_dotfile.git .emacs.d
* git status
* git add README.md
* git commit -m "Changed something in README.me"
* git push

### Get the most current version of emacs for OSX with brew:
* brew install emacs --HEAD --use-git-head --cocoa --with-gnutls

