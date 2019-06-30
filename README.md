# emacs_dotfile

My personal emacs configuration, tuned for my needs. One of which is to be able to use multiple emacs configurations. On such alternative is 'emacs-live', which I want to be able to isolate from my main emacs configuration.

### To use this as the default emacs init file:
* git clone git@github.com:prairie-guy/emacs_dotfile.git .emacs.d
* Run ./setup.sh to configure emacs with Ess, which is not part of the package libraries. It adds functionality for both R and Julia.
* On a OSx, uncomment pbpaste and pbcopy. These commmand don't work on Ubuntu.

### To use as a secondary emacs init file:
 * git clone git@github.com:prairie-guy/emacs_dotfile.git .emacs_dotfile
 * Update .bashrc to include: alias em='/usr/local/bin/emacs -q -l ~/.emacs_dotfile/init.el'
 * Run setup.sh to configure emacs with Ess, which is not part of the package libraries. It adds functionality for both R and Julia.
 
 * Alternatively:
 * cp emacs_basic.sh ~/bin/.
  * Update .bashrc to include alias em='/Users/cdaniels/bin/emacs_basic.sh'
* Run setup.sh to configure emacs with Ess, which is not part of the package libraries. It adds functionality for both R and Julia.

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

## Helm is the major framework used. Important keys and concepts for helm:
* For help: http://tuhdo.github.io/helm-intro.html
* Buffer matching vs. tab based
* `"M-x"` has been remapped to M-x-helm
* `"C-x b"` helm-mini, showing buffers
* `"C-x C-f"` helm-find-files
* `"M-y"` helm yank
* `"C-z"` zap to char (Not helm, but useful)
* `"M-x helm"` see other helm commands
* `"C-c h"` is the primary helm command-key
* `"C-c h i"` helm-semantic-or-imenu, showing functions and variables
* `"C-c h l"` helm-locate, searching file sytem for specific file
* `"C-c h o"` helm-occur, list of matches in the current buffer to jump back and forth
* `"C-c h a"` helm-apropos
* `"C-c h h g"` helm-info-gnus
* `"C-c h h i"` helm-info-at-point
* `"C-c h h e"` helm-info-emacs
* `"C-C h e"` opens etag buffer (To use etags, execute `etags *.py /project-root-dir`)
* `"M-." `opens the definition (based upon etag)
* `"C o"` jumps out of Emacs Command History Mode (entered with `"M-x"` and showing history of prior commands )




