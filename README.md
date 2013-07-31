# Notice

    I'm not good at English.
    So please correct my English.

# el-bundler.el

## Commentary
'el-bundler.el' referred to "bundler" of programming language ruby, and created it.
 Unlike package management, only the function of simple management is offered.
 These manage that in which I have installed what from init.el(or any config file).

## Installation

    $git clone --recursive https://github.com/daic-h/el-bundler.git

## How to use basic

to require the el-bundler.

    (require 'el-bundler)

define the package.

    (el-bundler: packages (:type "github":name "daic-h/el-bundler"))

You can install, update, and remove by defining the package.

    (el-bundler: install)
    (el-bundler: install!); delete a file that is not defined in the package, perform the install.
    (el-bundler: update)
    (el-bundler: update-all)
    (el-bundler: remove)

Also, these commands can be called interactive.

call (el-bundler:initialize), the package file is loaded if defined has been installed.

## Customization

Definition of package

- Common
 - :init-submodule init a submodule to if you want to clone, _default t_
 - :byte-compile   byte-compiled if you want to clone,       _default t_
 - :load-package   to load when call the el-bundler:itialize,   _default t_

These default values can be changed by el-bundler:configure

- github
 - :type "github"
 - :name -"username/repository", _required_
 - :branch - branch name when you clone, _default "master"_

- git
 - :type "git"
 - :url - url of the repository, _required_
 - :name - the name of the package, _required_
 - :branch - branch name when you clone, _default "master"_

Argument by (el-bundler:configure)

 - :dir - directory where the package is installed, _default "~/.emacs.d/el-bundler"_
 - :smp - number to be executed at the same time in the install and update, _default 1_
 - :init-submodule - it will be the initial value of the package, _default t_
 - :byte-compile   - it will be the initial value of the package, _default t_
 - :load-package   - it will be the initial value of the package, _default t_

## Sample Setting

    (require 'el-bundler)

    (el-bundler:packages
     (:type "github" :name "daic-h/initialize")
     (:type "github" :name "emacsmirror/shell-pop"))

    (el-bundler:configure :dir "~/.emacs.d/elisp/bundle/" :smp 3)
    (el-bundler:initialize)

## Credits

 kiwanami/emacs-deffered plays an important feature of this library.