# EmacsConfig

## Structure of emacs configuration files

The `.emacs` file contains nothing more than a function call of function `load-file` loading relevant files in my GitHub repository.

* The file `more.el` prepares functions which are utilized in other `more-xxx.el` files.

* For each language I use, there is a seperate preparation of special functions as well as key-binding configurations.

* Python is used most often. In `more-python.el` the configurations make my Python work-flow a little bit like ipython-notebook in plain-text with very flexible editing.