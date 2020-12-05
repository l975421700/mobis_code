#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jul 19 10:13:41 2020

@author: gao
"""



# =============================================================================
# region python version management
# https://opensource.com/article/19/5/python-3-default-mac

# install pyenv for python environment management
brew install pyenv
# brew upgrade pyenv

# Check latest available python version and install
pyenv install --list
pyenv install 3.8.3

# check current default python version and set
pyenv version

pyenv global 3.8.3

python3 --version

# The power of pyenv comes from its control over our shell's path.
# In order for it to work correctly,
# we need to add the following to our configuration file
# (.zshrc for me, possibly .bash_profile for you):
echo -e 'if command -v pyenv 1>/dev/null 2>&1; then\n  eval "$(pyenv init -)"\nfi' >> ~/.zshrc

# After that command, our dotfile (.zshrc for zsh or .bash_profile for Bash) should look include these lines:
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

# start a new terminal instance, remove the aliases we used in the sections above
exec $0
which python
python -V
pip -V

# update pip
pip install --upgrade pip

# check available python version on local computer
ls ~/.pyenv/versions/
# remove certain version
rm -rf ~/.pyenv/versions/3.8.2

# endregion
# =============================================================================


# =============================================================================
# region python environment management

# conda create --name mobis
# conda env list
# conda env remove -n mobis

source /nas/qigao/miniconda3/bin/activate
conda activate mobis

conda install pip



# endregion
# =============================================================================


# =============================================================================
# region install packages
conda activate mobis

pip install radian




# endregion
# =============================================================================








