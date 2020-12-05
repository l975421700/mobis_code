
'''
These commands is for MAC OS
'''

# Install Git 
git --version

brew install git

download latest version from:
https://git-scm.com/ download/mac

# Set identity
git config --global user.name "l975421700"
git config --global user.email gaoqg229@gmail.com

# set default editor
git config --global core.editor emacs

# Clone a git repository
git clone https://github.com/l975421700/DEoAI_code.git



# Add files and commit “add precisely this content to the next commit”
git add python/ProGit_command.py
git commit -m 'Initial project version'


# Checking the Status of Your Files
git status

# Ignoring Files
echo .DS_Store >> .gitignore
# remove all file endig with .DS_Store
find . -name .DS_Store -print0 | xargs -0 git rm -f --ignore-unmatch

# Viewing Your Staged and Unstaged Changes
git diff

# replenish a commit
git commit --amend

# Unstaging a Staged File
git reset HEAD <file>

# discard changes in working directory
git checkout -- <file>


# download
git fetch origin
git pull 

# upload
git push origin master


# Show remote branch
git remote show origin


# Creating a New Branch
git branch testing
# Switching Branches
git checkout testing
# creating and switching branches
git checkout -b <newbranchname>

# check graphical representation of branches
git log --oneline --decorate --graph --all

# Rebase for a linear history
git checkout experiment
git rebase master
git checkout master
git merge experiment

# more complicated rebase
git rebase --onto master server client



