# test git commit
#
library(lubridate)
now()
date()

Turn Git on for a folder
$ git init

Check status of changes to a repository
$ git status

View changes to files
$ git diff

Add a file's changes to be committed
$ git add <FILENAME>
To add all files changes
$ git add

To commit (aka save) the changes you've added with a short message describing the changes
$ git commit -m "<your commit message>"


$ git config. For instance, 'JLord' isn't the same as 'jlord'

To change your username set with Git, just do the same command you did earlier, but with the correct capitalization:

$ git config --global user.username <USerNamE>
 git config --global user.username iphrank

 git remote add origin https://github.com/iphrank/myrepo.git

 git push origin master

 Add remote connections (multiple);
$ git remote add <REMOTENAME> <URL>

Set a URL to a remote
$ git remote set-url <REMOTENAME> <URL>

Pull in changes
$ git pull <REMOTENAME> <BRANCHNAME>

View remote connections
$ git remote -v

Push changes
$ git push <REMOTENAME> <BRANCH>

When you fork a repository, you're creating a copy of it on your GitHub account. Your fork begins its life as a remote repository. Forks are used for creating your own version of a project or contributing back fixes or features to the original project.

Once a project is forked, you then clone (aka copy) it from GitHub to your computer to work on locally.

Clone Fork Locally

Now, in terminal, clone the repository onto your computer. It will create a new folder for the repository so no need to create one. But make sure you aren't cloning it inside of another Git repository folder! So, if you're still inside of the 'hello-world' repository you worked in on the earlier challenges, back out of that folder. You can leave the folder you're in (and be in the folder that it was inside of) by 'changing directory' with two dots:

$ cd ..

Now clone:
$ git clone <URLFROMGITHUB>

Go into the folder for the fork it created (in this case, named 'patchwork')

$ cd patchwork

Connect to the Original Repository

But what if the original repository you forked from changes? You'll want to be able to pull in those changes too. So let's add another remote connection, this time to the original, github.com/jlord/patchwork, repository with its URL, found on the right hand side of the original on GitHub.

You can name this remote connection anything you want, but often people use 'upstream', let's use that for this.

$ git remote add upstream https://github.com/jlord/patchwork.git


git remote add upfork https://github.com/iphrank/patchwork.git
git push

fatal: The current branch master has no upstream branch.
To push the current branch and set the remote as upstream, use

    git push --set-upstream origin master

    GitHub Pages

GitHub will automatically serve and host static website files in branches named 'gh-pages'. Since the project you forked creates a website, its main branch is 'gh-pages' instead of 'master'. All sites like this can be found using this pattern for the URL:

http://githubusername.github.io/repositoryname

Check status of changes to a repository
$ git status
View changes to files
$ git diff
Add a file's changes to be committed
$ git add <FILENAME>
To add all files changes
$ git add .
To commit (aka save) the changes you've added with a short message describing the changes
$ git commit -m "<your commit message>"

A common error is not having your GitHub username match the case of the one you set with git config. For instance, 'JLord' isn't the same as 'jlord'
To change your username set with Git, just do the same command you did earlier, but with the correct capitalization:

$ git config --global user.username <USerNamE>

Add remote connections
$ git remote add <REMOTENAME> <URL>
Set a URL to a remote
$ git remote set-url <REMOTENAME> <URL>
Pull in changes
$ git pull <REMOTENAME> <BRANCHNAME>
View remote connections
$ git remote -v
Push changes
$ git push <REMOTENAME> <BRANCH>


Add remote connections
$ git remote add <REMOTENAME> <URL>
View remote connections
$ git remote -v

You can create and switch to a branch in one line:
$ git checkout -b <BRANCHNAME>
Create a new branch:
$ git branch <BRANCHNAME>
Move onto a branch:
$ git checkout <BRANCHNAME>
List the branches:
$ git branch
Rename a branch you're currently on:
$ git branch -m <NEWBRANCHNAME>
Verify what branch you're working on
$ git status

Check Git status
$ git status
Pull in changes from a remote branch
$ git pull <REMOTENAME> <REMOTEBRANCH>
See changes to the remote before you pull in
$ git fetch --dry-run

Merge a branch into current branch
$ git merge <BRANCHNAME>
Change the branch you're working on
$ git checkout <BRANCHNAME>
Delete a local branch
$ git branch -d <BRANCHNAME>
Delete a remote branch
$ git push <REMOTENAME> --delete <BRANCHNAME>
Pull from a remote branch
$ git pull <REMOTENAME> <BRANCHNAME>
