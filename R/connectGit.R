library(usethis)
library(devtools)
use_git()
use_github(protocol = 'https')
use_readme_rmd()       #after running this command, you will need to go 'knit' the readme file,
#                       might need to edit the example


#A note about PATs and RStudio and GitHub - - note that this might be old
# from: https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r

  # Now that you’ve created a Personal Access Token, we need to store it so that
  # RStudio can access it and know to connect to your GitHub account. The gitcreds_set()
  # function from the gitcreds package will help you here. You’ll enter your GitHub username
  # and the Personal Access Token as your password (NOT your GitHub password, as I initially
  # thought). Once you’ve done all of this, you have connected RStudio to GitHub!

