#!/bin/sh

#init git 
#git init

#add files 
git add  *.R *.sh *submit* 

#commit files 
git commit -m "update files with comments!"


#set the origin, only for the first time
#git remote add origin git@github.com:ychenatsinca/SPOT_CLASS.git

#add branch name, here is main 
#git branch -M main

#push commit files to the server/origin as master or branch 
#git push -u origin main

#git remote add origin git@github.com:ychenatsinca/SPOT_CLASS.git
#git branch -M main
git push -u origin main
