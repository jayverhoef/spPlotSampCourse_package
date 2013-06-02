# Roxygen to create help files
cd "/media/Hitachi2GB/00NMML/RPackages/spPlotSampCourse_package"
R
library(roxygen2)
roxygenize("spPlotSampCourse", roclets = c("namespace","rd"))

# build R package in Linux
cd "/media/Hitachi2GB/00NMML/RPackages/spPlotSampCourse_package"
R CMD build spPlotSampCourse
R CMD INSTALL spPlotSampCourse_1.0.tar.gz

# knitr
cd "/media/Hitachi2GB/00NMML/RPackages/spPlotSampCourse_package/spPlotSampCourse/inst/doc/IntroToSpatialStat"
R
library(knitr)
knit("IntroToSpatialStat.Rnw")

# windows 32-bit using VirtualBox
cd /home/jay/00BoiseCorvallis/splmm_package
Rcmd INSTALL --build splmm

#-------------------------------------------------------------------------------
#                    GITHUB
#-------------------------------------------------------------------------------

#github cloud commits
cd "/media/Hitachi2GB/00NMML/RPackages/spPlotSampCourse_package"
git add .
git commit -m 'update 2 June 2013 -- still adding content'
git push github.spPlotSampCourse_package master
git pull github.spPlotSampCourse_package master

#to see an earlier version
cd "/media/Hitachi2GB/00NMML/RPackages/spPlotSampCourse_package"
git log splmm.R
git show c742d1026aa031baf9409c0262b5bc944bb736d1:./covParmIni.R

#initializing stuff
cd "/media/Hitachi2GB/00NMML/RPackages/spPlotSampCourse_package"
git init
git config --global jay.verhoef 'Jay Ver Hoef'
git config --global user.email jay.verhoef@gmail.com
git remote add github.spPlotSampCourse_package https://github.com/jayverhoef/spPlotSampCourse_package.git
git remote -v

#clean up directory before committing it
find . -type f -iname \*~ -delete
