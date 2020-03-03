#' @section Overview:
#' 
#' This package attempts to automate as much as possible the creation and submission process of 
#' abstracts for the LatinR conference. You write in RMarkdown and all submission details are 
#' added to the yaml header. Then, before submission, the package does some basic checks, renders
#' the document using the provided LaTeX template and finally submits it. 
#' 
#' @section Creating a submission:
#' 
#' If you use RStudio, you can fo to File -> New File -> R Markdown -> From Template and 
#' select "LatinR submission article". This will create a skeleton document along with other 
#' helper files into the selected folder. 
#' 
#' You'll see that all the important data is managed by the yaml header. You can either populate it
#' manually or, a bit easier, use [latinr_wizard()] to launch a small user interface that will guide
#' you and finally give you a valid yaml header that you can copy and paste into your document. 
#' 
#' 
#' @section Submitting and article:
#' 
#' One you're happy with your article, it's time to submit it. First, if you haven't already, you'll
#' need to create an account at https://easychair.org/account/signup and then save your 
#' login information using [latinr_password_set()]. 
#' 
#' To submit your article use [latinr_submit()]. By default, it will search for .Rmd files in the 
#' current working directory and if it finds only one, it process it. First, you will be shown the 
#' submission information (such as author details, title, keywords, etc...) so you can check that 
#' everything is in order. Then, it will render and open the PDF so you can also check the finished
#' submission. 
#' 
#' If all it's OK, the article will be submitted. Be aware that this process is still a bit 
#' experimental so things can fail. It's VERY IMPORTANT that at the end of the process you go to
#' your account an make a final check so that you're sure that the submission information, as well as
#' the article PDF have been correctly submitted. If you find any problem, correct them right there 
#' on the website. 
#' 
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
