### LICENCE.md
library(usethis)
use_gpl3_license()

### cran-comments.md
use_cran_comments()

### NEWS.md
library(newsmd)

my_news <- newsmd(text=c(paste0("## version 1.0.0"),
                         "", "---", "", "### NEWS.md setup", "",
                         "- added NEWS.md creation with [newsmd]", ""))

#my_news <- newsmd(file="NEWS.md", text=NULL)
my_news$add_version("1.1.0")
my_news$add_subtitle("Bugfixes")
my_news$add_bullet(c("Fix koplsCenterK",
                     "Fix koplsPredict",
                     "Fix plots"))
my_news$add_subtitle("Changes")
my_news$add_bullet(c("Add predict function",
                     "Add margin and softmax for confidence score",
                     "Improve vignettes",
                     "Add tests",
                     "Clean code"))
my_news$write()

#my_news <- newsmd(file="NEWS.md", text=NULL)
my_news$add_version("1.1.1")
my_news$add_subtitle("Changes")
my_news$add_bullet(c("Add loadings, VIP, RV, Y outputs for permuted models"))
my_news$write()
