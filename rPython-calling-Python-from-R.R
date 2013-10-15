# Description : Using rPython to invoke Python code
# Website : http://statcompute.wordpress.com/2013/10/13/rpython-r-interface-to-python/

# Installation notes:
# You will need to install libpython-dev, python-dev to invoke this
# Otherwise you will get an error "python-config not found"
# Also install pandas packages using pip install pandas

doInstall <- TRUE
toInstall <- c("rPython")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

### load r data.frame ###
data(iris)
r_df1 <- iris
class(r_df1)
head(r_df1, n = 3)
### pass r data.frame to python dict ###
python.assign('py_dict1', r_df1)
python.exec('print type(py_dict1)')
### convert python dict to pandas DataFrame ###
python.exec('import pandas as pd')
python.exec('py_df = pd.DataFrame(py_dict1)')
python.method.call('py_df', 'info')
python.exec('print py_df.head(3)')
### convert pandas DataFrame back to dict ###
python.exec('py_dict2 = py_df.to_dict(outtype = "list")')
### pass python dict back to r list ###
r_list <- python.get('py_dict2')
class(r_list)
### convert r list to r data.frame ###
r_df2 <- data.frame(r_list)
class(r_df2)
head(r_df2, n = 3)