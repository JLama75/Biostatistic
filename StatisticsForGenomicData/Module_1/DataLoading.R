## ----- Load_data------------------------------------------------------------------
#connetion from the url document
#eset/expression set data R datas
con=url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bodymap_eset.RData")

#then load the file from that url
load(file=con)

#Once loaded close the connection
close(con)

## ----- expression_Set------------------------------------------------------------------
bm = bodymap.eset
bm

## ----- expression_data------------------------------------------------------------------
exp_data = exprs(bm)
dim(exp_data)
head(exp_data, n=5)

## ----- pheno_data------------------------------------------------------------------

pheno_data = pData(bm)
dim(pheno_data)
head(pheno_data)


##----- feature_data------------------------------------------------------------------

feature_data = fData(bm)
dim(feature_data)
head(feature_data)

dim(fData(bodymap.eset))
fData(bodymap.eset)[1:10,1]

##----- session_info------------------------------------------------------------------
devtools::session_info()
