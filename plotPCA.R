# Usage: Rscript -i infile.covar -c component1-component2 -a annotation.file -o outfile.eps

library(optparse)
library(ggplot2)
library(ggrepel)

option_list <- list(make_option(c('-i','--in_file'), action='store', type='character', default=NULL, help='Input file (output from ngsCovar)'),
                    make_option(c('-c','--comp'), action='store', type='character', default=1-2, help='Components to plot'),
                    make_option(c('-a','--annot_file'), action='store', type='character', default=NULL, help='Annotation file with individual classification (2 column TSV with ID and ANNOTATION)'),
                    make_option(c('-o','--out_file'), action='store', type='character', default=NULL, help='Output file')
                    )
opt <- parse_args(OptionParser(option_list = option_list))

# Annotation file is in plink cluster format

#################################################################################

# Read input file
covar <- read.table(opt$in_file, stringsAsFact=FALSE);

# Read annot file
annot <- read.table(opt$annot_file, sep="\t", header=TRUE); # note that plink cluster files are usually tab-separated instead

# Parse components to analyze
comp <- as.numeric(strsplit(opt$comp, "-", fixed=TRUE)[[1]])
print("1")
print(comp)
# Eigenvalues
eig <- eigen(covar, symm=TRUE);
eig$val <- eig$val/sum(eig$val);
cat1<- cat(signif(eig$val, digits=3)*100,"\n");

print(cat1)

print("2")
print(eig)

# Plot
PC <- as.data.frame(eig$vectors)
colnames(PC) <- gsub("V", "PC", colnames(PC))
PC$Pop <- factor(annot$CLUSTER)
print("3")
print(PC)
title <- paste("PC",comp[1]," (",signif(eig$val[comp[1]], digits=3)*100,"%)"," / PC",comp[2]," (",signif(eig$val[comp[2]], digits=3)*100,"%)",sep="",collapse="")
print("4")
x_axis = paste("PC",comp[1],sep="")
y_axis = paste("PC",comp[2],sep="")
print("5")
print(x_axis)
print(y_axis)

PC$FID <-factor(annot$FID)
cbbPalette <- c("darkblue", "deepskyblue1", "goldenrod1", "darkorange3", "darkseagreen3", "forestgreen", "brown")
                #navy, orange, maroon, red, rosa, verde, blue, brown

ggplot() + coord_fixed(ratio=1) + geom_point(shape=17, data=PC, aes_string(x=x_axis, y=y_axis, colour="FID"), size=2) + geom_text_repel(data=PC, aes_string(x=x_axis, y=y_axis, label="Pop", colour="FID"), show.legend=F, size=2.5, nudge_x = 0.002, nudge_y = 0.002 , box.padding = unit(0.20, "lines")) + scale_colour_manual(values=cbbPalette) + ggtitle(title) + theme_classic() + theme(legend.position="top") + theme(legend.text = element_text(size = 16)) + theme(legend.title=element_blank())
  #+ coord_fixed(ratio=1)                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
#, box.padding = unit(0.30, "lines")_repel
#, nudge_x = 0.2, nudge_y = 0.2, box.padding = unit(0.30, "lines")

print("6")
ggsave(opt$out_file, width = 10, height = 10, limitsize = FALSE)
print("7")
unlink("Rplots.pdf", force=TRUE)

