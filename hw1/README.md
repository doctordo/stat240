
# Comments:
To edit - use the .Rnw file. The .tex file is created automatically, and shouldn't be used for editing. All code should be written between "< < chunk_name > > =" and "@" symbols, and if you do not want to print the code to the pdf, add the < < chunk_name, echo=FALSE > > = option. 

For knitting, the relevant calls are probably:
q1$mean.nr
q1$mean.wr
q1$se.nr
q1$se.wr
q1$hist.nr
q1$hist.wr

q2$mean.nr
...
...
...

q4$hist.wr
