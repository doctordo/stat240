
# Comments:
To edit - use the .Rnw file. The .tex file is created automatically, and shouldn't be used for editing. All code should be written between "< < chunk_name > > =" and "@" symbols, and if you do not want to print the code to the pdf, add the < < chunk_name, echo=FALSE > > = option. 

For knitting, the relevant calls are probably: <br />
q1$mean.nr <br />
q1$mean.wr <br />
q1$se.nr <br />
q1$se.wr <br />
q1$hist.nr <br />
q1$hist.wr <br />

q2$mean.nr <br />
... <br />
... <br />
...

q4$hist.wr
