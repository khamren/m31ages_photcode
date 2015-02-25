gfit1d ("@dellist",
"apcor.tab", function="legendre", order=1, xmin=INDEF, xmax=INDEF, ps=yes,
errorpars="", samplepars="", interactive=no, device="stdgraph", cursor="",
version="16Sep97")

tdump ("apcor.tab",
cdfile="STDOUT", pfile="STDOUT", datafile="apcor.lst", columns="coeff1",
rows="-", pwidth=120)

