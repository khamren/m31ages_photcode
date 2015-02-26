procedure fillphot(infile, band)
#  Steve Majewski - Yerkes Observatory - May 1989

string	infile {prompt='information file '}
string	band	{prompt='band '}
struct  *alist

begin
string	iinfile, iband, template, junk, image, xband, fieldname, out1, bad
string 	cerror, tempo, temp3, shit, barf, name
int	night, holdnight , mmm, nnn, runnum, tot, pier
string	airmass, sigma, fwhmpsf, rmag, rcolor, starid, seqnum, ccdframe 
real	magerr, colerr, wt, merr, ut, exptime, s1


iinfile=infile
iband=band
runnum = 0
tot = 0
holdnight=1
print(" ALL MAG FILES MUST BE .mag.1 AND IN THIS DIRECTORY")
print(" TEMPLATE FILES MUST BE IN THIS DIRECTORY.")
print(" (Make sure templates nondegenerate and with correct colors and mags.")
print(" ***Make sure psfinfo file has two initial blank lines***")
print(" ***NOTE: This version of program allows E format in output weights! ***")
print("           (may need to edit to fix E's)                                ")

list=iinfile
#nnn=fscan(list, junk, junk)
#nnn=fscan(list,junk,junk,junk,junk,junk,junk,junk,junk,junk)
while (fscan (list, ccdframe, image, xband, night, fieldname, airmass, ut, exptime) != EOF) {
	if (xband == iband) {
		#print("eat")
		tempo=mktemp("tmp")
#		txdump (str(image)+".mag.1", 
#		"ID,MAG[2],MERR[2],PIER[2]", "yes", headers=no, parameters=yes, >tempo)

                fields(str(image)+".mag","1,4,5", lines="1-", quit_if_miss=no, print_file_n=no, > tempo)

		template=str(fieldname)+".template"		
		temp3=mktemp("tmp")
		#print("shit")
		#join (template, tempo, > temp3)  old iraf
		join (template, tempo, output=temp3, delim=" ", missing="Missing", maxchars=161,
shortest=yes, verbose=yes)
		#print("die")
		alist=temp3
		#print(template)
		nnn=0
	 	while (fscan(alist,starid,rmag,rcolor,magerr,colerr,name,shit,s1,merr,pier) !=EOF) {
		        
#                       print(log10(exptime))
                       exptime=real(exptime)
#                       print(s1,merr)
                       s1=s1+2.5*log10(exptime)
                       pier=0
			if (pier==0 && merr <8.99 && name!="psfstar") {
				if(night == holdnight) 
					runnum += 1
				else {
					runnum = 1
					holdnight = night
				}
#evaluate the weight as inverse of quadrature addition of measured error
#and quoted error in magnitudes.  The function is normalized so that the
#highest possible wt = 1.00
				if (merr < 0.001) merr = 0.001
				if (magerr < 0.001) magerr = 0.001
				wt = 0.0014142135624 / sqrt(merr**2 + magerr**2)
#
#for formatting purposes
				if (runnum <= 9)  seqnum = "  "+str(runnum)
				if (runnum >= 10 && runnum <= 99) {
					seqnum = " "+str(runnum)
					}
				if (runnum >= 100) seqnum = str(runnum)
				tot += 1				
			        bad=" "
			        if (pier != 0) bad="B"
			        out1=" "+str(ccdframe)+"-"+str(starid)+str(bad)
				print (out1," ",rmag," ",rcolor," ",airmass," ",s1," ",night,seqnum," ",ut," ",wt)
			}
		}
	delete (tempo, go_ahead=yes)
	delete (temp3, go_ahead=yes)
	}
}
print(" ")
print(" total number of objects = ", tot) 
end
