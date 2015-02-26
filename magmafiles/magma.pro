;=====================================================================
;
;   Magma - Magnitudes out of Mike's Ass
;
;   This is a revised version of Steve Majewski's getmags program.  It is designed
;   to, given a set of transformation equations and a set of observations, iteratively
;   solve for color and magnitude in multiple passbands.  If you give it several 
;   observations in one passband, it will average the observations.
;   Please note that this is just a solution program.  It is your responsibility
;   to make sure you have all your ducks in a row - that the photometry 
;   solutions work and so forth.
;   Also note that any stars which has an indefinite color is solved
;   as though the color was zero.
;   Because transformation equations are put in separetely for each frame,
;   magma can happily combine data from different nights, different observing
;   runs and different telescopes.  The lion's share of hard work is actually
;   done by DAOMASTER or some other program.
;
;   In addition to solving for photometry, MAGMA has the capability to 
;   calculate and apply the offsets of invidual observations from the 
;   average of observations.  This allows non-photometric data to 
;   be integrated into the case, so long as there is sufficient overlap
;   with photometric data.  Non-photometric data is user designated.
;
;   MAGMA will also calculate the variability of stars (using the Welsh Stetson
;   technique as lifted bodily from DAOMASTER).  It is recommend that users fix
;   zero point offsets before looking for reliable variability data.
;
;    Note that this program is designed for overlapping frames.  Non-overlapping
;    frames should be solved separately.
;
;   CALLING SEQUENCE:  Magma,magfile,trans,transsig,magout,/multimag
;
;   magfile = a text file containing ID, position and magnitude and mag err
;	for all the stars.  They should already be matched.  THe last two
;	columns are chi and sharp (daophot) or dummy variables (apphot)
;	this program is designed to read in the filename.raw output from
;	Stetson's DAOMASTER program.  See "inmag.tst" for an example.
;
;   magout = the output file which will contain ID, position, mag, merr chi
;   and sharp for all stars.  Note that the list will be by filter in alphabetical
;   order.  See "outmag.test" for an example.
;
;   trans = the transformation equations.  This is a textfile containing, for each band
;	zero offset, airmass term, color term, color-airmass, color squared term,
;	airmass, aperture correction, timing correction, passband.
;	The variables should be in the same order as the magfile.  Put in 0.0
;	if the variable was not solved for.  The equation for V would be of 
;   	the form:
;
;   mV = V + v1 + v2 * XV + v3 * (B-V) + v4 * XV*(B-V) + v5 * (B-V) * (B-V)
;                 - (aperture correction) - (timing correction)
;
;             SEE MY FILE "bigtrain.tst" for an example
;
;	transsig = the transformation equation errors.  They should be in order
;	  of error in zero point, airmass term, color term, airmass-color and
;	 color-squared.  You can set these all to zero if you wish for observational
;	errors only.
;
;	One of the most common mistakes for users of MAGMA is to screw up the numbers.
;       Each line in the transform file should have a corresponding line in transsig
;	and two columns (magnitude and error) in the input file.
;
;
;   History:  1989:  getmags.for written by Majewski
;       1997/10/20 - Alpha testing commenced
;       1997/11/17 - First succesful alpha run. 
;	1997/11/18 - Beta testing commenced. 
;	1997/11/22 - Version 1.1 - photometric weighting scheme, aperture correction,
;		frame variation finder, timing correction added.  Error prop fixed.
;		Was causing all sorts of problems.
;	1997/11/24 - 1.101 - will now print frame offsets to file
;	1997/12/3 - 1.3 - frame offset procedure rewritten, index at top of photometry file
;				new menu added, frame names added
;       1997/12/12 - 1.302 - frame offset weighting added, frame offset cutoff 
;                    introduced, can now handle non-overlapping data.
;       1998/1/13 - 1.310 - CMD viewer added
;       1998/3/38 - 1.4 - offset software revision, variability parameter added
;			transformation uncertainties added, error averaging fixed
;	1998/6/14 - 1.5 - Fire Torpedoes, status checker and offset reset added
;       1998/11/11 - fixed bug in simplerr.  Derivative was wrong.
;	1999/7/6 - changed numstar to long integer so it can deal with catalogues
;		of 32000+ stars
;	1999/8/25 - 1.6 added variable plucking module, eliminated header, expanded id printing
;			to eight digits, added id offset option
;       2000/6/7 - will now automatically find number of stars and has option (multimag)
;                  to not print out variability parameters
;	2000/6/27 - cleaned up some inefficient for-do loops, changed printout to overwrite
;       2001/5/30 - changed read and print procedures so that x and y positions are now read in
;                   and put out as strings (although mastable remains unchanged).  This allows
;                   sexigesimal xy's to be used
;       2003/2/14 - 2.0 many many changes.  The variability module and frame offset
;                   modules were completely revised and replaced as past versions
;                   did both incorrectly.  Frame status and offset user interfaces
;                   streamlined and improved.  Significant change to now weight by
;                   flux error.
;
;		Future - Add option for direct solution?
;		read in and compare standards
;		option to kill mismatched stars or 3-sig variants or variables? 
;               print CMD to file
;		make CC diagram, options on error display for CMD (none, rising crosses,
;		-average in bin around mag-,sidebar horizontal and vertical
;
;    Note: Output filters are in alphabetical order. ***** <-=========****
;
;=====================================================================

common magcommon, passband, zpterm,amterm,colterm,amcolterm,colsqterm,$
		outbands,colband,colsign,airmass,numobs,numbands,$
		aptc,timecor,framename,photometric,frameoff,frameoffsig,offcut,$
		offnum,numstar,wsbands

common errcommon, zptermsig,amtermsig,coltermsig,amcoltermsig,colsqtermsig

FUNCTION BNRY,X,F
;=====================================================================
; I lifted this from PBS's DAOMASTER code. It finds the first item in a
; sorted list whose value is greater than F times largest value in array X
;=====================================================================

n=n_elements(x)-1
;set the target value
T = F*X[N]

;dummy check
if x[0] gt t then begin
   bnry=1
   return,bnry
endif
if x[n] lt t then begin
   bnry=0
   return,bnry
endif

;first step, set the pointer halfway along the array
K=max([1,floor(0.5*n)],k)
bnry=k

while x(bnry) lt t or x(bnry-1) gt t do begin
if x(bnry) lt t then bnry=min([n,bnry+k],bnry)
if x(bnry-1) gt t then bnry=max([1,bnry-k],bnry)
k=max([1,(k+1)/2],k)
endwhile
if x(bnry-1) eq t then bnry=bnry-1

return,bnry
end


PRO simplerr,inerr,outerr,cl,cler,bandnum
;=====================================================================
;
;   This propogates photometric error through the transformation equations
;   This version takes into account error in transformation constants, although
;   it assumes perfect airmass
;
; sigma(V)=sqrt[ sigma(mV)^2 + + sigma(v1)^2 + (XV*sigma(v2))^2+((B-V)*sigma(v3))^2 
;		+ (XV*(B-V)*sigma(v4))^2 + (B-V*sigma(v5))^2]
;                +(v3+v4*XV*+v5*2*(B-V))^2*sigma(B-V)^2               
;
;=====================================================================

common magcommon
common errcommon

if (inerr gt 9.9) then begin
   outerr=9.9999
endif else begin
   temper=inerr^2+zptermsig[bandnum]^2+(airmass[bandnum]*amtermsig[bandnum])^2
   temper=temper+(cl*coltermsig[bandnum])^2 + (airmass[bandnum]*cl*amcoltermsig[bandnum])^2
   temper=temper+(cl*cl*colsqtermsig[bandnum])^2
   temper=temper+(colterm[bandnum]+amcolterm[bandnum]*airmass[bandnum]+2*colsqterm[bandnum]*cl)^2*cler^2
   outerr=sqrt(temper)
endelse

end

PRO simplestar,inmag,outmag,colr,bandnum
;=====================================================================
;
;   This uses the solved colors and other terms to find the magnitude in each band
;
;   V = mV - v1 - v2 * XV - v3 * (B-V) - v4 * XV*(B-V) - v5 * (B-V) * (B-V)
;         +(aperture correction) + (time correction)
; 
;=====================================================================

common magcommon

if (inmag gt 99.9) then begin
    outmag=99.9999
endif else begin
   outmag=inmag-zpterm(bandnum)-amterm(bandnum)*airmass(bandnum)-colterm(bandnum)*colr
   outmag=outmag-amcolterm(bandnum)*airmass(bandnum)*colr-colsqterm(bandnum)*colr*colr
   outmag=outmag+aptc(bandnum)
   if (timecor(bandnum) gt 0) then begin
       outmag=outmag+2.5*alog10(timecor(bandnum))
   endif
endelse

end

PRO solvestar,instar,outstar,tempstar
;=====================================================================
;
;   The heart of the program.  This iteratively solves for each star.  It applies 
;   the transformation equation assuming a color of zero.  It then averages the 
;   passbands that are common, solves for the color and resolves for each
;   magnitude given the new color, gradually iterating until convergence.
;   
;   The solved magntide is in the form: (V in the example)
;   V = mV - v1 - v2 * XV - v3 * (B-V) - v4 * XV*(B-V) - v5 * (B-V) * (B-V)
;         +(aperture correction)+(time correction)
;
;  It sends back to the code outstar, an array of average values in each
;  passband and tempstar, an array of the individual solved magnitudes
;
;=====================================================================

common magcommon

clr=fltarr(numobs)
clrerr=fltarr(numobs)
outclr=fltarr(numbands)
outclrerr=fltarr(numbands)
tempstar=fltarr(2*numobs)
laststar=fltarr(numbands)

;transfer over the position,id,chi and sharp

outstar(0)=instar(0)
outstar(1)=instar(1)
outstar(2)=instar(2)
outstar(2*numbands+3)=instar(2*numobs+3)
outstar(2*numbands+4)=instar(2*numobs+4)

;First we set the color terms to zero, then solve for the magnitudes using
;simplestar

for a=0,numobs-1 do begin
	clr(a)=0
	clrerr(a)=0
	simplestar,instar(2*a+3),newmag,clr(a),a
	tempstar(2*a)=newmag
	simplerr,instar(2*a+4),newerr,clr(a),clrerr(a),a
	tempstar(2*a+1)=newerr
endfor

; Now output the average magnitudes in each passband

for b=0,numbands-1 do begin
	find=0
	totalmag=0
	totalerr=0
	totalwt=0
	rightband=where(passband(*) eq outbands[b],count)
	if count ne 0 then goodband=where(instar(2*rightband+3) lt 99.99,count)
	if count ne 0 then begin
	   goodband=rightband[goodband]
	   wt=1/tempstar(2*goodband+1)^2
	   totalwt=total(wt)
	   totalmag=total(tempstar(2*goodband)*wt)
	   totalerr=total(tempstar(2*goodband+1)^2*wt)
	   outstar(2*b+3)=totalmag/totalwt
	   outstar(2*b+4)=sqrt(1/totalwt)
	endif
	if count eq 0 then begin
	   outstar(2*b+3)=99.9999
	   outstar(2*b+4)=9.9999
	endif
endfor

;Now begin the iteration loop

niter=0
converge=0

for r=0,numbands-1 do begin
     laststar(r)=0
endfor

while (converge eq 0) do begin

; First set the color term.  Passbands with an indefinite color will have it set to zero.

	for d=0,numbands-1 do begin
		if (colsign(d) eq 1) then begin
			for e=0,numbands-1 do begin
				if (outbands(e) eq colband(d)) then begin
				     if ((outstar(2*e+3) gt 99.9) or (outstar(2*d+3) gt 99.9)) then begin
				         outclr(d)=0
				         outclrerr(d)=0
				     endif else begin
				        outclr(d)=outstar(2*d+3)-outstar(2*e+3)
;to avoid the color^2 recurssion, color error is is taken from instrumental errors
                                        getclrerr=where(passband(*) eq colband(d))
                                        checkclrerr=where(instar(2*getclrerr+4) lt 9.9,count)
                                        if count ne 0 then begin
                                          checkclrerr=getclrerr[checkclrerr]
                                          clrerrwt=1/instar(2*checkclrerr+4)^2
                                          totalclrerr=total(clrerrwt)
                                          outclrerr(d)=sqrt(1/totalclrerr)
                                        endif
				     endelse
				endif
			endfor
		endif else begin
			for e=0,numbands-1 do begin
				if (outbands(e) eq colband(d)) then begin
				     if ((outstar(2*e+3) gt 99.9) or (outstar(2*d+3) gt 99.9)) then begin
				        outclr(d)=0
				        outclrerr(d)=0
				     endif else begin
				        outclr(d)=outstar(2*e+3)-outstar(2*d+3)
;to avoid the color^2 recurssion, color error is is taken from instrumental errors
                                        getclrerr=where(passband(*) eq colband(d))
                                        checkclrerr=where(instar(2*getclrerr+4) lt 9.9,count)
                                        if count ne 0 then begin
                                          checkclrerr=getclrerr[checkclrerr]
                                          clrerrwt=1/instar(2*checkclrerr+4)^2
                                          totalclrerr=total(clrerrwt)
                                          outclrerr(d)=sqrt(1/totalclrerr)
					endif
				     endelse
				endif
			endfor
		endelse
	endfor

; Now associate each observed magnitude with the proper color

	for l=0,numobs-1 do begin
   	  for m=0,numbands-1 do begin
             if (passband(l) eq outbands(m)) then begin
	             clr(l)=outclr(m)
	             clrerr(l)=outclrerr(m)
	     endif
     	  endfor
	endfor

; Now resolve the star

	for f=0,numobs-1 do begin
	   simplestar,instar(2*f+3),newmag,clr(f),f
	   tempstar(2*f)=newmag
	   simplerr,instar(2*f+4),newerr,clr(f),clrerr(f),f
	   tempstar(2*f+1)=newerr
	endfor

; Now average the common passbands, output them to the outbound magnitude
; the iteration loop recycles until the solution is good
; we convert to fluxes for this part

for g=0,numbands-1 do begin
	rightband=where(passband(*) eq outbands(g),count)
	if count ne 0 then goodband=where(instar(2*rightband+3) lt 99.99,count)
	if count ne 0 then begin
	   goodband=rightband[goodband]
	   wt=1/tempstar(2*goodband+1)^2
	   fluxes=10^(-tempstar(2*goodband)/2.5)
	   fluxerr=0.921034*fluxes*tempstar[2*goodband+1]
	   fluxwt=1/fluxerr^2
	   totalwt=total(fluxwt)
           totalflux=total(fluxes*fluxwt)
	   totalerr=total(tempstar(2*goodband+1)^2*wt)
	   outstar(2*g+3)=-2.5*alog10(totalflux/totalwt)
	   outflxerr=sqrt(1/totalwt)
	   outstar(2*g+4)=1.0857*outflxerr/(totalflux/totalwt)
	endif
	if count eq 0 then begin
	   outstar(2*g+3)=99.9999
	   outstar(2*g+4)=9.9999
	endif
endfor

; Check for convergence

converge=1

for t=1,numbands-1 do begin
         if ((outstar(2*t+3) lt laststar(t)-.002) or (outstar(2*t+3) gt laststar(t)+.002)) then converge=0
endfor

if (converge eq 0) then begin
        for l=0,numbands-1 do begin
              laststar[l]=outstar[2*l+3]
        endfor
endif

;
; go up to 20 iterations, send out an error message if it doesn't converge
;
	if (niter gt 30) then rint,'Star', instar[0] ,' failed to converge.'
	niter=niter+1
	
endwhile

end

PRO varpick,startable,indystar,wstable
;=====================================================================
;
;   This is a subprocess designed to make variability measures, idenfity
;   variables by the WS index.  Then print out a file for each variable
;   star containing time, magnitude and magnitude error (suitable for
;   use in PDM or other such curve-fitting program).
;
;=====================================================================

common magcommon

;first, we calculate the WS variability index
;presumably, all the offsets have been applied to the data.  We know go
;passband by passband and calculate a running index
;again, we convert to fluxes for both calculation and errors

print,''
print,'Calculating variability indices. This may take some time ...'
print,''
for ll=0,numstar-1 do begin
wstable[ll]=1.0
sum=0.0d
sumwt=0.0d
nvar=0
for g=0,numbands-1 do begin
       if wsbands[g] eq 1 then begin
 	 rightband=where(passband(*) eq outbands(g),count)
  	 if count ne 0 then goodband=where(indystar(ll,2*rightband) lt 99.99,count)
 	 if count gt 1 then begin
	   goodband=rightband[goodband]
	   fluxes=10^(-indystar(ll,2*goodband)/2.5)
	   fluxerr=0.921034*fluxes*indystar[ll,2*goodband+1]
	   fluxwt=1/fluxerr^2
	   avflux=10^(-startable[ll,2*g+3]/2.5)
	   b=fluxes-avflux
	   sum=sum+total(fluxwt*abs(b)/fluxerr)
	   sumwt=sumwt+total(fluxwt)
	   nvar=nvar+count
	 endif
       endif
endfor
if nvar gt 1 then b=sqrt(nvar/(nvar-1))
if nvar gt 1 then wstable[ll]=1.2533*b*sum/sumwt
endfor


;set boolean.  Default WS cut is 5.0
varopt=0
wsvarcut=5.0
numvar=n_elements(where(wstable(*) gt wsvarcut))

print,''
print,'The variable fitting code is designed identify variables in the'
print,'photometry sample and print time, magnitude and error to file.'
print,'Your task is to pick your variable stars to save time via'
print,'The calculated Welch-Stetson index.'

datefile='Abort'
print,''
print,'What is the name of the file that has the Julian Date for each;
print,'frame (type Abort to quit)?'
read,datefile

;read in the Julian dates

if (datefile ne 'Abort') then begin
    jdtimes=dblarr(numobs)
    readcol,datefile,jdtime,format='d'
endif else begin
    vardone=1
endelse

while (varopt ne 5) do begin

    print,''
    print,'Do you wish to
    print,'1 - Plot WS index vs. magnitude'
    print,'2 - Pick cut in WS index'
    print,'3 - Print variable photometry to file'
    print,'5 - Quit'
    print,''
    read,varopt
    
    if (varopt eq 1) then begin
         goodmags=where(vstmags[*] lt 99)
        plot,vstmags[goodmags],wslist[goodmags],PSYM=3,xtitle='Magnitude',ytitle='WS index'
    endif
    
    if (varopt eq 2) then begin
        print,''
        read,wsvarcut, PROMPT='What is the new cut in the WS index? '
        numvar=n_elements(where(wslist(*) gt wsvarcut))
    endif
    
    
    if (varopt eq 3) then begin
        rrrfile='Magma.Lyrae'
        print,'What name do you want for your variable file?'
        read,rrrfile
        for sc=0,n_elements(vstmags)-1 do begin
            if (wslist[sc] gt wsvarcut) then begin
;find # of observations, create an array of solved magnitudes, 
;solve each one. Set array elements for julian data and 
;photometry
              get_lun,unit
                rrfile=strcompress(rrrfile+string(sc+1), /remove_all)
                openw,unit,rrfile
                for b=0,n_elements(wsmark)-1 do begin
                    clr=0.0
                    clrerr=0.0
                    if (vstcolmags[sc] gt 90) then begin
                       print,'Warning - star has no color!'
                    endif else begin
                       if (wscolsign eq 1) then begin
                           clr=vstmags[sc]-vstcolmags[sc]
                       endif else begin
                           clr=vstcolmags[sc]-vstmags[sc]
                       endelse
                       clrerr=sqrt(wsfuzz[sc,b]^2+collie[sc]^2)
                    endelse
                    simplestar,wsdata[sc,b],vstsimp,clr,wsmark[b]
                    simplerr,wsfuzz[sc,b],vstsimpe,clr,clrerr,wsmark[b]
                    if (vstsimp lt 90) then printf,unit,format='(f14.4,2(f9.5))',jdtime[wsmark[b]],vstsimp,vstsimpe 
                endfor
                close,unit
               free_lun,unit
            endif
        endfor
        varsfit=1
    endif
            
endwhile

end

PRO cmdshow,catlog,numcat
;=====================================================================
;
;   This shows the user a CMD of his data with appropriate axes
;   It will determine its own axis limits by the minimum and maximum of the
;   non-INDEF magnitudes and colors
;
;=====================================================================

common magcommon

; First the user gives it the Magnitude passband.  It is checked to make sure its there

foundy=-1
Maxis=''
while ((Maxis ne 'end') and (foundy eq -1)) do begin
    print,''
    print,'Enter the name of the magnitude (Y-axis) passband'
    read,Maxis, PROMPT='(type "end" to finish)'
    if (Maxis ne 'end') then begin
         foundy=-1
         for Cmax=0,numbands-1 do begin
              if (outbands(Cmax) eq Maxis) then foundy=Cmax
         endfor
         if (foundy eq -1) then begin
              print,''
              print,'There is no passband ',Maxis,', you theorist!'
         endif
    endif    
endwhile

; Same with color band

foundx=-1
Caxis=''
while ((Caxis ne 'end') and (foundx eq -1)) do begin
    print,''
    print,'Enter the name of the color (X-axis) passband'
    read,Caxis, PROMPT='(type "end" to finish)'
    if (Caxis ne 'end') then begin
         foundx=-1
         for Cmay=0,numbands-1 do begin
              if (outbands(Cmay) eq Caxis) then foundx=Cmay
         endfor
         if (foundx eq -1) then begin
              print,''
              print,'There is no passband ',Caxis,', you theorist!'
         endif
    endif    
endwhile

if (Caxis ne 'end') and (Maxis ne 'end') then  begin

; If everything's cool, we get the color sign and title

    print,''
    print,'Is it (1) ', outbands(foundy), ' - ', outbands(foundx)
    print,'Or (2) ', outbands(foundx), ' - ',  outbands(foundy), '?'
    plotsign=0
    read,plotsign
    print,''
    tits=''
    read,tits, PROMPT='What title do you want on the CMD (return for none)?'
 
; determine the min and max of the star magnitudes, eliminating INDEF mags

    yavail=where(catlog(*,2*foundy+3) lt 99)
    ylow=where(catlog(yavail,2*foundy+3) ne 0)
    ylim=max(catlog(yavail,2*foundy+3))
    ybas=min(catlog(ylow,2*foundy+3))
    ylab=strcompress(outbands(foundy) +' Magnitude')
    chan,0
     
; and plot the two cases, finding the proper limits, eliminating all stars
; with INDEF colors (>50 or <-50) label appropriately and plot

    if (plotsign eq 1) then begin
         colorstrm=fltarr(numobs)
         colorstrm=catlog(*,2*foundy+3)-catlog(*,2*foundx+3)
         xavail=where(colorstrm lt 50)
         xgood=where(colorstrm gt -50)
         xlim=max(colorstrm(xavail))
         xbas=min(colorstrm(xgood))
         xlab=strcompress(outbands(foundy)+' - '+outbands(foundx)+' Color')
         plot,/ynozero,catlog(*,2*foundy+3)-catlog(*,2*foundx+3),catlog(*,2*foundy+3),PSYM=3,$
            XRANGE=[xbas,xlim], YRANGE=[ylim,ybas],$
            title='Color-Magnitude Diagram',xtitle=xlab,ytitle=ylab
    endif else begin
         colorstrm=fltarr(numobs)
         colorstrm=catlog(*,2*foundx+3)-catlog(*,2*foundy+3)
         xavail=where(colorstrm lt 50)
         xgood=where(colorstrm gt -50)
         xlim=max(colorstrm(xavail))
         xbas=min(colorstrm(xgood))
         xlab=strcompress(outbands(foundx)+' - '+outbands(foundy)+' Color')
         plot,/ynozero,catlog(*,2*foundx+3)-catlog(*,2*foundy+3),catlog(*,2*foundy+3),PSYM=3,$
            XRANGE=[xbas,xlim], YRANGE=[ylim,ybas],$
            title=tits,xtitle=xlab,ytitle=ylab
    endelse
    
endif

;The user is then prompted to redefine the axes if he so wishes

boundopt=''

while (boundopt ne 'end') do begin
    print,''
    print,'Do you want to redefine the limits of the axes (type "end" to quit)?'
    read,boundopt
    if (boundopt ne 'end') then begin
        print,''
        print,'NOTE: IDL will often round to the nearest integer on plots'
        print,''
        print,'Enter the maximum magnitude to be plotted'
        read,ylim
        print,'Enter the minimum magnitude to be plotted'
        read,ybas        
        print,'Enter the maximum color to be plotted'
        read,xlim
        print,'Enter the minimum color to be plotted'
        read,xbas
        if (plotsign eq 1) then begin
            plot,/ynozero,catlog(*,2*foundy+3)-catlog(*,2*foundx+3),catlog(*,2*foundy+3),PSYM=3,$
                XRANGE=[xbas,xlim], YRANGE=[ylim,ybas],$
                title=tits,xtitle=xlab,ytitle=ylab
        endif else begin
            plot,/ynozero,catlog(*,2*foundx+3)-catlog(*,2*foundy+3),catlog(*,2*foundy+3),PSYM=3,$
               XRANGE=[xbas,xlim], YRANGE=[ylim,ybas],$
               title=tits,xtitle=xlab,ytitle=ylab
        endelse
    endif
endwhile

end

PRO pickframe
;=====================================================================
;
;   This asks the user which frames he wishes to modify and what
;   status he wants to give them
;
;=====================================================================

common magcommon

inkill=''
while (inkill ne 'end') do begin
    print,''
    print,'Enter the name of the frame you wish to modify'
    read,inkill, PROMPT='(type "end" to finish)'
    if (inkill ne 'end') then begin
         found=-1
         for lk=0,numobs-1 do begin
              if (framename(lk) eq inkill) then found=lk
         endfor
         if (found eq -1) then begin
           print,''
           print,'There is no frame ',inkill
         endif else begin
           if photometric(found) eq -1 then print,'Frame ',inkill,' is currently non-overlapping.'
           if photometric(found) eq 0  then print,'Frame ',inkill,' is currently non-photometric.'
           if photometric(found) eq 1  then print,'Frame ',inkill,' is currently photometric.'
           newstat=2
           while newstat lt -1 or newstat gt 1 do begin
             print,'New status (-1=non-overlap, 0=non-photometric,1=photometric)?'
             read,newstat
             photometric(found)=newstat
           endwhile
         endelse
    endif    
endwhile

checkframe

end

PRO checkframe
;=====================================================================
;
;   This prints the status of each frame.  It's pretty straight-forward.
;   It also allows the user to modify the status of the frames
;
;=====================================================================

common magcommon

    for immbb=0,numbands-1 do begin
        print,'           For passband ', outbands(immbb)
        print,'-------------------------------------------------'
        print,'  Photometric   Non-photometric   Not Overlapped '
        thisband=photometric[where(passband[*] eq outbands[immbb])]
        theseframes=framename[where(passband[*] eq outbands[immbb])]
        good=where(thisband eq 1,ngood)
        bad=where(thisband eq 0,nbad)
        neutral=where(thisband eq -1,nno)
        nf=max([ngood,nbad,nno],a)
        goodf=strarr(nf) & goodf[*]=''
        badf=strarr(nf) & badf[*]=''
        neutralf=strarr(nf) & neutralf[*] = ''
        if ngood gt 0 then goodf[0:ngood-1]=theseframes[good]
        if nbad gt 0 then badf[0:nbad-1]=theseframes[bad]
        if nno gt 0 then nf[0:nbad-1]=theseframes[neutral]
        for a=0,nf-1 do begin
            print,goodf[a],badf[a],neutralf[a],format='(a15,2a17)'
        endfor
        print,'---------------------------------------------------'
    endfor
    modify='n'
    print,'Do you want to modify the status of any frame?'
    read,modify
    if modify eq 'y' or modify eq 'Y' or modify eq 'yes' then pickframe

end

PRO getoff,measures
;============================================================================
; This new procedure calculates frame to frame offsets in the correct way
;in each passband, we set the temporary zero point of the offsets to the
;FIRST frame and calculate each frame's offset to that one.  We compile
;a running master list made of measures from the first frame, augmented
;with corrected measures from subsequent frames of any INDEF star
;============================================================================

common magcommon

erroff=fltarr(numobs)
for shark=0,numbands-1 do begin
    foundfirst=-10
    masterms=fltarr(numstar,2)
    for whale=0,numobs-1 do begin
       if passband[whale] eq outbands[shark] then begin
          if foundfirst eq -10 then begin 
            frameoff[whale]=0.00
            masterms[*,0]=measures[*,whale*2]
            masterms[*,1]=measures[*,whale*2+1]
            foundfirst=whale
          endif else begin
;find matched stars
             goodmags=where(masterms[*,1] lt offcut and $
                            measures[*,whale*2+1] lt offcut,count)
;find new stars unmeasured in the first frame
             newmags=where(masterms[*,1] ge offcut and $
                            measures[*,whale*2+1] lt offcut,newcount)
             offnum[whale]=count
             if offnum[whale] gt offnum[foundfirst] then offnum[foundfirst]=offnum[whale]
             if count ne 0 then begin
;get the weights
                wt=1/(masterms[goodmags,1])^2+(measures[goodmags,2*whale+1])^2
;and offsets
                off=(masterms[goodmags,0]-measures[goodmags,2*whale])
;sort by offset
                stwt=wt[sort(off)]
                stoff=off[sort(off)]
;make each weight cumulative
                nmeas=n_elements(wt)
                for manta=1,nmeas-1 do begin
                   stwt[manta]=stwt[manta]+stwt[manta-1]
                endfor
;set the frame offset to the weighted median
;;                frameoff[whale]=stoff[bnry(stwt,0.5)]
                halfway=min(where(stwt gt 0.5*stwt[nmeas-1]))
                frameoff[whale]=stoff[halfway]
;errors to the weighted sigma dispersions
                sig1=min(where(stwt gt 0.6915*stwt[nmeas-1]))
                sig2=min(where(stwt gt 0.3085*stwt[nmeas-1]))
                frameoffsig[whale]=stoff[sig1]-stoff[sig2]
             endif else begin
                photometric[whale]=-1
                frameoff[whale]=0.0
             endelse
;now update the master list with new measures
             if newcount ne 0 then begin
                masterms[newmags,0]=measures[newmags,2*whale]+frameoff[whale]
                masterms[newmags,1]=measures[newmags,2*whale+1]
             endif
          endelse
       endif
    endfor
endfor

end 

PRO setframe
;=====================================================================
;
;   This calculates the offset correction to all data from an average of the
; photometric observations in that passband.
;
;=====================================================================

common magcommon

;First, we find the average offset for the photometric frames in each band and
;out this into avgcoloff.  This provides the zero point from which to set the offset
;as we only want the offset to be calculated with respect to the average of
;photometric data

avgcoloff=fltarr(numbands)
avgcoloff(*)=0
numcoloff=intarr(numbands)
numcoloff(*)=0

for karl=0,numbands-1 do begin
     for zeppo=0,numobs-1 do begin
         if ((passband(zeppo) eq outbands(karl)) AND (photometric(zeppo) eq 1)) then begin
              avgcoloff(karl)=avgcoloff(karl)+frameoff(zeppo)
              numcoloff(karl)=numcoloff(karl)+1
         endif
     endfor
     if (numcoloff(karl) gt 0) then avgcoloff(karl)=avgcoloff(karl)/numcoloff(karl)
endfor

; then reset each offset to relative to the average of photometric frames

for moe=0,numbands-1 do begin
     for curly=0,numobs-1 do begin
          if (passband(curly) eq outbands(moe)) AND (photometric(curly) gt -1) then frameoff(curly)=frameoff(curly)-avgcoloff(moe)
     endfor
endfor

end 

PRO fixframe
;============================================================================
;To improve things around here, I've split this process.  This calculates and
; then applies the frame to frame offsets.  The correction is to the zero point 
;  term.  On the idea that V = V (bad) + offset, the offset correction is SUBTRACTED 
; from the zero point term.
;
;=========================================================================

common magcommon

; calculate offsets

setframe

; now apply the offset to the zero point term

for zztop=0,numobs-1 do begin
    if (photometric(zztop) gt -1) then zpterm(zztop)=zpterm(zztop)-frameoff(zztop)
endfor

end

PRO iwuzframed,noninter
;=====================================================================
;
;   This shows the frame to frame offsets and allows the user to designate
;   frames as photometric or non-photometric.  A noninteractive switch
;   allows the automatic solving option to blaze through this.
;
;=====================================================================
common magcommon

; first, get the offsets

setframe

;put in a marker for bad frames
badbad=strarr(numobs)
badbad[*]=''
badframes=where(photometric eq 0,count)
if count gt 0 then badbad[badframes]='*'
; then print 'em (complicated, huh?)
printopt=''

if noninter eq 0 then begin
  for lion=0,numbands-1 do begin
      print,'For passband ', outbands(lion),':'
      print,'-------------------------------------'
      print,'     Frame     Offset   Sigma  Stars '
      for tiger=0,numobs-1 do begin                        
         if (passband(tiger) eq outbands(lion)) then begin
            print,format='(a14,2f8.4,i6,a1)',framename(tiger),frameoff(tiger),$
                   frameoffsig[tiger],offnum(tiger),badbad[tiger]
         endif
      endfor
  endfor
             
; You can also print to file

  print,'Do you wish to print to file?'
  read,printopt
endif
             
if (printopt eq 'y') or (printopt eq 'yes') or (printopt eq 'Y') or (noninter eq 1) then begin

   get_lun,unit
   openw,unit,append=1,'offset.log'
   for lion=0,numbands-1 do begin
      printf,unit,'For passband ', outbands(lion),':'
      printf,unit,'-------------------------------------'
      printf,unit,'     Frame     Offset   Sigma  Stars '
      for tiger=0,numobs-1 do begin                        
         if (passband(tiger) eq outbands(lion)) then begin
            printf,unit,format='(a14,2f8.4,i6,a1)',framename(tiger),$
            frameoff(tiger),frameoffsig[tiger],offnum(tiger),badbad[tiger]
         endif
      endfor
   endfor
   close,unit
   free_lun,unit
endif
             
end


PRO  Magma,magfile,trans,transsig,magout
;=====================================================================
;
;   This is the main program.  It reas in the data from magfile and trans
;   after asking for user input, initializes variables and then starts 
;   solving for each star.
;
;=====================================================================

common magcommon
common errcommon

; 
;  First things first, let's read in the magnitudes.  BUT WAIT!  We have to 
;  know how many bands we have and which ones we need to average and
;  which ones we need to solve for.
;
print, 'Magma will solve for multi-passband, multi-observation photometry.'
print,''
numbands=0

;transform file is read in.  Every frame is designated as non-photometric or 
;photometric at user request.  Transform errors are read in.

readcol,trans,zpterm,amterm,colterm,amcolterm,colsqterm,airmass,aptc,timecor,passband,$
framename,format='d,d,d,d,d,d,d,d,a,a'
readcol,transsig,zptermsig,amtermsig,coltermsig,amcoltermsig,colsqtermsig,$
format='d,d,d,d,d'
numobs=n_elements(zpterm)
photometric=intarr(numobs)

preston=2
while ((preston lt 0) or (preston gt 1)) do begin
    print,''
    print,'Do you wish your frames to default as photometric(1) or non-photometric (0)'
    read,preston
    photometric(*)=preston
endwhile

; to avoid bias in frame-to-frame offset calculations, the user is asked to
; input a magnitude error cut for stars used in these calculations.
; these stars will still be solved, of course

print,''
print,'What is the maximum magnitude error for stars used in frame offset calculations'
read,offcut

numstar=0l
;the file is read in once, to count the number of stars and load up the 
;positions into string arrays
readcol,magfile,dummy,raarray,decarray,format='f,a,a'
numstar=n_elements(dummy)

mastable=fltarr(numstar,2*numobs+5)

;mastable is where everything is stored, id, x, y, unsolved magnitudes, chi sharp

get_lun,unit
openr, unit, magfile
for i=0l,numstar-1 do begin
    instr=' '
    inline=fltarr(2*numobs+5)
    readf, unit, instr
    reads,instr,inline
    mastable(i,0:2*numobs+4)=inline(0:2*numobs+4)
endfor
close, unit
free_lun,unit

; Make a subset of the passband for each unique one

outbands=passband(uniq(passband, sort(passband)))
numbands=n_elements(outbands)
colband=strarr(numbands)
colsign=intarr(numbands)

;set up variable so bad filters can be removed from variable star searches
wsbands=intarr(numbands)
wsbands[*]=1

; OK, so we now have everything read in.  We need to figure out which colors
; we want to use
;
print, ''
print, 'Now you need to tell us which bands to use with which in color determination.
for c=0,numbands-1 do begin
	print, ''
	print,'For passband ', outbands(c)
	print,'What passband is the color generated with?'
	inband2='     '
	read,inband2
	colband(c)=inband2
; need to check that it's there
	print,'Is it (1) ', outbands(c), ' - ', inband2
	print,'Or (2) ', inband2, ' - ',  outbands(c), '?'
	insign=0
	read,insign
	colsign(c)=insign
endfor

; now we enter into the menu loop.  The user can do various things, including
; 1 - solve for the magnitudes, 2 - calculate and print frame offsets,
; 3 - review status of frames, 4 - change status of frames, 
; 6 - print to file, 7 - show a CMD
; 10 - reset all offsets, 11 - pull
; variables from the file, 12 - change the ID #s, 
; or 14 run everything in automatic mode (solve, offset, solve, etc.)
; then pull out variables, print to file, quit)
; A solvedonce variable is used to ensure the user solves before trying to
;
; calculate frame offsets or print to file.  Framedonce makes sure the
; frame calculations have been made before being applied.

vardone=0
magopt=0
solvedonce=0

while (magopt lt 15) do begin

print,''
print,'Do you wish to:'
print,''
print,'1 - Solve for magnitudes using iteration'

print,'2 - Review the good/bad status of frames'
print,'3 - Change good/bad frame status'
if (solvedonce gt 0) then print,'4 - Show frame to frame offsets'
if (solvedonce gt 0) then print,'5 - Apply frame to frame offsets'
if (solvedonce gt 0) then print,'6 - Print solved magnitudes to file'
if (solvedonce gt 0) then print,'7 - Show CMD'
if (solvedonce gt 0) then print,'8 - Reset all frame offsets to zero'
if (solvedonce gt 0) then print,'9 - Pluck variables from the sample'
print,'10 - Change passbands used in variability calculation'
print,'12 - Offset ID numbers'
print,'14 - Automated Solution'
print,'15 - exit'

print,''
read,magopt

if solvedonce eq 0 then begin
;initialize arrays
    goodstar=fltarr(2*numbands+5)
    manystars=fltarr(2*numobs)
    startable=fltarr(numstar,2*numbands+5)
    indystar=fltarr(numstar,2*numobs)
    offtally=fltarr(numobs)
    offnum=fltarr(numobs)
    frameoff=fltarr(numobs)
    frameoff[*]=0
    frameoffsig=fltarr(numobs)
    frameoffsig[*]=0.
    offnum=intarr(numobs)
    offnum[*]=0
endif

if (magopt eq 1) then begin

; Ah, the meat of the program.  One by one, each star is popped off of
; the observation file, thrown into the solution engine (SOLVESTAR), solved
; and then brought out as an average solution (goodstar) and individual measures
; (indystar).  After that, frame to frame residuals are calculated
;indystar is where the individual solved magnitudes will be stored

    print,''
    print,'solving . . . '
    print,''

    for d=0l,numstar-1 do begin
         solvestar,mastable(d,*),goodstar,manystars
         startable(d,*)=goodstar(*)
         indystar(d,*)=manystars(*)
    endfor
    getoff,indystar
    solvedonce=1

endif

if (magopt eq 2) then begin
     checkframe
endif

if (magopt eq 3) then begin
     pickframe
endif

if (magopt eq 4) then begin
     iwuzframed,0
endif

if (magopt eq 5) then begin
     fixframe
endif

if (magopt eq 6) then begin

;Header information is printed (an index of columns) and then
;the stars, one by one
;now set up for the various prinitng options

    get_lun,unit
    openw,unit,magout
    for d=0l,numstar-1 do begin
         if vardone eq 0 then begin
           printf,unit,format='(i8,1x,a14,1x,a14,1x,50(f9.4,:))',startable(d,0),raarray[d],decarray[d],$
              startable[d,3:2*numbands+4]
         endif
         if vardone eq 1 then begin
           printf,unit,format='(i8,1x,a14,1x,a14,1x,50(f9.4,:))',startable(d,0),raarray[d],decarray[d],$
              startable[d,3:2*numbands+4],wstable(d)
         endif
    endfor
    close,unit
    free_lun,unit

endif

if (magopt eq 7) then cmdshow,startable,numstar

if (magopt eq 8) then begin
;You have to re-read the transformation constants.
    readcol,trans,zpterm,amterm,colterm,amcolterm,colsqterm,airmass,aptc,timecor,passband,$
    framename,format='d,d,d,d,d,d,d,d,a,a'
    frameoff[*]=0
    frameoffsig[*]=0
    offnum[*]=0
endif

if (magopt eq 9) then begin
    wstable=fltarr(numstar)
    varpick,startable,indystar,wstable
    vardone=1
endif

if (magopt eq 10) then begin
    for chty=0,numbands-1 do begin
      print,'Should the variable star measures include the ', outbands[chty], ' passband?'
      print,'(0=no, 1=yes)'
      read,chtyy
      wsbands[chty]=chtyy
    endfor
endif

if (magopt eq 12) then begin
    idnumoffset=0l
    print,'What integer value do you wish to add to every ID number?'
    print,'New ID number = old number + offset'
    read,idnumoffset
    mastable[*,0]=mastable[*,0]+idnumoffset
    if (solvedonce) gt 0 then startable[*,0]=startable[*,0]+idnumoffset
endif

if (magopt eq 14) then begin
     print,''
     print,'This is an automated routine that will solve photometry, calculate'
     print,'offsets, apply them and resolve -- iterating until the offsets shrink'
     print,'to a level you will specify.  This should take a single iteration.'
     print,'There are a number of command line switches.  This automated routine'
     print,'can solve for variables, print to file and even identify non-photometric'
     print,'frames and remove them from the zero point setting, although it is'
     print,'highly recommend that you do that beforehand.  If you are ready 
     print,'to forge ahead, type one of the following commands:'
     print,''
     print,'Go -- solve photometry'
     print,'Cat -- solve photometry and print the catalogue to file'
     print,'Var -- solve photometry, identify variables and print to file'
;;     print,'Fix -- solve photometry, identify potential non-photometric frames'
;;     print,'        and correct for them (not recommended)'
     print,'Type any other command to abort.'
     print,''
     hahaha='Abort'
     read,hahaha, PROMPT='Choice?'
     if (hahaha eq 'Go') or (hahaha eq 'Cat') or (hahaha eq 'Var') or $
        (hahaha eq 'Fix') then begin
         cong=1.0
         print,''
         print,'What is the maximum absolute value of frame-to-frame offset that you'
         print,'want?'
         read,cong, PROMPT='-=>'
         torpconv=0
         torpniter=1
         while (torpconv eq 0) do begin

	    print,''
	    print,'Fire ',torpniter,'!'
            print,''
;Solve each star, continuing the lame-ass humor.

            for d=0l,numstar-1 do begin
              solvestar,mastable(d,*),goodstar,manystars
              startable(d,*)=goodstar(*)
              indystar(d,*)=manystars(*)
            endfor
            getoff,indystar
            setframe
            
;check for convergence, print offsets to file and correct them
;Per MHS, reversed order of "max" and "abs" in second statement below
;without the change "the only effect is to cause a false convergence if all your photometric 
;offsets happen to be negative"
             if ((max(frameoff) lt cong) and (max(abs(frameoff)) lt cong)) then begin
                torpconv=1
            endif else begin
                iwuzframed,1
                fixframe
            endelse
            
            torpniter=torpniter+1
            solvedonce=1 
         endwhile
     endif

;Header information is printed (an index of columns) and then
;the stars, one by one
;now set up for the various prinitng options

    if (hahaha eq 'Var') then begin
      wstable=fltarr(numstar)
      varpick,startable,indystar,wstable
      vardone=1
    endif
    
    if (hahaha eq 'Cat') or (hahaha eq 'Var') then begin
      get_lun,unit
      openw,unit,magout
      for d=0l,numstar-1 do begin
         if vardone eq 0 then begin
           printf,unit,format='(i8,1x,a14,1x,a14,1x,50(f9.4,:))',startable(d,0),raarray[d],decarray[d],$
              startable[d,3:2*numbands+4]
         endif
         if vardone eq 1 then begin
           printf,unit,format='(i8,1x,a14,1x,a14,1x,50(f9.4,:))',startable(d,0),raarray[d],decarray[d],$
              startable[d,3:2*numbands+4],wstable(d)
         endif
      endfor
      close,unit
      free_lun,unit
    endif

endif

endwhile
    
end
