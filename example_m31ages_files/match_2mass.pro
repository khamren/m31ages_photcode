PRO match_2mass

 tmass = read_2mass(/ecut)
 data = read_nir(/ecut,/calib)

 ntot = djs_angle_match(tmass.ra,tmass.dec,data.ra,data.dec,dtheta=(1./3600.),mcount=nmatch,mindx=match_ind,mdist=match_dist)

 tmass_ind = where(match_ind ne -1)
 data_ind  = match_ind(where(match_ind ne -1))


 ps_open,'calibrate',/col
 loadct,39
 
 !p.multi=[0,2,1,0] 


 resistant_mean,(data(data_ind).k-tmass(tmass_ind).k),2.5,k_mean,k_sigma,k_rej,goodvec=k_no
 resistant_mean,(data(data_ind).j-tmass(tmass_ind).j),2.5,j_mean,j_sigma,j_rej,goodvec=j_no
  print,'Mean K offset = ',k_mean,' +/- ',k_sigma
  print,'Mean J offset = ',j_mean,' +/- ',j_sigma

 resistant_mean,(data(data_ind).jk-tmass(tmass_ind).jk),2.5,jk_mean,jk_sigma,jk_rej,goodvec=jk_no
  print,'Mean J-K offset = ',jk_mean,' +/- ',jk_sigma
  print,'Mean J - Mean K = ',j_mean-k_mean,' +/- ',sqrt(k_sigma^2+j_sigma^2)


; -- K 

 plot,tmass.k,data.k,/nodata,xtit='2MASS K',ytit='M31AGES K',xrange=[11,18],yrange=[11,18],charsize=1.5, $
  xthick=5,ythick=5,charthick=2,xstyle=1,ystyle=1,/isotropic
 oplot,findgen(15)+8,findgen(15)+8,linestyle=1
 oplot,findgen(15)+8,findgen(15)+8+k_mean,linestyle=1
 oploterror,tmass(tmass_ind).k,data(data_ind).k,tmass(tmass_ind).kerr,data(data_ind).kerr,psym=sym(1),symsize=.5,col=250

 plot,tmass.k,data.k,/nodata,xtit='2MASS K',ytit='M31AGES K - 2MASS K',xrange=[10,16],yrange=[k_mean-1.0,k_mean+1.0],charsize=1.5, $
   xthick=5,ythick=5,charthick=2,xstyle=1,ystyle=1
 oplot,findgen(15)+8,fltarr(15)+k_mean,linestyle=1
 oploterror,tmass(tmass_ind).k,data(data_ind).k-tmass(tmass_ind).k,tmass(tmass_ind).kerr,sqrt(tmass(tmass_ind).kerr^2+data(data_ind).kerr^2),psym=sym(1),symsize=.5,col=250

 xyouts,0.12,0.90,'Ks Calibration',/norm,charthick=2,charsize=1.5
 xyouts,0.12,0.85,'Ks Offset = '+string(k_mean,format='(f8.2)'),/norm,charthick=2,charsize=1.5
 xyouts,0.15,0.80,' with err on mean = '+string(k_sigma,format='(f8.5)'),/norm,charthick=2,charsize=1.5
 xyouts,0.15,0.75,' using '+string(n_elements(k_no),format='(i4)')+' matched objects',/norm,charthick=2,charsize=1.5
 xyouts,0.15,0.70,' for scatter of '+string(sqrt(float(n_elements(k_no)))*k_sigma,format='(f8.5)'),/norm,charthick=2,charsize=1.5

; -- J

 plot,tmass.j,data.j,/nodata,xtit='2MASS J',ytit='M31AGES J',xrange=[11,18],yrange=[11,18],charsize=1.5, $
  xthick=5,ythick=5,charthick=2,xstyle=1,ystyle=1,/isotropic
 oplot,findgen(15)+8,findgen(15)+8,linestyle=1
 oplot,findgen(15)+8,findgen(15)+8+j_mean,linestyle=1
 oploterror,tmass(tmass_ind).j,data(data_ind).j,tmass(tmass_ind).jerr,data(data_ind).jerr,psym=sym(1),symsize=.5,col=250
 
 plot,tmass.j,data.j,/nodata,xtit='2MASS J',ytit='M31AGES J - 2MASS J',xrange=[10,16],yrange=[j_mean-1.0,j_mean+1.0],charsize=1.5, $
    xthick=5,ythick=5,charthick=2,xstyle=1,ystyle=1
 oplot,findgen(15)+8,fltarr(15)+j_mean,linestyle=1
 oploterror,tmass(tmass_ind).j,data(data_ind).j-tmass(tmass_ind).j,tmass(tmass_ind).jerr,sqrt(tmass(tmass_ind).jerr^2+data(data_ind).jerr^2),psym=sym(1),symsize=.5,col=250

 xyouts,0.12,0.90,'J Calibration',/norm,charthick=2,charsize=1.5
 xyouts,0.12,0.85,'J Offset = '+string(j_mean,format='(f8.2)'),/norm,charthick=2,charsize=1.5
 xyouts,0.15,0.80,' with err on mean = '+string(j_sigma,format='(f8.5)'),/norm,charthick=2,charsize=1.5
 xyouts,0.15,0.75,' using '+string(n_elements(j_no),format='(i4)')+' matched objects',/norm,charthick=2,charsize=1.5
 xyouts,0.15,0.70,' for scatter of '+string(sqrt(float(n_elements(j_no)))*j_sigma,format='(f8.5)'),/norm,charthick=2,charsize=1.5

 

; -- J-K

  plot,tmass.jk,data.jk,/nodata,xtit='2MASS (J-K)',ytit='M31AGES (J-K)',xrange=[-0.5,1.5],yrange=[-0.5,1.5],charsize=1.5, $
   xthick=5,ythick=5,charthick=2,xstyle=1,ystyle=1,/isotropic
  oplot,findgen(15)-5,findgen(15)-5,linestyle=1
  oplot,findgen(15)-5,findgen(15)-5+jk_mean,linestyle=1
  oploterror,tmass(tmass_ind).jk,data(data_ind).jk,tmass(tmass_ind).jkerr,data(data_ind).jkerr,psym=sym(1),symsize=.5,col=250
  
  plot,tmass.j,data.j,/nodata,xtit='2MASS (J-K)',ytit='M31AGES (J-K) - 2MASS (J-K)',xrange=[-0.5,1.5],yrange=[jk_mean-1.0,jk_mean+1.0],charsize=1.5, $
     xthick=5,ythick=5,charthick=2,xstyle=1,ystyle=1
  oplot,findgen(15)-5,fltarr(15)+jk_mean,linestyle=1
  oploterror,tmass(tmass_ind).jk,data(data_ind).jk-tmass(tmass_ind).jk,tmass(tmass_ind).jkerr,sqrt(tmass(tmass_ind).jkerr^2+data(data_ind).jkerr^2),psym=sym(1),symsize=.5,col=250

 xyouts,0.10,0.90,'(J-Ks) Calibration',/norm,charthick=2,charsize=1.5
 xyouts,0.10,0.85,'(J-Ks) Offset = '+string(jk_mean,format='(f6.2)'),/norm,charthick=2,charsize=1.5
 xyouts,0.12,0.80,' with err on mean = '+string(jk_sigma,format='(f8.5)'),/norm,charthick=2,charsize=1.5
 xyouts,0.12,0.75,' using '+string(n_elements(jk_no),format='(i4)')+' matched objects',/norm,charthick=2,charsize=1.5
 xyouts,0.12,0.70,' for scatter of '+string(sqrt(float(n_elements(jk_no)))*jk_sigma,format='(f8.5)'),/norm,charthick=2,charsize=1.5
 xyouts,0.10,0.65,'(J-Ks) implied = '+string((j_mean-k_mean),format='(f6.2)'),/norm,charthick=2,charsize=1.5

 

 ps_close 

END
