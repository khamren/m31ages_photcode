pro plot_nir

 data = read_nir(/ecut)
 ps_open,'n147_prephot_preview',/col,/encapsulate
 loadct,39

 pos1 = [0.08,0.58,0.53,0.98]
 pos2 = [0.53,0.58,0.98,0.98]
 pos3 = [0.08,0.08,0.53,0.5 ]
 pos4 = [0.53,0.08,0.98,0.5 ]


 m31 = where(data.jk gt 1.1 and data.jk lt 1.5)
 agb = where(data.jk gt 2.0)

  off = 1./60.
 xr = [max(data.ra)+off,min(data.ra)-off]
 yr = [min(data.dec)-off,max(data.dec)+off]
 xtit = Textoidl('RA (J2000)')
 ytit = Textoidl('Dec (J2000)')

 plot,data.ra,data.dec,/nodata,yr=yr,xr=xr,xstyle=1,ystyle=1,pos=pos1,$
  charsize=1.2,charthick=3,xthick=5,ythick=5,xtit=xtit,ytit=ytiti
 oplot,data.ra,data.dec,psym=sym(1),symsize=0.1 

 plot,data.ra,data.dec,/noerase,/nodata,yr=yr,xr=xr,xstyle=1,ystyle=1,pos=pos2,$
   charsize=1.2,charthick=3,xthick=5,ythick=5,xtit=xtit,ytit=' ',ytickname=replicate(' ',5)
 oplot,data(m31).ra,data(m31).dec,psym=sym(1),symsize=0.1
 oplot,data(agb).ra,data(agb).dec,psym=sym(1),symsize=0.3,col=250


 yr = [max(data.j),18.]
 xr = [0.1,max(data.jk)]
 xtit = Textoidl('J-K_s')
 ytit = textoidl('J')

 plot,data.jk,data.j,/noerase,/nodata,yr=yr,xr=xr,xstyle=1,ystyle=1,pos=pos3,$
  charsize=1.2,charthick=3,xthick=5,ythick=5,xtit=xtit,ytit=ytit
 oplot,data.jk,data.j,psym=sym(1),symsize=0.1

 plot,data.jk,data.j,/noerase,/nodata,yr=yr,xr=xr,xstyle=1,ystyle=1,pos=pos4,$
  charsize=1.2,charthick=3,xthick=5,ythick=5,xtit=xtit,ytit=' ',ytickname=replicate(' ',10)
 oplot,data(m31).jk,data(m31).j,psym=sym(1),symsize=0.1
 oplot,data(agb).jk,data(agb).j,psym=sym(1),symsize=0.3,col=250



 ps_close

end
