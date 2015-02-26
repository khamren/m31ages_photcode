pro plot_opt

 data = read_opt_nir()

 ps_open,'optical',/encapsulate

 !p.multi = [0,2,2,0]

 plot,data.jk,data.j,psym=3,yr=[21,14],xr=[0,3],ytit='J',xtit='J-K',$
  xthick=5,ythick=5,charthick=3,xstyle=1,ystyle=1

 plot,data.ri,data.i,psym=3,yr=[22,15],xr=[0,3],ytit='I',xtit='R-I',$
  xthick=5,ythick=5,charthick=3,xstyle=1,ystyle=1

 plot,data.jk,data.ct,psym=3,xr=[0,3],yr=[-1,1],xtit='J-K',ytit='CN-TiO',$
  xthick=5,ythick=5,charthick=3,xstyle=1,ystyle=1
 
 plot,data.ri,data.ct,psym=3,xr=[0,3],yr=[-1,1],xtit='R-I',ytit='CN-TiO',$
  xthick=5,ythick=5,charthick=3,xstyle=1,ystyle=1

 ps_close

end
