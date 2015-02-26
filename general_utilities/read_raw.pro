function read_raw,filename,hdr

 ; -- assumes default mag order is i then v 

 hdr = read_dao_header(filename)

  nhdr = 3
  fmt = 'l,f,f,f,f,f,f,f,f'
 readcol,filename,id,x,y,f8,f8err,f6,f6err,chi,shp,format=fmt,skipline=nhdr
  n_in =  n_elements(x)

 str = {id:long(-99), x:0.0, y:0.0, f6:0.0, f6err:0.0, f8:0.0, f8err:0.0, col:0.0, colerr:0.0,$
	sharp:0.0, chi:0.0, flag:0, match_ind:-1}

 str_arr = replicate(str,n_in)
 str_arr.id  = id
 str_arr.x   = x
 str_arr.y   = y
 str_arr.f6  = f6
 str_arr.f8  = f8
 str_arr.col = f6-f8
 str_arr.f6err = f6err
 str_arr.f8err = f8err
 str_arr.colerr = sqrt(f6err^2+f8err^2)
 str_arr.sharp = shp
 str_arr.chi   = chi


 return,str_arr

end
