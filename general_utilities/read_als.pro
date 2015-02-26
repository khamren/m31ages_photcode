function read_als,filename,hdr

  hdr = read_dao_header(filename)

  nhdr = 3
  fmt = 'l,f,f,f,f,f,f,f,f'
 readcol,filename,id,x,y,mag,magerr,sky,iter,chi,sharp,format=fmt,skipline=nhdr
  n_in =  n_elements(x)

 str = {id:long(-99), x:0.0, y:0.0, mag:0.0, magerr:0.0, iter:0.0, sky:0.0, sharp:0.0, chi:0.0}

 str_arr = replicate(str,n_in)
 str_arr.id  = id
 str_arr.x   = x
 str_arr.y   = y
 str_arr.mag  = mag ;+ 9.15
 str_arr.magerr = magerr ;+ 8.26
 str_arr.sharp = sharp
 str_arr.sky = sky
 str_arr.chi   = chi
 str_arr.iter   = iter

 return,str_arr

end
