;
;
;
; .r plot_k_profile
; 
;

z = FINDGEN(110)*10.0
sf = FLTARR(110)
sf1 = FLTARR(110)
sf2 = FLTARR(110)

pblh = 1000.0

prtz = pblh 
sfs = 4.0*prtz/27.0             ;maximum value of sf(z) = z*(1-z/h)^2 
prtnk = 1

FOR k=1,100 DO BEGIN
   sf[k] = (z[k]*(1.0-z[k]/prtz)^2)/sfs
   sf1[k] = (z[k]*(1.0-z[k]/prtz)^3)/sfs
   sf2[k] = (z[k]*(1.0-z[k]/prtz)^0.5)/sfs
   IF ( z[k] GT pblh ) THEN sf[k] = 0.0

ENDFOR


plot,sf[*],z[*]
oplot,sf1[*],z[*],linestyle=1
oplot,sf2[*],z[*],linestyle=2


END
