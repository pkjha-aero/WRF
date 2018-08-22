;
;
;
;
;

u = fltarr(10)
v = fltarr(10)
ws = fltarr(10)
wd = fltarr(10)

u[0] = 3.0
u[1] = 3.9
u[2] = 2.4
u[3] = 1.5
u[4] = 4.2
u[5] = 1.7
u[6] = 3.6
u[7] = 2.9
u[8] = 5.2
u[9] = 6.4

v[0] = 8.3
v[1] = 9.2
v[2] = 3.5
v[3] = 7.0
v[4] = 6.4
v[5] = 8.1
v[6] = 8.1
v[7] = 7.7
v[8] = 9.5
v[9] = 6.9


for k = 0,9 DO BEGIN
 
ws[k] = sqrt( u[k]*u[k] + v[k]*v[k] )
wd[k] = u[k]/v[k]

endfor

usum = 0.0
vsum = 0.0
wssum = 0.0
wdsum = 0.0


umean = mean(u)
vmean = mean(v)

wsmean = mean(ws)

wdmean = mean(wd)

ws_after = sqrt( umean*umean + vmean*vmean )

wd_after = umean/vmean


print,wsmean,ws_after 

print,wdmean,wd_after 

end
