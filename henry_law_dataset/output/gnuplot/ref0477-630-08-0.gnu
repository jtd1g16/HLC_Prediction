# load "ref0477-630-08-0.gnu"
# chem = "carbon monoxide"

set terminal postscript eps color
set title "ref = 477; chem = carbon monoxide; casrn = 630-08-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9760361E-05 * exp(  -1304.247    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.1144252E-04 point
set label "" at    293.1500    ,   0.1047577E-04 point
set label "" at    298.1500    ,   0.9689272E-05 point
set label "" at    303.1500    ,   0.9050239E-05 point
set label "" at    308.1500    ,   0.8531366E-05 point
set label "" at    298.1500    ,   0.9760361E-05 point ps 2 pt 6

plot [280:310] H(T)
