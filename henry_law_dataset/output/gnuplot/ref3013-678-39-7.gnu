# load "ref3013-678-39-7.gnu"
# chem = "8-2FTOH"

set terminal postscript eps color
set title "ref = 3013; chem = 8-2FTOH; casrn = 678-39-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1977985E-03 * exp(  -3137.340    *(1/   298.    -1/T))

set label "" at    309.6000    ,   0.1318257E-03 point
set label "" at    314.5000    ,   0.1174898E-03 point
set label "" at    319.6000    ,   0.9772372E-04 point
set label "" at    323.0000    ,   0.8709636E-04 point
set label "" at    298.1500    ,   0.1977985E-03 point ps 2 pt 6

plot [300:330] H(T)
