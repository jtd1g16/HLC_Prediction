# load "ref2290-_CAS-84.gnu"
# chem = "mercury dihydroxide"

set terminal postscript eps color
set title "ref = 2290; chem = mercury dihydroxide; casrn = _CAS-84"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    126.0611     * exp(  -4191.607    *(1/   298.    -1/T))

set label "" at    298.1500    ,    126.0611     point
set label "" at    283.1500    ,    265.4785     point
set label "" at    298.1500    ,    126.0611     point ps 2 pt 6

plot [280:300] H(T)
