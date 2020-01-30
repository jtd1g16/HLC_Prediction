# load "ref0727-75-89-8.gnu"
# chem = "2,2,2-trifluoroethanol"

set terminal postscript eps color
set title "ref = 727; chem = 2,2,2-trifluoroethanol; casrn = 75-89-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5813584     * exp(  -5863.501    *(1/   298.    -1/T))

set label "" at    288.0000    ,    1.185644     point
set label "" at    298.0000    ,   0.5746758     point
set label "" at    308.0000    ,   0.3042932     point
set label "" at    318.0000    ,   0.1738331     point
set label "" at    298.1500    ,   0.5813584     point ps 2 pt 6

plot [280:320] H(T)
