# load "ref0800-17088-73-2.gnu"
# chem = "bis(hydroxymethyl)peroxide"

set terminal postscript eps color
set title "ref = 800; chem = bis(hydroxymethyl)peroxide; casrn = 17088-73-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    4449.298     * exp(  -8384.840    *(1/   298.    -1/T))

set label "" at    283.1500    ,    19738.47     point
set label "" at    295.1500    ,    5921.540     point
set label "" at    298.1500    ,    4449.298     point ps 2 pt 6

plot [280:300] H(T)
