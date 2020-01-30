# load "ref3100-74-99-7.gnu"
# chem = "propyne"

set terminal postscript eps color
set title "ref = 3100; chem = propyne; casrn = 74-99-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6678484E-03 * exp(  -2054.011    *(1/   298.    -1/T))

set label "" at    294.2611    ,   0.7238719E-03 point
set label "" at    310.9278    ,   0.5079419E-03 point
set label "" at    344.2611    ,   0.2680393E-03 point
set label "" at    377.5944    ,   0.1553759E-03 point
set label "" at    298.1500    ,   0.6678484E-03 point ps 2 pt 6

plot [290:380] H(T)
