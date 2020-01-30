# load "ref3033-7803-51-2.gnu"
# chem = "phosphorus trihydride"

set terminal postscript eps color
set title "ref = 3033; chem = phosphorus trihydride; casrn = 7803-51-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5948722E-04 * exp(  -3040.245    *(1/   298.    -1/T))

set label "" at    276.1500    ,   0.1367020E-03 point
set label "" at    283.1500    ,   0.9945342E-04 point
set label "" at    296.1500    ,   0.6330783E-04 point
set label "" at    301.1500    ,   0.5445554E-04 point
set label "" at    298.1500    ,   0.5948722E-04 point ps 2 pt 6

plot [270:310] H(T)
