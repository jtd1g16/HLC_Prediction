# load "ref0862-74-85-1.gnu"
# chem = "ethene"

set terminal postscript eps color
set title "ref = 862; chem = ethene; casrn = 74-85-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4805693E-04 * exp(  -2315.365    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.9951146E-04 point
set label "" at    283.1500    ,   0.7133122E-04 point
set label "" at    293.1500    ,   0.5371857E-04 point
set label "" at    303.1500    ,   0.4315098E-04 point
set label "" at    298.1500    ,   0.4805693E-04 point ps 2 pt 6

plot [270:310] H(T)
