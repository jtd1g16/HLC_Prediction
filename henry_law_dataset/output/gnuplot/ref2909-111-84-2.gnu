# load "ref2909-111-84-2.gnu"
# chem = "nonane"

set terminal postscript eps color
set title "ref = 2909; chem = nonane; casrn = 111-84-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1803403E-05 * exp(  -7308.057    *(1/   298.    -1/T))

set label "" at    287.9500    ,   0.4093312E-05 point
set label "" at    287.9500    ,   0.4510997E-05 point
set label "" at    293.2000    ,   0.2707359E-05 point
set label "" at    293.2000    ,   0.2748379E-05 point
set label "" at    298.1500    ,   0.1803403E-05 point ps 2 pt 6

plot [280:300] H(T)
