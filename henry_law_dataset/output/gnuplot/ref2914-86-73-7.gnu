# load "ref2914-86-73-7.gnu"
# chem = "2,3-benzindene"

set terminal postscript eps color
set title "ref = 2914; chem = 2,3-benzindene; casrn = 86-73-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7874016E-01 * exp(  -7434.059    *(1/   298.    -1/T))

set label "" at    277.1500    ,   0.5208333     point
set label "" at    298.1500    ,   0.7874016E-01 point
set label "" at    298.1500    ,   0.7874016E-01 point ps 2 pt 6

plot [270:300] H(T)
