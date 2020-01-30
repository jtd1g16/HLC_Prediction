# load "ref2914-120-12-7.gnu"
# chem = "anthracene"

set terminal postscript eps color
set title "ref = 2914; chem = anthracene; casrn = 120-12-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1538462     * exp(  -6518.863    *(1/   298.    -1/T))

set label "" at    277.1500    ,   0.8064516     point
set label "" at    298.1500    ,   0.1538462     point
set label "" at    298.1500    ,   0.1538462     point ps 2 pt 6

plot [270:300] H(T)
