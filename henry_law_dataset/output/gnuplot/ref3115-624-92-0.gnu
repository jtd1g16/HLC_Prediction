# load "ref3115-624-92-0.gnu"
# chem = "dimethyl disulfide"

set terminal postscript eps color
set title "ref = 3115; chem = dimethyl disulfide; casrn = 624-92-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6506197E-02 * exp(  -3167.621    *(1/   298.    -1/T))

set label "" at    313.0000    ,   0.3987169E-02 point
set label "" at    323.0000    ,   0.2790817E-02 point
set label "" at    333.0000    ,   0.2170271E-02 point
set label "" at    343.0000    ,   0.1622930E-02 point
set label "" at    298.1500    ,   0.6506197E-02 point ps 2 pt 6

plot [310:350] H(T)
