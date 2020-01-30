# load "ref3115-75-18-3.gnu"
# chem = "dimethyl sulfide"

set terminal postscript eps color
set title "ref = 3115; chem = dimethyl sulfide; casrn = 75-18-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4787753E-02 * exp(  -2782.752    *(1/   298.    -1/T))

set label "" at    313.0000    ,   0.3142641E-02 point
set label "" at    323.0000    ,   0.2268111E-02 point
set label "" at    333.0000    ,   0.1785223E-02 point
set label "" at    343.0000    ,   0.1437079E-02 point
set label "" at    298.1500    ,   0.4787753E-02 point ps 2 pt 6

plot [310:350] H(T)
