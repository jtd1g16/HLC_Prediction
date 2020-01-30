# load "ref2997-112-30-1.gnu"
# chem = "1-decanol"

set terminal postscript eps color
set title "ref = 2997; chem = 1-decanol; casrn = 112-30-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6538163E-01 * exp(  -5252.688    *(1/   298.    -1/T))

set label "" at    333.1500    ,   0.1042753E-01 point
set label "" at    343.1500    ,   0.6289308E-02 point
set label "" at    353.1500    ,   0.4273504E-02 point
set label "" at    298.1500    ,   0.6538163E-01 point ps 2 pt 6

plot [330:360] H(T)
