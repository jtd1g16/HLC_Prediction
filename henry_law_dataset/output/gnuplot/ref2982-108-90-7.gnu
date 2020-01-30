# load "ref2982-108-90-7.gnu"
# chem = "chlorobenzene"

set terminal postscript eps color
set title "ref = 2982; chem = chlorobenzene; casrn = 108-90-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1745211E-02 * exp(  -1319.677    *(1/   298.    -1/T))

set label "" at    323.1500    ,   0.1272228E-02 point
set label "" at    333.1500    ,   0.1038310E-02 point
set label "" at    343.1500    ,   0.1004390E-02 point
set label "" at    298.1500    ,   0.1745211E-02 point ps 2 pt 6

plot [320:350] H(T)
