# load "ref1148-108-90-7.gnu"
# chem = "chlorobenzene"

set terminal postscript eps color
set title "ref = 1148; chem = chlorobenzene; casrn = 108-90-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3066793E-02 * exp(  -1966.209    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.3650634E-02 point
set label "" at    303.1500    ,   0.2570492E-02 point
set label "" at    313.1500    ,   0.2110095E-02 point
set label "" at    323.1500    ,   0.1962916E-02 point
set label "" at    298.1500    ,   0.3066793E-02 point ps 2 pt 6

plot [290:330] H(T)
