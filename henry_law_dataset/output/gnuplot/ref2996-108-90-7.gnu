# load "ref2996-108-90-7.gnu"
# chem = "chlorobenzene"

set terminal postscript eps color
set title "ref = 2996; chem = chlorobenzene; casrn = 108-90-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1523851E-02 * exp(  -2277.209    *(1/   298.    -1/T))

set label "" at    318.1500    ,   0.9579639E-03 point
set label "" at    323.1500    ,   0.8381349E-03 point
set label "" at    333.1500    ,   0.6726642E-03 point
set label "" at    343.1500    ,   0.5560442E-03 point
set label "" at    353.1500    ,   0.4698139E-03 point
set label "" at    298.1500    ,   0.1523851E-02 point ps 2 pt 6

plot [310:360] H(T)
