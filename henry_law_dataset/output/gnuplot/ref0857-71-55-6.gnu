# load "ref0857-71-55-6.gnu"
# chem = "1,1,1-trichloroethane"

set terminal postscript eps color
set title "ref = 857; chem = 1,1,1-trichloroethane; casrn = 71-55-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7934304E-03 * exp(  -1283.250    *(1/   298.    -1/T))

set label "" at    313.1500    ,   0.6337199E-03 point
set label "" at    333.1500    ,   0.5306930E-03 point
set label "" at    343.1500    ,   0.4416237E-03 point
set label "" at    353.1500    ,   0.4018728E-03 point
set label "" at    298.1500    ,   0.7934304E-03 point ps 2 pt 6

plot [310:360] H(T)
