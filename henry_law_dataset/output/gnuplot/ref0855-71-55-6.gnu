# load "ref0855-71-55-6.gnu"
# chem = "1,1,1-trichloroethane"

set terminal postscript eps color
set title "ref = 855; chem = 1,1,1-trichloroethane; casrn = 71-55-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5959142E-03 * exp(  -3491.978    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.7224792E-03 point
set label "" at    303.1500    ,   0.4985758E-03 point
set label "" at    313.1500    ,   0.3374507E-03 point
set label "" at    298.1500    ,   0.5959142E-03 point ps 2 pt 6

plot [290:320] H(T)
