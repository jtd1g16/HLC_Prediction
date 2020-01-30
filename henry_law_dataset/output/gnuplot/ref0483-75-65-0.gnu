# load "ref0483-75-65-0.gnu"
# chem = "2-methyl-2-propanol"

set terminal postscript eps color
set title "ref = 483; chem = 2-methyl-2-propanol; casrn = 75-65-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6765623     * exp(  -8306.978    *(1/   298.    -1/T))

set label "" at    273.0000    ,    8.811162     point
set label "" at    298.0000    ,   0.6861175     point
set label "" at    298.1500    ,   0.6765623     point ps 2 pt 6

plot [270:300] H(T)
