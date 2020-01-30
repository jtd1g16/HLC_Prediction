# load "ref3126-74-82-8.gnu"
# chem = "methane"

set terminal postscript eps color
set title "ref = 3126; chem = methane; casrn = 74-82-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1197097E-04 * exp(  -2431.297    *(1/   298.    -1/T))

set label "" at    274.3500    ,   0.2431756E-04 point
set label "" at    283.3700    ,   0.1819368E-04 point
set label "" at    285.6100    ,   0.1721810E-04 point
set label "" at    298.1500    ,   0.1197097E-04 point ps 2 pt 6

plot [270:290] H(T)
