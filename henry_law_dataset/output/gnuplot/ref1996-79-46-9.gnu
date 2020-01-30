# load "ref1996-79-46-9.gnu"
# chem = "2-nitropropane"

set terminal postscript eps color
set title "ref = 1996; chem = 2-nitropropane; casrn = 79-46-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8419766E-01 * exp(  -4484.813    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.1106838     point
set label "" at    303.1500    ,   0.6442597E-01 point
set label "" at    313.1500    ,   0.4039555E-01 point
set label "" at    323.1500    ,   0.2673522E-01 point
set label "" at    298.1500    ,   0.8419766E-01 point ps 2 pt 6

plot [290:330] H(T)
