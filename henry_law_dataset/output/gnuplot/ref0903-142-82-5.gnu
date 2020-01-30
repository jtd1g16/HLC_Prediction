# load "ref0903-142-82-5.gnu"
# chem = "heptane"

set terminal postscript eps color
set title "ref = 903; chem = heptane; casrn = 142-82-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1183952E-04 * exp(  -3726.572    *(1/   298.    -1/T))

set label "" at    299.1500    ,   0.1095362E-04 point
set label "" at    308.9500    ,   0.8258798E-05 point
set label "" at    318.1500    ,   0.5180703E-05 point
set label "" at    298.1500    ,   0.1183952E-04 point ps 2 pt 6

plot [290:320] H(T)
