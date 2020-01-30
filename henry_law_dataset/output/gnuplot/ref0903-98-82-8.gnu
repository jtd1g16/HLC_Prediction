# load "ref0903-98-82-8.gnu"
# chem = "(2-propyl)-benzene"

set terminal postscript eps color
set title "ref = 903; chem = (2-propyl)-benzene; casrn = 98-82-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8722067E-03 * exp(  -3271.574    *(1/   298.    -1/T))

set label "" at    301.1500    ,   0.7558579E-03 point
set label "" at    308.1500    ,   0.6464124E-03 point
set label "" at    319.2500    ,   0.4128819E-03 point
set label "" at    298.1500    ,   0.8722067E-03 point ps 2 pt 6

plot [300:320] H(T)
