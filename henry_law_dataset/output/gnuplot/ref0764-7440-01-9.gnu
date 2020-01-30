# load "ref0764-7440-01-9.gnu"
# chem = "neon"

set terminal postscript eps color
set title "ref = 764; chem = neon; casrn = 7440-01-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4518139E-05 * exp(  -531.7162    *(1/   298.    -1/T))

set label "" at    284.6500    ,   0.4960646E-05 point
set label "" at    288.1500    ,   0.4785048E-05 point
set label "" at    292.9500    ,   0.4653349E-05 point
set label "" at    297.5500    ,   0.4477751E-05 point
set label "" at    301.9500    ,   0.4433852E-05 point
set label "" at    304.4500    ,   0.4359223E-05 point
set label "" at    305.2500    ,   0.4359223E-05 point
set label "" at    298.1500    ,   0.4518139E-05 point ps 2 pt 6

plot [280:310] H(T)
