# load "ref0903-111-65-9.gnu"
# chem = "octane"

set terminal postscript eps color
set title "ref = 903; chem = octane; casrn = 111-65-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3005571E-04 * exp(  -8007.120    *(1/   298.    -1/T))

set label "" at    301.0500    ,   0.2550175E-04 point
set label "" at    308.1500    ,   0.1065791E-04 point
set label "" at    318.1500    ,   0.5963278E-05 point
set label "" at    298.1500    ,   0.3005571E-04 point ps 2 pt 6

plot [300:320] H(T)
