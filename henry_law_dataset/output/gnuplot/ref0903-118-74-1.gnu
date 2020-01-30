# load "ref0903-118-74-1.gnu"
# chem = "hexachlorobenzene"

set terminal postscript eps color
set title "ref = 903; chem = hexachlorobenzene; casrn = 118-74-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3835079E-04 * exp(  -572.5358    *(1/   298.    -1/T))

set label "" at    299.1500    ,   0.3810540E-04 point
set label "" at    319.1500    ,   0.3379863E-04 point
set label "" at    298.1500    ,   0.3835079E-04 point ps 2 pt 6

plot [290:320] H(T)
