# load "ref0857-123-91-1.gnu"
# chem = "1,4-dioxane"

set terminal postscript eps color
set title "ref = 857; chem = 1,4-dioxane; casrn = 123-91-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.412830     * exp(  -5136.301    *(1/   298.    -1/T))

set label "" at    313.1500    ,   0.6214296     point
set label "" at    333.1500    ,   0.2317720     point
set label "" at    343.1500    ,   0.1444039     point
set label "" at    353.1500    ,   0.9808421E-01 point
set label "" at    298.1500    ,    1.412830     point ps 2 pt 6

plot [310:360] H(T)
