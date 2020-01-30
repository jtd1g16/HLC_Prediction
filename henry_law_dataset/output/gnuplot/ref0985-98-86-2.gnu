# load "ref0985-98-86-2.gnu"
# chem = "1-phenylethanone"

set terminal postscript eps color
set title "ref = 985; chem = 1-phenylethanone; casrn = 98-86-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9662852     * exp(  -11952.96    *(1/   298.    -1/T))

set label "" at    288.0000    ,    3.796476     point
set label "" at    290.5000    ,    2.760123     point
set label "" at    293.0000    ,    2.160452     point
set label "" at    298.0000    ,   0.9386012     point
set label "" at    298.1500    ,   0.9662852     point ps 2 pt 6

plot [280:300] H(T)
