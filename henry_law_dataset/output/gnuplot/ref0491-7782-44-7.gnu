# load "ref0491-7782-44-7.gnu"
# chem = "oxygen"

set terminal postscript eps color
set title "ref = 491; chem = oxygen; casrn = 7782-44-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1289951E-04 * exp(  -1370.856    *(1/   298.    -1/T))

set label "" at    275.4500    ,   0.2034704E-04 point
set label "" at    288.1500    ,   0.1508077E-04 point
set label "" at    298.1500    ,   0.1256115E-04 point
set label "" at    298.1500    ,   0.1258343E-04 point
set label "" at    298.1500    ,   0.1255830E-04 point
set label "" at    298.1500    ,   0.1259058E-04 point
set label "" at    298.1600    ,   0.1255773E-04 point
set label "" at    303.1400    ,   0.1164578E-04 point
set label "" at    318.1400    ,   0.9711663E-05 point
set label "" at    318.1600    ,   0.9724803E-05 point
set label "" at    318.1600    ,   0.9727025E-05 point
set label "" at    318.1700    ,   0.9712685E-05 point
set label "" at    318.1700    ,   0.9716608E-05 point
set label "" at    328.1400    ,   0.8926834E-05 point
set label "" at    298.1500    ,   0.1289951E-04 point ps 2 pt 6

plot [270:330] H(T)
