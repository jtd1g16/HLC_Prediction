# load "ref0857-123-86-4.gnu"
# chem = "butyl ethanoate"

set terminal postscript eps color
set title "ref = 857; chem = butyl ethanoate; casrn = 123-86-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2344243E-01 * exp(  -4306.254    *(1/   298.    -1/T))

set label "" at    313.1500    ,   0.1205988E-01 point
set label "" at    333.1500    ,   0.4909813E-02 point
set label "" at    343.1500    ,   0.3441861E-02 point
set label "" at    353.1500    ,   0.2581522E-02 point
set label "" at    298.1500    ,   0.2344243E-01 point ps 2 pt 6

plot [310:360] H(T)
