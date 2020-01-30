# load "ref1925-144-62-7.gnu"
# chem = "ethanedioic acid"

set terminal postscript eps color
set title "ref = 1925; chem = ethanedioic acid; casrn = 144-62-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    7084322.     * exp(  -9793.942    *(1/   298.    -1/T))

set label "" at    268.1500    ,   0.2863331E+09 point
set label "" at    273.1500    ,   0.1436585E+09 point
set label "" at    278.1500    ,   0.7428917E+08 point
set label "" at    283.1500    ,   0.3965369E+08 point
set label "" at    288.1500    ,   0.2174557E+08 point
set label "" at    293.1500    ,   0.1229953E+08 point
set label "" at    298.1500    ,    7123888.     point
set label "" at    303.1500    ,    4221199.     point
set label "" at    298.1500    ,    7084322.     point ps 2 pt 6

plot [260:310] H(T)
