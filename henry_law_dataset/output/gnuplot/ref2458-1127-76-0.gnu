# load "ref2458-1127-76-0.gnu"
# chem = "1-ethylnaphthalene"

set terminal postscript eps color
set title "ref = 2458; chem = 1-ethylnaphthalene; casrn = 1127-76-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2206375E-01 * exp(  -4775.201    *(1/   298.    -1/T))

set label "" at    296.2500    ,   0.2435896E-01 point
set label "" at    295.0500    ,   0.2731147E-01 point
set label "" at    286.3500    ,   0.4158185E-01 point
set label "" at    281.5500    ,   0.5724204E-01 point
set label "" at    298.4500    ,   0.2135847E-01 point
set label "" at    300.6500    ,   0.1920197E-01 point
set label "" at    303.4500    ,   0.1664669E-01 point
set label "" at    298.1500    ,   0.2206375E-01 point ps 2 pt 6

plot [280:310] H(T)
