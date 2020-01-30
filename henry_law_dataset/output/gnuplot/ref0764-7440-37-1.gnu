# load "ref0764-7440-37-1.gnu"
# chem = "argon"

set terminal postscript eps color
set title "ref = 764; chem = argon; casrn = 7440-37-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1375075E-04 * exp(  -1148.583    *(1/   298.    -1/T))

set label "" at    283.8500    ,   0.1795490E-04 point
set label "" at    287.4500    ,   0.1633062E-04 point
set label "" at    291.3500    ,   0.1518923E-04 point
set label "" at    297.5500    ,   0.1365275E-04 point
set label "" at    302.9500    ,   0.1242356E-04 point
set label "" at    309.7500    ,   0.1136998E-04 point
set label "" at    312.5500    ,   0.1101878E-04 point
set label "" at    320.9500    ,   0.1014079E-04 point
set label "" at    322.6500    ,   0.9921292E-05 point
set label "" at    329.4500    ,   0.9350598E-05 point
set label "" at    339.4500    ,   0.8867704E-05 point
set label "" at    344.4500    ,   0.8516507E-05 point
set label "" at    347.2500    ,   0.8384809E-05 point
set label "" at    298.1500    ,   0.1375075E-04 point ps 2 pt 6

plot [280:350] H(T)
