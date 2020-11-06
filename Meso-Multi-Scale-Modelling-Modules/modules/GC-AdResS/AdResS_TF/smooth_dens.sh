#!/usr/bin/env python

import sys

from numpy import *
from scipy.signal import savgol_filter
from scipy import integrate
from scipy import interpolate
import matplotlib.pyplot as plt

# only hybrid region and a little around
f = file('dens_mix_hy', 'r')
f_x = file('xnew_ntrpl', 'r')

tgt = loadtxt(f, usecols=(0,1,2))
tgt_x = loadtxt(f_x, usecols=(0))

r = tgt[:,0]
dr = tgt[:,1]
dtr = tgt[:,1]
xnew = tgt_x[1]

prefac = sys.argv[1] 
diff_r = sys.argv[2]
T = sys.argv[3]

# kb*T in kJ/(mol K)
#kbT = kb*T*N/1000
kb = 1.38068452e-23
N = 6.022141e23
kbT = -1.0*0.00831451*T

rho_zero = tgt[:,2]
avg_rho = average(rho_zero)

#compressability: 4.5e-5
kappa = 4.5e-5

#prefactor: M(mol)/(rho**2 compress)
#M(CAT)=107.077 
#M(ANN)=35.45300
# we set M=1 !!!
#==================================
#prepot =  1.0/(avg_rho*kappa)
#prefac =  1.0/(square(avg_rho)*kappa)

print "avg_rho",avg_rho
print "prefac", prefac

tck = interpolate.splrep(r, dr)  
dr_s = interpolate.splev(r, tck, der=0)
dr_d = interpolate.splev(r, tck, der=1)

dr_p = dr_s*kbT*prefac

force_r = dr_d*prefac

SPL = column_stack((r, dr_s))
savetxt('dens_spline', SPL)

DAT = column_stack((r, force_r))
savetxt('dens_smooth', DAT)

POT = column_stack((r, dr_p))
savetxt('pot_smooth', POT)

plt.plot(r, dr) 
plt.plot(r, dr_s)
#plt.show()

plt.plot(r,dr_p)
plt.plot(r,force_r)
#plt.show()

