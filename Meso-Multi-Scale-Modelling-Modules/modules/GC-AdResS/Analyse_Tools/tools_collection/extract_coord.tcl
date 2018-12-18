#!/bin/sh
package require topotools 1.2
mol new conf.gro
mol addfile traj_comp.xtc type xtc waitfor all first 0 last -1 step 1

topo writevarxyz WCG.xyz selmod "name WCG and (x>285 and x<315)"
exit


