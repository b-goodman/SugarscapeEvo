# SugarscapeEvo

Aims to implement classic sugarscape but using agent populations with evolutionary potential.  Agent behaviour encoded as genotype via something akin to gene expression programming and karva notation.  After breeding, progeny may develop new traits by random mutations in genotype. What will the results be after N generations of selection?  

Still in development.  So far:
 - Classic sugarscape.  Agents search for food and breed.  Agent dies if energy=0.
 - Customisable initial conditions.
 - Review simulation with animated output and data plots.
 - User interface
 - Save(load) current(prior) configs/results
 - Compiled code optimised for speed.
 - Automated web installer.  No need to copy/paste package files.
 
Try it now!

1. Evaluate in notebook
 ```
 <<https://raw.githubusercontent.com/b-goodman/SugarscapeEvo/master/\webInstaller
 ```
2. Main interface will open
  * To view/edit configuration: File>Start New Session.
  * To run simulation: Evaluation>Run Simulation.
  * To view results: Data>Review Session Data.

3. To (re)open interface, evaluate in notebook:
```
<< sugarLaunch`
```
