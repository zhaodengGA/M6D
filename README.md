#                                    M6D <br>

M6D is a project name, which contains a Kinetic fluid moments model for a magnetized plasma with collisions and the corresponding numerical simulation codes. <br><br>

A novel method aimed at a kinetic moments closure for a magnetized plasma with collisions is proposed. The velocity distribution function for each species is expanded in 8 Gaussian Radial Basis Functions (GRBFs) which are essentially drifted Maxwellians at eight representative 3D-velocity points of drift. The vector of 8 fluid moments (for particle density, 3 particle fluxes, total energy density, and 3 energy fluxes) has an  8x8 analytic linear matrix relation to the vector of 8 GRBF density weighs in 3D-real space. The 8 fluid moments with sources for each species are advanced in time while the 8 GRBF weighs are determined from the 8x8 inverse matrix.  <br><br>

The two closure moments (for the stress tensor and the energy weighted stress tensor) are linearly determined from the GRBF weighs. Most importantly the velocity moments of the nonlinear Coulomb Fokker-Planck collision operator ‘Rosenbluth et al, Phys. Rev 107, 1957’ are evaluated from the GRBF weights. Generalization from 8 to 12, 16, 20 .., in an energy weighted moment hierarchy is straightforward.<br><br>

The electric field follows form a generalized vorticity (quasi-neutral charge conservation) equation. A strong drift ordering approximation can be applied to eliminate any spuriously unstable high frequency cyclotron motions. <br><br>

A novel weak drift ordering two-time step scheme avoids the vorticity equation by following ion cyclotron motion in time to get the electric field with ion gyro-averaging.  Inclusion of low-beta magnetic perturbations is straightforward.



