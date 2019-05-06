
include "globals.mzn";

% Each side of the cube has 3 small cubes
int: side = 3;
% Number of small cubes
int: ncubes = side * side * side;
% Number of cube join points
int: njoints = ncubes - 1;
% Number of "inner" cubes
int: ninner = njoints - 1;

% Axes
array[1..3] of int: axis_delta = [ 1, 3, 9 ];
% Possible moves
set of int: delta_vals = { a * p | a in axis_delta, p in {-1, 1} };

% Two types of the inner cubes
% S - straight, joins to the other cubes on the opposite sides
% T - turn, joins to the other cubes on adjacent sides (90 deg turn)
enum cube_type = {S, T};
array[1..ninner] of cube_type:
  types = [S,T,T,T,T,T,T,T,T,T,S,T,T,T,T,T,T,T,S,T,S,T,T,T,T];

% Position for each cube and moves (deltas)
array[1..ncubes] of var 1..ncubes: pos;
array[1..njoints] of var delta_vals: delta;

% Each cube has to occupy a different position.
constraint all_different(pos);

% Tie cube positions to deltas.
constraint forall (p in 1..njoints) (pos[p] - pos[p+1] = delta[p]);

% Straight cubes do not change direction (delta), turns do.
constraint forall (p in 1..ninner where types[p] == S) (delta[p] == delta[p+1]);
constraint forall (p in 1..ninner where types[p] == T) (delta[p] != delta[p+1]);

% Avoid row/coumn overflow.
constraint forall (p in 1..njoints) (
  exists (a in axis_delta) (
    ((pos[p]-1) div (3*a) = (pos[p+1]-1) div (3*a) /\ pos[p] mod a = pos[p+1] mod a)
  )
);

% Arbitrarily set positions of some cubes.
% This removes some symmetric solutions.
constraint pos[1] = 1;
constraint pos[2] = 2;
constraint pos[4] = 6;

solve satisfy;

output [show(pos)];