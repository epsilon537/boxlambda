# 2D Vectors

`vec2 ( x y -- vec )`

- Create a 2D vector value out of x and y coordinates. Only the lower 16 bits of the x and y values are used.

`vec2+ ( v1 v2 -- vsum )`

- Add 2 2D vectors.

`vec2- ( v1 v2 -- vdiff )`

- Subtract 2 2D vectors.

`vec2.x ( v -- x )`

- Extract the x components of a 2D vector.

`vec2.y ( v -- y )`

- Extract the y component of a 2D vector.

`vec2.xy ( v -- x y )`

- Extract the x and y components of a 2D vector.

`vec2dot ( v1 v2 -- v1.v2 )`

- Compute the dot product of 2 2D vectors.


`vec2* ( v n -- v )`

- Scale a 2D vector. Regular `*` is faster, but this version avoid overflow rollover in the other dimension.

`.vec2 ( v -- )`

- Print a 2D vector.

