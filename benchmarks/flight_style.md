# Crop flight styles

Measured 2026-10-04 on two catalog fields, both at the hangar cap (`max_cells` 8000), generic vertical, sortie schedule, partition auto. Coverage is 100% and 4 drones in every row.

Orchard style rotates the grid so boustrophedon rows follow the long axis of the block. Open-field style is the same east-west auto pattern as a field with no crop. This is one block each, not a matrix.

## Fresno almonds (`dwr_2023_1020339`, 18 ac)

Long axis is 90° (north–south). Orchard rows follow it. Generic stays east–west and lets auto pick the pattern.

| | makespan | length | partition | grid |
| --- | --- | --- | --- | --- |
| Orchard rows (boustro, long axis) | 113.25 s | 1460 | strip | 52×27 |
| Generic (auto, east–west) | 141.75 s | 1614 | region | 27×52 |

On this block the row-aligned plan is shorter (length −9.5%, makespan −20%). That is this polygon, not a claim about orchards in general. The style exists so passes follow the trees.

## Minnesota corn (`csb_2024_271724010507790`, 18 ac)

Open-field does not change the grid or the algorithm. Length matches. Makespan differs by 0.5 s, which is solver noise on the same pattern.

| | makespan | length | partition | grid |
| --- | --- | --- | --- | --- |
| Open-field sweep (auto, east–west) | 142.5 s | 1618 | region | 30×55 |
| Generic (auto, east–west) | 143.0 s | 1618 | region | 30×55 |

Tie on flight length.
