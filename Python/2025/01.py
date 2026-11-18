# Day 1: Secret Entrance
# https://adventofcode.com/2025/day/1

from pathlib import Path
import re

TRACK_SIZE = 100
START_POSITION = 50

# ------------------------------------------------------------
# Load Data
# ------------------------------------------------------------
lines = Path("inputs/2025/01.txt").read_text().splitlines()

# ------------------------------------------------------------
# Parse moves
# ------------------------------------------------------------
directions = []
distances = []
distance_signed = []

for line in lines:
    d = re.search(r"(L|R)", line).group(1)
    n = int(re.search(r"\d+", line).group(0))
    directions.append(d)
    distances.append(n)
    distance_signed.append(n if d == "R" else -n)

# ------------------------------------------------------------
# Compute start and end positions
# ------------------------------------------------------------
start_positions = [START_POSITION]

for d in distance_signed[:-1]:
    start_positions.append((start_positions[-1] + d) % TRACK_SIZE)

end_positions = [
    (s + d) % TRACK_SIZE
    for s, d in zip(start_positions, distance_signed)
]

# ------------------------------------------------------------
# Part 1 — Moves that land on position 0
# ------------------------------------------------------------
lands_on_0 = sum(ep == 0 for ep in end_positions)
print(f"Times landed on position 0: {lands_on_0}")

# ------------------------------------------------------------
# Part 2 — Count moves that pass through position 0
# ------------------------------------------------------------

def cross_right(start, dist, track):
    return max(0, (start + dist) // track - start // track)

def cross_left(start, dist, track):
    return max(0, (start - 1) // track - (start + dist - 1) // track)

crosses = []
for s, d in zip(start_positions, distance_signed):
    if d > 0:
        crosses.append(cross_right(s, d, TRACK_SIZE))
    else:
        crosses.append(cross_left(s, d, TRACK_SIZE))

crosses_0 = sum(crosses)
print(f"Times dial points at 0: {crosses_0}")
