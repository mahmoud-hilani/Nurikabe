# Nurikabe Solver

This repository contains a Prolog-based solver for the Nurikabe puzzle, a logic game where you create islands of connected cells based on given clues while ensuring all rules are satisfied.

---

## Features

- Implements intelligent strategies to solve Nurikabe puzzles efficiently.
- Handles grid constraints such as connected sea cells, isolated islands, and 2x2 sea blocks.
- Visualizes the grid dynamically during the solving process.
- Utilizes logical rules and search algorithms to ensure all puzzle constraints are satisfied.

---

## How It Works

The solver applies a set of rules and strategies to solve the puzzle:

### 1. **Grid Representation**

- A fixed grid size of `7x7` (or configurable based on the example) is defined.
- Clues (`fxd_cell/3`) represent cells with specific numbers, indicating the size of the island starting at that cell.
- Solved cells are marked as either:
  - **Land (`green`)**: Part of an island.
  - **Sea (`blue`)**: Water separating islands.

### 2. **Rules and Constraints**

- **Connected Sea Rule**: All sea cells must form a single connected group.
- **No 2x2 Sea Blocks**: Prevents any 2x2 squares of sea cells.
- **Island Constraints**:
  - Each island contains exactly one fixed cell.
  - The size of each island matches the number in the fixed cell.
  - Adjacent islands are separated by at least one sea cell.

### 3. **Algorithms and Strategies**

- **Breadth-First Search (BFS)**: Used to verify connectivity of sea cells and islands.
- **Rule-Based Solving**: The following strategies are implemented:
  - Surrounding clues with sea cells when the island size is 1.
  - Separating fixed cells with sea cells to avoid invalid configurations.
  - Preventing the formation of invalid 2x2 sea blocks.
  - Wrapping completed islands with sea cells to ensure isolation.
  - Filling empty cells dynamically to expand islands or complete seas.

### 4. **Dynamic Visualization**

- The grid is printed dynamically during solving, providing a visual representation of progress.

---

## How to Run the Code

1. Install a Prolog interpreter such as [SWI-Prolog](https://www.swi-prolog.org/).
2. Clone this repository:

   ```bash
   git clone https://github.com/mahmoud-hilani/nurikabe-solver.git

   ```

3. Load the file into the Prolog environment:
   prolog
   ?- [nurikabe_solver].

4.Use the solving/0 predicate to run the solver:
prolog
?- solving.

# Example Grids for Nurikabe Solver

This repository includes a file named `examples.txt`, which contains multiple predefined grid configurations for testing the Nurikabe solver.

## How to Use Example Grids

1. Open the `examples.txt` file to view various grid examples. Each example includes:

   - The **grid size** (`grid_size/2`).
   - **Fixed cells** (`fxd_cell/3`) representing the clues.
   - Optionally, **solved cells** (`solve_cell/3`) to visualize specific configurations.

2. Copy one of the grid examples into your Prolog environment.

3. Load the solver and test the grid by running the following command:
   ```prolog
   ?- solving.
   ```
