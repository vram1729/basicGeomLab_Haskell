# Functional Art & Tessellations in Haskell

This project is a Haskell-based generative art engine inspired by **GeomLab** and the mathematical tiling patterns of **M.C. Escher** (specifically his "Square Limit" series). Using the `Graphics.Gloss` library, it treats images and shapes as basic units that can be transformed, rotated, and composed into complex, recursive tessellations.



## 🎨 Overview

The core philosophy of this code is **compositional geometry**. Instead of drawing shapes at specific coordinates manually, the artwork is built by:
1.  **Primitives**: Defining a base shape (a triangle via `renderTriangleLL` or a BMP image).
2.  **Transformations**: Applying geometric shifts such as `rotateAroundTopLeft` and scaling.
3.  **Tile Composition**: Combining these primitives into higher-level tiles (`t`, `u`, `v`).
4.  **Tessellation**: Arranging those tiles into a 3x3 "Square Limit" structure or repeating them across a grid.

## 🚀 Getting Started

### Prerequisites

To run this code, you need the **GHC (Glasgow Haskell Compiler)** and the **Gloss** library.

1.  **Install Haskell:** We recommend using [GHCup](https://www.haskell.org/ghcup/).
2.  **Install Gloss:**
    ```bash
    cabal update
    cabal install gloss
    ```

### Asset Requirement
The program is configured to load an external image.
* Place a BMP file named `fishstock.bmp` in the root directory.
* *Note:* The code is currently set to use the `rtTriangle` (vector triangle) as the primary primitive, but can be switched to use the BMP by uncommenting the `fish = myFish` line in the `drawing` function.

## Key Technical Features
1. Advanced Coordinate Logic

Standard Gloss rotations occur around the center of a shape. To enable perfect tiling where edges meet precisely, this project implements rotateAroundTopLeft. This ensures that when shapes are scaled or rotated, they remain anchored to a predictable corner.

2. Manual Bounding Box Calculation

Because Gloss doesn't expose bounding box data for all transformed pictures, this project includes a robust, recursive boundingBox function.

It calculates the (min,max) coordinates for Polygons, Translations, Scales, and Rotations.

This is critical for the rotateAroundTopLeft function to find the pivot point dynamically.

3. Escher-style Composition

The drawing function demonstrates how a single primitive is transformed into a complex scene:

Tiles t & u: Base combinations of the fish/triangle.

Tiles s & c: "Side" and "Corner" tiles that handle the recursive-style reduction in size toward the edges.

squareLimit2: The final 3x3 grid assembly of all transformed tiles.

## Code Structure
renderTriangleLL: Creates the base vector triangle.

tessellate: Uses list comprehensions to repeat a pattern across a 2D coordinate plane.

boundingBox: The geometric engine that determines the spatial extent of any Picture.

drawing: The functional "canvas" where the logic of the tiling is defined.

## Acknowledgments
GeomLab: For the inspiration regarding functional programming-based art.

Peter Henderson: For the "Functional Geometry" framework that describes how to build Escher's Square Limit using functional composition.


### Running the Program
Save the code as `Main.hs` and run:
```bash
runhaskell Main.hs
