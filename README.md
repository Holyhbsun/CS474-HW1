# CS474-HW1
CS474 HW1 fuzzy logic

This project is fuzzy logic DSL for CS474 HW1. The report.docx include the detail explanation of the implementation and the semantics of this DSL.

## Installation

### Prerequisites

To run this project, you'll need:
- **Scala 2.13.x**
- **SBT (Scala Build Tool)**
- **Git** for version control.

### Steps

1. **Clone the Repository**:
   Open a terminal and run the following command:
   ```bash
   git clone https://github.com/Holyhbsun/CS474-HW1.git
   cd CS474-HW1

2. **Build the Project**:
   You can use SBT to build the project. In the project directory, run:
   ```bash
    sbt compile

3. **Run the Program**:
   To run the program, use:
   ```bash
    sbt run

4. **Run Tests**:
   To run the test, use:
   ```bash
    sbt test

## Usage
The following are the fuzzy functions implemented in this project.
### Fuzzy Gate Operations
- **ADD**: Adds two inputs. (capped at 1.0)
- **MULT**: Multiplies two inputs.
- **XOR**: Computes the absolute difference between two inputs.

### Fuzzy Set Operations
- **ADD**: Adds membership values from two input sets (capped at 1.0).
- **MULT**: Multiplies membership values from two sets.
- **XOR**: Computes the absolute difference between membership values from two input sets.
- **UNION**: Takes the maximum membership value for each element across two input sets.
- **INTERSECTION**: Takes the minimum membership value for each element across input two sets.
- **COMPLEMENT**: Inverts the membership value of each element from 1.0.
- **α-cut**: Filters elements whose membership values are greater than a given threshold.