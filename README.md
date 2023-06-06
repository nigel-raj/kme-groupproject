# PC Advisory System

To run, create a local database and import the .csv files.
The names of the tables should be cpu, gpu, storage, ram, motherboard and psu.

## How it works
- Run the code with the correct database config 
- In the PC Parts page, you need to select one PC part from each category
- Only then the Compatibility table under the Selection page will be generated
- Benchmarks is calculated using my own formula (esentiallly comparing with other parts of the same type), can be modified if there is time
- Compatibility is implemented based on the report decision rules, I have tested it and it does work

## Bugs
- After you have chosen 1 part from each type and the Compatibility table is generated, if you press on the Clear button, all previously selected parts cannot be selected again, but other parts can be selected
- You need to scroll to the top in Selection page to view the Selections table

## To-do
- Implement 'Generate Build' module
- Implement 'Parts Database' module
