# PC Advisory System

To run, create a local database and import the .csv files.
The names of the tables should be cpu, gpu, storage, ram, motherboard and psu.

## How it works
- Run the code with the correct database config 
- In the Selection page, you need to select one PC part from each category
- Only then the Compatibility table will be generated
- Benchmarks is calculated using my own formula (esentiallly comparing with other parts of the same type)
- Compatibility is implemented based on the report decision rules, I have tested it and it does work

## Bugs
- After you have chosen 1 part from each type and the Compatibility table is generated, if you press on the Clear button, all previously selected parts cannot be selected again, but other parts can be selected
