# PC Advisory System

The website for the shiny app: https://ahmadwahnan.shinyapps.io/pc-advisory-system-by-nigel/

## How it works
- In the PC Parts page, you need to select one PC part from each category
- Only then the Compatibility table under the Selection page will be generated
- Benchmarks is calculated using my own formula (esentiallly comparing with other parts of the same type)
- Compatibility is implemented based on the report decision rules

## Decision Rules
IF CPU.Socket = Motherboard.Socket
AND CPU.Compatible_Chipset ∈ Motherboard.Chipset
THEN isCPUcompatible = TRUE ELSE isCPUcompatible = FALSE

IF GPU.PCIE_Bus_Interface ∈ Motherboard.Expansion_Slots
THEN isGPUcompatible = TRUE ELSE isGPUcompatible = FALSE

IF RAM.Type ∈ Motherboard.RAM_Type
AND RAM.Bus_Interface ∈ Motherboard.RAM_Interface
AND RAM.Total_Capacity <= Motherboard.RAM_Max_Capacity
AND RAM.Clock_Speed <= Motherboard.RAM_Max_Speed
AND RAM.Channels <= Motherboard.RAM_Max_Channels
THEN isRAMcompatible = TRUE ELSE isRAMcompatible = FALSE

IF StorageDrive.Bus_Interface ∈ Motherboard.Storage_Slots
THEN isStorageDriveCompatible = TRUE ELSE isStorageDriveCompatible = FALSE

IF CPU.Socket = Motherboard.Socket
AND CPU.Compatible_Chipset ∈ Motherboard.Chipset
AND GPU.PCIE_Bus_Interface ∈ Motherboard.Expansion_Slots
AND RAM.Type ∈ Motherboard.RAM_Type
AND RAM.Bus_Interface ∈ Motherboard.RAM_Interface
AND RAM.Total_Capacity <= Motherboard.RAM_Max_Capacity
AND RAM.Clock_Speed <= Motherboard.RAM_Max_Speed
AND RAM.Channels <= Motherboard.RAM_Max_Channels
AND StorageDrive.Bus_Interface ∈ Motherboard.Storage_Slots
THEN isMotherboardCompatible = TRUE ELSE isMotherboardCompatible = FALSE

IF PSU.Power_Output >= CPU.TDP + GPU.Max_TDP + StorageDrive.Max_TDP
THEN isPSUcompatible = TRUE ELSE isPSUcompatible = FALSE

IF isCPUcompatible 
AND isGPUcompatible 
AND isRAMcompatible
AND isStorageDriveCompatible 
AND isMotherboardCompatible
AND isPSUcompatible 
THEN isBuildCompatible = TRUE ELSE isBuildCompatible = FALSE

## Bugs
- After you have chosen 1 part from each type and the Compatibility table is generated, if you press on the Clear button, all previously selected parts cannot be selected again, but other parts can be selected
- You need to scroll to the top in Selection page to view the Selections table

## To-do
- Implement 'Generate Build' module
- Implement 'Parts Database' module
