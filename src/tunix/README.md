TUNIX
=====

Dispatch KERNAL I/O calls to drivers.

Logical file names are always translated
so they won't collide with other
processes.

# Planned system calls

Downcase letters denote byte values.

## General

* "SX" kill all processes and drivers and return to plain BASIC.
* "SN" stops multi-tasking.
* "SM" resumes multi-tasking.

## Processes

* "P" returns a list of process IDs, together with flags.
  (Running or sleeping,)
* "PI" returns ID of the current process.
* "PF" creates a copy of the current process, sharing all its open
  resources and returns its ID.  Returns 0 to the child.
* "PEpathname" replace current process by executable.
* "PXc" exits current process with a code (byte).
* "PWp" waits for a process to exit and returns its exit code.
* "PKp" kills a process (with exit code 255).
* "PSp" stops a process.
* "PRp" resumes a process.

Probably required at some point, at least to keep out background
processes as long as something else is running:
* "PPpl" Set priority level of process (0: highest, 255: lowest)

## Drivers

* "D" returns a list of devices and drivers.
* "DRdvv" register vectors for a particular device.
  Global to all processes.  Two additional vectors allow block-wise
  reads and writes.
* "DA" allocates per-process IO page (for low-level driver code).
* "DG" allocates global IO page (for interrupts).
* "DCx" commits global IO page to other process as they have them on
  different banks (per-process IO23 area).
* "DFx" frees IO page.

## Extended memory

* "MA" allocates a bank.
* "MFx" frees a bank.
