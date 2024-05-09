xxd
===

# Purpose

The traditional Unix tool that is commonly used to convert
bytes into text strings is `xxd`.  This utility creates a
hex dump of a given file or standard input.  It can also
convert a hex dump back to its original binary form.  Below
are details and common usages of `xxd` for converting bytes
into text strings:


# Basic Usage

* To create a hex dump from a binary file:
  ```bash
  xxd file.bin
  ```

* To convert a hex dump back to binary:
  ```bash
  xxd -r file.hex file.bin
  ```

* To output plain hex bytes without formatting:
  ```bash
  xxd -p file.bin
  ```

* To convert plain hex bytes back into binary:
  ```bash
  xxd -p -r file.hex file.bin
  ```

# Additional Options

## Options

* `-b`: Output in binary rather than hex.
* `-c`: Specify the number of bytes per line.
* `-s`: Skip to a specific offset in the input.
* `-l`: Limit the output to a certain length.

## Examples

* Create a hex dump with 10 bytes per line:
  ```bash
  xxd -c 10 file.bin
  ```
* Create a binary dump instead of hex:
  ```bash
  xxd -b file.bin
  ```

# Other Tools

Although `xxd` is widely used, there are other tools
available that can perform similar tasks:

* `hexdump` or `od` (octal dump): These tools are also part
  of the traditional Unix toolkit and can be used to display
  binary data in various formats, including hexadecimal.
