The INGLE project
=================

* [Build instructions](BUILD.md)

This is an attempt to turn the Commodore
VIC-20 into a serious work machine with
help of the UltiMem expansion [^ultimem]
and some well-tested philosophies.

[^ultimem]: http://www.go4retro.com/products/ultimem/

# Serious work and Unix

What's serious work?  That's a good
question.  We all might agree that
serious work requires playing excellent
games in advance.  Serious work surely
is:

* Fun to do.
* Time is not wasted.
* One can switch from one task to an-
  other swiftly.
* Single mistakes can be undone without
  having to start over completely.
* Repetitive tasks can be delegated to
  the machine (which is what it was in-
  vented for).
* It's easy to collaborate with others.
* Feedback loops are short, allowing for
  rapid iteration and improvement.
* There's a clear goal or outcome that
  guides the work, providing focus and
  direction.
* The work leverages one's skills and
  knowledge, challenging them to grow
  and adapt.
* It contributes to a larger purpose or
  mission, giving it meaning beyond the
  immediate task.
* Tools and processes are designed to
  minimize friction and maximize output,
  harnessing the full potential of
  technology.
* Knowledge is shared openly, fostering
  a culture of learning and continuous
  improvement.
* There is an emphasis on sustainable
  practices, ensuring that the work can
  continue without burning out the
  individuals involved.
* Creativity and innovation are
  encouraged, allowing for the
  exploration of new ideas and
  solutions.

Bridging the concept of "serious work"
with the Unix philosophy unveils a
harmonious relationship between the
ethos of productive, meaningful labor
and the guiding principles of Unix
design.  Serious work demands
efficiency, adaptability, and
reliability—qualities that are deeply
ingrained in the Unix philosophy and
its implementation across computing
environments.

# The UNIX philosophy

The Unix philosophy emphasizes building
simple, short, clear, modular, and
extensible code that can be easily
maintained and repurposed by developers
other than its creators.  It advocates
for small, composable programs that do
one thing well and work together over a
universal interface, typically text
streams.  This approach encourages the
use of leverage and collaboration,
aiming for efficiency and enabling
complex tasks to be accomplished with
combinations of simple tools.

Unix makes its philosophy a reality
through several core principles and
functionalities:

* Pipelines and Filters:
  Unix uses a powerful mechanism called
  pipelines, allowing the output of one
  program to become the input of
  another, facilitating the creation of
  complex data processing sequences
  with simple commands.
* Text as a Universal Interface:
  Most Unix tools operate on and
  produce text, which acts as a
  universal interface between programs,
  making data and functionality easily
  composable and extensible.
* Shell Scripting:
  Unix shell scripting allows users to
  automate tasks by combining existing
  tools in scripts, which can perform
  complex operations through simple
  commands.
* Filesystem as a Database:
  In Unix, everything is considered a
  file (regular files, directories,
  hardware devices, etc.), which
  simplifies data management and
  manipulation.
* Modularity:
  Unix provides a set of small tools,
  each designed to perform a
  well-defined function excellently.
  These tools can be combined in
  various ways to perform complex
  tasks, embodying the "do one thing
  and do it well" principle.
* Portability and Open Standards:
  Unix and its derivatives adhere to
  open standards, ensuring software and
  scripts are portable across different
  Unix-like systems.
* Source Code Availability:
  The availability of Unix source code
  for study and modification encourages
  understanding, innovation, and
  adaptation, allowing users to tailor
  the system to their needs.

Through these mechanisms, Unix opera-
tionalizes its philosophy, fostering an
environment of efficiency, simplicity,
and modularity that has influenced the
development of many operating systems
and software projects.

# The VIC-20

The Commodore VIC-20, released in 1981
as an early home computer, is designed
for education, basic computing, and
video games.  When considering the Unix
philosophy and the functionalities that
embody Unix's effectiveness, the VIC-20
presents several key limitations or
absences:

* Limited Resources:
  With an initial 5 KB of RAM,
  expandable by 37 KB, the VIC-20
  restricts the complexity of programs
  it can run.  This includes the
  incapability to host a Unix-like
  operating system or sophisticated
  text processing tools.
* Operating System:
  Its native operating system is far
  simpler than Unix, focusing on basic
  input/output operations with
  peripherals and running user
  programs, without the multitasking,
  multi-user capabilities, or the
  powerful shell environment Unix
  offers.
* Pipelines and Filters:
  The concept of using pipelines and
  filters, where the output of one
  program feeds directly into the input
  of another, requires shell and
  operating system support not present
  in the VIC-20.
* Text as a Universal Interface:
  Although capable of handling text,
  the VIC-20 does not center around
  text as a universal interface for
  system interaction and application
  integration, unlike Unix systems.
* Modularity and Composability:
  Limited memory and a simpler
  operating system prevent the VIC-20
  from supporting the level of
  modularity and composability inherent
  in Unix systems.  Programs on the
  VIC-20 are typically monolithic and
  designed to operate in isolation.
* Shell Scripting:
  The VIC-20 lacks a command-line shell
  powerful enough for Unix-like
  scripting.  While it supports BASIC
  programming, this does not offer the
  extensive flexibility and power of
  Unix shell scripting for task
  automation and tool integration.
* Filesystem as a Database:
  Compared to Unix, the VIC-20's
  filesystem capabilities are
  elementary.  It does not treat
  everything as a file in the Unix
  manner, which limits how data and
  devices are managed and interacted
  with.
* Source Code Availability and Portability:
  The VIC-20 operates within a largely
  closed software ecosystem, with
  software typically distributed on
  cartridges, tapes, and disks.  It
  does not partake in the open,
  source-available, and portable nature
  of Unix software that fosters
  modification, adaptation, and
  sharing.

While the VIC-20 marks an important step
in making computing accessible to a
wider audience, its hardware and
software constraints mean it cannot
embody the Unix philosophy or offer
comparable levels of functionality and
flexibility as Unix systems.

# The UltiMem

The UltiMem expansion is an enhancement
for the Commodore VIC-20 that
significantly extends its capabilities,
addressing several limitations.  By
providing up to 1MB of addressable RAM
and additional 8MB Flash ROM, the
UltiMem can influence the VIC-20's
compatibility with the Unix philosophy
and functionalities in the following
ways:

* Expanded Resources:
  Increased RAM: The substantial boost
  in memory from 5KB to up to 1MB
  allows for more complex applications
  and data processing tasks, mitigating
  the original system's limitations.
* Enhanced Storage and Program Complexity:
  Additional ROM Space: With more ROM
  space, users can store and access a
  larger library of software, including
  more sophisticated programs that were
  previously unfeasible due to memory
  constraints.
* Possibility for More Advanced Operating Systems:
  While still not on par with Unix, the
  memory expansion allows for the
  development and use of more complex
  operating systems that could offer
  features like basic multitasking or
  shell capabilities.
* Improvement in Modularity and Composability:
  The expanded memory and storage could
  enable the VIC-20 to run programs
  that are more modular, allowing for
  a rudimentary form of the
  composability found in Unix systems,
  though within the context and
  limitations of 8-bit computing.
* Better Support for Programming and Scripting:
  With more memory, users could
  potentially engage in more complex
  BASIC programming or explore other
  programming environments that UltiMem
  makes feasible, thus slightly moving
  towards automation and scripting
  capabilities.
* Enhanced File Management:
  Although not transforming the VIC-20
  into a Unix-like system, the
  increased memory and ROM space can
  support more advanced file management
  software, improving data organization
  and accessibility.
* Increased Potential for Community and Sharing:
  UltiMem facilitates the creation and
  distribution of more complex
  software, fostering a community of
  developers and users who share,
  modify, and expand upon existing
  programs.

While the UltiMem expansion significant-
ly enhances the VIC-20's capabilities,
it's vital to note that the system
remains fundamentally limited by its
8-bit architecture and cannot fully
realize the Unix philosophy.  The
expansion allows for more sophisticated
software development and a better user
experience but does not transform the
VIC-20 into a Unix-like system.  It
does, however, make strides in
addressing memory-related limitations,
allowing for enhanced functionality
within the scope of what's possible on
an 8-bit home computer platform.

# The TUNIX kernel

TUNIX is an operating system designed
for the Commodore VIC-20 with UltiMem
expansion, emphasizing multi-tasking and
Unix-style KERNAL I/O API.  It provides
features such as multi-tasking, extended
memory management, loadable drivers, and
system calls via a unique file device
mechanism, where commands are sent by
opening a file with a specific name on
device #31, opening the system to all
programming languages that support
regular file I/O.

The differences TUNIX makes:

* Extended Functionality:
  It significantly extends the VIC-20's
  capabilities, introducing Unix-like
  features to an 8-bit platform.
* Multi-tasking:
  By enabling preemptive multi-tasking,
  it allows the VIC-20 to run multiple
  processes concurrently, a
  considerable enhancement over its
  default capabilities.
* Memory Management
  TUNIX makes efficient use of the
  UltiMem expansion, detecting and
  disabling defective RAM banks and
  managing extended memory banks for
  process use.
* Loadable Drivers
  Support for loadable drivers
  increases the VIC-20's versatility,
  allowing it to interface with a
  broader range of hardware and
  software.
* Unix-Style KERNAL I/O API
  This facilitates the development of
  portable and efficient applications,
  leveraging a familiar Unix-style API.
* System Calls via File Device
  This innovative approach allows TUNIX
  to implement system calls in a way
  that is compatible with the VIC-20's
  architecture, providing a flexible
  method for process control, file
  management and driver interaction.

# Wanted applications

TUNIX should include a Linux-compatible
terminal emulator with additional
graphics and DOM-tree manipulation for
document rendering, so the applications
it should provide can range from basic
utilities to more complex software,
leveraging these unique features.  Here
are some applications suited for TUNIX:

* Shell:
  A simple shell for scripting with
  pipes.
* Text Editor:
  A powerful text editor that supports
  syntax highlighting and can handle
  both plain text and code efficiently.
  Integration with the graphics
  capabilities for inline image viewing
  or diagrams would be a plus.

  At the moment a VI clone is available
  which runs without TUNIX and brings
  its own terminal emulator.
* File Manager:
  A graphical file manager that
  utilizes the DOM-tree manipulation
  capabilities for an interactive and
  intuitive interface.  It should
  support basic file operations like
  copy, move, delete and properties
  viewing.

  A file manager has been implemented
  based on a obselete system call API.
* Web Browser:
  A simple web browser capable of
  rendering basic HTML and CSS,
  leveraging the DOM-tree manipulation
  for rendering pages.  It doesn't need
  to be full-featured but should handle
  simple websites and documentation.
  Barrier-free web sites should help
  with this a lot.
* Email Client:
  A lightweight email client with
  support for popular protocols like
  IMAP and SMTP.  Integration with the
  graphics capabilities for viewing
  attachments without leaving the
  application would be beneficial.
* Document Viewer and Editor:
  Applications for viewing and editing
  documents, including text,
  spreadsheets, and presentations.  The
  ability to render simple graphics and
  manipulate the document structure
  through the DOM-tree would
  distinguish it from basic text-only
  editors.

  This would be the VI clone with
  variable fonts and inline images.
* Media Player:
  A media player capable of playing
  audio and video files, utilizing the
  system's graphics capabilities for
  playback controls and possibly
  visualizations for audio files.

  There'll be video one day.  A tape
  audio player can be ported.
* Graphics Editor:
  A basic graphics editor for creating
  and editing images.  It doesn't need
  to be as advanced as professional
  software but should offer essential
  tools for pixel art, drawing, and
  basic image manipulation.
  Displaying by cat'ing them might be
  an idea.
* Programming IDE:
  An Integrated Development Environment
  (IDE) tailored for the VIC-20
  development, including a code editor,
  compiler integration, and debugging
  tools.  It should make good use of
  the terminal for command-line tools
  and the graphical interface for code
  navigation.
* Network Utilities:
  Assuming that someone is willing to
  port uIP or ip65.

  A suite of network utilities for
  diagnostics, including ping,
  traceroute, and network interface
  statistics. These tools would benefit
  from a graphical interface for
  visualizing network performance.
* Games:
  Simple games that demonstrate the
  capabilities of TUNIX, including
  puzzle games, text adventures, and
  simple arcade games.  These can
  showcase both the terminal's
  capabilities and its graphical
  interface.

  However, all existing games as of 2024
  can be used alongside TUNIX as none of
  them ulitizes the UltiMem yet and the
  banks the use are not used by TUNIX.

Developing these applications for TUNIX
would not only showcase its unique
features but also provide a robust
environment for users to perform a wide
range of tasks, from productivity and
development to entertainment.

# Writing applications

All programming languages that support
file I/O can utilize TUNIX.  Here'a
a selection and what they are all about:

## BASIC

The BASIC (Beginner's All-purpose
Symbolic Instruction Code) programming
language, developed in the 1960s by
John Kemeny and Thomas Kurtz, was
designed with a clear philosophy aimed
at making computing accessible to a
broader audience.  Its philosophy can be
distilled into several key principles:

* Accessibility:
  BASIC was created to democratize
  programming, making it accessible to
  students and users without a strong
  mathematical or technical background.
  The language's syntax and structure
  were designed to be easy to learn and
  understand, with keywords closely
  resembling English.
* Education:
  At its core, BASIC was intended as an
  educational tool, enabling students to
  interact directly with computers,
  learn programming concepts, and solve
  problems.  It was developed at a time
  when computing resources were scarce
  and interaction with computers was
  mainly through batch processing.
  BASIC changed that by offering an
  interactive environment.
* Simplicity:
  One of the guiding principles of BASIC
  was to keep the language simple, both
  in its syntax and in the conceptual
  model it presented to programmers.
  Programs are written in straight-
  forward sequences of commands, making
  the flow of execution easy to follow
  and understand.
* Interactivity:
  BASIC was one of the first programming
  languages to support an interactive
  mode of operation, thanks to the
  development of time-sharing systems.
  This allowed programmers to write a
  line of code, run it, and see the
  result immediately, facilitating a
  trial-and-error approach to learning
  and problem-solving.
* Portability:
  While initially designed for a
  specific time-sharing system, BASIC
  quickly became known for its
  portability across different computer
  systems.  This was both a practical
  and philosophical choice, extending
  the language's accessibility to a
  wide range of platforms and users.
* Versatility:
  Despite its simplicity and focus on
  beginners, BASIC was designed to be
  versatile enough for more complex
  programming tasks.  Over time, various
  versions of BASIC introduced
  additional features that expanded its
  capabilities, allowing it to be used
  for everything from simple educational
  exercises to complex business
  applications.
* Community and Sharing:
  The simplicity and wide adoption of
  BASIC fostered a community of users
  who shared code, ideas, and learning
  resources.  Magazines, books, and user
  groups played a significant role in
  the dissemination of BASIC programs
  and programming knowledge, embodying
  the collaborative spirit of the early
  computing era.

In summary, the BASIC philosophy is
about making computing and programming
accessible, understandable, and
enjoyable for beginners, while still
offering enough depth for more advanced
problem solving.  It reflects an
optimistic view of technology as a tool
for education and empowerment, a view
that greatly influenced the development
and proliferation of personal computing.

## ANSI-C (cc65 & Small-Cng)

The philosophy of the C programming
language emphasizes simplicity,
efficiency, and flexibility.  It's
designed to provide low-level access to
memory, simple set of keywords, and
minimal runtime support, favoring direct
manipulation of hardware resources.
This approach allows for efficient
implementations of algorithms and data
structures, suitable for system
programming and applications requiring
high performance.  C encourages a
disciplined approach to program design
and resource management, reflecting its
origins in systems programming and its
widespread use in developing operating
systems, embedded systems, and high-
performance applications.

## The cc65 compiler suite

The cc65 compiler suite used with INGLE
is a comprehensive toolchain for
developing software for 6502-based
systems.  It includes a C compiler,
assembler, linker, and a set of
libraries.  Here’s how cc65 helps in
developing applications for TUNIX:

* Cross-platform Development:
  Since cc65 runs on modern platforms,
  developers can write, compile and
  test their code on faster, more
  convenient systems before deploying
  to a VIC-20 running TUNIX.
* Comprehensive Libraries:
  cc65 provides libraries that abstract
  away the hardware specifics, allowing
  for more portable code.  Libraries
  specific to the 6502 architecture and
  possibly to TUNIX's extensions could
  simplify tasks like UI development,
  file handling, and communication with
  hardware.
* Assembly Language Integration:
  Developers can mix C and assembly
  language for performance-critical
  sections, offering the best balance
  between development efficiency and
  application speed.
* Community and Support:
  A large community and a wealth of
  documentation around cc65 can provide
  support and libraries, which could
  speed up the development of TUNIX
  applications.
* Ease of Learning and Use:
  For developers familiar with C but not
  with 6502 assembly, cc65 makes the
  VIC-20 platform more accessible,
  broadening the potential developer
  base for TUNIX applications.
* Standard Tools for Build Processes:
  cc65’s toolchain supports standard
  software development practices, such
  as makefiles for automating builds,
  which is beneficial for larger
  projects or teams.
* Compatibility with Existing Code:
  There's a wealth of existing code
  written for cc65.  Developers can
  leverage this, adapting it for TUNIX
  applications, which can accelerate
  development.
* Support for Advanced Features:
  Features like bank switching in the
  UltiMem expansion can be managed more
  easily with the support of the cc65
  toolchain, enabling more complex
  applications that require extensive
  memory.

In summary, the cc65 compiler suite
provides a powerful and flexible
development environment for the VIC-20,
significantly enhancing the capabilities
for developers building applications for
TUNIX.  It lowers the barrier to entry
for new developers, enables the
creation of more complex and efficient
applications, and fosters a vibrant
development community around this retro
computing platform.

## C Programming With Small-C

A self-hosting port of the Small-C
compiler is on its way, but it will
not replace cc65 anytime soon.

## FORTH (VFORTH)

VFORTH, or any variant of Forth, adheres
to a philosophy that significantly
diverges from more conventional
programming paradigms, embracing
minimalism, directness, and a deep
connection between the programmer and
the machine's workings. Forth as a
language is known for its unique
approach to problem-solving, program
structure, and interaction with the
computer system. Here’s a look at the
core principles that underpin the Forth
philosophy:

* Simplicity and Minimalism:
  Forth is designed to be simple and
  minimalistic, both in its syntax and
  the size of its compiler/interpreter.
  This simplicity extends to programs
  written in Forth, which are composed
  of a series of definitions (or
  "words") that define new operations.
* Extensibility:
  A fundamental aspect of Forth is its
  extensibility.  Programmers can define
  new words (functions or procedures)
  using existing ones, effectively
  growing the language to suit their
  particular needs.  This means that
  Forth can be as high-level or as
  low-level as required by the task at
  hand.
* Stack-based Computation:
  Forth uses a stack-based model for
  computation, contrasting with the
  register-based models of many other
  languages.  This approach simplifies
  the language's syntax and compiler
  design and encourages a different
  style of thinking about program flow
  and data manipulation.
* Interactivity:
  Like Lisp, Forth environments
  typically include an interactive shell
  (REPL), allowing programmers to test
  parts of their program incrementally
  and interact with the running system
  directly.  This facilitates
  experimentation and rapid development.
* Efficiency and Directness:
  Forth programs often run very
  efficiently, and the language allows
  direct access to hardware resources,
  making it suitable for systems
  programming and embedded systems
  development.  Forth's philosophy
  embraces getting as close to the
  machine's metal as possible, without
  unnecessary abstractions.
* Readability and Understandability:
  While Forth code can be highly
  concise and efficient, it also
  encourages writing readable and
  understandable code.  The creation of
  well-named words can lead to programs
  that read like a series of high-level
  instructions, tailored to the problem
  domain.
* Resource-Conscious Development:
  Given its history and design goals,
  Forth promotes being mindful of
  resource usage, making it particularly
  well-suited for constrained environ-
  ments where memory and processing
  power are limited.
* Problem Solving and Creativity:
  The Forth philosophy emphasizes
  problem-solving and creativity.  It
  encourages programmers to think about
  problems in new ways, often leading
  to innovative solutions that would be
  less obvious in more structured or
  restrictive languages.
* Self-Sufficiency:
  A Forth system is typically
  self-contained, including its
  compiler, interpreter, and debugger
  in a compact package.  This self-
  sufficiency reflects a philosophy of
  independence and control over the
  computing environment.
* Community and Adaptability:
  The Forth community values adapt-
  ability, with the language itself and
  its implementations evolving in
  response to new challenges and
  environments.  This adaptability is
  mirrored in the diverse applications
  of Forth, from embedded systems to
  desktop applications.

In essence, the Forth philosophy is
about empowerment, directness, and
efficiency.  It empowers the programmer
to work closely with the computer,
crafting solutions that are both elegant
and close to the hardware, all within a
minimalist and extensible language
framework.

## The Lisp Philisophy

The Lisp philosophy centers around a
few core principles that reflect both
its design ethos and the way it's used
to solve problems. Lisp, as one of the
oldest high-level programming languages,
has a rich history of development and
use in various domains, especially in
artificial intelligence (AI), symbolic
processing, and rapid prototyping.

Here are the key aspects of the Lisp
philosophy:

* Code as Data (and Vice Versa):
  One of Lisp's most fundamental
  principles is that code and data are
  interchangeable, thanks to its uniform
  syntax.  This principle, known as
  homoiconicity, allows Lisp programs
  to manipulate their own code as a data
  structure, enabling powerful macros
  and metaprogramming techniques.
* Simplicity and Elegance:
  Lisp syntax is minimalistic, which
  makes it not only easy to learn but
  also highly expressive.  The
  simplicity of its syntax, with the use
  of parentheses for both code and data
  structure notation, enables elegant
  solutions to complex problems.
* Extensibility:
  Lisp is designed to be extended.
  Users can define new operators and
  constructs that feel as natural as
  built-in ones.  This ability to grow
  the language toward specific problem
  domains is a key part of the Lisp
  philosophy.
* Interactivity and Rapid Prototyping:
  Lisp environments traditionally offer
  an interactive REPL (Read-Eval-Print
  Loop), allowing developers to
  interact with the running program,
  test functions incrementally, and see
  results immediately.  This supports a
  rapid prototyping approach, where
  developers can experiment and iterate
  quickly.
* Recursion and Functional Programming:
  Lisp embraces recursion as a primary
  mechanism for iteration and promotes a
  functional programming style, where
  functions are first-class citizens,
  and immutable data structures are
  preferred.  This emphasis supports
  clear, concise, and correct code.
* Symbolic Processing:
  Lisp was designed with symbolic
  processing in mind. Its ability to
  easily manipulate symbols and lists
  makes it particularly suited for tasks
  that involve symbolic computations,
  such as AI, compilers, and theorem
  proving.
* Emphasis on Artificial Intelligence:
  From its inception, Lisp has been
  closely associated with AI research.
  Its features are well-suited to
  pattern matching, tree-based data
  structures, and rule-based logical
  reasoning, making it a preferred
  language for AI projects for many
  years.
* Community and Evolution:
  The Lisp community values the
  continuous evolution of the language
  and its dialects, guided by the
  experiences and needs of its users.
  This has led to the creation of
  numerous Lisp dialects, each tailored
  to different needs and philosophies.

All in all, the Lisp philosophy is about
flexibility, power, and simplicity.  It
advocates for an interactive development
process with tools that empower
developers to write concise, expressive
code, and it encourages the extension
and evolution of the language to meet
the specific needs of various domains.

As good as this sounds TUNIX will not
come with a Lisp interpreter to run
applications but merely C programs that
use Lisp data structures for document
processing.

# What's Missing in this Documentation:

* Detailed Installation Guide:
  Instructions for installing TUNIX on
  a VIC-20 equipped with UltiMem are
  not provided.
* Comprehensive API Reference:
  While the manual lists some system
  calls, a more comprehensive
  reference, including all possible
  commands and their arguments, would
  be beneficial.
* Examples and Tutorials:
  Practical examples and tutorials on
  using TUNIX for common tasks would
  help users get started more quickly.
* Troubleshooting and Common Issues:
  A section dedicated to
  troubleshooting common issues and how
  to resolve them would be useful.
* Performance Considerations:
  Information on the impact of TUNIX on
  the VIC-20's performance, including
  any limitations or bottlenecks
  introduced by the OS.
* Compatibility Information:
  Details on compatibility with
  existing VIC-20 software and hardware
  peripherals would help users
  understand how TUNIX fits into their
  current setup.
* Security Features:
  The documentation does not detail any
  security features or practices
  implemented by TUNIX, which would be
  important for multi-user or networked
  environments.
* Contribution Guidelines:
  For an open-source project,
  guidelines on how users can
  contribute to TUNIX's development
  would foster community involvement.
BLOG.md  BOOKS.md  BUILD.md  PLAN.md  README.md
