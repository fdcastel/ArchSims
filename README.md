# ArchSims

Simulators for usage in computer architecture classes.



## Overview

This project is a [_clean room_](https://en.wikipedia.org/wiki/Clean_room_design) implementation of the [hypothetical machines](https://pt.wikipedia.org/wiki/M%C3%A1quinas_hipot%C3%A9ticas_da_Universidade_Federal_do_Rio_Grande_do_Sul) used in computer architecture classes from [Institute of Informatics](http:////www.inf.ufrgs.br) of Universidade Federal do Rio Grande do Sul. 

The goal is to faithfully recreate the [instruction set architecture](https://en.wikipedia.org/wiki/Instruction_set_architecture) for all machines. To accomplish this the code has extensive test coverage. Each implementation was made following the original specifcations made by professors Raul Fernando Weber and Taisy Silva Weber.

There is no UI. But there are plans to make one in the future.



## Content

This repository contains:

  - Implementations for Neander, Ahmes, Ramses and Cesar
  - Assemblers for Ramses and Cesar
  - A Ramses emulator for Cesar (_it was a cold and rainy day..._)
  - Thorough test cases
    - An adapter to test Ramses emulator with same test cases used in Ramses.
  - A command line utility for simple demos.



## Prerequisites

  - .NET Core 2.0



## To run the tests

```
dotnet test
```



## To run the samples

Just run the `.cmd` scripts from `Samples` folder (Windows only).



## Special thanks

  - Prof. Raul Fernando Weber (_in memorian_)

![](img/Weber.jpg)
