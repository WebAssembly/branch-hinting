.. index:: custom section, section, binary format

Custom Sections
---------------

This appendix defines dedicated :ref:`custom sections <binary-customsec>` for WebAssembly's :ref:`binary format <binary>`.
Such sections do not contribute to, or otherwise affect, the WebAssembly semantics, and like any custom section they may be ignored by an implementation.
However, they provide useful meta data that implementations can make use of to improve user experience or take compilation hints.

Currently, two dedicated custom sections are defined: the :ref:`name section<binary-namesec>` and the :ref:`branch hints section<binary-branchHintsSec>`.



.. index:: ! name section, name, Unicode UTF-8
.. _binary-namesec:

Name Section
~~~~~~~~~~~~

The *name section* is a :ref:`custom section <binary-customsec>` whose name string is itself :math:`\text{name}`.
The name section should appear only once in a module, and only after the :ref:`data section <binary-datasec>`.

The purpose of this section is to attach printable names to definitions in a module, which e.g. can be used by a debugger or when parts of the module are to be rendered in :ref:`text form <text>`.

.. note::
   All :ref:`names <binary-name>` are represented in |Unicode|_ encoded in UTF-8.
   Names need not be unique.


.. _binary-namesubsection:

Subsections
...........

The :ref:`data <binary-customsec>` of a name section consists of a sequence of *subsections*.
Each subsection consists of a

* a one-byte subsection *id*,
* the |U32| *size* of the contents, in bytes,
* the actual *contents*, whose structure is depended on the subsection id.

.. math::
   \begin{array}{llcll}
   \production{name section} & \Bnamesec &::=&
     \Bsection_0(\Bnamedata) \\
   \production{name data} & \Bnamedata &::=&
     n{:}\Bname & (\iff n = \text{name}) \\ &&&
     \Bmodulenamesubsec^? \\ &&&
     \Bfuncnamesubsec^? \\ &&&
     \Blocalnamesubsec^? \\
   \production{name subsection} & \Bnamesubsection_N(\B{B}) &::=&
     N{:}\Bbyte~~\X{size}{:}\Bu32~~\B{B}
       & (\iff \X{size} = ||\B{B}||) \\
   \end{array}

The following subsection ids are used:

==  ===========================================
Id  Subsection                                 
==  ===========================================
 0  :ref:`module name <binary-modulenamesec>`
 1  :ref:`function names <binary-funcnamesec>`    
 2  :ref:`local names <binary-localnamesec>`
==  ===========================================

Each subsection may occur at most once, and in order of increasing id.


.. index:: ! name map, index, index space
.. _binary-indirectnamemap:
.. _binary-namemap:

Name Maps
.........

A *name map* assigns :ref:`names <syntax-name>` to :ref:`indices <syntax-index>` in a given :ref:`index space <syntax-index>`.
It consists of a :ref:`vector <binary-vec>` of index/name pairs in order of increasing index value.
Each index must be unique, but the assigned names need not be.

.. math::
   \begin{array}{llclll}
   \production{name map} & \Bnamemap &::=&
     \Bvec(\Bnameassoc) \\
   \production{name association} & \Bnameassoc &::=&
     \Bidx~\Bname \\
   \end{array}

An *indirect name map* assigns :ref:`names <syntax-name>` to a two-dimensional :ref:`index space <syntax-index>`, where secondary indices are *grouped* by primary indices.
It consists of a vector of primary index/name map pairs in order of increasing index value, where each name map in turn maps secondary indices to names.
Each primary index must be unique, and likewise each secondary index per individual name map.

.. math::
   \begin{array}{llclll}
   \production{indirect name map} & \Bindirectnamemap &::=&
     \Bvec(\Bindirectnameassoc) \\
   \production{indirect name association} & \Bindirectnameassoc &::=&
     \Bidx~\Bnamemap \\
   \end{array}


.. index:: module
.. _binary-modulenamesec:

Module Names
............

The *module name subsection* has the id 0.
It simply consists of a single :ref:`name <binary-name>` that is assigned to the module itself.

.. math::
   \begin{array}{llclll}
   \production{module name subsection} & \Bmodulenamesubsec &::=&
     \Bnamesubsection_0(\Bname) \\
   \end{array}


.. index:: function, function index
.. _binary-funcnamesec:

Function Names
..............

The *function name subsection* has the id 1.
It consists of a :ref:`name map <binary-namemap>` assigning function names to :ref:`function indices <syntax-funcidx>`.

.. math::
   \begin{array}{llclll}
   \production{function name subsection} & \Bfuncnamesubsec &::=&
     \Bnamesubsection_1(\Bnamemap) \\
   \end{array}


.. index:: function, local, function index, local index
.. _binary-localnamesec:

Local Names
...........

The *local name subsection* has the id 2.
It consists of an :ref:`indirect name map <binary-indirectnamemap>` assigning local names to :ref:`local indices <syntax-localidx>` grouped by :ref:`function indices <syntax-funcidx>`.

.. math::
   \begin{array}{llclll}
   \production{local name subsection} & \Blocalnamesubsec &::=&
     \Bnamesubsection_2(\Bindirectnamemap) \\
   \end{array}


.. index:: ! branch hints section, hint
.. _binary-branchHintsSec:
.. _binary-funchints:

Branch Hints Section
~~~~~~~~~~~~~~~~~~~~

The *branch hints section* is a :ref:`custom section <binary-customsec>` whose name string is :math:`\text{branchHints}`.
The branch hints section should appear only once in a module, and only before the :ref:`code section <binary-codesec>`.

The purpose of this section is to aid the compilation of conditional branch instructions, by providing a hint that a branch is very likely (or unlikely) to be taken.

An implementation is not required to follow the hints, and this section can be entirely ignored.

The section contains a vector of *function branch hints* each representing the branch hints for a single function.

Each *function function hints* structure consists of

* the :ref:`function index <binary-funcidx>` of the function the hints are referring to,
* a single 0 byte,
* a vector of *branch hints* for the function.

Each *branch hint* structure consists of

* a |U32| indicating the meaning of the hint:

=====  ===========================================
Value  Meaning                                    
=====  ===========================================
 0x00  branch likely not taken
 0x01  branch likely taken
=====  ===========================================

* the |U32| byte offset of the hinted instruction from the first instruction of the function.


.. math::
   \begin{array}{llclll}
   \production{branch hints section} & \Bbranchhintssec &::=&
     \Bsection_0(\Bfuncbranchhints) \\
   \production{function branch hints} & \Bfuncbranchhints &::=&
     \Bfuncidx~\hex{00}~\Bvec(\Bbranchhint) \\
   \production{branch hint} & \Bbranchhint &::=&
     \Bbranchhintkind~~\X{instoff}{:}\Bu32 \\
   \production{branch hint kind} & \Bbranchhintkind &::=&
     \hex{00} \\ &&&
     \hex{01} \\
   \end{array}

