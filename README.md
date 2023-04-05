# Table of Contents

- [Description](#description)
- [Getting The Binary](#getting_the_binary)
- [Building From Source](#building_from_source)
- [How to Use](#how_to_use)
- [Examples](#examples)

<a name="description"></a>
# Description

xml2ecl is a command-line tool that examines XML data and deduces the
ECL RECORD definitions necessary to parse it.  The resulting ECL definitions are returned
via standard out, suitable for piping or pasting into your favorite IDE.

## ECL Record Definitions ???

[HPCC Systems](https://hpccsystems.com) is a big data system that is programmed using a
declarative language called Enterprise Control language (ECL).  It is a schema-on-read
system, meaning that you supply a schema to the function that reads data for processing.
An "ECL record definition" in this context means that schema:  xml2ecl generates the
schema as text that can be pasted into an IDE and used within an ECL program.

<a name="getting_the_binary"></a>
# Getting The Binary

Head over the [releases](https://github.com/dancamper/xml2ecl/releases) section of
the Github repo and choose the version that matches your operating system.  Decompress
the file and put the result somewhere on your PATH for easy reference.

<a name="building_from_source"></a>
# Building From Source

This project was written using [Steel Bank Common Lisp (SBCL)](http://www.sbcl.org) and
it has not been tested with other flavors of Lisp.  There are very few dependencies,
however, so it should work with minimal modifications with all of the Common Lisp
distributions.

The following dependencies are required:

- [ASDF](https://asdf.common-lisp.dev) (version 3.3.0 or later)
- [Quicklisp](https://www.quicklisp.org/beta/)
  - Packages installed via QuickLisp by the build script
    - [adopt](https://docs.stevelosh.com/adopt/) (parsing common-line options and arguments)
    - [flexi-streams](https://edicl.github.io/flexi-streams/) (Flexible bivalent streams)
    - [fxml](https://github.com/ruricolist/FXML) (SAX-style XML parsing)
    - [with-user-abort](https://github.com/compufox/with-user-abort) (handles ctrl-c)
- [Buildapp](https://www.xach.com/lisp/buildapp/) (used to build the binary)
  - Note that the `buildapp` binary as well as your Lisp's binary must be on your PATH.

## Build Instructions (for *nix-compatible systems)

1. Clone this repo:  `git clone https://github.com/dancamper/xml2ecl.git`
1. Change directory: `cd xml2ecl`
1. Run build script: `./build_binary.sh`

Built binary is `bin/xml2ecl`.  You can move or copy it to a location on your path.

<a name="how_to_use"></a>
# How to Use

Usage: `xml2ecl [OPTIONS] [FILE...]`

xml2ecl examines XML data and deduces the ECL RECORD definitions necessary to
parse it. The resulting ECL definitions are returned via standard out, suitable
for piping or copying and pasting into your favorite IDE.

XML data can be supplied as one or more files or via standard input.

Multiple files, if provided, are parsed as if they should have the same record
structure. This is useful for cases where you suspect that not all XML
key/value objects are fully defined in one file, but other files may contain the
missing data.

ECL records will be created with fields named after the keys found in XML objects.
Every field will have an XPATH attribute added so the ECL reader can correctly
read everything, no matter what the field is named.

ECL keywords, in general, should not be used as field names in record definitions.
xml2ecl will prefix those fields with "f_" when defining those field names.  Other
minor changes to the field names are also made (such as converting dashes to
underscores).

The last ECL record definition in the output will be the "root" definition; it
is the one you should pass to the ECL DATASET() function (and a commented-out
example DATASET() call is provided in the output).  If you pass exactly
one file to xml2ecl then that record definition will be named after the file.
If you pass multiple files, or stream XML data in via standard input, then the
layout will be named TOPLEVEL with some added items to make it unique.

```none
Options:
  -v, --version         Display version and exit.
  -h, --help            Display help and exit.
  -s STRING-TYPE, --string-type STRING-TYPE
                        ECL datatype to use for strings; must be one of
                        UTF8|STRING|VARSTRING; defaults to UTF8
```

The `-h` and `-v` options should be obvious.

The -s option allows you to override the ECL datatype used for string values.
Because XML data is normally in UTF-8 format, `UTF8` is the default ECL data type for
those values.  However, if you know that the data is in plain ASCII then you can override
the type with this option.  The acceptable values are:

- `UTF8`: A UTF-8 string; this is the default.
- `STRING`: An ASCII string.
- `VARSTRING`:  A C-style null-terminated ASCII string.  Don't use this unless you know why you need it.

<a name="examples"></a>
# Examples

Assuming file foo.xml contains the following contents:

```xml
<node start="12" end="98.76">
    <foo>bar</foo>
</node>
```

Simple parsing of those contents.  The `end` XML key is an ECL keyword, so it
was modified with the `f_` prefix and an ECL XPATH markup added.

```none
$ xml2ecl foo.xml

NODE_LAYOUT := RECORD
    UNSIGNED start {XPATH('@start')};
    REAL f_end {XPATH('@end')};
    UTF8 foo {XPATH('foo')};
END;

FOO_LAYOUT := RECORD
    DATASET(NODE_LAYOUT) node {XPATH('node')};
END;

// ds := DATASET('~data::foo', FOO_LAYOUT, XML('/'));
````

You can pipe XML content instead of reading a file.  Note that if you pipe
multiple files then xml2ecl will treat them all as a single file, which may
change the ECL record definitions.

Example of piping the contents of a single file:

```none
$ cat foo.xml | xml2ecl 

NODE_LAYOUT := RECORD
    UNSIGNED start {XPATH('@start')};
    REAL f_end {XPATH('@end')};
    UTF8 foo {XPATH('foo')};
END;

TOPLEVEL_226_LAYOUT := RECORD
    DATASET(NODE_LAYOUT) node {XPATH('node')};
END;

// ds := DATASET('~data::toplevel_226', TOPLEVEL_226_LAYOUT, XML('/'));
````

Simple example of overriding the default string ECL data type:

```none
$ xml2ecl -s STRING foo.xml

NODE_LAYOUT := RECORD
    UNSIGNED start {XPATH('@start')};
    REAL f_end {XPATH('@end')};
    STRING foo {XPATH('foo')};
END;

FOO_LAYOUT := RECORD
    DATASET(NODE_LAYOUT) node {XPATH('node')};
END;

// ds := DATASET('~data::foo', FOO_LAYOUT, XML('/'));
````

If you process multiple XML files at once, xml2ecl assumes that each file represents
a separate example of the same underlying structure.  This is useful, as variations in
XML field values could be discovered and "filled in" by these additional data files.

Assuming a second file baz.xml with the following contents:

```xml
<node start="42" end="false" incr="3.5">
    <foo>frob</foo>
</node>

```

Notice that the `end` field contains a boolean instead of a float, and there is an
additional field named `incr` in the object.  The two layouts from the two files
were merged:

```none
$ xml2ecl foo.xml baz.xml 

NODE_LAYOUT := RECORD
    UNSIGNED start {XPATH('@start')};
    STRING f_end {XPATH('@end')}; // boolean, float
    REAL incr {XPATH('@incr')};
    UTF8 foo {XPATH('foo')};
END;

TOPLEVEL_226_LAYOUT := RECORD
    DATASET(NODE_LAYOUT) node {XPATH('node')};
END;

// ds := DATASET('~data::toplevel_226', TOPLEVEL_226_LAYOUT, XML('/'));
```
