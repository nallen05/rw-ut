 
# RW-UT

the functions `READ-TIME-STRING` and `WRITE-TIME-STRING` are for reading and writing
lisp universal time as strings
 
universal times stored as strings using `READ-TIME-STRING` and `WRITE-TIME-STRING`
have the following advantages over normal universal times stored as integers:
 
1. They are human readable and thus less prone to bugs
2. They are easier to use when interacting with external databases or other programs that expect times dates encoded as strings
3. They are ISO-8601 compliant (by default)

       CL-USER> (defvar now (get-universal-time))
       NOW
 
       CL-USER> now
       3425557791
 
       CL-USER> (use-package :rw-ut)
       T
 
       CL-USER> (rw-ut:write-time-string now)
       "2008/07/20 15:49:51"
 
       CL-USER> (rw-ut:read-time-string *)
       3425557791
 
       CL-USER> (eql * now)
       T
 
       CL-USER> (rw-ut:write-time-string now "MM-DD-YY")
       "07-20-08"
 
       CL-USER> (rw-ut:read-time-string * "MM-DD-YY")
       3425500800
 
       CL-USER> (rw-ut:write-time-string * "YYYY/MM/DD)
       "2008/07/20"
 
## PATTERNS
 
`READ-TIME-STRING` and `WRITE-TIME-STRING` are designed to work with "patterns".
patterns are strings that control how `READ-TIME-STRING` and `WRITE-TIME-STRING`
read and write universal time as strings.
 
Patterns look like the psuedocode described in the the ISO-8601-2004_E document
in section 3.4.2 "Characters used in place of digits or signs" (dont worry, it's
also the same psuedocode used in the Wikipedia article on ISO-8601 ;-).
 
The `#\?` character introduces a "breakpoint". When writing (with `WRITE-TIME-STRING`),
everything after a breakpoint will be omitted if there is nothing non-boring
after it. when reading (with `READ-TIME-STRING`), everything after the breakpoint
can be omitted and will be assumed to be boring values ("boring values" means 1 for
month and day and 0 for everything else)
 
`READ-TIME-STRING` and `WRITE-TIME-STRING` currently understand the following
patterns:
 
    YYYY -- year, 4 digits
    YY   -- year, 2 digits
    MM   -- month, 2 digits
    M    -- month, 1-2 digits
    DD   -- day, 2 digits
    D    -- day, 1-2 digits
    hh   -- hour, 2 digits
    h    -- hour, 1-2 digits
    mm   -- minute, 2 digits
    m    -- minute, 1-2 digits
    ss   -- second, 2 digits
    s    -- second, 1-2 digits
 
The `#\\` char is the escape char, use it to read or write things that look like
patterns but aren't
 
##  BREAK POINTS (the `#\?`)
 
Anything after a `#\?` can be "omitted", since `READ-TIME-STRING` and
`WRITE-TIME-STRING` are lazy and don't like to deal with minumum values if they
don't have to (boring balues being 1 for dates and months, 0 for everything else)
 
    RW-UT> (write-time-string (read-time-string "2008") "YYYY-MM-DD")
    "2008-01-01"
 
     RW-UT> (write-time-string (read-time-string "2008") "YYYY-MM?-DD")
    "2008-01"
 
    RW-UT> (write-time-string (read-time-string "2008") "YYYY?-MM?-DD")
    "2008"
 
## THE DEFAULT PATTERN
 
The default pattern for both `READ-TIME-STRING` and `WRITE-TIME-STRING` is
 
    YYYY?/MM?/DD? hh?:mm?:ss
 
## TOLERANCE
 
`READ-TIME-STRING` will accept two kinds of junk:
 
1. Different literal characters that don't effect the length of the string. For example, the default pattern can read strings that look like this:
 
       YYYY-MM-DD hh:mm:ss
 
   but it can also read strings that look like this:
 
       YYYY-MM-DDThh:mm:ss
 
   _Note: numberic characters in the wrong spot will probably screw things up because
it looks like part of the date_
       
2. there can be junk at the _end_ of the string. so the default pattern can read string that look like this:
 
       YYYY-MM-DD hh:mm:ss
 
   but it can also read strings that look like this:
 
       YYYY-MM-DD hh:mm:ss.0000Z
 
## API

* `READ-TIME-STRING (string &optional (pattern "YYYY-MM-DD hh:mm:ss"))`

- - -
 
_Function_. Reads `STRING` according to the pattern string `PATTERN` and returns a
Universal Time intiger.
 
* `WRITE-TIME-STRING (ut &optional (pattern "YYYY-MM-DD hh:mm:ss"))`

- - -
 
_Function_. Writes the universal time integer `UT` as a string according to the
pattern string `PATTERN`
