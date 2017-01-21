# Changes

* 2017-01-21 label command parsing
* 2017-01-20 begincom and endcom
* 2017-01-19 includeDictionary command
* 2017-01-15 def becomes a real command definition, no support for arguments yet
* 2016-12-09 beginchapter, endchapter and bye commands
* 2016-12-02 input command support
* 2016-12-02 def, chapref command support
* 2016-12-01 $A$ and $A\\sub b$ formula support
* 2016-12-01 oftype, thefunction and newtermidx commands
* 2016-11-30 displaythree command
* 2016-11-30 seevar, seefuns, varref and funref, ie, thenextfigure, typeref, metavar commands
* 2016-11-30 issue and endissue command - noop for now, let's implement them whenever we actually render list of issues
* 2016-11-30 \term, \newterm commands, begin/end subsubsubsubsubsubsubsection commands in bulk
* 2016-11-30 Basic parsing for text blocks
* 2016-11-28 print-xml now supports stream
* 2016-11-28 \defineSection command support
* 2016-11-28 Test subsystem
* 2016-11-27 To make things easier, dpANS sources are now a part of the repo
* 2016-11-26 Added framework to define commands, basic functions to generate document from stream of commands
* 2016-11-23 Parser can process the whole file now
* 2016-11-20 Get a flat stream of commands/text blocks for a file
* 2016-11-20 Parser is in state were it can parse the whole file with some luck
