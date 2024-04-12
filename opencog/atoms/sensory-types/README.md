
Sensory Atom Types
------------------
This defines some atom types, currently, just nodes, that indicate
how to connect to the external-world. Some of the current and
envisioned types include:

* `FileIONode` -- read and write files and directories.
* `IRChatNode` -- IRC chatbot connection
* `TwitterNode` -- Twitterbot Node
* Vision & sound: see the [opencog/vision](https://github.com/opencog/vision)
  git repo.

Only the first is being implemented.  The second has some useful
scaffolding code in the old opencog repo that needs to be copied here.
The third is currrently a daydream. The fourth exists as a
proof-of-concept, implementing a wrapper around OpenCV.

General information about the `sensory_types.script` file can be found
in the 
[AtomSpace atom_types](https://github.com/opencog/atomspace/tree/master/opencog/atoms/atom_types)
directory.
