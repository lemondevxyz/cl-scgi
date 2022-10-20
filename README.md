## cl-scgi
cl-scgi is a library server implementation of the simple common gateway interface.

The library aims to be stable at a certain point with enough tests to ensure that the functionality isn't hindred by one-time bugs. 

Currently, the status of the library is pre-alpha, with many of the features planned for being more or less missing.

Finally, the library aims to escape the Common Curse™ of Common Lisp libraries missing documentation. It aims to be extremely usable with examples all around.

Check out the [blog post](https://lemondev.xyz/posts/cl-scgi-scgi-in-common-lisp/) in-order to understand `cl-scgi` a lot better.

## checklist
- [x] Parsing Messages (with unit tests)
- [x] ASDF System definition
- [x] Unix Server
- [x] TCP Server
- [x] Better error handling through condition messages
- [x] Multi-threaded functionality
- [ ] Switch to fast-io for most operations
