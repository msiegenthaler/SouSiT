SouSiT
======

Haskell library for *Sources*, *Sinks* and *Transformers*.
The data is pushed from the source through the transfomers into the sink. The sink or a transfomer can decide at any time to end the transfer (see Iteratees).
The transformers are very reusable since they can not depend on side effects, so they can be used with files as well as with simple lists.

Allows you to build pipelines such as:

    ghci> listSource [1..10] $$ T.map(+1) =$= T.buffer 3 0 (+) =$ listSink
    [9,18,27,11]

its possible to mix various type of sources and sinks, such as in:

    ghci> fileSourceLine "myfile.txt" $$ T.drop 1 =$= T.map (++ "!") =$ listSink
    ["Hello Mario!", "How're you doing?!"]

Available Sources
-----------------
* List
* Handle (IO)
* File

More types of sources can be added by users of the library. Consider using actionSource or bracketActionSource as a help if you do so.



Available Sinks
---------------
* List
* Handle (IO)
* File
* monadic sink

More types of sinks can be added by users of the library. Consider using either actionSink or the monadic sink interface if implementing your own sinks.

### Monadic Sink
You can write sink in monad style

Example that reads the first input, then skips the next three values and then reads two values and terminates. It outputs a tuple with the three values read.

    mySink = do v1 <- input
                skip 3
                v2 <- input
                v3 <- input
                return (v1, v2, v3)
