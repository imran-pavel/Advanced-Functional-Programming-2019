{-|

Write an Erlang program that spawns two processes and gives them both a list of integers and the other processes' process identifier.
Then the processes start taking turns, sending each other at one time one integer from the list.
If the reciever recieves an integer that is in its original list of integers, it sends information about that to the sender.
Then both will terminate.

-}


{-

May be I'll do it later

         (---)   (---)
               \
                \
             ____\
             
            \_____/

-}


