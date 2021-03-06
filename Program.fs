﻿open System

type Timed<'a> =
    {
        Started : DateTimeOffset
        Stopped : DateTimeOffset
        Result : 'a 
    }
    member this.Duration = this.Stopped - this.Started


    module Untimed =
        let map f x = 
            { Started = x.Started; Stopped = x.Stopped; Result = f x.Result }

        let withResult newResult x = x |> map(fun _ -> newResult) 

module Timed = 
    let capture clock x = 
        let now = clock()
        {Started = now; Stopped = now ; Result = x}
    let map clock f x = 
        let result = f x.Result
        let stopped = clock()
        {Started = x.Started; Stopped = stopped; Result = result}
    let timeOn clock f x = 
        x |> capture clock |> map clock f

module Clocks = 
    let machineClock () = DateTimeOffset.Now
    let strime (x : DateTimeOffset) = x.ToString "Y"
    let acclock (start :DateTimeOffset) rate () = 
        let now = DateTimeOffset.Now
        let elapsed = now - start
        start.AddTicks(elapsed.Ticks * rate)
    
    open System.Collections.Generic

    let qlock (q : Queue<DateTimeOffset> ) = q.Dequeue
    let seqlock(l : DateTimeOffset seq ) = l |> Queue<DateTimeOffset> |> qlock


// Module 2

type todo = unit 
let todo () = ()

//Auxiliary Types 
type MessageHandler = unit -> Timed<unit>
//State Data 

type ReadyData = Timed<TimeSpan list>  
// The type is declared on the base of ShouldPoll function 
// however having done so will make idle 
// return Timpespans... Which is suspicious at least
// So we carry around times in the NoMessage Data  
type ReceivedMessageData = Timed<TimeSpan list * MessageHandler>
type NoMessageData = Timed<TimeSpan list >


//States

type PollingConsumer = 
| ReadyState of ReadyData
| ReceivedMessageState of ReceivedMessageData
| NoMessageState of NoMessageData
| StoppedState 


// If the consumer is in stopped state it will stay in a stopped state
// This mHo lo eans no data associated with Stopped state -> Function degenerates to value

let transitionFromStopped : PollingConsumer = 
    StoppedState


// We can introduce a function on the fly as an input
// After we can contemplate the function and see that some states are actually timed
// TransitionFromMessage Is a higher order function because it depends on two functions shouldIdle and Idle
// If we do not supply these two functions the compiling system will complain

let transitionFromNoMessage shouldIdle idle (nm : NoMessageData) =
 if nm |> shouldIdle
 then idle () |> Untimed.withResult nm.Result |> ReadyState
 else StoppedState

let transitionFromready shouldPoll poll (r : ReadyData) = 
 if r |> shouldPoll
 then 
    let msg = poll ()
    match msg.Result with 
        | Some h -> msg |>  Untimed.withResult (r.Result, h) |>  ReceivedMessageState
        | None -> msg |> Untimed.withResult r.Result |> NoMessageState
 else StoppedState


// We have seen how to implement the Ready State, This was a bit more involved that what we have seen before,
 // We needed to look back at some types as well as had two more functions to our todo list


 // We use a tuple in the Received Message Data 
 // We measure the tie it took to handle the state 

let transitionFromReceived (rm : ReceivedMessageData) =
 let durations, handleMessage = rm.Result
 let t = handleMessage ()
 let PollDuration  = rm.Duration
 let handleDuration = t.Duration
 let totalDuration = PollDuration + handleDuration
 t |> Untimed.withResult (totalDuration :: durations) |> ReadyState



 // We used a top Down programming approach using the types to keep a to do list.
 // Recap --> Arguments of function are related to 
 // Injection of arguments (Closely related to dependency Injection)
 // Contemplation (function signatures)
 // To do List 
