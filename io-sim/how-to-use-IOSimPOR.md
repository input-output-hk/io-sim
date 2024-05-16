How to use Control.Monad.IOSimPOR
=================================

IOSimPOR is a version of IOSim that supports finding race conditions, by

 - allowing many alternative schedules to be explored, and

 - searching through schedules in an order which (empirically) is
   likely to play well with shrinking; when a test case that provokes
   a race condition is shrunk to a smaller test case that suffers from
   the same race condition, then it is likely that IOSimPOR will
   actually provoke the race condition in the smaller test too.

To do so, IOSimPOR detects potential racing steps when a test is run,
and then re-runs the same test many times with one or more pairs of
racing steps reversed. IOSimPOR runs each equivalent schedule at most
once.

IOSimPOR will usually not explore all possible schedules, because
there are too many for this to be feasible (even infinitely many, in
some cases). It prioritises races that occur earlier in a test
execution, and prioritises making a small number of race reversals
over making many.

It can test non-terminating programs with an infinite trace; in such
cases it only reverse races in the part of the trace that the property
under test actually depends on. Thus properties that select, for
example, the first simulated 24 hours of trace events, can be tested
using IOSimPOR; only races that occur in the first 24 hours will be
reversed.

IOSimPOR assumes that execution is much faster than the passage of
time, so steps that occur at different simulated times are considered
not to race, and will not be reversed.

Exploring all possible schedules is possible with the right
parameters, but not usually advisable, because it is so very very
expensive. When IOSimPOR is used in a QuickCheck property, then many
test cases will not even have a possibility of provoking a fault,
whatever the schedule; it usually makes more sense to explore a
limited number of schedules each, for a large number of cases, than to
explore a small number of cases very very thoroughly.

Do not be surprised that tests run slowly. Each "test" is running (by
default) 100 different schedules; each run is more costly than a run
using vanilla IOSim, because race conditions and data dependencies
need to be tracked.

The IOSimPOR API
----------------

The common way to use IOSim is to call

```hs
runSimTrace :: forall a. (forall s. IOSim s a) -> Trace a
```

which returns a trace, and then express the property to test as a
predicate on the trace. Since IOSimPOR explores many traces, then the
property to test is given as a continuation instead, which is called
for each trace generated. The common way to use IOSimPOR is thus

```hs
exploreSimTrace :: forall a test. (Testable test, Show a) =>
                   (ExplorationOptions->ExplorationOptions) ->
                   (forall s. IOSim s a) ->
                   (Maybe (Trace a) -> Trace a -> test) ->
                   Property
```

The continuation is the last parameter, with type

```hs
Maybe (Trace a) -> Trace a -> test
```

Here the second parameter of the continuation is the trace that the
property should check. The first parameter simply provides information
that may be useful for debugging---if present, it is a similar trace
that passed the same test, from which the failing trace was derived by
reversing one race.

IOSimPOR Options
----------------

`exploreSimTrace` can be controlled by a number of options, which are
obtained by passing the function passed as the first parameter to a
default set of options. Passing the identity function id as the first
parameter uses the defaults.

The options are:

 - the schedule bound, default 100, set by withScheduleBound. This is
   an upper bound on the number of schedules with race reversals that
   will be explored; a bound of zero means that the default schedule
   will be explored, but no others. Setting the bound to zero makes
   IOSimPOR behave rather like IOSim, in that only one schedule is
   explored, but (a) IOSimPOR is considerably slower, because it still
   collects information on potential races, and (b) the IOSimPOR
   schedule is different (based on priorities, in contrast to IOSim's
   round-robin), and plays better with shrinking.

 - the branching factor, default 3, set by withBranching. This is the
   number of alternative schedules that IOSimPOR tries to run, per
   race reversal. With the default parameters, IOSimPOR will try to
   reverse the first 33 (100 `div` 3) races discovered using the
   default schedule, then (if 33 or more races are discovered), for
   each such reversed race, will run the reversal and try to reverse
   two more races in the resulting schedule. A high branching factor
   will explore more combinations of reversing fewer races, within the
   overall schedule bound. A branching factor of one will explore only
   schedules resulting from a single race reversal (unless there are
   fewer races available to be reversed than the schedule bound).

 - the step time limit in microseconds, default 100000 (100ms), set by
   withStepTimeLimit. A simulation step consists of pure computation
   followed by an IO action of some sort; we expect in most cases that
   the pure computation will be fast. However, a bug might cause the
   pure computation to fall into an infinite loop. This can be
   detected by applying a time limit to the overall test, for example
   using QuickCheck's function within, but if such a time limit is
   exceeded, then no information about the trace-so-far is
   preserved. Instead IOSimPOR can apply a time limit to each step; if
   the time limit is exceeded, then the trace up to that point is
   passed to the continuation, terminated by a TraceLoop
   constructor. The time limit should be large enough that the
   intended pure computations always finish well within that
   time. Note that garbage collection pauses are not included in the
   time limit (which would otherwise need to be considerably longer
   than 100ms to accommodate them), provided the -T flag is supplied to
   the run-time system.

 - the schedule control to replay, default Nothing, set by
   withReplay. When a test fails, exploreSimTrace displays the
   schedule control which provoked it. The schedule control specifies
   how to modify the default schedule to reproduce the failure; by
   saving the schedule control and passing it back to exploreSimTrace
   for replay, then it is possible to rerun a failed test with
   additional diagnostics, without repeating the search for a schedule
   that makes it fail.

IOSimPOR Statistics
-------------------

When a tested property passes, IOSimPOR displays some statistics about
the schedules that were actually run. An example of this output is:

```
Branching factor (5865 in total):
45.51% 1
43.00% 0
 6.58% 2
 2.23% 3
 0.85% 4
 0.46% 30-39
 0.38% 5
 0.32% 6
 0.24% 10-19
 0.19% 20-29
 0.15% 8
 0.07% 7
 0.02% 9

Modified schedules explored (100 in total):
22% 100-199
22% 90-99
18% 0
 7% 30-39
 4% 4
 4% 60-69
 4% 80-89
 3% 20-29
 3% 3
 3% 50-59
 3% 70-79
 2% 10-19
 2% 2
 2% 40-49
 1% 1

Race reversals per schedule (5865 in total):
50.04% 1
45.05% 2
 3.10% 3
 1.71% 0
 0.10% 4
```

The first table shows us the branching factor at each point where
IOSimPOR explored alternative schedules: in this case, in 43% of cases
we were at a leaf of the exploration, so no alternative schedules were
explored; 45% of the time we explored one alternative schedule; in a
few cases larger numbers of schedules were explored.

The second table shows us how many schedules we explored for each
test; in this case we used the default bound of 100 alternative
schedules, and we reached that bound in 22% of tests--in the other
tests, there were fewer than 100 possible schedules to explore. In 18%
of the tests, there were no alternative schedules to explore---since
these tests were randomly generated, then these 18% were probably
rather trivial tests.

The final table shows us how many races were reversed in each
alternative schedule; in this case most alternative schedules just
reversed one or two races, but we did reach a maximum of four in a
small number of tests.

IOSimPOR Scheduling
-------------------

IOSimPOR distinguishes between non-racy threads and system threads; test
threads are intended to be part of the test itself, whereas system threads are
part of the system under test. The main thread in a test is always a non-racy
thread; non-racy threads can fork system threads, but not vice versa. Test
threads always take priority over system threads, and IOSimPOR does not
attempt to reverse races involving test threads. So when writing a test, one
can assume that whatever the test threads do takes place first (at each
simulated time point), and then the system threads run and any races between
them are explored. Likewise, if a blocked non-racy thread responds to an event
in a system thread, then the test thread's response will take place before the
system threads continue execution. Distinguishing non-racy threads from system
threads drastically reduces the set of races that IOSimPOR needs to consider.

To start a system thread, a non-racy thread may call

```hs
exploreRaces :: MonadTest m => m ()
```

All threads forked (using forkIO) after such a call will be system
threads. If there is no call to exploreRaces in a test, then all the
threads forked will be non-racy threads, and IOSimPOR will not reverse any
races---so it is necessary to include a call to exploreRaces somewhere
in a test.

IOSimPOR ThreadIds contain a list of small integers; the main thread
contains the list `[]`, its children are numbered `[1]`, `[2]`, `[3]` and so
on, then threads forked by `[2]` are numbered `[2,1]`, `[2,2]`, `[2,3]` and so
on. The id of a forked thread always extends its parent's id, and successively
forked children of the same thread are numbered 1, 2, 3 and so on. Thread Ids
give us useful information for debugging; it is easy to see which thread forked
which other, and relatively easy to map thread ids in debugging output to
threads in the source code.

Except when reversing races, IOSimPOR schedules threads using
priorities. Non-racy threads take priority over system threads, but
otherwise priorities are determined by the thread ids: the thread with
the lexicographically greatest thread Id has the highest priority. As
a result, threads behave in a very predictable manner: newly forked
threads take priority over their parent thread, and children forked
later take priority over children forked earlier from the same parent
thread. Knowing these priorities helps in understanding traces.

When IOSimPOR reverses races, this is done by using a 'schedule
control'. The schedule control is displayed in the test output when a
test fails. Here is an example:

```
Schedule control:
  ControlAwait [ScheduleMod (ThreadId [4],0)
                            ControlDefault
                            [(ThreadId [1],0),
                             (ThreadId [1],1),
                             (ThreadId [2],0),
                             (ThreadId [2],1),
                             (ThreadId [3],0)]]

ThreadId [4] delayed at time Time 0s
  until after:
    ThreadId [2]
    ThreadId [3]
```

The schedule control consists of one or more schedule modifications,
each of which specifies that an event should be delayed until after a
sequence of other events. The events consist of a thread id and a
per-thread event number. The interpretation of the schedule
modification above is that when the default scheduler would have
performed the first event in thread `[4]` (that is, (`ThreadId [4],0`)),
then it should be delayed, and the list of events in the third field
of the schedule modification should be performed first. In this case
there is only one schedule modification, and so only one event is
delayed, but in general a schedule control may delay a series of
events. A failing test can be replayed by using withReplay to supply
the schedule control as an option to exploreSimTrace.

The final part of the output tells us how the schedule in the failing
test differed from the schedule in the most similar passing test:
namely, thread `[4]` was delayed (at simulated time 0) until after the
events in threads `[2]` and `[3]`. Since the schedule control delays
thread `[4]` until after steps in threads `[1]`, `[2]` and `[3]`, then we can
conclude that the last passing test already delayed thread `[4]` until
after the events in thread `[1]`.

Potential IOSimPOR Errors
-------------------------

Most reported errors are a result of faults in the code under test,
but if you see an error of the following form:

```
Exception:
  Can't follow ControlFollow [(ThreadId [5],0)] []
    tid: ThreadId [5]
    tstep: 0
    runqueue: [ThreadId [3],ThreadId [2],ThreadId [1]]
```

then the probable cause is a bug in IOSimPOR itself. The message
indicates that IOSimPOR scheduler is trying to follow a schedule
modification that specifies that thread `[5]` should run next, but this
is impossible because thread `[5]` is not in the `runqueue` (the list of
runnable threads). If you supplied a schedule control explicitly,
using `withReplay`, then you may perhaps have supplied a schedule
control that does not match the version of the code you are running:
in this case the exception is your fault. But if this message appears
while you are exploring races, then it indicates a problem in
IOSimPOR's dependency analysis: IOSimPOR has constructed a schedule as
a result of race reversal that tries to run thread `[5]` at this point,
because the dependency analysis indicates that thread `[5]` ought to be
runnable---but it is not.

Another similar instance of such a problem is the following assertion failure:

```
InternalError "assertion failure: Thread {4} not runnable"
assertion failure: Thread {4} not runnable
```

This indicates that IOSimPOR was following a schedule where the next scheduled
thread was thread 4. However, this thread does not exist in the `runqueue`,
possibly because it was blocked and not unblocked before being scheduled
again.

To debug this, follow these steps:

1. **Get the shrunken test input**: Minimize the test input to simplify the
   failure case.
2. **Retrieve the failing schedule**: Obtain the failing Schedule control the
   failure case.
3. **Create a unit test**: Use the failing input in a unit test and run it
   with `explorationDebugLevel = 2` to display the races in each scheduled
   run.
4. **Analyze the output**: Manually review the output, focusing on the
   schedule that leads to the failure. Look for the schedule in the
   `RacesFound` log messages.
5. **Examine the faulty schedule**: Investigate the trace of the identified
   schedule to understand why it causes the failure, i.e. find the race which
   leads to the error.
6. **Implement the fix**: Correct the identified issue.

Most likely, the root cause lies within the vector clocks logic or the
`updateRaces` function, particularly in the management of the
`stepInfoConcurrent` and `stepInfoNonDep` sets, which are crucial for race
discovery. So analyse `updateRaces` function along the execution trace to see
if everything makes sense.
