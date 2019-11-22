use "random"
use "time"
use "term"
use "promises"

primitive Correct
primitive TooLow
primitive TooHigh
primitive Invalid

type GuessState is (Correct | TooLow | TooHigh | Invalid)

class Oracle
  var _secret: U64

  new create(max: U64) =>
    (var secs, var nanos) = Time.now()
    let r = Rand.create(secs.u64(), nanos.u64())
    _secret = r.int(max)

  fun check_guess(input: String): GuessState =>
    try
      match input.u64()?
        | _secret => Correct
        | let guess: U64 if guess < _secret => TooLow
        | let guess: U64 if guess > _secret => TooHigh
      else
        Invalid
      end
    else
      Invalid
    end

  fun secret(): U64 =>
    _secret

class UIHandler is ReadlineNotify
  var _out: OutStream
  var _orc: Oracle val
  var _i: U64 = 0

  new create(orc: Oracle val, out: OutStream) =>
    _out = out
    _orc = orc

  fun ref apply(line: String, prompt: Promise[String]) =>
    if line == "quit" then
        prompt.reject()
    else
        _i = _i + 1
        match _orc.check_guess(line)
          | Correct =>
              let plural = if _i > 1 then "es" else "" end
              _out.print("You guessed it! (" + _i.string() + " guess" + plural + ")")
              prompt.reject()
          | TooLow => prompt("Too low, guess again > ")
          | TooHigh => prompt("Too high, guess again > ")
          | Invalid => prompt("That's not an unsigned 64-bit int, guess again > ")
        end
    end

actor Main
  new create(env: Env) =>
    let orc = recover val Oracle(100) end
    env.out.print("(Hint: the number is " + orc.secret().string() + ")")

    let handler = recover UIHandler(orc, env.out) end
    let term = ANSITerm(Readline(consume handler, env.out), env.input)
    term.prompt("Guess > ")

    let notify = object iso
      let term: ANSITerm = term
      fun ref apply(data: Array[U8] iso) => term(consume data)
      fun ref dispose() => term.dispose()
    end

    env.input(consume notify)
