use "random"
use "time"
use "term"
use "promises"

class Handler is ReadlineNotify
  var _answer: U64
  var _out: OutStream

  new create(answer: U64, out: OutStream) =>
    _answer = answer
    _out = out

  fun ref apply(line: String, prompt: Promise[String]) =>
    match line
      | "quit" => prompt.reject()
      | _answer.string() => _out.print("Correct!"); prompt.reject()
    else
      try
        let guess = line.u64()?
        if guess < _answer then
          prompt("Too low, guess again > ")
        else
          prompt("Too high, guess again > ")
        end
      else
        prompt("That's not an unsigned 64-bit int, guess again > ")
      end
    end

actor Main
  new create(env: Env) =>
    let rand = Rand
    let t1 = Time.now()._1.u64()
    let t2 = Time.now()._2.u64()
    let r = rand.create(t1, t2)
    let x = r.int(100)

    env.out.print("(Hint: the number is " + x.string() + ")")

    // Building a delegate manually
    let handler: ReadlineNotify iso = recover Handler(x, env.out) end
    let term = ANSITerm(Readline(consume handler, env.out), env.input)
    term.prompt("Guess > ")

    let notify = object iso
      let term: ANSITerm = term
      fun ref apply(data: Array[U8] iso) => term(consume data)
      fun ref dispose() => term.dispose()
    end

    env.input(consume notify)
