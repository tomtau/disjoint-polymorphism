--> "logging..."

def extend S [U*S] (first : S) (second : U) : S&U = first ,, second

type Person   = {name : String, male : Bool}
type Loggable = {log : T -> String}
trait person(n : String, s : Bool) { self =>
  def name = n
  def male = s
}
trait consoleLogger { self =>
  def log() = "logging..."
}

val jim = new[Person & Loggable] person("jim", true) & consoleLogger


type Employee = {name : String, male : String}
trait employee(n : String, s: String) { self =>
  def name = n
  def male = s
}
-- The following doesn't type check
-- val fool = new[Employee & Person] employee("Tom", "yes") & person("Jim", true)

-- The following type checks
-- Though the type looks ugly
val fool = new[(T & {male : String}) & Person] employee("Tom", "yes") \ { name : String } & person("Jim", true)

main = jim.log ()


