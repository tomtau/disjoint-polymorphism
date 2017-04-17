--> "I am a duck, I can fly, I can swim"


type Swimming = { swim : T -> String }
trait swimming { self =>
  def swim() = "I can swim"
}


type Flying = { fly : T -> String }
trait flying { self =>
  def fly() = "I can fly"
}

type Bird = { name : String }
trait duck inherits swimming & flying { self =>
  def name = "I am a duck"
}

val superDuck = new[Swimming & Flying & Bird] duck
main = superDuck.name ++ ", " ++ superDuck.fly() ++ ", " ++ superDuck.swim()
