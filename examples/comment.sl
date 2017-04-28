--> "Have fun!4.0"

type Comment = { content : T -> String }
trait comment(content : String) { self : Comment =>
  def content() = content
}


type Up = { upvotes : T -> Double }
trait up(upvotes : Double) { self : Up =>
  def upvotes() = upvotes
}

val test = new[Comment & Up] comment("Have fun!") & up(4)
main = test.content() ++ (test.upvotes()).toString
