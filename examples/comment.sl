--> "Have fun!4.0"

type Comment = { content : String };
trait comment(content : String) { self : Comment =>
  content = content
};


type Up = { upvotes : Double };
trait up(upvotes : Double) { self : Up =>
  upvotes = upvotes
};

test = new[Comment & Up] comment("Have fun!") & up(4);
main = test.content ++ (test.upvotes).toString
