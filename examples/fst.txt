--> true

def fst A [B*A] (x : A&B) : A = x

def snd A [B*A] (x : A&B) : B = x

main = snd Double Bool (1,,true)
