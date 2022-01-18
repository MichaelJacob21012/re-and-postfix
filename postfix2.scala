// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// let- and right-associativity
abstract class Assoc
case object RA extends Assoc
case object LA extends Assoc

// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

// (8) Implement the extended version of the shunting yard algorithm.
// This version should properly account for the fact that the power 
// operation is right-associative. Apart from the extension to include
// the power operation, you can make the same assumptions as in 
// basic version.

 def is_op(op: String) : Boolean = {
  if (ops.contains(op)){
    true
  }
  else {
    false
  }
}

 def precRA(op1: String, op2: String) : Boolean = {
   if (precs(op1) < precs(op2)){
     true
   }
   else {
     false
   }
 }
  def precLA(op1: String, op2: String) : Boolean = {
   if (precs(op1) <= precs(op2)){
     true
   }
   else {
     false
   }
 }
 def prec(op1: String, op2: String) : Boolean = assoc(op1) match{
   case RA => precRA(op1,op2)
   case _ => precLA(op1,op2)
 }

 def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = {
   if (toks == Nil){
    out:::st
  }
  else if (is_op(toks.head) == true){
    if (st == Nil || (st.head == "(") ){
      syard(toks.tail,toks.head::st,out)
    }
    else{
      if (prec(toks.head,st.head) == true){
       syard(toks,st.tail,out:::List(st.head))
      }
      else{
        syard(toks.tail,toks.head::st,out)
      }
    }
  }
  else if (toks.head == "("){
    syard(toks.tail,toks.head::st,out)
  }
  else if (toks.head == ")"){
     if (st.head == "("){
      syard(toks.tail,st.tail,out)
    }
    else {
      syard(toks,st.tail,out:::List(st.head))
    }
  }
  else {
     syard(toks.tail,st,out:::List(toks.head))
  }
}


// test cases
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


// (9) Implement a compute function that produces a Long(!) for an
// input list of tokens in postfix notation.

def compute(toks: Toks, st: List[Long] = Nil) : Long = {
   if (toks == Nil){
     st.head
   }
   else if (is_op(toks.head) == true){
     val operand1 = st.head
     val operand2 = st.tail.head
     val operator = toks.head
     if (operator == "+"){
       compute (toks.tail, (operand1 + operand2).toLong :: st.drop(2))
     }
     else if (operator == "-"){
       compute (toks.tail, (operand2 - operand1).toLong :: st.drop(2))
     }
     else if (operator == "*"){
       compute (toks.tail, (operand1 * operand2).toLong :: st.drop(2))
     }
     else if (operator == "/"){
       compute (toks.tail, (operand2 / operand1).toLong :: st.drop(2))
     }
     else {
       compute (toks.tail, (math.pow(operand2, operand1)).toLong :: st.drop(2))
     }
   }
   else {
     compute(toks.tail,toks.head.toLong::st)
   }
}



// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

