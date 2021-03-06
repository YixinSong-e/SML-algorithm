val input = TextIO.openIn("input.txt");

fun printInt (a:int) =
    print(Int.toString(a)^" ");

fun printIntInf (a:IntInf.int) =
    print(IntInf.toString(a)^" ");


fun printReal (a:real) =
    print(Real.toString(a)^" ");

fun printString (a:string) =
    print(a^" ");

fun getInt () =
    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input);

fun getIntInf () =
    Option.valOf (TextIO.scanStream (IntInf.scan StringCvt.DEC) input);

fun getReal () =
    Option.valOf (TextIO.scanStream (Real.scan) input);

fun printEndOfLine () =
    print("\n");

fun printIntTable ( [] ) = ()
  | printIntTable ( x::xs ) = 
    let
	val tmp = printInt(x)
    in
	printIntTable(xs)
    end;

fun printIntInfTable ( [] ) = ()
  | printIntInfTable ( x::xs ) = 
    let
	val tmp = printIntInf(x)
    in
	printIntInfTable(xs)
    end;

fun getIntTable ( 0 ) = []
  | getIntTable ( N:int) = getInt()::getIntTable(N-1);

fun getIntInfTable ( 0 ) = []
  | getIntInfTable ( N:int) = getIntInf()::getIntInfTable(N-1);

fun getIntVector ( 0 ) =  Vector.fromList []
  | getIntVector ( N:int) = Vector.fromList(getIntTable(N));

fun getIntInfVector ( 0 ) = Vector.fromList []
  | getIntInfVector ( N:int) = Vector.fromList(getIntInfTable(N));


(*****Begin*****)
val n = getInt();
val lst = getIntTable(n);
val stack = Array.array(n+1,0);
val head = Array.array(1,0);
val tail = Array.array(1,0);
val pairs = ListPair.zip(List.tabulate(n,fn x=>x),lst);
fun trversal(s,n,ans) = if(s=n) then ans else 
let
  val cu = List.nth(lst,s)
  val hd = Array.sub(head,0)
  val tl = Array.sub(tail,0)
  val aaans = Array.array(1,ans)
  val mmm = if(cu = 0) then 
    let
      val m = Array.update(stack,hd+1,s)
      val uph = Array.update(head,0,hd+1)
    in 0 end
else if(tl = hd) then 0 else
 let
   
   val toppos = Array.sub(stack,hd)
   
   val upans = Array.update(aaans,0,Int.max(ans,s-toppos+1))
   val uph = Array.update(head,0,hd-1)
 in 0 end
  val aans = Array.sub(aaans,0)
in trversal(s+1,n,aans) end;
val anss = trversal(0,n,0);
printInt(anss);
(*****End*****)







