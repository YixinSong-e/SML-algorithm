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
val m = getInt();
val arr = Array.array(n,0);
val anarr = Array2.array(n,n,0);
fun read(m) = let
  val fk = Array.update(arr,m,getInt())
in 0 end;
val k = List.tabulate(n,read);
fun get(x) = let
  val k = Array.sub(arr,x)
  val m = List.tabulate(x+1,fn y => if(x = 0) then let val m =
    Array2.update(anarr,y,x,k) in k end
     else let 
    val kk = Array2.sub(anarr,y,x-1)
    val ms = Int.max(k,kk)
    val mm = Array2.update(anarr,y,x,ms)
    in ms end)
   in m end;
val dai = List.tabulate(n,get);
fun pr(x) = let
  val a = getInt() - 1
  val b = getInt() - 1
  val c = Array2.sub(anarr,a,b)
  val m = printInt(c)
in c end;
val z = List.tabulate(m,pr);
val cc = Array2.sub(anarr,0,0);

(*****End*****)



